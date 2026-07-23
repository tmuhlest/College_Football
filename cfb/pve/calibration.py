"""Phase 1.5: Empirical calibration of year-to-year projection constants.

Estimates two constants used in projection.py:
  - regression_factor: fraction of a player's PER-PLAY rate deviation from the
    position mean that persists to the following season (Kelley coefficient)
  - sigma_rate: year-to-year per-play rate noise (EPA/play) not captured by the
    posterior width alone (injuries, teammate turnover, development). Projection
    scales this by each player's projected volume: yr_component = sigma_rate x
    proj_snaps.

METHOD (rate scale, same-team panel, exposure-weighted OLS):
  For each consecutive season pair in the lookback window, build a same-player,
  same-team, same-position panel of ok/thin players and regress per-play rate
  deviations:
    rate = season_points_added / snaps_est
    (rate_t+1 - pos_mean_t+1) ~ slope * (rate_t - pos_mean_t)
  weighted by effective exposure w = (n_t * n_t1) / (n_t + n_t1), so
  low-snap pairs (whose observed rates are mostly measurement noise) do not
  drag the estimates. Slope → regression_factor, weighted residual SD →
  sigma_rate. Weighted average across year-pairs (by panel size).

  WHY RATE, NOT CUMULATIVE: cumulative season points year-over-year residuals are
  dominated by VOLUME changes (a same-team QB going 100 → 500 snaps swings
  cumulative EPA hugely at an unchanged per-play rate). Calibrating on
  cumulative residuals produced a flat ~55 EPA sigma floor for every QB
  regardless of data richness. Rate residuals isolate per-play skill drift;
  volume risk is an explicit, documented projection assumption instead.

  WHY SAME-TEAM: cross-team movers absorb team-environment variance (new OL,
  scheme, receivers) into sigma. The transfer flag in project_points() (1.5x
  inflation) already prices cross-team uncertainty separately.

CALIBRATION CAVEATS (V1.5):
  - Rates are computed from raw observed points / estimated snaps, not posterior
    player_offset draws — residual team-effect noise (OC change, OL turnover)
    remains in sigma_rate. Replace with posterior-based calibration once
    multiple historical model fits exist.
  - Exposure weighting reduces, but does not eliminate, errors-in-variables
    attenuation of the slope from noisy low-snap rates.

PORTAL ERA BOUNDARY:
  Unlimited transfer portal began for the 2021 season. Data before 2021 represents
  a different player mobility regime and is excluded (min_season default = 2021).

OUTPUT:
  models/artifacts/projection_constants.json
  {
    "calibration_seasons": [2021, 2022, 2023, 2024],
    "by_position": {
      "QB": {"regression_factor": 0.55, "sigma_rate": 0.07, "n_obs_total": 210},
      ...
    },
    "pooled_regression_factor": 0.62,
    "pooled_sigma_rate": 0.08
  }
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd

ARTIFACTS_DIR = Path(__file__).parent.parent / "models" / "artifacts"
PROCESSED_DIR = Path(__file__).parent.parent / "data" / "processed"

POSITIONS = ["QB", "RB", "WR", "TE"]
_MIN_PANEL_OBS = 5  # skip a (position, year-pair) cell with fewer matched players


def _load_season_points(season: int) -> pd.DataFrame | None:
    """Load processed points parquet, pulling from CFBD if not cached."""
    path = PROCESSED_DIR / f"player_points_{season}.parquet"
    if path.exists():
        return pd.read_parquet(path)
    print(f"  Pulling points data for {season}...")
    try:
        from pve.ingest import run_ingest
        run_ingest([season])
        if path.exists():
            return pd.read_parquet(path)
    except Exception as exc:
        print(f"  WARNING: Could not pull {season} data: {exc}")
    return None


def _ols_regression(
    x: np.ndarray, y: np.ndarray, w: np.ndarray | None = None
) -> tuple[float, float]:
    """Weighted OLS: y ~ slope * x (no intercept — deviations already mean-centered).

    w = per-observation weights (effective exposure). Low-snap players' observed
    rates are mostly measurement noise; weighting keeps them from dragging both
    the slope (errors-in-variables attenuation) and the residual SD.
    Returns (slope, residual_sd) where residual_sd is the weighted residual SD.
    """
    if len(x) < 2:
        return (0.0, float(np.std(y)) if len(y) > 0 else 0.0)
    if w is None:
        w = np.ones_like(x)
    slope = float(np.sum(w * x * y) / np.sum(w * x * x))
    resid = y - slope * x
    resid_sd = float(np.sqrt(np.sum(w * resid ** 2) / np.sum(w)))
    return slope, resid_sd


def _fit_year_pair(
    df_t: pd.DataFrame,
    df_t1: pd.DataFrame,
) -> dict[str, dict]:
    """Run weighted OLS on a single season→season transition for each position.

    Panel: same player, same team (no transfers), same position, ok/thin both
    seasons. Regression is on PER-PLAY rates (season_points_added / snaps_est),
    weighted by effective exposure w = (n_t * n_t1) / (n_t + n_t1).

    Same-team filtering removes cross-team environment changes (new scheme,
    new supporting cast) — those are priced separately by the 1.5x transfer
    flag in project_points(). Rate scale removes volume swings (role changes)
    that dominated the old cumulative-points residuals.

    Returns {position: {slope, resid_sd, n_obs}} with resid_sd in EPA/play.
    """
    # keep ok/thin players from both seasons
    df_t  = df_t[df_t["data_status"].isin({"ok", "thin"})].copy()
    df_t1 = df_t1[df_t1["data_status"].isin({"ok", "thin"})].copy()

    # merge on player_id; include team + snaps from both sides
    panel = df_t.merge(
        df_t1[["player_id", "season_points_added", "position", "team", "snaps_est"]],
        on="player_id",
        suffixes=("_t", "_t1"),
    )
    # keep rows where position is consistent AND player stayed on same team
    panel = panel[
        (panel["position_t"] == panel["position_t1"])
        & (panel["team_t"] == panel["team_t1"])
    ].copy()
    panel["position"] = panel["position_t"]

    # per-play rates; guard against zero/missing snaps
    for side in ("_t", "_t1"):
        snaps = pd.to_numeric(panel[f"snaps_est{side}"], errors="coerce")
        panel[f"snaps{side}"] = snaps
        panel[f"rate{side}"] = panel[f"season_points_added{side}"] / snaps.clip(lower=1.0)
    panel = panel.dropna(subset=["rate_t", "rate_t1", "snaps_t", "snaps_t1"])

    results = {}
    for pos in POSITIONS:
        sub = panel[panel["position"] == pos]
        if len(sub) < _MIN_PANEL_OBS:
            continue
        # exposure weights: harmonic-mean-style effective play count per pair
        n_t  = sub["snaps_t"].to_numpy(dtype=float)
        n_t1 = sub["snaps_t1"].to_numpy(dtype=float)
        w = (n_t * n_t1) / (n_t + n_t1)

        # weighted position means so deviations are centered on the same scale
        rate_t  = sub["rate_t"].to_numpy(dtype=float)
        rate_t1 = sub["rate_t1"].to_numpy(dtype=float)
        pos_mean_t  = float(np.average(rate_t,  weights=w))
        pos_mean_t1 = float(np.average(rate_t1, weights=w))
        dev_t  = rate_t  - pos_mean_t
        dev_t1 = rate_t1 - pos_mean_t1

        slope, resid_sd = _ols_regression(dev_t, dev_t1, w=w)
        slope = float(np.clip(slope, 0.0, 1.0))

        # ── Measurement-error correction ────────────────────────────────────
        # Observed rates carry sampling noise ~ s²/n (s = per-play EPA sd), so
        # the raw residual conflates true year-to-year process noise with
        # measurement noise — badly for WR/TE, whose per-target rates rest on
        # 30-100 involvement plays. The squared residual decomposes linearly:
        #   E[resid²_i] = process_var + s² · c_i,   c_i = 1/n_t1 + slope²/n_t
        # so a weighted regression of resid² on c_i identifies process_var
        # (intercept) and s² (slope) simultaneously.
        resid = dev_t1 - slope * dev_t
        c = 1.0 / n_t1 + (slope ** 2) / n_t
        sw = np.sqrt(w)
        design = np.vstack([np.ones_like(c), c]).T * sw[:, None]
        coef, *_ = np.linalg.lstsq(design, (resid ** 2) * sw, rcond=None)
        process_var, s2 = float(coef[0]), float(coef[1])

        if s2 <= 0:
            # Degenerate decomposition — fall back to the uncorrected residual.
            process_var = resid_sd ** 2
            s2 = 0.0
        # Floor at 0.01 EPA/play so a lucky decomposition can't zero-out risk.
        sigma_process = float(np.sqrt(max(process_var, 1e-4)))

        # Errors-in-variables disattenuation: measurement noise in the year-t
        # rates biases the OLS slope toward 0 by the reliability ratio
        # lambda = true_var / observed_var. Guard the ratio and re-clamp.
        var_dev_t = float(np.average(dev_t ** 2, weights=w))
        meas_t = s2 * float(np.average(1.0 / n_t, weights=w))
        lam = (var_dev_t - meas_t) / var_dev_t if var_dev_t > 0 else 1.0
        lam = float(np.clip(lam, 0.2, 1.0))
        slope_corrected = float(np.clip(slope / lam, 0.0, 1.0))

        results[pos] = {
            "slope": slope_corrected,
            "resid_sd": sigma_process,
            "n_obs": len(sub),
        }
    return results


def compute_projection_constants(
    end_season: int,
    lookback: int = 3,
    min_season: int = 2021,
) -> dict:
    """Calibrate year-to-year regression constants from historical season pairs.

    Parameters
    ----------
    end_season : int
        Last season included in the calibration window (typically the season
        immediately before the first training season, e.g. 2024 when training
        on [2024, 2025]).
    lookback : int
        Number of season-to-season transitions to include (default 3).
    min_season : int
        Hard floor on the calibration window — excludes pre-portal-era data
        (default 2021, the first unlimited-transfer-portal season).

    Returns the constants dict and saves to models/artifacts/projection_constants.json.
    """
    first_season = max(end_season - lookback, min_season)
    seasons = list(range(first_season, end_season + 1))
    print(f"  Calibration window: {seasons[0]}–{seasons[-1]}  "
          f"({len(seasons) - 1} transition pair(s))")

    # Load points data for all seasons in the window
    season_data: dict[int, pd.DataFrame] = {}
    for s in seasons:
        df = _load_season_points(s)
        if df is not None:
            season_data[s] = df
        else:
            print(f"  WARNING: Skipping {s} — data unavailable.")

    # Collect OLS results per (year-pair, position)
    per_position_results: dict[str, list[dict]] = {pos: [] for pos in POSITIONS}
    for i in range(len(seasons) - 1):
        s_t, s_t1 = seasons[i], seasons[i + 1]
        if s_t not in season_data or s_t1 not in season_data:
            print(f"  WARNING: Skipping transition {s_t}→{s_t1} — missing data.")
            continue
        print(f"  Fitting {s_t}→{s_t1}...", end=" ")
        pair_results = _fit_year_pair(season_data[s_t], season_data[s_t1])
        total_obs = sum(v["n_obs"] for v in pair_results.values())
        print(f"{total_obs} matched players across positions")
        for pos, stats in pair_results.items():
            per_position_results[pos].append(stats)

    if not any(per_position_results.values()):
        print("  ERROR: No calibration data available. Returning hardcoded defaults.")
        return _hardcoded_defaults()

    # Weighted average across year-pairs for each position
    by_position: dict[str, dict] = {}
    all_slopes, all_sigmas, all_n = [], [], []

    for pos in POSITIONS:
        entries = per_position_results[pos]
        if not entries:
            continue
        weights = np.array([e["n_obs"] for e in entries], dtype=float)
        slopes  = np.array([e["slope"]    for e in entries])
        sigmas  = np.array([e["resid_sd"] for e in entries])

        weighted_slope = float(np.average(slopes, weights=weights))
        weighted_sigma = float(np.average(sigmas, weights=weights))
        n_total        = int(weights.sum())

        by_position[pos] = {
            "regression_factor": round(weighted_slope, 3),
            "sigma_rate":        round(weighted_sigma, 4),
            "n_obs_total":       n_total,
            "n_transitions":     len(entries),
        }
        all_slopes.append(weighted_slope)
        all_sigmas.append(weighted_sigma)
        all_n.append(n_total)

    all_n_arr = np.array(all_n, dtype=float)
    pooled_regression = float(np.average(all_slopes, weights=all_n_arr))
    pooled_sigma_rate = float(np.average(all_sigmas, weights=all_n_arr))

    constants = {
        "calibration_seasons": seasons,
        "by_position": by_position,
        "pooled_regression_factor": round(pooled_regression, 3),
        "pooled_sigma_rate": round(pooled_sigma_rate, 4),
    }

    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    out_path = ARTIFACTS_DIR / "projection_constants.json"
    with open(out_path, "w") as f:
        json.dump(constants, f, indent=2)

    # Print summary (cumulative-equivalent shown at a 300-play reference season)
    print(f"\n  Calibrated projection constants → {out_path}")
    print(f"  {'Position':<8} {'Reg. Factor':>12} {'Sigma (EPA/play)':>17} {'~EPA @300 plays':>16} {'N obs':>8}")
    print("  " + "-" * 65)
    for pos, v in by_position.items():
        print(f"  {pos:<8} {v['regression_factor']:>12.3f} {v['sigma_rate']:>17.4f} "
              f"{v['sigma_rate'] * 300:>16.1f} {v['n_obs_total']:>8}")
    print(f"  {'Pooled':<8} {pooled_regression:>12.3f} {pooled_sigma_rate:>17.4f} "
          f"{pooled_sigma_rate * 300:>16.1f}")

    return constants


def _hardcoded_defaults() -> dict:
    """Fallback when calibration data is unavailable.

    Carries only the legacy cumulative "pooled_sigma" — projection.py detects
    the absence of sigma_rate keys and falls back to the flat cumulative path.
    """
    return {
        "calibration_seasons": [],
        "by_position": {},
        "pooled_regression_factor": 0.62,
        "pooled_sigma": 27.9,
    }


def load_projection_constants() -> dict:
    """Load calibrated constants from JSON, falling back to hardcoded defaults."""
    path = ARTIFACTS_DIR / "projection_constants.json"
    if path.exists():
        with open(path) as f:
            return json.load(f)
    return _hardcoded_defaults()
