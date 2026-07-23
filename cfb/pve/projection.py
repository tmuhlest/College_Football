"""Phase 3: Posterior predictive projection layer.

V2 — the model estimates per-play rates; this layer converts back to
cumulative-EPA projections for the decision layer.

For each player in the fitted model:
  - Extract posterior samples of their per-play rate: alpha[position] +
    team_effect[team] + beta_class[next class] + player_offset[player].
    The class effect uses the player's NEXT year class (FR->SO, SO->JR, ...)
    so the development curve is applied going forward — this is the aging curve.
    The year effect is evaluated at 0 (the zero-sum average training
    environment) since the future season's environment is unknown.
  - Multiply by proj_snaps — the player's most-recent-season estimated
    involvement plays — to get a cumulative-EPA projection. THIS IS AN EXPLICIT
    USAGE ASSUMPTION: the projection assumes the player repeats last season's
    volume. For a role change (backup -> starter or vice versa), the GM should
    override via manual points. proj_snaps and proj_points_per_play are exported so the
    assumption is auditable.
  - For transfers: inflate the predictive SD by TRANSFER_SIGMA_INFLATION to
    represent environment-portability uncertainty (D8 — part of the rate came
    from their old scheme/QB/line; we don't know how much).
  - Regression to the position prior is implicit in the hierarchical structure:
    players with few plays have player_offset pulled toward 0 automatically —
    the exposure likelihood leaves their rate weakly identified, so the
    posterior stays close to the position/team/class baseline.

PROJECTION CONSTANTS:
  Year-to-year regression factor and sigma are loaded at runtime from
  models/artifacts/projection_constants.json (written by pve/calibration.py).
  Position-specific values are used when available; the pooled fallback is used
  for any position not represented in the calibration data.
  Hardcoded fallbacks apply only when the JSON file does not exist yet.

Output: data/processed/projected_points.parquet — the clean seam between the shared
points-added engine (Part A) and the team-specific decision layer (Part B).
"""

from __future__ import annotations

import json
from pathlib import Path

import arviz as az
import numpy as np
import pandas as pd

from pve.calibration import load_projection_constants

PROCESSED_DIR = Path(__file__).parent.parent / "data" / "processed"
PROJECTION_META_FILENAME = "projected_points.meta.json"
TRANSFER_SIGMA_INFLATION = 1.5

# Hardcoded fallbacks — used only if projection_constants.json is absent.
# These are empirically derived from 2023→2024 CFBD data (notebook 02).
# Run train_engine.py to compute multi-year calibrated values.
YEAR_TO_YEAR_REGRESSION: float = 0.62   # pooled empirical slope (2023→2024)
YEAR_TO_YEAR_SIGMA: float = 27.9        # pooled empirical residual SD, EPA

# Aging curve: each player is projected at their NEXT class year.
# GS stays GS (eligibility is flagged separately); UNK stays UNK.
_NEXT_CLASS = {"FR": "SO", "SO": "JR", "JR": "SR", "SR": "GS", "GS": "GS", "UNK": "UNK"}


def extract_player_posterior(
    idata: az.InferenceData,
    fit_df: pd.DataFrame,
    idx: dict,
) -> pd.DataFrame:
    """Compute per-player posterior summaries from the fitted rate model.

    The player's per-play rate posterior is
      rate = alpha[position] + team_effect[team] + beta_class[next class]
             + player_offset[player]
    (year_effect evaluated at 0 = average training environment; opp_quality at
    0 = average schedule — appropriate for an unknown future season).

    Cumulative-EPA summaries are rate x proj_snaps, where proj_snaps is the
    player's most-recent-season estimated involvement plays (explicit usage
    assumption, exported for auditability).

    Returns one row per unique player in fit_df with fitted_points_mean, fitted_points_sd,
    player_offset_points (all cumulative EPA), plus proj_snaps and fitted_rate_mean.
    """
    posterior = idata.posterior
    alpha       = posterior["alpha"].values        # (chain, draw, n_pos)
    team_effect = posterior["team_effect"].values  # (chain, draw, n_teams)
    player_off  = posterior["player_offset"].values  # (chain, draw, n_players)
    beta_class  = posterior["beta_class"].values   # (chain, draw, n_class)

    positions    = idx["positions"]
    teams        = idx["teams"]
    players      = idx["players"]
    class_levels = idx["class_levels"]

    # One row per unique player; use most recent team/season/snaps if player
    # appears in both training seasons.
    player_rows = (
        fit_df.sort_values("season", ascending=False)
        .drop_duplicates("player_id", keep="first")
        .reset_index(drop=True)
    )

    records = []
    for _, row in player_rows.iterrows():
        pid = str(row["player_id"])
        pos = row["position"]

        pos_i = np.searchsorted(positions, pos)
        player_i = np.searchsorted(players, pid)

        # team effect: use the player's most recent team
        team = row["team"]
        if team in teams:
            team_i = np.searchsorted(teams, team)
            te = team_effect[:, :, team_i]
        else:
            te = np.zeros_like(alpha[:, :, pos_i])

        # Aging curve: project at the player's NEXT class year. class_levels
        # contains only levels observed in training — if the advanced class is
        # unobserved (e.g., SR -> GS with no GS training rows), hold the player
        # at their current class rather than apply a data-free effect. If even
        # the current class is unobserved (e.g., UNK never fit), use no class
        # effect at all.
        curr_class = str(row.get("year_class", "UNK") or "UNK")
        next_class = _NEXT_CLASS.get(curr_class, "UNK")
        if next_class in class_levels:
            class_i = class_levels.index(next_class)
        elif curr_class in class_levels:
            class_i = class_levels.index(curr_class)
        else:
            class_i = None

        # Usage assumption: most-recent-season involvement plays.
        proj_snaps = float(
            pd.to_numeric(pd.Series([row.get("snaps_est")]), errors="coerce")
            .fillna(1.0)
            .clip(lower=1.0)
            .iloc[0]
        )

        # Decompose into baseline (position + team + class) and the player's
        # per-play deviation. Year-to-year regression downstream applies only
        # to the player offset — the baseline is next year's environment.
        class_term = beta_class[:, :, class_i] if class_i is not None else 0.0
        baseline_rate_samples   = (alpha[:, :, pos_i] + te + class_term).ravel()
        player_rate_off_samples = player_off[:, :, player_i].ravel()
        rate_samples            = baseline_rate_samples + player_rate_off_samples

        records.append({
            "player_id":          row["player_id"],
            "player":             row["player"],
            "team":               row["team"],
            "conference":         row["conference"],
            "position":           row["position"],
            "data_status":        row["data_status"],
            "year_class":         curr_class,
            "proj_snaps":         proj_snaps,
            # Un-regressed posterior rate; project_points exports the regressed
            # rate as proj_points_per_play (= proj_points_mean / proj_snaps, exact identity).
            "fitted_rate_mean":   float(np.mean(rate_samples)),
            # Cumulative-EPA scale for the downstream decision layer:
            "fitted_points_mean":        proj_snaps * float(np.mean(rate_samples)),
            "fitted_points_sd":          proj_snaps * float(np.std(rate_samples)),
            "player_offset_points": proj_snaps * float(np.mean(player_rate_off_samples)),
        })

    return pd.DataFrame(records)


def project_points(
    player_summaries: pd.DataFrame,
    transfer_flags: dict[str, bool] | None = None,
    train_seasons: list[int] | None = None,
) -> pd.DataFrame:
    """Apply transfer uncertainty inflation and compute 80%/95% credible intervals.

    transfer_flags: mapping player_id → True if the player is entering the portal
    and changing programs.  Transfers get TRANSFER_SIGMA_INFLATION applied to their
    predictive SD (D8 — environment portability uncertainty).

    train_seasons: the seasons the model was trained on. When provided, a
    metadata sidecar (projected_points.meta.json) is written alongside the
    parquet recording the training window and the projection target season
    (max + 1) so the decision layer can label eligibility/target years without
    hardcoding them.

    Regression factors and sigma are loaded from models/artifacts/projection_constants.json
    (position-specific where calibrated, pooled fallback otherwise). The
    year-to-year aleatory sigma is in RATE units (EPA/play, "sigma_rate") and is
    scaled by each player's projected volume before entering the quadrature —
    so the aleatory term varies by player rather than imposing one flat
    cumulative floor. Older constants files with only a cumulative "sigma" are
    still honored as a legacy fallback.

    The z-multipliers for normal CIs:
      80% CI: ± 1.282 SD
      95% CI: ± 1.960 SD
    """
    if transfer_flags is None:
        transfer_flags = {}

    constants = load_projection_constants()
    by_pos = constants.get("by_position", {})
    default_reg   = constants.get("pooled_regression_factor", YEAR_TO_YEAR_REGRESSION)
    default_sigma = constants.get("pooled_sigma", YEAR_TO_YEAR_SIGMA)

    rows = []
    for _, row in player_summaries.iterrows():
        is_transfer = transfer_flags.get(str(row["player_id"]), False)
        pos = row["position"]

        # Look up position-specific constants; fall back to pooled
        pos_constants = by_pos.get(pos, {})
        reg_factor = pos_constants.get("regression_factor", default_reg)

        proj_snaps = row.get("proj_snaps")

        # Forward-project to next season:
        # 1. Regress player_offset toward 0 (position+team+class baseline) by the
        #    persistence factor. fitted_points_mean = baseline + player_offset, so:
        #    proj_points_mean = baseline + reg_factor * player_offset
        #              = fitted_points_mean - (1 - reg_factor) * player_offset
        player_offset_points = row.get("player_offset_points", 0.0)
        proj_points_mean = row["fitted_points_mean"] - (1.0 - reg_factor) * player_offset_points

        # 2. Predictive SD: combine posterior estimation uncertainty with year-to-year
        #    aleatory uncertainty (injuries, teammate turnover, development).
        #    The aleatory term is calibrated in RATE units (EPA/play, same-team
        #    stayers) and scaled by the projected volume — so it varies by player
        #    instead of imposing one flat cumulative floor on every projection.
        #    Legacy fallback: older constants files carry a cumulative "sigma".
        posterior_sd = row["fitted_points_sd"]
        sigma_rate = pos_constants.get(
            "sigma_rate", constants.get("pooled_sigma_rate")
        )
        if sigma_rate is not None and proj_snaps is not None:
            yr_component = float(sigma_rate) * float(proj_snaps)
        else:
            yr_component = pos_constants.get("sigma", default_sigma)
        proj_points_sd = float(np.sqrt(posterior_sd ** 2 + yr_component ** 2))

        # 3. For transfers: additionally inflate SD for environment-portability uncertainty.
        if is_transfer:
            proj_points_sd *= TRANSFER_SIGMA_INFLATION

        # Projected per-play rate AFTER regression — proj_points_mean = proj_points_per_play
        # x proj_snaps holds exactly, so any projection can be audited by hand.
        proj_points_per_play = (
            proj_points_mean / float(proj_snaps)
            if proj_snaps is not None and float(proj_snaps) > 0
            else None
        )

        rows.append({
            "player_id":      row["player_id"],
            "player":         row["player"],
            "team":           row["team"],
            "conference":     row["conference"],
            "position":       row["position"],
            "data_status":    row["data_status"],
            "year_class":     row.get("year_class", "UNK"),
            "is_transfer":    is_transfer,
            "proj_snaps":     proj_snaps,
            "proj_points_per_play": proj_points_per_play,
            "proj_points_mean":      proj_points_mean,
            "proj_points_sd":        proj_points_sd,
            "proj_points_ci_80_lo":  proj_points_mean - 1.282 * proj_points_sd,
            "proj_points_ci_80_hi":  proj_points_mean + 1.282 * proj_points_sd,
            "proj_points_ci_95_lo":  proj_points_mean - 1.960 * proj_points_sd,
            "proj_points_ci_95_hi":  proj_points_mean + 1.960 * proj_points_sd,
        })

    df = pd.DataFrame(rows)
    PROCESSED_DIR.mkdir(parents=True, exist_ok=True)
    out_path = PROCESSED_DIR / "projected_points.parquet"
    df.to_parquet(out_path, index=False)
    print(f"Projected points → {out_path}  ({len(df)} players)")

    if train_seasons:
        seasons = sorted(int(s) for s in train_seasons)
        meta = {
            "train_seasons": seasons,
            "season_range": f"{seasons[0]}-{seasons[-1]}",
            "projection_season": seasons[-1] + 1,
        }
        meta_path = PROCESSED_DIR / PROJECTION_META_FILENAME
        meta_path.write_text(json.dumps(meta, indent=2))
        print(f"Projection metadata → {meta_path}  {meta}")

    return df


def load_projections() -> pd.DataFrame:
    path = PROCESSED_DIR / "projected_points.parquet"
    if not path.exists():
        raise FileNotFoundError(f"No projections at {path}. Run train_engine first.")
    return pd.read_parquet(path)


def load_projection_metadata() -> dict | None:
    """Return the projection metadata sidecar, or None if it is absent.

    Keys: train_seasons (list[int]), season_range (str), projection_season (int).
    Absent when projections were produced before metadata was introduced — callers
    should fall back gracefully (e.g., to the current calendar year).
    """
    path = PROCESSED_DIR / PROJECTION_META_FILENAME
    if not path.exists():
        return None
    try:
        return json.loads(path.read_text())
    except (json.JSONDecodeError, OSError):
        return None
