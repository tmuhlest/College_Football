"""Phase 2: Hierarchical Bayesian points-added model (PyMC 5).

V2 MODEL — per-play efficiency with snap-count exposure.

The response is season-total points added (cumulative EPA), but the model now
decomposes it into per-play skill x volume instead of treating the season
total as the atomic skill unit:

  # Likelihood (exposure form):
  points_obs[i] ~ StudentT(nu,  mu    = snaps[i] * rate[i],
                             sigma = sigma_play[pos] * sqrt(snaps[i]))

  # Per-play rate (all effects in EPA-per-involvement-play units):
  rate[i] = alpha[pos] + team_effect[team] + player_offset[player]
            + beta_class[class] + year_effect[season] + beta_opp * opp_quality_z[i]

Structural choices and rationale:

  EXPOSURE MODEL (snaps as offset):
    Season-total EPA bundles per-play efficiency with snap volume. Modeling
    mu = snaps * rate separates "how good per play" from "how much they played."
    Measurement noise on the season total scales as sqrt(snaps) (a sum of
    per-play noise terms), so a 400-play starter pins down their rate ~6x more
    precisely than a 12-play backup — the model now handles thin data through
    the likelihood itself. The former THIN_SIGMA_MULTIPLIER hack is removed.

  POSITION-SPECIFIC sigma_player AND sigma_play:
    Observed per-play rate spread differs by position (2025 data: QB sd 0.13,
    RB 0.10, WR 0.22, TE 0.20 EPA/play — pass catchers are per-target, QBs
    per-dropback). A single pooled sigma_player over-widens TE/RB uncertainty
    and mis-calibrates QB shrinkage. Both scale parameters get dims="position".

  STUDENT-T LIKELIHOOD:
    Season outcomes have outliers (Heisman seasons, injury-shortened years).
    A Normal likelihood lets one extreme season drag the position hyperpriors;
    StudentT with estimated dof (nu ~ Gamma(2, 0.1), mean 20) is robust: if the
    data look Normal, nu goes large and it collapses to the Normal model.

  CLASS-YEAR DEVELOPMENT CURVE (beta_class):
    Class years enter as a zero-sum categorical effect on the per-play rate,
    restricted to levels OBSERVED in the training data (2024-2025 contains
    only FR/SO/JR/SR). This gives thin-data players a better prior anchor
    (a freshman with 15 plays shrinks toward the freshman baseline, not the
    overall mean) and lets the projection layer apply an aging curve by
    advancing each player one class before projecting. When the advanced
    class is unobserved (SR -> GS with no GS training rows), the projection
    holds the player at their current class rather than applying a data-free
    effect. CAVEAT: the fitted class gradient partly reflects survivorship
    (weaker players exit or lose snaps between years), so it can overstate
    pure development; the projection layer's calibrated regression factor
    partially offsets this by shrinking individual deviations.

  YEAR EFFECT (year_effect):
    Seasons enter as a zero-sum effect absorbing league-wide scoring drift
    between training years. Projections evaluate at 0 (the average training
    environment) since the future season's environment is unknown.

NON-CENTERED RATIONALE (unchanged from V1):
  In the centered form, player_offset[p] ~ Normal(0, sigma_player) directly
  scales with sigma_player. When sigma_player is small (or uncertain), the
  joint posterior has a "funnel" geometry — the offset and sigma are tightly
  correlated, creating a space that HMC can barely sample from (divergences,
  high R-hat, slow mixing). The non-centered form separates these:
    player_offset_raw[p] ~ Normal(0, 1)  <- always unit scale
    player_offset[p] = player_offset_raw[p] * sigma_player[pos[p]]
  This is strictly better for sparse hierarchical models (1-2 obs per player).

PRIOR CALIBRATION (per-play EPA units; cumulative equivalents at a 300-play
reference season shown for intuition):
  Observed 2025 per-play rates: QB mean 0.10, RB 0.04, WR 0.24, TE 0.22.
    alpha ~ Normal(0.15, 0.25):     95% interval [-0.34, 0.64] EPA/play —
                                    covers all position means with room
    sigma_conf ~ HalfNormal(0.05):  95th pct ~0.10/play (~30 EPA @300) —
                                    plausible SEC/MAC efficiency gap
    sigma_team ~ HalfNormal(0.04):  95th pct ~0.08/play (~24 EPA @300)
    sigma_player ~ HalfNormal(0.15): 95th pct ~0.30/play — brackets the
                                    observed per-position rate spreads
    sigma_play ~ HalfNormal(1.5):   weakly informative on residual per-play
                                    noise (posterior lands ~0.5-1.0). Note the
                                    sqrt(snaps) scaling assumes iid per-play
                                    noise; within-game correlation would make
                                    the effective sample size somewhat smaller
                                    (mild overdispersion, unmodeled in V2)
    beta_opp ~ Normal(0, 0.05):     1 SD schedule shift = ±15 EPA @300 plays
    beta_class ~ ZeroSumNormal(0.05): class gaps up to ~±0.1/play at 2 sigma
    year_effect ~ ZeroSumNormal(0.03): small league-wide drift between seasons
    nu ~ Gamma(2, 0.1):             mean 20 — data decides tail heaviness

KNOWN CONFOUNDING (not modeled, V1/V2):
  - Scheme and talent are inseparable in team_effect. An Air Raid team_effect
    partly reflects scheme inflation; that component does not transfer.
  - A WR's EPA depends on who is throwing to them (QB quality confounded).
  - snaps_est is an involvement-rate proxy (targets/carries), not true snap
    counts — the exposure variable itself carries measurement error.
  - Future usage is not modeled: projections assume most-recent-season volume.

Players below SNAP_GATE are excluded (data_status == "insufficient"):
their involvement estimates are too unreliable to serve as exposure.
Posterior is saved as ArviZ InferenceData (netCDF) for downstream projection.
"""

from __future__ import annotations

from pathlib import Path

import arviz as az
import numpy as np
import pandas as pd
import pymc as pm

ARTIFACTS_DIR = Path(__file__).parent.parent / "models" / "artifacts"

# Canonical class-year ordering. UNK = missing roster data.
# The model only fits levels actually observed in the training window —
# including unobserved levels in the ZeroSumNormal would create pure-prior
# "slack" components (and break identification of the observed block), and the
# projection layer would then apply data-free effects to real players.
CLASS_LEVELS = ["FR", "SO", "JR", "SR", "GS", "UNK"]


def _build_index_maps(fit_df: pd.DataFrame) -> dict:
    """Build integer index arrays for all categorical variables."""
    positions, pos_idx = np.unique(fit_df["position"].values, return_inverse=True)
    conferences, conf_idx = np.unique(fit_df["conference"].fillna("FCS").values, return_inverse=True)
    teams, team_idx = np.unique(fit_df["team"].values, return_inverse=True)
    players, player_idx = np.unique(fit_df["player_id"].astype(str).values, return_inverse=True)
    seasons, season_idx = np.unique(fit_df["season"].values, return_inverse=True)

    # map each team to its conference index (use most common conference per team)
    team_conf_series = (
        fit_df[["team", "conference"]]
        .fillna({"conference": "FCS"})
        .groupby("team")["conference"]
        .agg(lambda x: x.mode()[0])
    )
    team_conf_idx = np.array([
        np.searchsorted(conferences, team_conf_series[t]) for t in teams
    ])

    # map each unique player to their position index (mode across their rows) —
    # needed because sigma_player is position-indexed
    player_pos_series = (
        fit_df.assign(_pid=fit_df["player_id"].astype(str))
        .groupby("_pid")["position"]
        .agg(lambda x: x.mode()[0])
    )
    player_pos_idx = np.array([
        np.searchsorted(positions, player_pos_series[p]) for p in players
    ])

    # class-year index per observation; anything outside the known levels -> UNK.
    # Levels are restricted to those OBSERVED in the fit data (canonical order
    # preserved): a ZeroSumNormal over unobserved levels would produce pure-prior
    # slack components that the projection layer would apply to real players.
    yc = fit_df["year_class"].fillna("UNK").astype(str)
    yc = yc.where(yc.isin(CLASS_LEVELS), "UNK")
    observed_set = set(yc)
    class_levels = [c for c in CLASS_LEVELS if c in observed_set]
    class_idx = np.array([class_levels.index(c) for c in yc])

    return {
        "positions": positions, "pos_idx": pos_idx,
        "conferences": conferences, "conf_idx": conf_idx,
        "teams": teams, "team_idx": team_idx,
        "players": players, "player_idx": player_idx,
        "team_conf_idx": team_conf_idx,
        "player_pos_idx": player_pos_idx,
        "class_levels": class_levels, "class_idx": class_idx,
        "seasons": seasons, "season_idx": season_idx,
    }


def build_model(df: pd.DataFrame) -> tuple[pm.Model, pd.DataFrame, dict]:
    """Construct the PyMC model.

    df must contain columns:
      player_id, player, team, conference, position, season, season_points_added,
      snaps_est, year_class, data_status, opp_quality_z

    Only rows with data_status in {"ok", "thin"} are used — below SNAP_GATE
    the involvement-based exposure estimate itself is unreliable.
    Returns (model, fit_df, index_maps).
    """
    fit_df = (
        df[df["data_status"].isin({"ok", "thin"})]
        .copy()
        .reset_index(drop=True)
    )
    fit_df["opp_quality_z"] = fit_df["opp_quality_z"].fillna(0.0)

    idx = _build_index_maps(fit_df)
    opp_quality = fit_df["opp_quality_z"].to_numpy(dtype=float)
    points_obs = fit_df["season_points_added"].to_numpy(dtype=float)

    # Exposure: estimated involvement plays. The ok/thin filter guarantees
    # snaps_est >= SNAP_GATE, but guard anyway — sqrt(0) would zero the
    # likelihood sd and clip(1) keeps the model defined for any input.
    snaps = (
        pd.to_numeric(fit_df["snaps_est"], errors="coerce")
        .fillna(1.0)
        .clip(lower=1.0)
        .to_numpy(dtype=float)
    )
    snaps_sd_scale = np.sqrt(snaps)

    coords = {
        "position":   idx["positions"].tolist(),
        "conference": idx["conferences"].tolist(),
        "team":       idx["teams"].tolist(),
        "player":     idx["players"].tolist(),
        "year_class": idx["class_levels"],
        "season":     idx["seasons"].tolist(),
        "obs":        fit_df.index.tolist(),
    }

    with pm.Model(coords=coords) as model:

        # ── Hyperpriors (per-play EPA scale; see module docstring) ────────────
        sigma_conf   = pm.HalfNormal("sigma_conf",   sigma=0.05)
        sigma_team   = pm.HalfNormal("sigma_team",   sigma=0.04)
        # Position-specific: per-play rate spread differs by position because
        # snaps_est counts involvement plays (per-target for WR/TE, per-dropback
        # for QB). One pooled sigma would mis-calibrate shrinkage everywhere.
        sigma_player = pm.HalfNormal("sigma_player", sigma=0.15, dims="position")
        sigma_play   = pm.HalfNormal("sigma_play",   sigma=1.5,  dims="position")

        # ── Position intercepts (per-play rate) ───────────────────────────────
        # Normal(0.15, 0.25): 95% interval [-0.34, 0.64] EPA/play.
        # Observed 2025 position means run 0.04 (RB) to 0.24 (WR).
        alpha = pm.Normal("alpha", mu=0.15, sigma=0.25, dims="position")

        # ── Non-centered conference effects ───────────────────────────────────
        conf_offset_raw = pm.Normal("conf_offset_raw", mu=0, sigma=1, dims="conference")
        conf_effect = pm.Deterministic(
            "conf_effect", conf_offset_raw * sigma_conf, dims="conference"
        )

        # ── Non-centered team effects (nested within conference) ───────────────
        team_offset_raw = pm.Normal("team_offset_raw", mu=0, sigma=1, dims="team")
        team_effect = pm.Deterministic(
            "team_effect",
            conf_effect[idx["team_conf_idx"]] + team_offset_raw * sigma_team,
            dims="team",
        )

        # ── Non-centered per-player skill deviation (position-scaled) ─────────
        # The key deliverable: player_offset[p] is each player's per-play EPA
        # above what their position + team baseline predicts.
        player_offset_raw = pm.Normal("player_offset_raw", mu=0, sigma=1, dims="player")
        player_offset = pm.Deterministic(
            "player_offset",
            player_offset_raw * sigma_player[idx["player_pos_idx"]],
            dims="player",
        )

        # ── Class-year development curve ───────────────────────────────────────
        # Zero-sum so it is identified separately from alpha (which absorbs the
        # position mean). Projection advances each player one class (aging curve).
        beta_class = pm.ZeroSumNormal("beta_class", sigma=0.05, dims="year_class")

        # ── Season (league environment) effect ─────────────────────────────────
        # Zero-sum; absorbs scoring-environment drift between training years.
        year_effect = pm.ZeroSumNormal("year_effect", sigma=0.03, dims="season")

        # ── Opponent quality slope (per-play) ──────────────────────────────────
        beta_opp = pm.Normal("beta_opp", mu=0, sigma=0.05)

        # ── StudentT degrees of freedom ────────────────────────────────────────
        # Gamma(2, 0.1): mean 20. Small nu = heavy tails (outlier seasons);
        # large nu recovers the Normal likelihood. The data decides.
        nu = pm.Gamma("nu", alpha=2, beta=0.1)

        # ── Likelihood: exposure form ──────────────────────────────────────────
        # mu = plays x per-play rate; sd grows with sqrt(plays), so high-volume
        # players pin their rate down precisely and thin players stay uncertain.
        rate = (
            alpha[idx["pos_idx"]]
            + team_effect[idx["team_idx"]]
            + player_offset[idx["player_idx"]]
            + beta_class[idx["class_idx"]]
            + year_effect[idx["season_idx"]]
            + beta_opp * opp_quality
        )
        pm.StudentT(
            "points_obs",
            nu=nu,
            mu=snaps * rate,
            sigma=sigma_play[idx["pos_idx"]] * snaps_sd_scale,
            observed=points_obs,
            dims="obs",
        )

    return model, fit_df, idx


def fit_model(
    df: pd.DataFrame,
    draws: int = 4000,
    tune: int = 2000,
    chains: int = 4,
    target_accept: float = 0.95,
    season_range: str = "2024-2025",
) -> tuple[az.InferenceData, pd.DataFrame, dict]:
    """Fit the points-added model and save the posterior as a netCDF file.

    Returns (idata, fit_df, index_maps).
    """
    model, fit_df, idx = build_model(df)

    with model:
        idata = pm.sample(
            draws=draws,
            tune=tune,
            chains=chains,
            target_accept=target_accept,
            return_inferencedata=True,
            progressbar=True,
        )

    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    out_path = ARTIFACTS_DIR / f"points_posterior_{season_range}.nc"
    idata.to_netcdf(str(out_path))
    print(f"Posterior saved → {out_path}")

    # Per-variable max — .to_array() would broadcast all vars to the union of
    # their dims (position x conference x team x player x class x season), a
    # multi-GiB allocation. Max over each variable separately is equivalent.
    rhat = az.rhat(idata)
    max_rhat = max(float(rhat[v].max()) for v in rhat.data_vars)
    print(f"Max R-hat: {max_rhat:.4f}  ({'OK' if max_rhat < 1.01 else 'WARN — check traces'})")

    # Report divergences — a non-zero count with the non-centered form is a red flag
    n_divergences = int(idata.sample_stats["diverging"].values.sum())
    if n_divergences > 0:
        print(f"WARN: {n_divergences} divergent transitions detected. "
              "Consider increasing target_accept or checking prior calibration.")
    else:
        print("No divergent transitions.")

    return idata, fit_df, idx


def load_posterior(season_range: str = "2024-2025") -> az.InferenceData:
    path = ARTIFACTS_DIR / f"points_posterior_{season_range}.nc"
    if not path.exists():
        raise FileNotFoundError(f"No posterior at {path}. Run train_engine first.")
    return az.from_netcdf(str(path))
