"""train_engine: Phase 1–3 pipeline.

Pulls CFBD data, fits the hierarchical Bayesian points-added model, and writes
projected_points.parquet — the clean seam between the shared engine and the
team-specific decision layer.

Run once per season (or whenever you want to refresh the model artifact).
Typical runtime: 30–60 minutes depending on hardware (MCMC sampling).

Seasons are not hardcoded. By default the engine trains on the most recent
DEFAULT_NUM_SEASONS completed seasons, derived from today's date and aligned to
the transfer-portal calendar: a CFB regular season finishes in early December
and the winter portal window opens then, so from December onward THAT year's
season is the most recent one to train on; before December it is the prior year.

  Run in Jul 2026  -> train [2024, 2025], project 2026 season
  Run in Dec 2026  -> train [2025, 2026], project 2027 season  (2026 just wrapped)

Override explicitly with --seasons anytime. The projection target season is
always max(seasons) + 1.

Each training season's raw pull is cached to
data/processed/player_points_{season}.parquet — if that file already exists,
train_engine reuses it instead of hitting the CFBD API again. Pass
--refresh-data to force a re-pull (e.g. for a season still in progress).

Usage:
    conda activate pve
    python scripts/train_engine.py                    # auto: last 2 completed seasons
    python scripts/train_engine.py --seasons 2025 2026
    python scripts/train_engine.py --num-seasons 3
    python scripts/train_engine.py --refresh-data      # force re-pull, ignore cache
    # or, after `pip install -e .`:
    train-engine
"""

from __future__ import annotations

import argparse
import sys
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

import pandas as pd
from pve.calibration import compute_projection_constants
from pve.ingest import run_ingest
from pve.points_model import fit_model
from pve.projection import extract_player_posterior, project_points

DEFAULT_NUM_SEASONS = 2    # training-window width when --seasons is not given
CALIBRATION_LOOKBACK = 3   # number of historical season-pairs to average
# Month (1-12) from which the current year's season counts as the most recent
# usable one. A CFB regular season finishes in early December and the winter
# transfer-portal window opens then, so December is the flip point.
SEASON_COMPLETE_MONTH = 12


def resolve_seasons(
    explicit: list[int] | None,
    num_seasons: int,
    today: datetime | None = None,
) -> list[int]:
    """Return the sorted, de-duplicated training seasons.

    If explicit seasons are given, use them. Otherwise derive the most recent
    `num_seasons` completed seasons, aligned to the transfer-portal calendar:
    from December (SEASON_COMPLETE_MONTH) onward the current year's season has
    just wrapped and becomes the most recent usable season; before then it is
    the prior year. `today` is injectable for testing.

      Jul 2026 -> latest completed 2025 -> [2024, 2025]  (project 2026)
      Dec 2026 -> latest completed 2026 -> [2025, 2026]  (project 2027)
    """
    if explicit:
        return sorted(set(int(s) for s in explicit))
    today = today or datetime.now()
    latest_completed = today.year if today.month >= SEASON_COMPLETE_MONTH else today.year - 1
    return list(range(latest_completed - num_seasons + 1, latest_completed + 1))


def main() -> None:
    parser = argparse.ArgumentParser(description="PVE Train Engine")
    parser.add_argument(
        "--seasons",
        type=int,
        nargs="+",
        default=None,
        metavar="YEAR",
        help="Explicit training seasons, e.g. --seasons 2025 2026. "
             "If omitted, defaults to the most recent completed seasons "
             "derived from the current calendar year.",
    )
    parser.add_argument(
        "--num-seasons",
        type=int,
        default=DEFAULT_NUM_SEASONS,
        help=f"Training-window width used only when --seasons is omitted "
             f"(default {DEFAULT_NUM_SEASONS}).",
    )
    parser.add_argument(
        "--data-only",
        action="store_true",
        help="Run Phase 1 (data pull + calibration) only, skipping the MCMC "
             "model fit. Useful for refreshing cached data or calibration "
             "constants without the full ~30-60 min sampling run.",
    )
    parser.add_argument(
        "--refresh-data",
        action="store_true",
        help="Re-pull CFBD data for every training season even if "
             "data/processed/player_points_{season}.parquet is already cached "
             "(default: reuse cached season files and skip the API call). "
             "Use this if a season's data may have changed since it was pulled, "
             "e.g. a season still in progress.",
    )
    args = parser.parse_args()

    seasons = resolve_seasons(args.seasons, args.num_seasons)
    season_range = f"{seasons[0]}-{seasons[-1]}"
    projection_season = seasons[-1] + 1

    print("=" * 60)
    print("  Player Value Engine — Train Engine")
    print(f"  Training seasons : {seasons}"
          f"{'  (auto from current year)' if not args.seasons else ''}")
    print(f"  Projection target: {projection_season} season")
    if args.data_only:
        print("  Mode: data pull only (--data-only)")
    print("=" * 60)

    # ── Phase 1: ingest ──────────────────────────────────────────
    print("\n[Phase 1] Data pipeline...")
    combined_df = run_ingest(seasons, force_refresh=args.refresh_data)

    n_total  = len(combined_df)
    n_ok     = (combined_df["data_status"] == "ok").sum()
    n_thin   = (combined_df["data_status"] == "thin").sum()
    n_insuff = (combined_df["data_status"] == "insufficient").sum()
    print(f"  Player-seasons ingested: {n_total:,}")
    print(f"  data_status → ok: {n_ok}  thin: {n_thin}  insufficient: {n_insuff}")
    print(f"  opp_quality_z nulls (FCS/non-FBS): {combined_df['opp_quality_z'].isna().sum()}")

    # ── Phase 1.5: calibrate projection constants ─────────────────
    # Calibrate on the historical season-pairs immediately before the training
    # window's start (e.g., for a 2024-2025 window: 2021→2022, 2022→2023,
    # 2023→2024). Writes models/artifacts/projection_constants.json.
    print("\n[Phase 1.5] Calibrating year-to-year regression constants...")
    compute_projection_constants(
        end_season=min(seasons),
        lookback=CALIBRATION_LOOKBACK,
    )

    if args.data_only:
        print("\n" + "=" * 60)
        print("  Data pull complete (--data-only). Skipping model fit.")
        print("  Run train_engine.py (without --data-only) for the full model.")
        print("=" * 60)
        return

    # ── Phase 2: fit model ────────────────────────────────────────
    print("\n[Phase 2] Fitting hierarchical Bayesian model...")
    print("  This takes ~30–60 minutes. MCMC chains will show progress below.")
    idata, fit_df, idx = fit_model(combined_df, season_range=season_range)

    # ── Phase 3: project ──────────────────────────────────────────
    print("\n[Phase 3] Generating projected points...")
    summaries = extract_player_posterior(idata, fit_df, idx)

    # Detect transfers: players whose team changed between the two most recent seasons.
    # Transfers get a 1.5x predictive SD inflation in project_points() to reflect
    # environment-portability uncertainty (new scheme, new supporting cast).
    # Players appearing only in the most recent season (freshmen, FCS arrivals with
    # no prior sufficient data) default to is_transfer=False — conservative.
    if len(seasons) >= 2:
        s_prev, s_curr = seasons[-2], seasons[-1]
        teams_prev = (
            combined_df[combined_df["season"] == s_prev]
            .drop_duplicates("player_id")
            .set_index("player_id")["team"]
        )
        teams_curr = (
            combined_df[combined_df["season"] == s_curr]
            .drop_duplicates("player_id")
            .set_index("player_id")["team"]
        )
        common_pids = teams_prev.index.intersection(teams_curr.index)
        transfer_flags = {
            str(pid): (teams_prev[pid] != teams_curr[pid])
            for pid in common_pids
        }
        n_transfers = sum(transfer_flags.values())
        print(f"  Detected {n_transfers} transfers "
              f"(team changed {s_prev}→{s_curr} among players with data in both seasons)")
    else:
        transfer_flags = {}

    project_points(summaries, transfer_flags=transfer_flags, train_seasons=seasons)

    print("\n" + "=" * 60)
    print("  Done. Run `run_decision` with a team input workbook.")
    print("=" * 60)


if __name__ == "__main__":
    main()
