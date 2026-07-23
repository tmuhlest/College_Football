"""run_decision: Phase 4-6 pipeline.

Reads a team's Excel input workbook, runs the financial layer and optimizer,
and writes an Excel report workbook.

Usage:
    python scripts/run_decision.py --input templates/input_template.xlsx --output report.xlsx
    # or, after `pip install -e .`:
    run-decision --input ... --output ...
"""

import argparse
import sys
from datetime import datetime
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).parent.parent))

from pve.excel_io import read_input_workbook, write_output_workbook
from pve.financial import marginal_win_value
from pve.optimizer import (
    compute_marginal_points,
    compute_reservation_value,
    solve_roster,
)
from pve.projection import load_projections, load_projection_metadata


def main():
    parser = argparse.ArgumentParser(description="PVE Decision Layer")
    parser.add_argument("--input", required=True, help="Path to team input workbook (.xlsx)")
    parser.add_argument("--output", required=True, help="Path for output report workbook (.xlsx)")
    parser.add_argument("--lambda-risk", type=float, default=0.5,
                        help="Risk-aversion parameter λ (default 0.5). Higher = more risk-averse.")
    args = parser.parse_args()

    print("=== PVE Decision Layer ===")

    # Load shared posteriors
    print("[1/5] Loading projected points...")
    projections = load_projections()

    # Projection target / eligibility year comes from the training metadata
    # (max training season + 1). Fall back to the current calendar year if the
    # sidecar is missing (projections predating metadata).
    _meta = load_projection_metadata()
    if _meta and _meta.get("projection_season"):
        projection_season = int(_meta["projection_season"])
        print(f"  Trained on {_meta.get('season_range', '?')} → projecting {projection_season} season")
    else:
        # Fallback for projections predating the metadata sidecar: infer the
        # target season from the portal calendar (from December the upcoming
        # season is next year; before then it is the current year).
        _now = datetime.now()
        projection_season = _now.year + 1 if _now.month >= 12 else _now.year
        print(f"  No projection metadata found — assuming {projection_season} target season")

    # Read team inputs
    print("[2/5] Reading team input workbook...")
    inputs = read_input_workbook(args.input)
    budget = inputs["budget"]
    preseason_wins = inputs["preseason_wins"]
    win_curve = inputs["win_curve"]
    depth_chart = inputs["depth_chart"]
    candidates = inputs["candidates"]
    print(f"  Budget: ${budget:,.0f} | Preseason wins: {preseason_wins} | Candidates: {len(candidates)}")

    # Financial layer
    print("[3/5] Computing marginal win value...")
    marginal_win_dollar = marginal_win_value(win_curve, preseason_wins)
    print(f"  Marginal $/win at {preseason_wins} wins: ${marginal_win_dollar:,.0f}")

    # Marginal points added
    print("[4/5] Computing marginal points and reservation values...")
    marginal_df = compute_marginal_points(candidates, depth_chart, projections)
    valued_df = compute_reservation_value(marginal_df, marginal_win_dollar)

    # Show valuation summary before optimizing
    print("\n  Candidate valuations:")
    print(f"  {'Player':<22} {'Marg.Wins':>10} {'Max Bid':>12} {'NIL Ask':>10} {'Edge':>11}")
    for _, r in valued_df.iterrows():
        mw  = f"{r['marginal_mean_wins']:+.2f}" if pd.notna(r.get("marginal_mean_wins")) else "  n/a"
        rv  = f"${r['reservation_value']:,.0f}" if pd.notna(r.get("reservation_value")) else "  n/a"
        ask = f"${r['asking_price']:,.0f}"
        sur = f"${r['surplus']:,.0f}" if pd.notna(r.get("surplus")) else "  n/a"
        print(f"  {r['player']:<22} {mw:>10} {rv:>12} {ask:>10} {sur:>11}")

    # Optimizer
    print("\n[5/5] Running optimizer...")
    result = solve_roster(valued_df, budget, lambda_risk=args.lambda_risk)
    selected_df = result["selected_df"]
    print(f"  Selected {len(selected_df)} players | Spend: ${result['total_spend']:,.0f} / ${budget:,.0f}")

    # Write report
    # Add WAA (wins-above-average) CI columns and year_class for the candidate
    # board display. proj_points_ci_* are in raw EPA units; dividing by
    # EPA_PER_WIN yields the WAA credible-interval bounds (wins).
    from pve.financial import EPA_PER_WIN
    from pve.ingest import ELIGIBILITY_WARN_CLASSES

    # Compute depth chart projected wins (starter/backup) for the Depth Comparison tab.
    # projections.player_id is a clean string (from parquet); depth chart IDs may be
    # ints or floats from openpyxl — normalize to the same canonical string format.
    _proj_by_pid = projections.set_index("player_id")["proj_points_mean"]

    def _dc_wins(pid) -> float | None:
        if pid is None:
            return None
        pid_s = str(pid).strip()
        if pid_s in ("", "nan", "None"):
            return None
        try:
            f = float(pid_s)
            if f == int(f):
                pid_s = str(int(f))
        except (ValueError, TypeError):
            pass
        if pid_s in _proj_by_pid.index:
            return round(float(_proj_by_pid[pid_s]) / EPA_PER_WIN, 3)
        return None

    dc = inputs["depth_chart"].copy()
    dc["starter_wins"] = dc["starter_id"].apply(_dc_wins)
    dc["backup_wins"]  = dc["backup_id"].apply(_dc_wins)
    inputs["depth_chart"] = dc
    # player_id dtype mismatch: parquet stores as object/str, Excel returns int64.
    # Standardize both sides to str before joining so types align.
    ci_cols = ["player_id", "proj_points_ci_80_lo", "proj_points_ci_80_hi",
               "proj_points_ci_95_lo", "proj_points_ci_95_hi", "year_class"]
    ci_df = projections[ci_cols].copy()
    ci_df["player_id"] = ci_df["player_id"].astype(str)
    candidate_board = valued_df.copy()
    candidate_board["player_id"] = candidate_board["player_id"].astype(str)
    candidate_board = candidate_board.merge(ci_df, on="player_id", how="left")
    candidate_board["year_class"] = candidate_board["year_class"].fillna("UNK")
    _ci_points_to_waa = {
        "proj_points_ci_80_lo": "waa_ci_80_lo",
        "proj_points_ci_80_hi": "waa_ci_80_hi",
        "proj_points_ci_95_lo": "waa_ci_95_lo",
        "proj_points_ci_95_hi": "waa_ci_95_hi",
    }
    for src, waa_col in _ci_points_to_waa.items():
        candidate_board[waa_col] = candidate_board[src] / EPA_PER_WIN

    # Soft eligibility warnings for SR/GS players
    warn_players = candidate_board[
        candidate_board["year_class"].isin(ELIGIBILITY_WARN_CLASSES)
    ]
    if not warn_players.empty:
        print("\n  ELIGIBILITY NOTICE — verify before signing:")
        for _, r in warn_players.iterrows():
            print(f"  ⚠  {r['player']} ({r['year_class']}) was a {r['year_class']} "
                  f"in the training data — confirm {projection_season} eligibility")

    write_output_workbook(
        output_path=args.output,
        candidate_board=candidate_board,
        selected_df=selected_df,
        total_spend=result["total_spend"],
        inputs=inputs,
        projections=projections,
        lambda_risk=args.lambda_risk,
        projection_season=projection_season,
        season_range=(_meta or {}).get("season_range"),
    )

    print(f"\nDone. Report written to {args.output}")


if __name__ == "__main__":
    main()
