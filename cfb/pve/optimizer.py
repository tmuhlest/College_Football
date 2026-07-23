"""Phase 5: Marginal value computation and budget-constrained roster optimizer.

For each candidate (points = cumulative EPA; WAA = points ÷ EPA_PER_WIN):
  marginal_points = projected points(player) - projected points(displaced incumbent)
  marginal WAA    = marginal_points ÷ EPA_PER_WIN   (marginal wins over the backup)
  reservation_$   = marginal WAA × marginal_$/win
  surplus         = reservation_$ - asking_price

Optimizer: maximize Σ (marginal_mean_wins - λ * marginal_var_wins)
           subject to Σ asking_price ≤ budget, surplus ≥ 0 per selected player
Solver: PuLP (binary knapsack). The objective is in WAA (wins) units so the
variance penalty λ is interpretable and does not get swamped by EPA-scale values.

D15 manual values: players flagged "insufficient" use user_manual_points.
They carry zero model variance and are labeled "user-asserted" in output.
"""

from __future__ import annotations

import pandas as pd
import pulp

from pve.financial import EPA_PER_WIN


def compute_marginal_points(
    candidates: pd.DataFrame,
    depth_chart: pd.DataFrame,
    projections: pd.DataFrame,
) -> pd.DataFrame:
    """Compute marginal points added for each candidate over the incumbent they'd displace.

    candidates columns: player_id, player, position, asking_price, user_manual_points (optional)
    depth_chart columns: position, starter_id, backup_id
    projections columns: player_id, proj_points_mean, proj_points_sd, data_status

    Returns candidates with added columns:
      proj_points_mean, proj_points_sd, data_status, source,
      incumbent_points,
      marginal_points, marginal_points_var
    """
    proj_lookup = projections.set_index("player_id")

    # Group by position; for each position collect all backup values.
    # Candidate displaces the weakest backup (minimum proj_points_mean) at that position.
    position_incumbents: dict[str, tuple[float, float]] = {}  # pos -> (mean, var)
    for pos, group in depth_chart.groupby("position"):
        backups = []
        for bid in group["backup_id"]:
            # Safely convert backup_id to string; guard against None, "", and any NaN variant.
            # pd.isna() handles float NaN, pd.NA, np.nan, and None uniformly.
            try:
                missing = pd.isna(bid)
            except (TypeError, ValueError):
                missing = False
            bid_str = str(bid).strip() if not missing and bid != "" else ""
            if bid_str and bid_str in proj_lookup.index:
                inc = proj_lookup.loc[bid_str]
                backups.append((float(inc["proj_points_mean"]), float(inc["proj_points_sd"]) ** 2))
            else:
                backups.append((0.0, 0.0))
        position_incumbents[pos] = min(backups, key=lambda t: t[0])

    rows = []
    for _, cand in candidates.iterrows():
        # player_id must be string to match the proj_lookup index (parquet stores as object/str).
        # openpyxl returns numeric cell values as int/float; coerce here so the lookup works
        # regardless of whether the user typed the ID as a number or string in Excel.
        pid = str(cand["player_id"]).strip()
        pos = cand["position"]

        # --- candidate value ---
        if cand.get("user_manual_points") not in (None, ""):
            cand_mean = float(cand["user_manual_points"])
            cand_var = 0.0
            source = "user-asserted"
        elif pid in proj_lookup.index and proj_lookup.loc[pid, "data_status"] != "insufficient":
            row = proj_lookup.loc[pid]
            cand_mean = float(row["proj_points_mean"])
            cand_var = float(row["proj_points_sd"]) ** 2
            source = "model"
        else:
            cand_mean = None
            cand_var = None
            source = "insufficient"

        # --- incumbent value ---
        inc_mean, inc_var = position_incumbents.get(pos, (0.0, 0.0))

        marginal_points = (cand_mean - inc_mean) if cand_mean is not None else None
        marginal_points_var = (cand_var + inc_var) if cand_var is not None else None

        rows.append({
            **cand.to_dict(),
            "proj_points_mean": cand_mean,
            "proj_var": cand_var,
            "source": source,
            "incumbent_points": inc_mean,
            "marginal_points": marginal_points,
            "marginal_points_var": marginal_points_var,
        })

    return pd.DataFrame(rows)


def compute_reservation_value(
    marginal_df: pd.DataFrame,
    marginal_win_dollar: float,
) -> pd.DataFrame:
    """Add marginal_wins, reservation_value, and surplus columns.

    marginal_points is in cumulative EPA units.  Dividing by EPA_PER_WIN converts
    to fractional marginal wins (WAA) before multiplying by the $/win curve.
    The variance is also scaled to wins^2 so the optimizer objective stays in a
    sensible unit (wins) rather than mixing EPA-scale means with EPA^2-scale variances.
    """
    df = marginal_df.copy()

    def _to_wins(m):
        return m / EPA_PER_WIN if m is not None else None

    df["marginal_mean_wins"] = df["marginal_points"].apply(_to_wins)
    df["marginal_var_wins"] = df["marginal_points_var"].apply(
        lambda v: v / (EPA_PER_WIN ** 2) if v is not None else None
    )
    df["reservation_value"] = df["marginal_mean_wins"].apply(
        lambda m: m * marginal_win_dollar if m is not None else None
    )
    df["surplus"] = df.apply(
        lambda r: (r["reservation_value"] - r["asking_price"])
        if r["reservation_value"] is not None
        else None,
        axis=1,
    )
    return df


def solve_roster(
    valued_df: pd.DataFrame,
    budget: float,
    lambda_risk: float = 0.5,
) -> dict:
    """Binary knapsack: maximize risk-adjusted marginal wins within budget.

    Objective: Σ (marginal_points - λ * marginal_points_var) for selected players
    Constraints:
      Σ asking_price ≤ budget
      surplus ≥ 0 for each selected player (no negative-surplus selections)

    Returns a dict with keys: selected_df, total_spend, total_marginal_points.
    """
    # only players with a valid model or manual estimate and non-negative surplus
    eligible = valued_df[
        valued_df["marginal_points"].notna() & (valued_df["surplus"].fillna(-1) >= 0)
    ].copy().reset_index(drop=True)

    if eligible.empty:
        return {"selected_df": pd.DataFrame(), "total_spend": 0.0, "total_marginal_points": 0.0}

    prob = pulp.LpProblem("portal_roster", pulp.LpMaximize)
    x = [pulp.LpVariable(f"x_{i}", cat="Binary") for i in range(len(eligible))]

    # Objective in wins units: mean_wins - λ * var_wins
    # Using win-scale quantities avoids the variance (EPA^2) swamping the mean (EPA).
    obj_coeffs = [
        float(row["marginal_mean_wins"]) - lambda_risk * float(row["marginal_var_wins"] or 0.0)
        for _, row in eligible.iterrows()
    ]
    prob += pulp.lpSum(c * xi for c, xi in zip(obj_coeffs, x))
    prob += pulp.lpSum(float(row["asking_price"]) * xi for (_, row), xi in zip(eligible.iterrows(), x)) <= budget

    status = prob.solve(pulp.PULP_CBC_CMD(msg=0))
    if pulp.LpStatus[status] == "Infeasible":
        print("  WARNING: LP infeasible — no selection satisfies budget and surplus constraints.")

    # Use > 0.5 threshold to handle floating-point values near 1.0
    selected_mask = [(pulp.value(xi) or 0) > 0.5 for xi in x]
    selected_df = eligible[selected_mask].copy()
    selected_df["selected"] = True

    return {
        "selected_df": selected_df,
        "total_spend": float(selected_df["asking_price"].sum()),
        "total_marginal_points": float(selected_df["marginal_points"].sum()),
    }
