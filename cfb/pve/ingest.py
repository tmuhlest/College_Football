"""Phase 1: Pull CFBD data, extract EPA, aggregate to player-season points added.

CFBD endpoints used:
  GET /ppa/players/season  — per-player season-level total/average PPA
  GET /player/usage        — snap percentage + usage rates per player per season
  GET /stats/teams         — season-level team offensive play totals (passAttempts + rushingAttempts)
  GET /ppa/teams           — per-team per-game defensive EPA (for opponent quality index)
  GET /games               — season schedule (team → opponent mapping)

Usage gate (D15):
  data_status = "ok"           — snaps_est >= THIN_GATE, full estimate
  data_status = "thin"         — snaps_est in [SNAP_GATE, THIN_GATE); included in
                                 the model — the exposure likelihood (sd ∝
                                 sqrt(snaps)) widens their CI automatically
  data_status = "insufficient" — snaps_est < SNAP_GATE, no model estimate

Note: snaps_est is derived from snap_pct_overall × team_total_plays.
snap_pct_overall measures involvement rate (targets/carries), not actual snaps on
the field, so it undercounts pass catchers. SNAP_GATE is intentionally low (10)
to avoid incorrectly flagging starting WRs/TEs as insufficient.
"""

from __future__ import annotations

import os
from pathlib import Path

import cfbd
import pandas as pd
from dotenv import load_dotenv

load_dotenv()

SKILL_POSITIONS = {"QB", "RB", "WR", "TE"}
SNAP_GATE = 10   # below this: no model estimate (D15)
THIN_GATE = 20   # below this: widened CI — involvement-rate proxy undercounts pass catchers

_YEAR_MAP = {1: "FR", 2: "SO", 3: "JR", 4: "SR", 5: "GS"}
ELIGIBILITY_WARN_CLASSES = {"SR", "GS"}  # flag these for user verification

RAW_DIR = Path(__file__).parent.parent / "data" / "raw"
PROCESSED_DIR = Path(__file__).parent.parent / "data" / "processed"


def _get_api_config() -> cfbd.Configuration:
    key = os.environ.get("CFBD_API_KEY", "").strip()
    if not key:
        raise EnvironmentError("CFBD_API_KEY not set. Copy .env.example to .env and add your key.")
    config = cfbd.Configuration()
    config.access_token = key  # cfbd uses access_token → Authorization: Bearer <key>
    return config


def pull_player_ppa(season: int, position_group: set[str] | None = None) -> pd.DataFrame:
    """Pull per-player season-level PPA for a given season.

    Uses the season endpoint (one row per player, no week/team filter required).
    season_points_added = total_ppa.all (sum of EPA contributions across all
    plays in the season).

    Returns a DataFrame with columns:
      player_id, player, team, conference, season, position,
      season_points_added (season total EPA), avg_ppa_all (per-play average EPA).
    """
    if position_group is None:
        position_group = SKILL_POSITIONS

    config = _get_api_config()
    with cfbd.ApiClient(config) as client:
        api = cfbd.MetricsApi(client)
        records = []
        for pos in position_group:
            results = api.get_predicted_points_added_by_player_season(year=season, position=pos)
            for r in results:
                records.append({
                    "player_id": r.id,
                    "player": r.name,
                    "team": r.team,
                    "conference": r.conference,
                    "season": season,
                    "position": pos,
                    "season_points_added": r.total_ppa.all if r.total_ppa else None,
                    "avg_ppa_all": r.average_ppa.all if r.average_ppa else None,
                    "avg_ppa_pass": getattr(r.average_ppa, "passing", None) if r.average_ppa else None,
                    "avg_ppa_rush": getattr(r.average_ppa, "rush", None) if r.average_ppa else None,
                })
        df = pd.DataFrame(records)

    RAW_DIR.mkdir(parents=True, exist_ok=True)
    df.to_parquet(RAW_DIR / f"player_ppa_{season}.parquet", index=False)
    return df


def pull_player_usage(season: int) -> pd.DataFrame:
    """Pull per-player usage (snap %, position) for a given season.

    The CFBD /player/usage endpoint returns usage rates (0–1 scale).
    Snap counts are estimated later by combining with team total offensive plays.
    """
    config = _get_api_config()
    with cfbd.ApiClient(config) as client:
        api = cfbd.PlayersApi(client)
        results = api.get_player_usage(year=season)
        records = [
            {
                "player_id": r.id,
                "player": r.name,
                "team": r.team,
                "conference": r.conference,
                "season": season,
                "position": r.position,
                "snap_pct_overall": r.usage.overall if r.usage else None,
                "snap_pct_pass": getattr(r.usage, "passing", None) if r.usage else None,
                "snap_pct_rush": r.usage.rush if r.usage else None,
            }
            for r in results
        ]
    df = pd.DataFrame(records)
    df.to_parquet(RAW_DIR / f"player_usage_{season}.parquet", index=False)
    return df


def pull_team_game_stats(season: int) -> pd.DataFrame:
    """Pull season-level offensive play totals per team.

    Uses StatsApi.get_team_stats (no week/team filter required).
    total_plays = passAttempts + rushingAttempts — the sum of all scrimmage plays.

    Returns columns: team, season, total_plays.
    """
    config = _get_api_config()
    with cfbd.ApiClient(config) as client:
        api = cfbd.StatsApi(client)
        results = api.get_team_stats(year=season)

    df = pd.DataFrame([
        {"team": r.team, "stat_name": r.stat_name, "stat_value": r.stat_value.actual_instance}
        for r in results
    ])

    pass_att = df[df["stat_name"] == "passAttempts"].set_index("team")["stat_value"]
    rush_att = df[df["stat_name"] == "rushingAttempts"].set_index("team")["stat_value"]
    total = (pass_att.add(rush_att, fill_value=0)).reset_index()
    total.columns = ["team", "total_plays"]
    total["season"] = season
    return total[["team", "season", "total_plays"]]


def pull_roster_years(season: int) -> pd.DataFrame:
    """Pull player year-in-school from the CFBD roster endpoint.

    Returns a DataFrame with columns: player_id, year_class (FR/SO/JR/SR/GS/UNK).
    year_class is used downstream as a soft eligibility signal — SR and GS players
    should be verified for next-season eligibility before signing.

    CFBD RosterPlayer.year: 1=FR, 2=SO, 3=JR, 4=SR, 5=GS.
    Players not found in the roster data get year_class="UNK".
    """
    config = _get_api_config()
    with cfbd.ApiClient(config) as client:
        api = cfbd.TeamsApi(client)
        results = api.get_roster(year=season)

    records = [
        {
            "player_id": r.id,
            "year_class": _YEAR_MAP.get(r.year, "UNK"),
        }
        for r in results
    ]
    df = pd.DataFrame(records).drop_duplicates("player_id")
    df.to_parquet(RAW_DIR / f"roster_years_{season}.parquet", index=False)
    return df


def estimate_snap_counts(
    ppa_df: pd.DataFrame,
    usage_df: pd.DataFrame,
    team_stats_df: pd.DataFrame,
) -> pd.DataFrame:
    """Estimate absolute snap counts from usage % × team total offensive plays.

    This is an approximation (D15 note): labeled 'snaps_est' in output.
    Skill position snap % is relative to total offensive plays on that player's team.
    """
    avg_team_plays = (
        team_stats_df.groupby(["team", "season"])["total_plays"].sum().reset_index()
    )

    merged = usage_df.merge(avg_team_plays, on=["team", "season"], how="left")
    merged["snaps_est"] = (merged["snap_pct_overall"].fillna(0) * merged["total_plays"]).round(0).astype("Int64")
    return merged[["player_id", "player", "team", "conference", "season", "position", "snaps_est"]]


def aggregate_to_points(ppa_df: pd.DataFrame, snap_df: pd.DataFrame) -> pd.DataFrame:
    """Join season-level points added with snap estimates.

    ppa_df already has one row per player-season with
    season_points_added = total_ppa.all. Joins snap estimates from the usage
    data for the D15 gate.
    """
    merged = ppa_df.merge(
        snap_df[["player_id", "season", "snaps_est"]],
        on=["player_id", "season"],
        how="left",
    )
    return merged


def apply_usage_gate(
    df: pd.DataFrame,
    snap_threshold: int = SNAP_GATE,
    thin_threshold: int = THIN_GATE,
) -> pd.DataFrame:
    """Add a data_status column based on estimated snap counts (D15).

      "insufficient" → snaps_est < snap_threshold  (no model estimate)
      "thin"         → snaps_est in [snap_threshold, thin_threshold)  (widened CI)
      "ok"           → snaps_est >= thin_threshold
    """
    df = df.copy()
    snaps = df["snaps_est"].fillna(0)
    df["data_status"] = "ok"
    df.loc[snaps < thin_threshold, "data_status"] = "thin"
    df.loc[snaps < snap_threshold, "data_status"] = "insufficient"
    return df


def build_opponent_quality(season: int) -> pd.DataFrame:
    """Build a schedule-adjusted opponent quality index (opp_quality_z) per team.

    Steps:
      1. Pull each team's per-game defensive EPA via get_predicted_points_added_by_team.
         Aggregate to season-average → how good each team's defense was all year.
      2. Pull the regular-season schedule via get_games to map team → list of opponents.
      3. For each team, average the defensive EPA of every opponent they faced.
         This is "schedule difficulty": a team that faced tougher defenses gets a
         more negative avg_opp_def_ppa (since lower def_ppa = better defense).
      4. Z-score normalize across all teams so the covariate enters the Bayesian
         model on a unit scale (mean=0, sd=1).

    Returns columns: team, season, opp_quality_z

    Joins to player points on (team, season): every player on the same team gets the
    same opp_quality_z because they faced the same schedule.

    FCS opponents with no defensive EPA data are skipped; schedules heavy on FCS
    opponents will have their index based on fewer FBS games (acceptable for V1).
    """
    config = _get_api_config()

    # Step 1: season-average defensive EPA per team
    with cfbd.ApiClient(config) as client:
        team_ppa_rows = cfbd.MetricsApi(client).get_predicted_points_added_by_team(year=season)

    def_records = [
        {"team": r.team, "def_ppa": r.defense.overall}
        for r in team_ppa_rows
        if r.defense and r.defense.overall is not None
    ]
    def_df = (
        pd.DataFrame(def_records)
        .groupby("team", as_index=False)
        .agg(def_ppa=("def_ppa", "mean"))
    )
    def_lookup = def_df.set_index("team")["def_ppa"]

    # Step 2: regular-season schedule → team → [opponent, ...]
    with cfbd.ApiClient(config) as client:
        games = cfbd.GamesApi(client).get_games(year=season, season_type="regular")

    schedule: dict[str, list[str]] = {}
    for g in games:
        if not g.completed:
            continue
        for team, opponent in [(g.home_team, g.away_team), (g.away_team, g.home_team)]:
            schedule.setdefault(team, []).append(opponent)

    # Step 3: schedule-average opponent defensive EPA per team
    records = []
    for team, opponents in schedule.items():
        opp_vals = [def_lookup[opp] for opp in opponents if opp in def_lookup.index]
        if opp_vals:
            records.append({
                "team": team,
                "season": season,
                "avg_opp_def_ppa": sum(opp_vals) / len(opp_vals),
            })

    opp_df = pd.DataFrame(records)

    # Step 4: z-score normalize
    mu = opp_df["avg_opp_def_ppa"].mean()
    sigma = opp_df["avg_opp_def_ppa"].std()
    opp_df["opp_quality_z"] = (opp_df["avg_opp_def_ppa"] - mu) / sigma

    RAW_DIR.mkdir(parents=True, exist_ok=True)
    opp_df.to_parquet(RAW_DIR / f"opp_quality_{season}.parquet", index=False)
    return opp_df[["team", "season", "opp_quality_z"]]


def run_ingest(seasons: list[int], force_refresh: bool = False) -> pd.DataFrame:
    """Full Phase 1 pipeline: pull, estimate snaps, aggregate points, apply gate.

    Writes data/processed/player_points_{season}.parquet for each season. If
    that file already exists for a season, it is loaded from disk instead of
    re-pulling from the CFBD API — pass force_refresh=True to override (e.g.
    for a season still in progress). Returns a combined multi-season DataFrame.
    """
    PROCESSED_DIR.mkdir(parents=True, exist_ok=True)
    all_frames = []

    for season in seasons:
        out_path = PROCESSED_DIR / f"player_points_{season}.parquet"
        if out_path.exists() and not force_refresh:
            print(f"Season {season} already cached at {out_path} — skipping API pull.")
            all_frames.append(pd.read_parquet(out_path))
            continue

        print(f"Ingesting season {season}...")
        ppa_df = pull_player_ppa(season)
        usage_df = pull_player_usage(season)
        team_stats = pull_team_game_stats(season)
        opp_quality = build_opponent_quality(season)
        roster_years = pull_roster_years(season)

        snap_df = estimate_snap_counts(ppa_df, usage_df, team_stats)
        points_df = aggregate_to_points(ppa_df, snap_df)
        points_df = apply_usage_gate(points_df)
        points_df = points_df.merge(opp_quality[["team", "season", "opp_quality_z"]],
                              on=["team", "season"], how="left")
        points_df = points_df.merge(roster_years[["player_id", "year_class"]],
                              on="player_id", how="left")
        points_df["year_class"] = points_df["year_class"].fillna("UNK")

        points_df.to_parquet(out_path, index=False)
        print(f"  Wrote {len(points_df)} players to {out_path}")
        all_frames.append(points_df)

    return pd.concat(all_frames, ignore_index=True)
