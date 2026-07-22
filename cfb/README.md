# Player Value Engine (PVE)

A tool to help general managers and teams target players in the transfer portal. I built this — the Player Value Engine, or PVE — to compute your program's private **reservation value**: the most you should be willing to pay for a given portal candidate, based on projected marginal wins converted to dollars through your school's specific win-value curve.

This is **not** a fair-market-value tool. It tells you what a player is worth *to your specific program*, given your win curve, preseason win projection, and current two-deep roster.

This is an incomplete tool. Right now it only values offensive skill positions, and the backbone of the player-value calculation is CFBD's built-in Expected Points Added (EPA). I plan to make it more robust and expand to other positions over time. At a minimum, it should help you figure out how to spend your skill-player budget, give you a sense of how many wins a player is likely to add (or their reasonable range), and help you price players against your own program's goals — not every team is chasing a conference title, so there's no reason to overpay or bid up players who are asking for more than they're likely to add. See the **V1 Limitations** section near the bottom for the current limitations of this version.

This tool is built in Python. I know a lot of General Managers won't be comfortable with Python, so as long as you have someone run the Python side for you, everything else — the inputs and the report — is in Excel, which I'd expect a General Manager to know how to use.

Player wins are calculated based on how a player performs relative to the average FBS player at their position — not a bench-level guy off the street. That baseline is what lets you compare a candidate to a normal, established player at that spot.

---

## Quick Start

```bash
# 1. Clone and set up environment
git clone https://github.com/yourname/cfb.git
cd cfb
conda env create -f environment.yml
conda activate pve

# 2. Add your CFBD API key (free — see below for how to set this up)
cp .env.example .env
# Edit .env: set CFBD_API_KEY=your_key_here

# 3. Pull data, fit the model, and export projections (~30-60 min, MCMC sampling)
python scripts/train_engine.py

# 4. Fill out your team's input workbook, then run the decision layer
#    (see the Full Workflow section below for how to fill out the workbook)
python scripts/run_decision.py \
  --input  data/your_program_input.xlsx \
  --output data/your_program_report.xlsx
```

---

## Getting a CFBD API Key

PVE uses the [College Football Data API](https://collegefootballdata.com) (same data source as cfbfastR). The free tier covers all V1 needs.

1. Go to [collegefootballdata.com](https://collegefootballdata.com) → click **Register for an API key**
2. Complete the short form (email, use-case description)
3. Copy the key into `.env`:
   ```
   CFBD_API_KEY=your_key_here
   ```

The key is free and typically approved within minutes.

---

## Installation

Requires [conda](https://docs.conda.io/en/latest/miniconda.html) or [mamba](https://mamba.readthedocs.io).

```bash
conda env create -f environment.yml
conda activate pve
```

To install as a package with entry-point shortcuts:
```bash
pip install -e .
# then use:  train-engine   and   run-decision  instead of  python scripts/...
```

---

## Full Workflow

### Step 1 — Train the Engine (once per season)

```bash
python scripts/train_engine.py                 # auto: last 2 completed seasons
python scripts/train_engine.py --seasons 2025 2026   # or set the window explicitly if you want to back test the results
```

**Seasons are chosen automatically** from the transfer-portal calendar — no code edit needed each year. A CFB regular season finishes in early December (when the winter portal opens), so from December on that year's season becomes the most recent one trained on; before then it's the prior year. The projection target is always `max(seasons) + 1`, recorded in `data/processed/projected_points.meta.json` so the report labels eligibility years correctly. Override anytime with `--seasons` (or widen the window with `--num-seasons 3`).

This runs:
1. **Phase 1 (Ingest):** Pulls two seasons of CFBD play-by-play EPA, player usage, and schedule data. Estimates snap counts. Flags players as `ok`, `thin`, or `insufficient` based on estimated snap count. Builds opponent quality (z-scored schedule-average defensive EPA).
2. **Phase 2 (Model):** Fits a hierarchical Bayesian model in PyMC 5 on **per-play efficiency with snap-count exposure** — position means, conference effects, team effects, a class-year development curve, season effects, and per-player skill deviations, with a Student-t likelihood. Players with few plays are automatically shrunk toward their position/team/class baseline (in hierarchical modeling this is called partial pooling), and their credible intervals (the expected range of this player's season outcomes) widen naturally because the exposure likelihood leaves their per-play rate weakly identified.
3. **Phase 3 (Project):** Extracts per-player posterior rate distributions, advances each player one class year (aging curve), multiplies by their most-recent-season play volume, and saves `data/processed/projected_points.parquet`.

Runtime: roughly 30–60 minutes on a laptop (dominated by MCMC sampling). The posterior is cached; subsequent `run_decision` calls are fast.

**Output:** `data/processed/projected_points.parquet` — one row per player with projected mean, SD, and 80%/95% credible intervals.

---

### Step 2 — Fill Out Your Team's Input Workbook

Start from the template:
```bash
python scripts/create_template.py  # regenerates templates/input_template.xlsx
```

Open `templates/input_template.xlsx` and fill in four tabs:

| Tab | What to fill in |
|-----|----------------|
| **Config** | **Skill Position Portal Budget** — QB/RB/WR/TE spend only (do not include OL, defense, or special teams). Preseason win projection (0–12), program name. |
| **Win Value Curve** | For each win 0–12: the dollar value your program places on gaining that win. Spike win 6 if bowl eligibility = job security. Spike 10–12 if you're a playoff contender. |
| **Two-Deep** | Your current starter and displaced player for each position slot. The `player_id` column comes from `projected_points.parquet` — leave blank to treat the incumbent as 0-value (replacement-level). |
| **Candidates** | One row per portal candidate. `player_id` from `projected_points.parquet`. `asking_price` in dollars. **FCS players and players not in CFBD PPA data will not have a `player_id`** — enter your own points-added (EPA) estimate in `manual_points` and leave `player_id` blank; they will be valued as "user-asserted." |

Save as `data/your_program_input.xlsx`.

---

### Step 3 — Run the Decision Layer

```bash
python scripts/run_decision.py \
  --input  data/your_program_input.xlsx \
  --output data/your_program_report.xlsx
```

Optional flag: `--lambda-risk 0.5` (default). Set to 0 for pure upside maximization, 1 for highest variance-penalization.

The terminal will print a valuation table showing each candidate's marginal wins, max bid, NIL ask, and edge before the optimizer runs. But it will be easier to read these values in the output Excel file.

**Output report tabs:**

| Tab | Contents |
|-----|---------|
| **Candidate Board** | Every candidate: marginal wins, 80% CI, max bid $, NIL ask $, edge $ |
| **Recommended Roster** | Budget-constrained selection with total spend and positional allocation |
| **Depth Comparison** | Current depth chart projected wins vs. candidate projected wins with credible intervals |
| **Inputs Echo** | Budget, depth chart, win-value curve — edit here to recalculate live |
| **Read-Me** | Scope statement, column guide, and how to adjust inputs |

**Column guide — Candidate Board:**

| Column | What it means |
|--------|--------------|
| Marginal Wins | Projected wins added over the backup this player would displace |
| Wins 80% Lo / Hi | 80% credible interval — narrow = high floor, wide = boom-or-bust |
| Max Bid $ | The most your program should pay before the deal stops penciling out |
| NIL Ask $ | What the player/agent is publicly asking for |
| Edge $ | Max Bid minus NIL Ask — positive = good deal, negative = overpay |

---

## Architecture

```
ingest.py   →  points_model.py  →  projection.py
(Phase 1)      (Phase 2)        (Phase 3)
                                     ↓
                           projected_points.parquet
                                     ↓
                   financial.py  +  optimizer.py
                   (Phase 4)        (Phase 5)
                                     ↓
                             excel_io.py (Phase 6)
```

### Units — Points Added (EPA) and WAA

The model works in **cumulative Expected Points Added (EPA)** — the raw `season_points_added` column and every `proj_points_*` field are points, not wins. **WAA (Wins Above Average)** is the wins-scale view of the same quantity: `WAA = points_added / EPA_PER_WIN`. A player with ~130 projected points added contributes roughly 1 additional win's worth of offensive value compared to an average player at their position.

**Contextualizing points-added scores (cumulative EPA/season):**
- Top starting QB: ~150–300
- Starting RB: ~80–150
- Starting WR: ~50–120
- Starting TE: ~20–60

The conversion from points to wins uses `EPA_PER_WIN = 130` (hardcoded in `pve/financial.py`). This is an empirical approximation from CFB analytics; adjust it if your analytical framework supports a different value. This can also be done in your excel.

### How the Projection Works (training seasons → projection year)

The engine uses its trained seasons of CFBD data (the most recent completed seasons by default, or whatever you pass via `--seasons`) to estimate each player's true **per-play skill rate**, then projects forward one year — to `max(seasons) + 1` — with explicit regression toward the positional baseline. For example, training on 2024–2025 projects the 2026 season; training on 2025–2026 projects 2027.

```
rate           = alpha[position] + team_effect + beta_class[NEXT class] + player_rate_offset
                 (per-play EPA, from Bayesian posterior; class advanced one year = aging curve)

fitted_mean    = proj_snaps × rate            (proj_snaps = most-recent-season play volume)

proj_points_mean      = fitted_mean − (1 − REGRESSION) × (proj_snaps × player_rate_offset)
               = volume × [ (position + team + class baseline, unchanged)
                            + REGRESSION × (player's individual per-play deviation) ]
```

The regression factor applies only to the player's *idiosyncratic* deviation — not to position, team, or class effects, which represent the environment they'll play in next year. A QB who posted 2× the typical QB per-play rate gets projected at `1 + REGRESSION × 1` times the baseline (not 2×).

**Regression factors and year-to-year sigmas are position-specific** and re-calibrated automatically each time `train_engine.py` runs, using a 3-year rolling window of same-team, same-position player pairs regressed on **per-play rates** (exposure-weighted so low-snap noise doesn't distort the estimates). The current values live in `models/artifacts/projection_constants.json` — sigma is stored in EPA-per-play units (`sigma_rate`). Historically, QBs show the lowest persistence (most environment-dependent) and TEs the highest.

Predictive SD combines posterior uncertainty with year-to-year aleatory noise, both scaled to the player's projected volume:
```
proj_sd = sqrt(posterior_sd² + (proj_snaps × sigma_rate)²)
```

Because both terms scale with volume and data richness, a 600-snap starter and a 20-snap backup get genuinely different uncertainty bands — there is no flat sigma floor. Transfers get an additional 1.5× inflation on top of this for environment-portability uncertainty.

**Important: `proj_points_mean` assumes the player repeats last season's play volume.** The model separates per-play skill from volume, but the cumulative projection multiplies them back together using the player's **most-recent-season** involvement count as the usage assumption. Both columns are exported in `projected_points.parquet` (and the report's Raw Data tab) so the assumption is exactly auditable: `proj_points_mean = proj_points_per_play × proj_snaps`.

**Practical implication:** if you are signing a player into a role that differs significantly from their historical role — for example, a Power 4 starter you intend to use as a depth piece, or a high-efficiency backup you plan to start — the volume assumption is wrong for them, and the projection scales accordingly. You can sanity-check any candidate by hand: take their `proj_points_per_play`, multiply by the snap count you actually expect to give them, and compare. For a formal override, enter your usage-adjusted estimate in the `Manual Points Added` column of your Candidates tab.

### The Hierarchical Model

The model estimates **per-play efficiency with snap-count exposure** and uses **non-centered parameterization** throughout to avoid HMC funnel pathology. All effects are in per-play EPA units; the likelihood operates on the season total via the exposure relationship:

```
# Hyperpriors (per-play EPA scale)
σ_conf        ~ HalfNormal(0.05)          # between-conference spread
σ_team        ~ HalfNormal(0.04)          # within-conference between-team spread
σ_player[pos] ~ HalfNormal(0.15)          # between-player spread — POSITION-SPECIFIC
σ_play[pos]   ~ HalfNormal(1.5)           # single-play noise — POSITION-SPECIFIC

# Position intercepts (per-play rate)
alpha[pos] ~ Normal(0.15, 0.25)           # covers observed position means 0.04–0.24

# Non-centered random effects (avoids HMC funnel)
conf_offset_raw[c]   ~ Normal(0, 1)
conf_effect[c]        = conf_offset_raw[c] × σ_conf

team_offset_raw[t]   ~ Normal(0, 1)
team_effect[t]        = conf_effect[conf[t]] + team_offset_raw[t] × σ_team

player_offset_raw[p] ~ Normal(0, 1)
player_offset[p]      = player_offset_raw[p] × σ_player[pos[p]]

# Class-year development curve (zero-sum, identified separately from alpha)
beta_class[FR..GS,UNK] ~ ZeroSumNormal(0.05)

# Season (league environment) effect
year_effect[s] ~ ZeroSumNormal(0.03)

# Opponent quality slope (per-play)
β_opp ~ Normal(0, 0.05)                   # ±15 EPA per 1 SD schedule over 300 plays

# Robust likelihood — exposure form
ν ~ Gamma(2, 0.1)                         # Student-t dof; large ν → Normal
rate[i] = alpha[pos] + team_effect[team] + player_offset[player]
        + beta_class[class[i]] + year_effect[season[i]] + β_opp × opp_quality_z[i]
points_obs[i] ~ StudentT(ν, μ = snaps[i] × rate[i], σ = σ_play[pos] × √snaps[i])
```

Why this structure:

- **Exposure likelihood** — season-total noise scales with √snaps, so a 400-play starter pins down their per-play rate ~6× more precisely than a 12-play backup. Thin-data handling is now built into the likelihood (the old 2× "thin" noise multiplier is gone), and skill is separated from volume.
- **Position-specific σ_player and σ_play** — observed per-play spread differs by position (QB ~0.13, RB ~0.10, WR ~0.22, TE ~0.20 in the current training data), partly because `snaps_est` counts involvement plays (per-target for pass catchers, per-dropback for QBs). One pooled sigma would mis-calibrate shrinkage for every position. These are re-estimated every retrain, so the exact values drift as the training window rolls forward.
- **Student-t likelihood** — robust to outlier seasons (Heisman years, injury-shortened years). The degrees of freedom ν are estimated; if the data look Normal, ν grows large and the model collapses to the Normal version.
- **Class-year curve** — freshmen with 15 plays shrink toward the freshman baseline rather than the overall mean, and projections advance each player one class (an aging curve).
- **Year effect** — absorbs league-wide scoring drift between training seasons; projections evaluate at the zero-sum average.

Partial pooling means players with few estimated snaps are automatically shrunk toward the position/team/class baseline. Transfers get 1.5× wider predictive SD to account for scheme-portability uncertainty.

**Known confounding (V1/V2):** `team_effect` captures scheme, talent, OL quality, and schedule simultaneously — these cannot be surgically separated. A WR's EPA also depends on QB quality. The class-year curve partly reflects survivorship (weaker players exit or lose snaps between seasons), so it can overstate pure development.

### Snap Count Estimation

CFBD does not expose raw snap counts. We estimate:
```
snaps_est = snap_pct_overall × total_team_plays
```
`snap_pct_overall` from CFBD measures involvement rate (targets/carries), not physical snap presence — it systematically undercounts WRs and TEs who are on the field but not targeted. Players with `snaps_est < 10` are flagged `insufficient`; use `manual_points` in the Candidates tab for these players.

### FCS Players and Unmodeled Candidates

`projected_points.parquet` contains **FBS players only** — the CFBD PPA endpoint does not include FCS data. FCS transfers and any player not found in CFBD PPA data will have no `player_id` to look up. To include them:

1. Leave `player_id` blank in the Candidates tab
2. Enter your own points-added (EPA) estimate in the `Manual Points Added` column
3. They will appear as "user-asserted" in the output with no model CI

### projected_points.parquet Is Not an Eligibility List

The parquet file reflects **historical performance** — it includes players who have since turned professional, graduated, or are otherwise ineligible. The file does not know who is in the current transfer portal. **You curate the Candidates tab.** Only include players who are actually available to sign. The output will flag SR and GS year-class players as eligibility risks to verify, but the curation of who belongs on the board is yours.

---

## Project Structure

```
cfb/
├── environment.yml           # conda environment
├── pyproject.toml            # package metadata
├── .env.example              # CFBD_API_KEY placeholder
├── .gitignore                # excludes .env, raw data, model artifacts
│
├── pve/                      # main package
│   ├── ingest.py             # Phase 1: CFBD pull, EPA, points, snap gating
│   ├── calibration.py        # Phase 1.5: empirical year-to-year regression calibration
│   ├── points_model.py       # Phase 2: hierarchical Bayesian model (PyMC 5)
│   ├── projection.py         # Phase 3: posterior predictive projections
│   ├── financial.py          # Phase 4: win-value curve, points→wins conversion
│   ├── optimizer.py          # Phase 5: marginal value, PuLP knapsack
│   └── excel_io.py           # Phase 6: read input workbook, write report
│
├── scripts/
│   ├── train_engine.py       # Phases 1–3: pull data, fit model, export projections
│   │                         #   --data-only flag skips MCMC (fast data/calibration refresh)
│   ├── run_decision.py       # Phases 4–6: read Excel input, write report
│   └── create_template.py    # Regenerate blank input_template.xlsx
│
├── templates/
│   └── input_template.xlsx   # Blank team input workbook
│
└── data/
    ├── your_program_input.xlsx   # Worked example: filled-in team input
    └── your_program_report.xlsx  # Worked example: generated report
```

---

## V1 Limitations

| Limitation | Details |
|-----------|---------|
| **Offense only** | QB, RB, WR, TE. No lineman, defensive or special teams modeling. |
| **Future usage not modeled** | The model separates per-play skill from volume, but `proj_points_mean` still assumes each player repeats their most-recent-season play count (`proj_snaps`, exported for auditability). A former starter projected at +0.8 wins assumes starter-level usage. If you're signing that player into a different role, rescale by hand (`proj_points_per_play` × expected snaps) or use `manual_points`. |
| **Snap count approximation** | Estimated from involvement rate, not actual snap tracking. WR/TE may be undercounted. Use `manual_points` for affected players. |
| **Win curve is regular season only** | CFP, conference championships not modeled. Wins 0–12. |
| **Max bid, not market price** | Computes your program's maximum willingness to pay, not the equilibrium NIL price. Market rates are determined by what other programs bid, not modeled here. |
| **Static depth chart** | Assumes the two-deep is fixed. Does not model cascade effects (e.g., adding a QB who then frees an existing player to move). |
| **Two seasons of training data (default)** | Defaults to the two most recent completed seasons, auto-selected from the transfer-portal calendar (widen with `--num-seasons`). A zero-sum year effect absorbs league-wide scoring drift, but true skill dynamics (AR(1) evolution) are not modeled — with only two seasons they would be barely identified. The projection layer's calibrated regression factor handles persistence empirically instead. |
| **EPA_PER_WIN is hardcoded** | 130 EPA ≈ 1 win is an approximation. Adjust in `pve/financial.py` if you have a better estimate. |

---

## License

MIT. Data from [collegefootballdata.com](https://collegefootballdata.com) — see their [terms of use](https://collegefootballdata.com/externalData).
