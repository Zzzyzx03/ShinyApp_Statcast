# Pitch Quality Lab

A pitcher's-eye, fully filterable Shiny dashboard built on Baseball Savant pitch-level data.

**Core metric:** CSW% = (Called Strikes + Whiffs) / Total Pitches

---

## Features

| Panel | What it shows |
|---|---|
| **Summary table** | CSW%, Whiff%, Called% for the current slice overall and by pitch type |
| **CSW% heatmap** | Catcher's-view 2D heatmap of where pitches earn strikes, updates live with every filter |
| **Chi-square test** | Random-sample stacked bar chart testing whether pitch type and strike outcome are independent |
| **Takeaway bar** | One-line summary of the active slice: filter count, pitch count, CSW/whiff/called rates |

---

## Filters (stack any combination)

- Pitch type
- Velocity bin (mph)
- Spin-rate bin (rpm)
- Count (balls-strikes)
- Total movement
- Release extension
- Handedness matchup
- Opponent (batting team)

---

## Requirements

```r
install.packages(c("shiny", "ggplot2"))
```

R ≥ 4.1 recommended.

---

## Data

The app expects a Baseball Savant CSV export named **`savant_data_2.csv`** placed in the same folder as `app.R`.

If the file is not found on disk, the sidebar shows a file uploader so you can drag-and-drop the CSV at runtime — no restart needed.

### Required columns

| Column | Type | Notes |
|---|---|---|
| `description` | string | Statcast pitch outcome (e.g. `swinging_strike`, `called_strike`) |
| `pitch_name` | string | Pitch type label |
| `release_speed` | numeric | Velocity in mph |
| `release_spin_rate` | numeric | Spin rate in rpm |
| `balls` | integer | Ball count before the pitch |
| `strikes` | integer | Strike count before the pitch |
| `pfx_x` | numeric | Horizontal movement in feet |
| `pfx_z` | numeric | Vertical movement in feet |
| `release_extension` | numeric | Extension in feet |
| `p_throws` | string | Pitcher handedness (`R` / `L`) |
| `stand` | string | Batter handedness (`R` / `L`) |
| `home_team` | string | Home team abbreviation |
| `away_team` | string | Away team abbreviation |
| `inning_topbot` | string | `Top` or `Bot` |
| `plate_x` | numeric | Horizontal pitch location in feet |
| `plate_z` | numeric | Vertical pitch location in feet |
| `sz_top` | numeric | Top of strike zone in feet |
| `sz_bot` | numeric | Bottom of strike zone in feet |
| `type` *(optional)* | string | Statcast outcome type: `B`, `S`, or `X`. Used for the chi-square binary. Falls back to CSW if absent. |

You can pull this data from [Baseball Savant](https://baseballsavant.mlb.com/statcast_search) using their CSV export or via the `baseballr` package.

---

## Running locally

```r
# Option 1 — from within the project folder
shiny::runApp("app.R")

# Option 2 — from any directory
shiny::runApp("/path/to/pitch-quality-lab/app.R")
```

Place `savant_data_2.csv` in the same directory as `app.R` before launching, or upload it via the sidebar after the app opens.

---

## Deploying to shinyapps.io

```r
install.packages("rsconnect")
rsconnect::deployApp("/path/to/pitch-quality-lab")
```

Make sure `savant_data_2.csv` is in the project folder before deploying, or rely on the runtime uploader (note: uploaded files are not persisted across sessions on shinyapps.io).

---

## Project structure

```
pitch-quality-lab/
├── app.R               # Complete Shiny app (UI + server in one file)
├── savant_data_2.csv   # Your data file — not tracked by git (see .gitignore)
└── README.md
```

---

## .gitignore

Add this to keep your CSV out of version control:

```
savant_data_2.csv
*.csv
.Rhistory
.RData
rsconnect/
```
