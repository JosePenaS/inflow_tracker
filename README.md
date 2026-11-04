# Inflow trend radar

Twice a day, collects the **rising** Google Trends related queries in the US
(last 1 hour, 4 hours and 1 day) for a few seed searches such as "why",
scores each query for fit with the Inflow Illustrated YouTube channel,
publishes a dashboard to GitHub Pages, and sends a phone alert when a strong
new idea shows up.

Built like `consumer-demand-intelligence`: R + curl for Google Trends,
OpenAI for scoring, Quarto for the site, GitHub Actions on a schedule.

## Files

| File | What it does |
|---|---|
| `seeds.txt` | Seed searches, one per line |
| `run_radar.R` | Main script: collect → save → score → alert. Settings at the top |
| `R/trends.R` | Google Trends requests (rising related queries only) |
| `R/scoring.R` | Channel-fit scoring (OpenAI, keyword fallback) |
| `R/notify.R` | ntfy phone alerts |
| `index.qmd`, `health.qmd`, `about.qmd` | Dashboard pages |
| `data/*.csv` | History, written by each run |
| `.github/workflows/radar.yml` | Schedule: 11:xx and 17:xx Chile time |

## Setup (once)

1. **Create the repo** on GitHub (public keeps Actions free) and push these files to `main`.
2. **Create the `gh-pages` branch** (Quarto publishes there):
   ```
   git checkout --orphan gh-pages
   git reset --hard
   git commit --allow-empty -m "Initialize gh-pages"
   git push origin gh-pages
   git checkout main
   ```
3. **Settings → Pages:** Source = *Deploy from a branch*, Branch = `gh-pages` / root.
4. **Settings → Actions → General → Workflow permissions:** *Read and write permissions*.
5. **Settings → Secrets and variables → Actions → New repository secret:**
   - `OPENAI_API_KEY`: same key as in consumer-demand-intelligence (optional; without it, keyword scoring is used).
   - `NTFY_TOPIC`: a long, hard-to-guess name, e.g. `inflow-radar-7f3k9q2x`. Anyone who knows it can read your alerts.
6. **Phone:** install the **ntfy** app (iOS / Android), tap **+**, and subscribe to the same topic on `ntfy.sh`.
7. **First run:** Actions tab → *Trend radar* → *Run workflow*. It takes ~15 minutes
   (the script pauses 20–45 s between Google requests to avoid HTTP 429).
   The dashboard appears at `https://<your-user>.github.io/<repo-name>/`.

## Running locally (RStudio)

Open the folder, then `source("run_radar.R")`. Set `Sys.setenv(OPENAI_API_KEY = "...", NTFY_TOPIC = "...")`
first if you want scoring and alerts. Preview the dashboard with `quarto preview`.

## When Google says no

Google Trends can answer `429 Too Many Requests`. The script retries with
pauses, stops early after three rate-limited searches in a row, saves the run
log anyway, and marks the workflow as failed so GitHub emails you. The
**Run health** page shows how recent runs went. If it keeps happening, remove
seeds or increase `SLEEP_MIN` / `SLEEP_MAX` in `run_radar.R`.
