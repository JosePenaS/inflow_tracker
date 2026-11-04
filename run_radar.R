# ============================================================
# Inflow Trend Radar
# ------------------------------------------------------------
# 1) For each seed in seeds.txt and each window (1h, 4h, 1 day),
#    collect Google Trends' RISING related queries for the US.
# 2) Append them to data/rising_queries.csv (history).
# 3) Score new queries for channel fit (OpenAI, or keywords).
# 4) Send one phone alert (ntfy) listing new high-fit ideas.
#
# Run locally from the repo folder with:  Rscript run_radar.R
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ------------------------------------------------------------
# Configuration
# ------------------------------------------------------------

GEO <- "US"
LOCAL_TZ <- "America/Santiago"

WINDOWS <- c(
  "1h"  = "now 1-H",
  "4h"  = "now 4-H",
  "24h" = "now 1-d"
)

# Pauses between Google requests (seconds). Larger = fewer 429 errors.
SLEEP_MIN <- 20
SLEEP_MAX <- 45
RETRY_WAIT_SECONDS <- 60
# Stop the run early after this many rate-limited fetches in a row.
STOP_AFTER_CONSECUTIVE_429 <- 3

OPENAI_MODEL <- "gpt-5-mini"

# Phone alert rules
ALERT_MIN_FIT <- 70         # channel-fit score 0-100
ALERT_MIN_GROWTH <- 300     # % growth (Breakout counts as 5000)
ALERT_COOLDOWN_DAYS <- 7    # don't alert the same query twice within this
ALERT_MAX_ITEMS <- 5

KEEP_HISTORY_DAYS <- 90

# ------------------------------------------------------------
# Paths and helpers
# ------------------------------------------------------------

ROOT <- {
  ws <- Sys.getenv("GITHUB_WORKSPACE")
  if (nzchar(ws)) ws else getwd()
}
DATA_DIR <- file.path(ROOT, "data")
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

for (f in c("http.R", "trends.R", "scoring.R", "notify.R")) {
  source(file.path(ROOT, "R", f), local = FALSE)
}

FILES <- list(
  rising = file.path(DATA_DIR, "rising_queries.csv"),
  log    = file.path(DATA_DIR, "run_log.csv"),
  scores = file.path(DATA_DIR, "scores.csv"),
  alerts = file.path(DATA_DIR, "alerts.csv")
)

read_or_empty <- function(path, empty) {
  if (!file.exists(path)) return(empty)
  out <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                         show_col_types = FALSE, progress = FALSE)
  # restore numeric/logical columns using the empty template's types
  for (col in names(empty)) {
    if (!col %in% names(out)) {
      out[[col]] <- rep(NA, nrow(out))
    }
    tmpl <- empty[[col]]
    if (is.numeric(tmpl)) out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
    if (is.logical(tmpl)) out[[col]] <- as.logical(out[[col]])
    if (is.integer(tmpl)) out[[col]] <- suppressWarnings(as.integer(out[[col]]))
  }
  out[, names(empty)]
}

empty_rising_history <- function() tibble(
  run_id = character(), run_time_chile = character(), window = character(),
  seed = character(), rank = integer(), query = character(),
  value_raw = character(), growth_pct = double(), is_breakout = logical(),
  trends_link = character()
)

empty_log <- function() tibble(
  run_id = character(), run_time_chile = character(), window = character(),
  seed = character(), status = character(), http = integer(),
  n_rising = integer(), note = character()
)

empty_alerts <- function() tibble(
  alerted_at_utc = character(), run_id = character(), query = character(),
  fit_score = double(), max_growth = double()
)

read_seeds <- function(path = file.path(ROOT, "seeds.txt")) {
  x <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8"))
  unique(x[nzchar(x) & !startsWith(x, "#")])
}

dashboard_url <- function() {
  explicit <- Sys.getenv("DASHBOARD_URL")
  if (nzchar(explicit)) return(explicit)
  repo <- Sys.getenv("GITHUB_REPOSITORY")          # "owner/repo"
  if (!nzchar(repo)) return("")
  parts <- strsplit(repo, "/")[[1]]
  paste0("https://", tolower(parts[1]), ".github.io/", parts[2], "/")
}

# ------------------------------------------------------------
# 1) Collect
# ------------------------------------------------------------

now_utc <- Sys.time()
RUN_ID <- format(now_utc, "%Y-%m-%d %H:%M", tz = "UTC")
RUN_TIME_CHILE <- format(now_utc, "%Y-%m-%d %H:%M", tz = LOCAL_TZ)

seeds <- read_seeds()
message("Run ", RUN_ID, " UTC (", RUN_TIME_CHILE, " Chile) | seeds: ",
        paste(seeds, collapse = ", "))

jobs <- expand.grid(
  seed = seeds, window = names(WINDOWS), stringsAsFactors = FALSE
)

cookie_file <- tempfile(fileext = ".cookies")
start_trends_session(cookie_file)
Sys.sleep(runif(1, 3, 6))

collected <- list()
log_rows <- list()
consecutive_429 <- 0

for (i in seq_len(nrow(jobs))) {
  seed <- jobs$seed[i]
  win <- jobs$window[i]
  time_code <- WINDOWS[[win]]

  if (consecutive_429 >= STOP_AFTER_CONSECUTIVE_429) {
    log_rows[[length(log_rows) + 1]] <- tibble(
      run_id = RUN_ID, run_time_chile = RUN_TIME_CHILE, window = win, seed = seed,
      status = "skipped", http = NA_integer_, n_rising = 0L,
      note = "skipped after repeated rate limiting"
    )
    next
  }

  message(sprintf("[%d/%d] '%s' | last %s", i, nrow(jobs), seed, win))

  res <- tryCatch(
    fetch_rising_queries(seed, time_code, cookie_file),
    error = function(e) list(status = "error", http = NA_integer_,
                             rows = empty_rising_tbl(), note = conditionMessage(e))
  )

  consecutive_429 <- if (res$status == "rate_limited") consecutive_429 + 1 else 0
  message("    -> ", res$status, " (", nrow(res$rows), " rising)",
          if (nzchar(res$note)) paste0(" ", res$note) else "")

  log_rows[[length(log_rows) + 1]] <- tibble(
    run_id = RUN_ID, run_time_chile = RUN_TIME_CHILE, window = win, seed = seed,
    status = res$status, http = as.integer(res$http),
    n_rising = nrow(res$rows), note = substr(res$note, 1, 200)
  )

  if (nrow(res$rows) > 0) {
    collected[[length(collected) + 1]] <- res$rows |>
      mutate(
        run_id = RUN_ID, run_time_chile = RUN_TIME_CHILE,
        window = win, seed = seed,
        trends_link = vapply(query, trends_explore_link, character(1),
                             time_code = time_code, USE.NAMES = FALSE)
      )
  }

  if (i < nrow(jobs)) Sys.sleep(runif(1, SLEEP_MIN, SLEEP_MAX))
}

run_rising <- bind_rows(empty_rising_history(), bind_rows(collected)) |>
  select(names(empty_rising_history()))
run_log <- bind_rows(empty_log(), bind_rows(log_rows))

# ------------------------------------------------------------
# 2) Save history
# ------------------------------------------------------------

cutoff <- format(now_utc - KEEP_HISTORY_DAYS * 86400, "%Y-%m-%d %H:%M", tz = "UTC")

history <- read_or_empty(FILES$rising, empty_rising_history()) |>
  bind_rows(run_rising) |>
  filter(run_id >= cutoff)
write_csv(history, FILES$rising, na = "")

log_history <- read_or_empty(FILES$log, empty_log()) |>
  bind_rows(run_log) |>
  filter(run_id >= cutoff)
write_csv(log_history, FILES$log, na = "")

message("Saved ", nrow(run_rising), " rising rows (",
        n_distinct(run_rising$query), " unique queries).")

# ------------------------------------------------------------
# 3) Score new queries
# ------------------------------------------------------------

scores <- read_or_empty(FILES$scores, empty_scores_tbl())
new_queries <- setdiff(unique(run_rising$query), scores$query)

if (length(new_queries) > 0) {
  message("Scoring ", length(new_queries), " new queries...")
  scores <- bind_rows(scores, score_queries(new_queries)) |>
    distinct(query, .keep_all = TRUE)
  write_csv(scores, FILES$scores, na = "")
}

# ------------------------------------------------------------
# 4) Phone alert for new high-fit ideas
# ------------------------------------------------------------

if (nrow(run_rising) > 0) {
  this_run <- run_rising |>
    group_by(query) |>
    summarise(
      max_growth = suppressWarnings(max(growth_pct, na.rm = TRUE)),
      breakout = any(is_breakout),
      windows = paste(intersect(names(WINDOWS), unique(window)), collapse = ","),
      .groups = "drop"
    ) |>
    left_join(scores, by = "query")

  alerts <- read_or_empty(FILES$alerts, empty_alerts())
  cooldown <- format(now_utc - ALERT_COOLDOWN_DAYS * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  recently_alerted <- alerts$query[which(alerts$alerted_at_utc >= cooldown)]

  picks <- this_run |>
    filter(fit_score >= ALERT_MIN_FIT,
           max_growth >= ALERT_MIN_GROWTH,
           !query %in% recently_alerted) |>
    arrange(desc(fit_score), desc(max_growth)) |>
    slice_head(n = ALERT_MAX_ITEMS)

  if (nrow(picks) > 0) {
    lines <- sprintf(
      "%d  %s  (%s, %s)%s",
      as.integer(picks$fit_score), picks$query,
      ifelse(picks$breakout, "Breakout", paste0("+", formatC(picks$max_growth, format = "d", big.mark = ","), "%")),
      picks$windows,
      ifelse(nzchar(coalesce(picks$video_angle, "")), paste0("\n    ", picks$video_angle), "")
    )
    title <- sprintf("%d new video idea%s trending", nrow(picks), if (nrow(picks) > 1) "s" else "")
    sent <- send_ntfy(title, paste(lines, collapse = "\n"),
                      click_url = dashboard_url(),
                      priority = if (max(picks$fit_score) >= 85) "high" else "default")

    if (isTRUE(sent)) {
      alerts <- bind_rows(alerts, tibble(
        alerted_at_utc = format(now_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        run_id = RUN_ID, query = picks$query,
        fit_score = picks$fit_score, max_growth = picks$max_growth
      ))
      write_csv(alerts, FILES$alerts, na = "")
    }
  } else {
    message("No new queries passed the alert rules this run.")
  }
}

# ------------------------------------------------------------
# 5) Exit status: fail the workflow if Google blocked everything,
#    so GitHub emails you. Data and logs are already saved.
# ------------------------------------------------------------

n_answered <- sum(run_log$status %in% c("ok", "empty"))
message("Fetches answered by Google: ", n_answered, "/", nrow(run_log))

if (n_answered == 0) {
  message("Google Trends did not answer any request (likely HTTP 429 rate limiting).")
  quit(status = 1, save = "no")
}
