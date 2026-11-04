# Helpers used by the Quarto pages (index.qmd, health.qmd).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(DT)
  library(htmltools)
})

LOCAL_TZ <- "America/Santiago"
WINDOW_ORDER <- c("1h", "4h", "24h")

read_data <- function(name) {
  path <- file.path("data", paste0(name, ".csv"))
  if (!file.exists(path)) return(NULL)
  df <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                        show_col_types = FALSE, progress = FALSE)
  if (nrow(df) == 0) return(NULL)
  df
}

num <- function(x) suppressWarnings(as.numeric(x))

growth_label <- function(growth, breakout) {
  ifelse(
    breakout,
    "Breakout",
    ifelse(is.na(growth), "",
           paste0("+", formatC(growth, format = "d", big.mark = ","), "%"))
  )
}

link_html <- function(text, url) {
  ifelse(
    is.na(url) | !nzchar(url),
    htmlEscape(text),
    sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>',
            htmlEscape(url, attribute = TRUE), htmlEscape(text))
  )
}

fit_badge <- function(score) {
  cls <- ifelse(is.na(score), "fit-na",
         ifelse(score >= 70, "fit-high", ifelse(score >= 40, "fit-mid", "fit-low")))
  lab <- ifelse(is.na(score), "–", as.character(as.integer(score)))
  sprintf('<span class="fit %s">%s</span>', cls, lab)
}

# One row per query, combining all windows/seeds of the given rows.
summarise_queries <- function(rows, scores) {
  rows |>
    mutate(growth = num(growth_pct), breakout = as.logical(is_breakout)) |>
    group_by(query) |>
    arrange(desc(growth), .by_group = TRUE) |>
    summarise(
      best_growth = suppressWarnings(max(growth, na.rm = TRUE)),
      breakout = any(breakout, na.rm = TRUE),
      windows = paste(intersect(WINDOW_ORDER, unique(window)), collapse = " · "),
      n_windows = length(intersect(WINDOW_ORDER, unique(window))),
      seeds = paste(unique(seed), collapse = ", "),
      link = first(trends_link),
      .groups = "drop"
    ) |>
    left_join(
      (scores %||% tibble(query = character())) |>
        select(any_of(c("query", "fit_score", "category", "entity", "video_angle", "reason"))),
      by = "query"
    ) |>
    add_missing_cols(c("fit_score", "category", "entity", "video_angle", "reason")) |>
    mutate(fit_score = num(fit_score))
}

add_missing_cols <- function(df, cols) {
  for (col in cols) if (!col %in% names(df)) df[[col]] <- rep(NA_character_, nrow(df))
  df
}

`%||%` <- function(x, y) if (is.null(x)) y else x

dt_table <- function(df, page_length = 15) {
  # Rows arrive already sorted in R, so DT keeps that order.
  DT::datatable(
    df,
    escape = FALSE,
    rownames = FALSE,
    class = "compact stripe hover",
    options = list(
      pageLength = page_length,
      order = list(),
      autoWidth = FALSE,
      scrollX = TRUE,
      dom = "ftip"
    )
  )
}

note <- function(text) htmltools::p(class = "empty-note", text)
