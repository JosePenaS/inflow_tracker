# ============================================================
# Google Trends client (unofficial web endpoints)
# Only collects the "rising" related queries widget, which keeps
# the number of requests per seed/window down to two.
# ============================================================

TRENDS_HEADERS <- c(
  paste0("User-Agent: ", BROWSER_UA),
  "Accept: application/json,text/plain,*/*",
  "Accept-Language: en-US,en;q=0.9",
  "Referer: https://trends.google.com/"
)

# Google Trends wants the timezone as minutes WEST of UTC.
# Chile is UTC-3 in summer and UTC-4 in winter, so compute it.
trends_tz_offset <- function(tz = LOCAL_TZ) {
  z <- format(as.POSIXlt(Sys.time(), tz = tz), "%z")   # e.g. "-0300"
  sign <- ifelse(substr(z, 1, 1) == "-", -1, 1)
  minutes <- as.integer(substr(z, 2, 3)) * 60 + as.integer(substr(z, 4, 5))
  as.character(-sign * minutes)
}

parse_trends_json <- function(text) {
  clean <- sub("^\\)\\]\\}',?\\s*", "", text)
  jsonlite::fromJSON(clean, simplifyVector = FALSE)
}

# GET with polite retry on 429 / transient errors.
trends_get <- function(url, cookie_file) {
  waits <- c(0, RETRY_WAIT_SECONDS, RETRY_WAIT_SECONDS * 2)
  last <- NULL
  for (w in waits) {
    if (w > 0) {
      message("    waiting ", w, "s before retrying (last status ", last$status, ")")
      Sys.sleep(w)
    }
    last <- curl_request(url, headers = TRENDS_HEADERS, cookie_file = cookie_file)
    if (is_ok(last)) return(last)
    # 4xx other than 429 will not get better with a retry
    if (!is.na(last$status) && last$status >= 400 && last$status < 500 &&
        last$status != 429) return(last)
  }
  last
}

start_trends_session <- function(cookie_file) {
  res <- curl_request(
    "https://trends.google.com/trends/?geo=US",
    headers = TRENDS_HEADERS,
    cookie_file = cookie_file
  )
  message("Trends session start: HTTP ", res$status)
  invisible(res$status)
}

# Public link a human can click to see the same view.
trends_explore_link <- function(query, time_code, geo = GEO) {
  build_query_url(
    "https://trends.google.com/trends/explore",
    list(q = query, date = time_code, geo = geo, hl = "en-US")
  )
}

empty_rising_tbl <- function() {
  tibble::tibble(
    rank = integer(),
    query = character(),
    value_raw = character(),
    growth_pct = double(),
    is_breakout = logical()
  )
}

# Returns list(status = "ok" | "empty" | "rate_limited" | "error",
#              http = <int>, rows = <tibble>, note = <chr>)
fetch_rising_queries <- function(seed, time_code, cookie_file,
                                 geo = GEO, hl = "en-US") {
  tz <- trends_tz_offset()

  explore_req <- list(
    comparisonItem = list(list(keyword = seed, geo = geo, time = time_code)),
    category = 0,
    property = ""
  )

  explore_url <- build_query_url(
    "https://trends.google.com/trends/api/explore",
    list(
      hl = hl, tz = tz,
      req = as.character(jsonlite::toJSON(explore_req, auto_unbox = TRUE))
    )
  )

  res <- trends_get(explore_url, cookie_file)
  if (!is_ok(res)) {
    return(list(
      status = if (identical(res$status, 429L)) "rate_limited" else "error",
      http = res$status, rows = empty_rising_tbl(), note = "explore request failed"
    ))
  }

  explore <- tryCatch(parse_trends_json(res$body), error = function(e) NULL)
  widgets <- explore$widgets %||% list()
  rq <- purrr::keep(widgets, ~ identical(toupper(.x$id %||% ""), "RELATED_QUERIES"))

  if (length(rq) == 0 || is.null(rq[[1]]$token)) {
    return(list(status = "empty", http = res$status, rows = empty_rising_tbl(),
                note = "no related-queries widget (not enough search volume)"))
  }
  rq <- rq[[1]]

  Sys.sleep(runif(1, 2, 5))

  widget_url <- build_query_url(
    "https://trends.google.com/trends/api/widgetdata/relatedsearches",
    list(
      hl = hl, tz = tz,
      req = as.character(jsonlite::toJSON(rq$request, auto_unbox = TRUE, null = "null")),
      token = rq$token
    )
  )

  res2 <- trends_get(widget_url, cookie_file)
  if (!is_ok(res2)) {
    return(list(
      status = if (identical(res2$status, 429L)) "rate_limited" else "error",
      http = res2$status, rows = empty_rising_tbl(), note = "related-queries request failed"
    ))
  }

  data <- tryCatch(parse_trends_json(res2$body), error = function(e) NULL)
  ranked <- data$default$rankedList %||% list()

  # rankedList[[1]] = top, rankedList[[2]] = rising
  rising <- if (length(ranked) >= 2) ranked[[2]]$rankedKeyword %||% list() else list()

  if (length(rising) == 0) {
    return(list(status = "empty", http = res2$status, rows = empty_rising_tbl(),
                note = "no rising queries in this window"))
  }

  rows <- purrr::imap_dfr(rising, function(r, i) {
    raw <- as.character(r$formattedValue %||% r$value %||% NA_character_)
    tibble::tibble(
      rank = as.integer(i),
      query = as.character(r$query %||% NA_character_),
      value_raw = raw,
      growth_pct = parse_growth(raw),
      is_breakout = grepl("breakout", tolower(raw))
    )
  })

  list(status = "ok", http = res2$status, rows = rows, note = "")
}

# "+3,550%" -> 3550, "Breakout" -> 5000 (Trends' Breakout means >5000%)
parse_growth <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    grepl("breakout", tolower(x)) ~ 5000,
    TRUE ~ suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
  )
}
