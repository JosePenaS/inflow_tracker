# ============================================================
# Channel-fit scoring
# Scores each NEW query once (cached in data/scores.csv).
# Uses OpenAI when OPENAI_API_KEY is set; otherwise a keyword
# heuristic so the pipeline still works without a key.
# ============================================================

CHANNEL_BRIEF <- paste(
  "Inflow Illustrated is a small YouTube channel that explains how public and",
  "private companies make money, mostly with Sankey diagrams of revenue, costs",
  "and profit. It also makes ranking videos (biggest companies by market cap,",
  "companies that grew or lost the most money this year) and videos on how",
  "governments such as the USA, China or the EU raise and spend money.",
  "Good topics are anchored in a company, an industry, a market, an economy or",
  "a government budget, and can be explained with financial numbers.",
  "Bad topics are accidents, crimes, celebrity gossip, sports, weather,",
  "website outages, games and other news with no money story."
)

empty_scores_tbl <- function() {
  tibble::tibble(
    query = character(),
    fit_score = double(),
    category = character(),
    entity = character(),
    video_angle = character(),
    reason = character(),
    scored_by = character(),
    scored_at_utc = character()
  )
}

score_queries <- function(queries) {
  queries <- unique(stats::na.omit(queries))
  if (length(queries) == 0) return(empty_scores_tbl())

  key <- Sys.getenv("OPENAI_API_KEY")
  if (nzchar(key)) {
    out <- tryCatch(
      score_with_openai(queries, key),
      error = function(e) {
        message("OpenAI scoring failed, using keyword scoring instead: ",
                conditionMessage(e))
        NULL
      }
    )
    if (!is.null(out)) return(out)
  } else {
    message("OPENAI_API_KEY not set: using keyword scoring.")
  }
  score_with_keywords(queries)
}

# ---------- OpenAI (Responses API, structured JSON output) ----------

score_with_openai <- function(queries, key) {
  chunks <- split(queries, ceiling(seq_along(queries) / 60))
  purrr::map_dfr(chunks, function(chunk) {
    res <- openai_score_chunk(chunk, key)
    Sys.sleep(1)
    res
  })
}

openai_score_chunk <- function(queries, key) {
  schema <- list(
    type = "object",
    additionalProperties = FALSE,
    required = list("items"),
    properties = list(
      items = list(
        type = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = list("query", "fit_score", "category", "entity",
                          "video_angle", "reason"),
          properties = list(
            query = list(type = "string"),
            fit_score = list(type = "integer"),
            category = list(type = "string", enum = list(
              "company", "industry", "markets", "economy",
              "government_finance", "personal_finance", "not_a_fit")),
            entity = list(type = "string"),
            video_angle = list(type = "string"),
            reason = list(type = "string")
          )
        )
      )
    )
  )

  prompt <- paste0(
    CHANNEL_BRIEF, "\n\n",
    "Below are Google search queries that are rising fast in the US right now. ",
    "For EACH query return:\n",
    "- fit_score: 0-100, how good a video idea it is for this channel ",
    "(80+ = clear money/company/government-finance story it could cover this week; ",
    "50-79 = possible with an angle; below 30 = not a fit).\n",
    "- category.\n",
    "- entity: the company, industry, country or government involved, or \"\" if none.\n",
    "- video_angle: one short line pitching the video for this channel, or \"\" if not a fit.\n",
    "- reason: under 15 words.\n",
    "Judge from the query text and what is plausibly in the news; do not invent facts. ",
    "Return the queries exactly as written.\n\nQueries:\n",
    paste0("- ", queries, collapse = "\n")
  )

  body <- list(
    model = OPENAI_MODEL,
    input = prompt,
    text = list(format = list(
      type = "json_schema", name = "query_scores", strict = TRUE, schema = schema
    ))
  )

  res <- curl_request(
    "https://api.openai.com/v1/responses",
    method = "POST",
    headers = c(paste0("Authorization: Bearer ", key),
                "Content-Type: application/json"),
    body_text = as.character(jsonlite::toJSON(body, auto_unbox = TRUE)),
    timeout_s = 240
  )
  if (!is_ok(res)) {
    stop("HTTP ", res$status, ": ", substr(res$body, 1, 500))
  }

  parsed <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  text <- openai_output_text(parsed)
  items <- jsonlite::fromJSON(text, simplifyVector = TRUE)$items

  if (is.null(items) || NROW(items) == 0) stop("empty scoring result")

  tibble::as_tibble(items) |>
    dplyr::filter(query %in% queries) |>
    dplyr::mutate(
      fit_score = pmin(pmax(as.numeric(fit_score), 0), 100),
      scored_by = OPENAI_MODEL,
      scored_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ) |>
    dplyr::distinct(query, .keep_all = TRUE) |>
    # anything the model skipped falls back to keywords
    (\(df) dplyr::bind_rows(df, score_with_keywords(setdiff(queries, df$query))))()
}

openai_output_text <- function(parsed) {
  if (!is.null(parsed$output_text)) return(parsed$output_text)
  for (o in parsed$output %||% list()) {
    for (c in o$content %||% list()) {
      if (identical(c$type, "output_text")) return(c$text)
    }
  }
  stop("no output_text in OpenAI response")
}

# ---------- Keyword fallback ----------

FIT_PATTERNS <- c(
  company = "\\b(stock|stocks|shares|earnings|revenue|profit|ipo|ceo|layoffs?|bankrupt\\w*|merger|acquisition|acquire|buyout|valuation|market cap|dividend|sales|subscribers|price increase|raising prices|closing stores|store closures|company|inc|corp)\\b",
  economy = "\\b(inflation|recession|interest rates?|fed|gdp|unemployment|jobs report|economy|tariffs?|trade war|shortage|prices|gas prices|egg prices|housing market|mortgage rates)\\b",
  government_finance = "\\b(budget|deficit|national debt|debt ceiling|shutdown|taxes|tax|irs|spending|stimulus|social security|medicare|snap|subsidy|subsidies)\\b",
  markets = "\\b(stock market|dow|nasdaq|s&p|bitcoin|crypto|gold price|oil price|bond|treasury|market crash|sell ?off)\\b"
)

NOISE_PATTERN <- "\\b(crash(ed)?|plane|jet|shooting|shot|killed|died|dead|death|murder|arrested|fire|storm|hurricane|tornado|earthquake|weather|down|outage|not working|game|score|nfl|nba|mlb|nhl|lyrics|movie|episode|season|trailer|celebrity|dating|divorce|pregnant|wedding)\\b"

score_with_keywords <- function(queries) {
  if (length(queries) == 0) return(empty_scores_tbl())
  q <- tolower(queries)
  hits <- vapply(FIT_PATTERNS, function(p) grepl(p, q, perl = TRUE), logical(length(q)))
  if (is.null(dim(hits))) hits <- matrix(hits, nrow = 1, dimnames = list(NULL, names(FIT_PATTERNS)))
  n_hits <- rowSums(hits)
  noise <- grepl(NOISE_PATTERN, q, perl = TRUE)

  # "crash" inside a market phrase is not noise
  noise <- noise & !grepl("market crash|stock crash|crypto crash", q)

  score <- pmin(30 + 25 * n_hits, 90)
  score <- ifelse(noise & n_hits == 0, 5, ifelse(noise, score - 25, score))
  score <- ifelse(n_hits == 0 & !noise, 20, score)

  category <- apply(hits, 1, function(r) if (any(r)) names(FIT_PATTERNS)[which(r)[1]] else "not_a_fit")

  tibble::tibble(
    query = queries,
    fit_score = as.numeric(score),
    category = unname(category),
    entity = "",
    video_angle = "",
    reason = ifelse(n_hits > 0, "keyword match", ifelse(noise, "news/noise keywords", "no money keywords")),
    scored_by = "keywords",
    scored_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}
