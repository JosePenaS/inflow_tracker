# ============================================================
# HTTP helper: every request goes through the system curl
# (curl.exe on Windows, curl elsewhere), same approach as
# consumer-demand-intelligence.
# ============================================================

CURL_BIN <- if (.Platform$OS.type == "windows") "curl.exe" else "curl"

BROWSER_UA <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/145.0.0.0 Safari/537.36"
)

# Returns list(status = <int>, body = <chr>). Never throws on HTTP errors;
# callers decide what a non-2xx status means.
curl_request <- function(
    url,
    method = "GET",
    headers = character(),
    body_text = NULL,
    cookie_file = NULL,
    timeout_s = 90
) {
  output_file <- tempfile(fileext = ".txt")
  body_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(output_file, body_file), force = TRUE), add = TRUE)

  args <- c(
    "--silent", "--show-error", "--location", "--compressed",
    "--request", method,
    "--connect-timeout", "20",
    "--max-time", as.character(timeout_s),
    "--output", shQuote(output_file),
    "--write-out", "%{http_code}"
  )

  for (h in headers) {
    args <- c(args, "--header", shQuote(h))
  }

  if (!is.null(cookie_file)) {
    args <- c(args, "--cookie-jar", shQuote(cookie_file),
              "--cookie", shQuote(cookie_file))
  }

  if (!is.null(body_text)) {
    writeLines(enc2utf8(body_text), body_file, useBytes = TRUE)
    args <- c(args, "--data-binary", paste0("@", shQuote(body_file)))
  }

  args <- c(args, shQuote(url))

  result <- tryCatch(
    suppressWarnings(system2(CURL_BIN, args = args, stdout = TRUE, stderr = TRUE)),
    error = function(e) e
  )

  status <- NA_integer_
  if (!inherits(result, "error") && length(result) > 0) {
    status <- suppressWarnings(as.integer(tail(result, 1)))
  }

  body <- ""
  if (file.exists(output_file) && isTRUE(file.info(output_file)$size > 0)) {
    body <- paste(readLines(output_file, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
  }

  list(status = status, body = body)
}

is_ok <- function(res) {
  !is.na(res$status) && res$status >= 200 && res$status < 300
}

url_encode <- function(x) {
  utils::URLencode(enc2utf8(as.character(x)), reserved = TRUE)
}

build_query_url <- function(base_url, params) {
  pairs <- vapply(
    names(params),
    function(n) paste0(n, "=", url_encode(params[[n]])),
    character(1)
  )
  paste0(base_url, "?", paste(pairs, collapse = "&"))
}
