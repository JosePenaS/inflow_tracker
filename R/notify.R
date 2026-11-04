# ============================================================
# Phone alerts through ntfy (https://ntfy.sh, free app for iOS/Android).
# Set the NTFY_TOPIC secret to a hard-to-guess topic name and
# subscribe to the same topic in the ntfy app.
# ============================================================

send_ntfy <- function(title, message_text, click_url = NULL,
                      priority = "default", tags = "chart_with_upwards_trend") {
  topic <- Sys.getenv("NTFY_TOPIC")
  if (!nzchar(topic)) {
    message("NTFY_TOPIC not set: skipping phone alert.")
    return(invisible(FALSE))
  }
  server <- Sys.getenv("NTFY_SERVER", "https://ntfy.sh")

  headers <- c(
    paste0("Title: ", ascii_only(title)),
    paste0("Priority: ", priority),
    paste0("Tags: ", tags),
    "Content-Type: text/plain; charset=utf-8"
  )
  if (!is.null(click_url) && nzchar(click_url)) {
    headers <- c(headers, paste0("Click: ", click_url))
  }

  res <- curl_request(
    paste0(sub("/$", "", server), "/", topic),
    method = "POST",
    headers = headers,
    body_text = message_text,
    timeout_s = 30
  )
  message("ntfy alert: HTTP ", res$status)
  invisible(is_ok(res))
}

# HTTP headers must be plain ASCII
ascii_only <- function(x) {
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
}
