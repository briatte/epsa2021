library(tidyverse)
library(rvest)

fs::dir_create("html")

# no need to collect day 1 (pre-session meetings only)
for (i in str_c("https://coms.events/epsa2021/en/day_", 2:3, ".html")) {

  f <- fs::path("html", fs::path_file(i))
  cat(f, ": ")

  if (!fs::file_exists(f)) {
    download.file(i, f, mode = "wb", quiet = TRUE)
  }

  s <- read_html(f) %>%
    html_nodes(xpath = "//a[contains(@href, 'session_')]") %>%
    html_attr("href") %>%
    str_replace("^\\.\\.", "https://coms.events/epsa2021")

  cat(length(s), "sessions\n")

  for (j in s) {

    f <- fs::path("html", fs::path_file(j))
    cat(f, ": ")

    if (!fs::file_exists(f)) {
      download.file(j, f, mode = "wb", quiet = TRUE)
    }

    a <- read_html(f) %>%
      html_nodes(xpath = "//a[contains(@href, 'abstract_')]") %>%
      html_attr("href") %>%
      str_replace("^\\.\\./\\.\\.", "https://coms.events/epsa2021/data")

    cat(length(a), "abstracts")

    for (k in a) {

      f <- fs::path("html", fs::path_file(k))
      if (!fs::file_exists(f)) {
        # `try` because of a few malformed URIs (as of 2021-06-22)
        # e.g. 'https://coms.events/epsa2021/data/abstracts/en/<abstract_125>'
        # in session 35
        try(download.file(k, f, mode = "wb", quiet = TRUE), silent = TRUE)
      }
      cat(".")

    }

    cat("\n")

  }

}

cat(
  length(fs::dir_ls("html", regexp = "session")), "sessions,",
  length(fs::dir_ls("html", regexp = "abstract")), "abstracts.\n"
)

# kthxbye
