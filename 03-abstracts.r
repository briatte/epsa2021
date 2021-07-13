library(tidyverse)
library(rvest)

fs::dir_create("data")

d <- tibble::tibble()
for (i in fs::dir_ls("html/abstracts")) {

  h <- read_html(i)

  d <- tibble::tibble(
    abstract = i,
    id = html_node(h, "meta[name='ID']") %>%
      html_attr("content"),
    paper_ref = html_node(h, "meta[name='paper_ref']") %>%
      html_attr("content"),
    title = html_node(h, "meta[name='abstracttitle']") %>%
      html_attr("content"),
    presenters = html_node(h, "meta[name='presenters']") %>%
      html_attr("content"),
    authors = html_node(h, "meta[name='authors']") %>%
      html_attr("content"),
    affiliations = html_node(h, "meta[name='affiliations']") %>%
      html_attr("content"),
    topic = html_node(h, "meta[name='topic']") %>%
      html_attr("content")
  ) %>%
    bind_rows(d)

}

d <- d %>%
  mutate(
    abstract = fs::path_file(abstract),
    presenters = str_remove_all(presenters, "(Dr|Prof)\\.") %>%
      str_squish(),
    authors = str_remove_all(authors, "(Dr|Prof)\\.") %>%
      str_squish()
  )

# View(d)

# 8 possible topics for papers
count(d, topic, sort = TRUE)

# N = 1001 authors; 170 of them (17%) authored 2 to 6 papers
str_remove_all(d$authors, "\\(.*?\\)") %>%
  str_split(",") %>%
  unlist() %>%
  str_squish() %>%
  table() %>%
  as.data.frame() %>%
  arrange(-Freq) %>%
  filter(Freq > 1)

# some authors have multiple affiliations
table(str_extract(d$authors, "\\(.*?\\)"))

# [DRAFT] trying to pair authors with affiliations
select(d, authors, affiliations) %>%
  mutate(
    ids = map(authors, str_extract_all, "\\(.*?\\)") %>%
      map(unlist) %>%
      map(str_split, ","),
    authors = str_remove_all(authors, "\\(.*?\\)") %>%
      str_split(",\\s"),
    affiliations = str_split(affiliations, ",\\s\\d\\.\\s")
  )

x$ids[ map(x$ids, length) == 0 ] <- list(1)

readr::write_tsv(d, "data/abstracts.tsv")

# wip
