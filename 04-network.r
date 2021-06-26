library(tidyverse)
library(ggraph)
library(tidygraph)

# [DRAFT] quick and dirty overview of main component

d <- readr::read_tsv("data/abstracts.tsv")

e <- str_split(d$affiliations, "(,\\s)?\\d\\.\\s") %>%
  map(str_subset, "\\w") %>%
  # weight = 1 / number of organizations
  map_dfr(~ crossing(i = .x, j = .x, w = 1 / (length(.x) - 1))) %>%
  # de-duplicate, remove self-ties
  filter(i < j)

# weights range 0.25-1
table(e$w)

tidygraph::as_tbl_graph(e) %>%
  tidygraph::activate(nodes) %>%
  mutate(
    wdegree = tidygraph::centrality_degree(weights = w),
    group = tidygraph::group_components(),
    label = str_remove_all(name, "\\sof"),
    label = if_else(
      str_count(label, "\\s") > 1,
      str_split(label, "\\s") %>%
        map(str_sub, 1, 1) %>%
        map_chr(str_c, collapse = ""),
      label
    ),
    label = if_else(wdegree > 3, label, NA_character_) %>%
      str_remove("University") %>%
      str_squish()
  ) %>%
  filter(group == 1) %>%
  ggraph::ggraph(layout = "stress") +
  ggraph::geom_edge_link0() +
  ggraph::geom_node_point(aes(size = wdegree)) +
  ggraph::geom_node_label(aes(label = label)) +
  ggraph::theme_graph() +
  guides(size = "none")

# ggsave("example-network.png", width = 7, height = 7)

# wip
