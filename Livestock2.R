required <- c(
  "tidyverse",  # dplyr, tidyr, ggplot2, readr, etc.
  "janitor",    # clean_names, tabyl
  "gt",         # pretty tables
  "forcats",    # factor helpers
  "scales",# label_comma, percent
  "dplyr",
  "stringr"
)


raw <- readr::read_csv(
  "Livestock statistics 2023.csv",
  na = c("", "NA", "N/A")
) |>
  janitor::clean_names()



dplyr::glimpse(raw)



cleaned <- raw %>%
  # Drop junk columns
  select(-starts_with("x")) %>%
  # Clean livestock columns (convert commas/dashes to NA/number)
  mutate(across(
    .cols = -county,   # all except county
    .fns = ~ as.numeric(str_replace_all(., "[^0-9]", ""))
  )) %>%
  # Fix county name casing
  rename(County = county)
