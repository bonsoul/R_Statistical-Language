install.packages("nflverse")

install.packages("feather")

# Only if pak isn’t already installed:
install.packages("pak")

# Install the querychat package via GitHub
pak::pak("posit-dev/querychat/pkg-r")

install.packages("remotes")
remotes::install_github("posit-dev/querychat", subdir = "pkg-r")




library(nflreadr)
library(dplyr)
library(feather)
game_data_all <- load_schedules(seasons = c(2024, 2025)) |>
  filter(!is.na(result))


# Refresh only 2025
fresh_2025 <- load_schedules(seasons = 2025, rebuild = TRUE)

game_data_all <- load_schedules(seasons = c(2024, 2025))


nflreadr::.clear_cache()        # Clears everything…
fresh_2025 <- load_schedules(seasons = 2025) 



library(dplyr)
game_data_all <- bind_rows(
  filter(game_data_all, season == 2024),
  fresh_2025
)


View(game_data_all)




cols_to_remove <- c("old_game_id", "gsis", "nfl_detail_id", "pfr", "pff", 
                    "espn", "ftn", "away_qb_id", "home_qb_id", "stadium_id")

games <- game_data_all |>
  select(-all_of(cols_to_remove))




games <- games |>
  mutate(
    team_won = case_when(
      home_score > away_score ~ home_team,
      away_score > home_score ~ away_team,
      .default = NA
    ),
    team_lost = case_when(
      home_score > away_score ~ away_team,
      away_score > home_score ~ home_team,
      .default = NA
    )
  )

write_feather(games, "games.feather")





data_dictionary <- dictionary_schedules |>
  filter(!(field %in% cols_to_remove) )

text_for_file <- paste0(data_dictionary$field, " (", data_dictionary$data_type, "): ", data_dictionary$description)

cat(text_for_file, sep = "\n", file = "data_dictionary.txt")




team_won (character): Name of winning team
team_lost (character): Name of losing team
