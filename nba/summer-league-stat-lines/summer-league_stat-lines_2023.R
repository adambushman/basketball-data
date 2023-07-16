library('tidyverse')

here::i_am("nba/summer-league-stat-lines/summer-league_stat-lines_2023.R")

data <- read.csv(
  here::here('nba/summer-league-stat-lines/summer-league_player_2023.csv')
) %>%
  as_tibble() %>%
  janitor::clean_names()

per_30 <- function(min, x) {
  return(round(x * 30 / min, 1))
}

totals <- function(gp, x) {
  return(round(gp * x))
}


new_data <-
  data %>%
  select(player, gp, min, fga, fta, pts, oreb, reb, ast, tov, stl, blk, pf) %>%
  mutate(
    across(-c(player, gp), ~totals(gp, .))
  ) %>%
  group_by(player) %>%
  summarise(across(everything(), sum)) %>%
  ungroup() %>%
  filter(min >= 50) %>%
  mutate(
    tsp = pts / ((fga + (fta * 0.44)) * 2), 
    ast_tov = ast / tov, 
    act = stl + blk - pf
  ) %>%
  select(
    player, min, pts, reb, ast, tov, stl, blk, pf, tsp, ast_tov, act
  ) %>%
  mutate(
    across(c(pts:act, -tsp), ~per_30(min, .))
  )


new_data %>%
  mutate(across(pts:act, percent_rank, .names = "{.col}_ptile", .unpack = TRUE)) %>%
  mutate(across(c(tov_ptile, pf_ptile), ~(1 - .))) %>%
  filter(player == 'Keyonte George') %>%
  glimpse()
