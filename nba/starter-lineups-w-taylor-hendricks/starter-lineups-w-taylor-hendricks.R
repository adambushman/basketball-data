library('tidyverse')


data <- data.table::fread(
  'https://raw.githubusercontent.com/ramirobentes/nba_pbp_data/main/lineup-final2024/data.csv'
)


get_intersect <- function(x,y) {
  intersect(x[[1]], y[[1]])
}


glimpse(as_tibble(data))

uj <- data %>%
  filter(team == "UTA") %>%
  mutate(
    taylor_flag = stringr::str_detect(lineup_team, "Taylor Hendricks")
  )


uj_alt <- 
  uj %>%
  group_by(game_id) %>%
  mutate(lineup_num = row_number(game_id)) %>%
  ungroup() %>%
  mutate(
    starters = case_when(
      lineup_num == 1 ~ lineup_team, 
      TRUE ~ as.character(NA)
    )
  ) %>%
  fill(starters) %>%
  filter(taylor_flag) %>%
  mutate(
    lineup_split = stringr::str_split(lineup_team, "(?<=[0-9])(?![0-9])|,"), 
    lineup_clean = purrr::map(lineup_split, clean), 
    starters_split = stringr::str_split(starters, "(?<=[0-9])(?![0-9])|,"), 
    starters_clean = purrr::map(starters_split, clean)
  )


test <- uj_alt %>%
  mutate(
    common = purrr::map2(lineup_clean, starters_clean, get_intersect), 
    common_num = purrr::map_int(common, length) / 2
  )


test %>% 
  summarise(
    all_poss = sum(poss_team), 
    w_starters = sum(ifelse(common_num >= 2, poss_team, 0)), 
    paste0(round(w_starters * 1000 / all_poss) / 10, "%")
  )
