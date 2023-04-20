library('tidyverse')
library('gt')
teams = hoopR::load_nba_team_box() %>%
  select(
    team_abbreviation,
    team_logo
  ) %>%
  distinct()
get_logo <- function(abb) {
  abb = unlist(abb)
  list(teams$team_logo[teams$team_abbreviation %in% abb])
}
get_pick_total <- function(abb) {
  abb = unlist(abb)
  sum(pick_total$pick_count[pick_total$team %in% abb])
}
draft_reps <- tibble::tibble(
  rep_handle = c(
    "@JazzJargon", "@CJRealHoops1", "@rgiss11", "@utahjazzfan47", "@zarinf", "@RichieOstler3",
    "@HirdItHereHoops", "@Bluedevilthoug1", "@LeifThulin", "@BullNamed_GUS", "@AJ3Jazz", "@jayjazz3",
    "@MarkW_E01", "@adam_bushman", "@cphilits"
  ),
  rep_name = c(
    "JazzJargon", "Calvy J", "Riley and five second round picks", "Jazz Fan", "Zarin Ficklin", "Richie Ostler",
    "Cooper Hird", "Bluedevilthoughts", "Leif Thulin", "colton: unofficial A's to Utah propaganda", "AJ", "Josh Roberts",
    "Mark", "Adam Bushman", "Cameron beta of alpha's"
  ),
  teams = as.character(NA)
)
pick_total <- tibble::tibble(
  team = c(
    "UTAH", "HOU", "CHA", "IND", "POR", "BKN", "ORL",
    "ATL", "DAL", "DET", "GS", "LAC", "LAL", "MEM", "MIA", "NO", "NY", "OKC", "SA", "SAC", "TOR", "WSH"
  ),
  pick_count = c(
    3, 2, 2, 3, 2, 2, 2,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  )
)
run_generator <- function(draft_reps) {
  available_teams <- setdiff(sort(unique(teams$team_abbreviation)), c("GIA", "LEB"))
  multi_pick_teams = sort(c("UTAH", "HOU", "CHA", "IND", "POR", "BKN", "ORL"))
  no_pick_teams = sort(c("NY", "MIN", "CHI", "CLE", "MIL", "BOS", "DEN", "PHI", "PHX"))
  num = length(multi_pick_teams)
  for(r in 1:num) {
    team1 = sample(multi_pick_teams, 1)
    team2 = sample(no_pick_teams, 1)
    # Clear Teams
    available_teams = setdiff(available_teams, c(team1, team2))
    no_pick_teams = setdiff(no_pick_teams, c(team1, team2))
    multi_pick_teams = setdiff(multi_pick_teams, c(team1, team2))
    draft_reps$teams[r] = list(c(team1, team2))
  }
  for(r in (1:(nrow(draft_reps) - num)) + num) {
    teams = sample(available_teams, 2)
    # Clear Teams
    available_teams = setdiff(available_teams, teams)
    draft_reps$teams[r] = list(teams)
  }
}
run_generator(sample_n(draft_reps, 15))
run_generator <- function(draft_reps) {
  available_teams <- setdiff(sort(unique(teams$team_abbreviation)), c("GIA", "LEB"))
  multi_pick_teams = sort(c("UTAH", "HOU", "CHA", "IND", "POR", "BKN", "ORL"))
  no_pick_teams = sort(c("NY", "MIN", "CHI", "CLE", "MIL", "BOS", "DEN", "PHI", "PHX"))
  num = length(multi_pick_teams)
  for(r in 1:num) {
    team1 = sample(multi_pick_teams, 1)
    team2 = sample(no_pick_teams, 1)
    # Clear Teams
    available_teams = setdiff(available_teams, c(team1, team2))
    no_pick_teams = setdiff(no_pick_teams, c(team1, team2))
    multi_pick_teams = setdiff(multi_pick_teams, c(team1, team2))
    draft_reps$teams[r] = list(c(team1, team2))
  }
  for(r in (1:(nrow(draft_reps) - num)) + num) {
    teams = sample(available_teams, 2)
    # Clear Teams
    available_teams = setdiff(available_teams, teams)
    draft_reps$teams[r] = list(teams)
  }
  draft_reps
}
run_generator(sample_n(draft_reps, 15))
draft_reps_full <-
  run_generator(sample_n(draft_reps, 15)) %>%
  mutate(
    team_logos = purrr::map(teams, get_logo),
    pick_total = purrr::map_int(teams, get_pick_total)
  ) %>%
  select(rep_handle, pick_total, team_logos)
styleTabl <- function(data) {
  data %>%
    gt() %>%
    gt::cols_label(
      rep_handle = "Draft Rep",
      pick_total = "Prospective Picks",
      team_logos = "Teams to Rep"
    ) %>%
    gtExtras::gt_img_multi_rows(team_logos)
}
gtExtras::gt_two_column_layout(
  gtExtras::gt_double_table(draft_reps_full, styleTabl, nrows = 8),
  vwidth = 635, vheight = 765
)
draft_reps_full <-
  run_generator(sample_n(draft_reps, 15)) %>%
  mutate(
    team_logos = purrr::map(teams, get_logo),
    pick_total = purrr::map_int(teams, get_pick_total)
  ) %>%
  select(rep_handle, pick_total, team_logos)
styleTabl <- function(data) {
  data %>%
    gt() %>%
    gt::cols_label(
      rep_handle = "Draft Rep",
      pick_total = "Prospective Picks",
      team_logos = "Teams to Rep"
    ) %>%
    gtExtras::gt_img_multi_rows(team_logos)
}
gtExtras::gt_two_column_layout(
  gtExtras::gt_double_table(draft_reps_full, styleTabl, nrows = 8),
  vwidth = 635, vheight = 765
)
draft_reps_full <-
  run_generator(sample_n(draft_reps, 15)) %>%
  mutate(
    team_logos = purrr::map(teams, get_logo),
    pick_total = purrr::map_int(teams, get_pick_total)
  ) %>%
  arrange(rep_handle) %>%
  select(rep_handle, pick_total, team_logos)
styleTabl <- function(data) {
  data %>%
    gt() %>%
    gt::cols_label(
      rep_handle = "Draft Rep",
      pick_total = "Prospective Picks",
      team_logos = "Teams to Rep"
    ) %>%
    gtExtras::gt_img_multi_rows(team_logos)
}
gtExtras::gt_two_column_layout(
  gtExtras::gt_double_table(draft_reps_full, styleTabl, nrows = 8),
  vwidth = 635, vheight = 765
)