library('tidyverse')
library('gt')


team_data = hoopR::load_nba_team_box() %>%
  select(
    team_abbreviation,
    team_logo
  ) %>%
  distinct()

get_logo <- function(abb) {
  abb = unlist(abb)
  
  results <- 
    team_data %>% 
    filter(team_abbreviation %in% abb) %>%
    arrange(team_abbreviation) %>%
    select(team_logo) %>%
    unlist(use.names = FALSE)
  
  results
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
    "ATL", "DAL", "DET", "GS", "LAC", "LAL", "MEM", "MIA", "NO", "OKC", "SA", "SAC", "TOR", "WSH"
  ),
  pick_count = c(
    3, 2, 2, 3, 2, 2, 2,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  )
)

order <- tibble::tibble(
  team = c(
    "HOU", "CHA", "DET", "SA", "POR", "ORL", "IND", "WSH", "UTAH", "DAL", 
    "ORL", "OKC", "TOR", "NO", "ATL", "UTAH", "LAL", "MIA", "GS", "HOU", 
    "BKN", "BKN", "POR", "SAC", "MEM", "IND", "CHA", "UTAH", "IND", "LAC"
  )
) %>% tibble::rowid_to_column("pick")

run_generator <- function(draft_reps) {
  
  available_teams <- setdiff(sort(unique(team_data$team_abbreviation)), c("GIA", "LEB"))
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
    
    draft_reps$teams[r] = list(sort(c(team1, team2)))
  }
  
  for(r in (1:(nrow(draft_reps) - num)) + num) {
    
    teams = sample(available_teams, 2)
    
    # Clear Teams
    available_teams = setdiff(available_teams, teams)
    
    draft_reps$teams[r] = list(sort(teams))
  }
  
  return(draft_reps)
}

draft_reps_full <-
  run_generator(sample_n(draft_reps, 15)) %>%
  mutate(
    team_logos = purrr::map(teams, get_logo),
    pick_total = purrr::map_int(teams, get_pick_total)
  ) %>%
  select(rep_handle, pick_total, teams, team_logos)


assignmentStyle <- function(data) {
  data %>%
    gt() %>%
    tab_header(
      title = "Mock Draft Team Assignments"
    ) %>%
    cols_align(
      align = "center", 
      columns = pick_total
    ) %>%
    gt::cols_label(
      rep_handle = "Draft Rep",
      pick_total = "Pick Count",
      team_logos = "Teams to Rep"
    ) %>%
    gtExtras::gt_img_multi_rows(team_logos) %>%
    tab_options(
      column_labels.background.color = "black", 
      heading.padding = 20
    )
}


gtExtras::gt_two_column_layout(
  gtExtras::gt_double_table(draft_reps_full, assignmentStyle, nrows = 8),
  vwidth = 635, vheight = 765
)


mock_order <-
  order %>%
  inner_join(
    draft_reps_full %>% unnest(cols = c(teams, team_logos)), 
    by = c("team" = "teams")
  ) %>%
  select(pick, team_logos, rep_handle)


mockStyle <- function(data) {
  data %>%
    gt() %>%
    tab_header(
      title = "Mock Draft Order"
    ) %>%
    gt::cols_label(
      pick = "Pick #", 
      team_logos = "Team", 
      rep_handle = "Draft Rep"
    ) %>%
    gtExtras::gt_img_multi_rows(team_logos) %>%
    tab_options(
      column_labels.background.color = "black", 
      heading.padding = 20
    )
}


gtExtras::gt_two_column_layout(
  gtExtras::gt_double_table(mock_order[16:30,], mockStyle, nrows = 8),
  vwidth = 635, vheight = 765
)

