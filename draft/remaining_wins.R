record <-
  tibble::tibble(
    team = c("UTA", "WAS", "IND", "ORL", "DAL"), 
    current_wins = c(36, 34, 34, 34, 37)
  )

sched_left <-
  tibble::tibble(
    team = c(
      rep("UTA", 4), 
      rep("WAS", 4), 
      rep("IND", 3), 
      rep("ORL", 4), 
      rep("DAL", 3)
    ), 
    matchup = c(
      c("vsLAL", "vsOKC", "vsDEN", "atLAL"), 
      c("vsWAS", "atATL", "vsWAS", "vsHOU"), 
      c("vsNYK", "vsDET", "atNYK"), 
      c("vsCLE", "vsCLE", "atBKN", "atMIA"), 
      c("vsSAC", "vsCHI", "vsDAL")
    ), 
    prob = c(
      rep(0, 4), #c(0.4, 0.57, 0.39, 0.16), 
      c(0.38, 0.32, 0.56, 0.82), 
      c(0.35, 0.78, 0.2), 
      c(0.39, 0.38, 0.25, 0.27), 
      c(0.69, 0.61, 0.89)
    )
  )


sched_grp <-
  sched_left %>%
  group_by(team) %>%
  summarise(probs = list(prob)) %>%
  ungroup()


teams = sched_grp$team

results <- 
  tibble(
    sim_id = c("10001"), 
    team = c("UTA"), 
    final_wins = c(0)
  )


for(id in 1:1000 + 10002) {
  for(t in teams) {
    probs = sched_grp$probs[sched_grp$team == t][[1]]
    wins = record$current_wins[record$team == t] + sum(rbinom(length(probs), 1, probs))
    
    results <- 
      results %>% 
      add_row(tibble_row(
        sim_id = as.character(id), 
        team = t, 
        final_wins = wins
    ))
  }
}

results_trim <-
  results %>%
  filter(final_wins != 0) %>%
  group_by(sim_id) %>%
  summarise(
    teams = list(team), 
    wins = list(final_wins)
  ) %>%
  ungroup()




calc_ties <- function(teams, wins) {
  data <- 
    tibble(
      teams = teams, 
      wins = wins
    ) %>%
    arrange(wins) %>%
    mutate(
      rank = min_rank(wins) + 5
    ) %>%
    group_by(rank) %>%
    summarise(teams = list(teams))
  
  check = list()
  
  for( i in 1:nrow(data) ) {
    id = glue::glue("pos_{data$rank[i]}")
    check[[ id ]] = data$teams[i][[1]]
  }
  
  check
}


results_full <-
  results_trim %>%
    mutate(
      test = purrr::map2(teams, wins, calc_ties)
    )

find_uta <- function(x) {
  "UTA" %in% x
}

get_len <- function(x) {
  length(x)
}

t <- 
  results_full %>%
  unnest_wider(test)

x <- 
  t %>% 
  select(col = pos_7) %>%
  mutate(
    uta = purrr::map(col, find_uta)[[1]], 
    num = lengths(col)
  )

x %>% 
  group_by(
    num, uta
  ) %>%
  summarise(
    perc = n() / nrow(.)
  ) %>%
  ungroup()
