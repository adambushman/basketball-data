library('tidyverse')

data <- read.csv(
  'https://raw.githubusercontent.com/ramirobentes/nba_pbp_data/main/lineup-final2024/data.csv'
)

clean <- function(x) {
  y = stringr::str_trim(x)
  list(y)
}

data_add <- 
  data %>%
  mutate(
    lineup_team_split = stringr::str_split(lineup_team, "(?<=[0-9])(?![0-9])|,"), 
    lineup_team_clean = purrr::map(lineup_team_split, clean)
  )

check <- function(x) {
  is.na(as.numeric(x))
}

players <- 
  data_add %>%
  filter(team == "UTA") %>%
  .$lineup_team_clean %>%
  unlist(., use.name = FALSE) %>%
  # stringr::str_trim(.) %>%
  unique(.) %>%
  .[purrr::map_lgl(., check)]


combos <- expand.grid(players, players, stringsAsFactors = FALSE)

includes <- function(x, p1, p2) {
  return(p1 %in% x[[1]] & p2 %in% x[[1]])
}

summary <- data.frame(
  matrix(NA, nrow = 0, ncol = 5)
)

colnames(summary) = c("player1", "player2", "mins", "poss", "pm")


for(i in 1:nrow(combos)) {
  data <- 
    data_add %>%
    filter(team == "UTA") %>%
    filter(purrr::map_lgl(lineup_team_clean, includes, combos$Var1[i], combos$Var2[i])) %>%
    summarise(
      mins = round(sum(secs_played) / 60, 0), 
      poss = sum(poss_team), 
      pm = (sum(pts_team) * 100 / sum(poss_team)) - (sum(pts_opp) * 100 / sum(poss_opp))
    ) %>%
    mutate(
      player1 = combos$Var1[i], 
      player2 = combos$Var2[i]
    ) %>%
    select(
      player1, player2, mins, poss, pm
    )
  
  summary <- rbind(summary, data)
}

write.csv(
  summary, 
  "C:/Users/Adam Bushman/Documents/Projects/d3-js-learn-and-practice/Data files/utah-jazz-lineups-24.csv", 
  row.names = FALSE
)
