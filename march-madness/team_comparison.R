# install.packages('hoopR')
# install.packages('tidyverse')
library('hoopR')
library('tidyverse')

# Teams
teams <- hoopR::espn_mbb_teams()

# Stats
cbb.trad <- hoopR::load_mbb_team_box()

split_shot <- function(x) {
  y_1 = stringr::str_sub(x, 1, stringr::str_locate(x, '-')[,1]-1)
  y_2 = stringr::str_sub(x, stringr::str_locate(x, '-')[,1]+1, stringr::str_length(x))
  
  return(c(as.numeric(y_1), as.numeric(y_2)))  
}

cbb.exp <-
  cbb.trad %>%
  mutate(
    fgm = unlist(map(field_goals_made_field_goals_attempted, ~split_shot(.)[1])), 
    fga = unlist(map(field_goals_made_field_goals_attempted, ~split_shot(.)[2])), 
    fgm3 = unlist(map(three_point_field_goals_made_three_point_field_goals_attempted, ~split_shot(.)[1])), 
    fga3 = unlist(map(three_point_field_goals_made_three_point_field_goals_attempted, ~split_shot(.)[2])), 
    ftm = unlist(map(free_throws_made_free_throws_attempted, ~split_shot(.)[1])), 
    fta = unlist(map(free_throws_made_free_throws_attempted, ~split_shot(.)[2])), 
    tsa = fga + (fta * 0.44), 
    pts = ((fgm - fgm3) * 2) + (fgm3 * 3) + (ftm), 
    tsp = pts / (tsa * 2), 
    pswing = as.numeric(offensive_rebounds) + as.numeric(steals)
  )


get_comp <- function(my_team) {
  
  check <-
    cbb.exp %>%
    filter(team_location == my_team) %>%
    inner_join(
      cbb.exp %>% 
        filter(team_location != my_team), 
      by = "game_id",
      suffix = c(".t", ".o")
    ) %>%
    summarise(
      a_tsa.t = sum(tsa.t) / n(), 
      a_tsa.o = sum(tsa.o) / n(), 
      a_pswing.t = sum(pswing.t) / n(), 
      a_pswing.o = sum(pswing.o) / n(), 
      
      a_lead.t = sum(as.numeric(largest_lead.t)) / sum(pts.t), 
      a_lead.o = sum(as.numeric(largest_lead.o)) / sum(pts.o), 
      
      a_tsp.t = sum(pts.t) / (sum(tsa.t) * 2), 
      a_tsp.o = sum(pts.o) / (sum(tsa.o) * 2), 
      v_tsp.t = sd(tsp.t), 
      v_tsp.o = sd(tsp.o), 
      
      ftr.t = sum(fta.t) / sum(fga.t), 
      ftr.o = sum(fta.o) / sum(fga.o), 
      fg3r.t = sum(fga3.t) / sum(fga.t), 
      fg3r.o = sum(fga3.o) / sum(fga.o)
    )
  
  stats <-
    check %>%
    pivot_longer(
      cols = everything(), 
      names_to = "name", 
      values_to = "value", 
      names_pattern = "(.*)"
    ) %>%
    mutate(
      metric = stringr::str_sub(name, 1, stringr::str_length(name)-2), 
      end = case_when(
        stringr::str_detect(name, ".o") ~ "OPPONENT", 
        TRUE ~ "TEAM"
      )
    ) %>%
    select(-name) %>%
    pivot_wider(
      names_from = end, 
      values_from = value
    ) %>%
    mutate(diff = TEAM - OPPONENT)
  
  print(stats)
  
}


get_comp("Maryland")
get_comp("West Virginia")
