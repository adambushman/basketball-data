library('rvest')
library('stringr')

###
# Custom functions
###

clean_teams <- function(team, type) {
  len = str_length(team)
  trailing = str_detect(str_sub(team, len-3, len), " ")

  if(trailing) {
    team = str_trim(str_sub(team, 1, len-3), side = "right")
  }
  team = str_trim(str_sub(team, 1, len-3), side = "right")
  
  if(str_detect(team, "Golden State")) {
    team = glue::glue("{team}W")
  } 
  else if(str_detect(team, "San Antonio")) {
    team = glue::glue("{team}S")
  }
  else if(str_detect(team, "New Orleans")) {
    team = glue::glue("{team}P")
  }
  else if(str_detect(team, "New York")) {
    team = glue::glue("{team}K")
  }
  
  len = str_length(team)
  
  abb = str_sub(team, len-2, len)
  full = str_sub(team, 1, len-3)
  
  if(type == 'abb') {
    return(abb)
  } else {
    return(full)
  }
}

###
# Web scraping
###

url = 'https://www.tankathon.com/'

page_tabl <-
  read_html(url) %>%
  html_element('.draft-board-table-container table') %>%
  html_table()

###
# Cleaning output
###

colnames(page_tabl) <- page_tabl[1,]

cleaned_order <-
  page_tabl[2:nrow(page_tabl),] %>%
  janitor::clean_names() %>%
  filter(pick != "" & pick != "END OF LOTTERY") %>% print(n = nrow(.))
  mutate(
    abb = purrr::map_chr(team, clean_teams, type = 'abb'), 
    team_full = purrr::map_chr(team, clean_teams, type = 'full'), 
    order = pick
  ) %>%
  select(order, abb, team_full)


glimpse(cleaned_order)
