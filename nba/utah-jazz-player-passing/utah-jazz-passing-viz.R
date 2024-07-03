library('tidyverse')
library('RSelenium')

# Get Utah Jazz player name parts

nba_players <- hoopR::load_nba_player_box() 

uj_players <- 
  nba_players%>% 
  filter(team_abbreviation == "UTAH") %>%
  select(athlete_display_name) %>%
  distinct() %>%
  mutate(
    name_parts = stringr::str_to_lower(athlete_display_name), 
    name_parts = stringr::str_split(name_parts, " ")
  ) %>%
  as_tibble()


# Read NBA players table containing ID

table_txt <- readr::read_file("C:/Users/Adam Bushman/Documents/table.txt")

extracted_table <- 
  rvest::read_html(table_txt) %>%
  rvest::html_elements("a") %>% 
  rvest::html_attr("href")


# Find Utah Jazz players and extract ID

checkVal <- function(x) {
  result = FALSE
  for(u in uj_players$name_parts) {
    if(stringr::str_detect(x,u[1]) & stringr::str_detect(x,u[2])) {
      result = TRUE
    }
  }
  
  return(result)
}

getID <- function(x) {
  v = stringr::str_sub(x, 9, stringr::str_locate_all(x, "/")[[1]][3,1] - 1)
  return(v)
}


uj_IDs <-
  tibble(link = extracted_table) %>%
  mutate(is_uj = purrr::map_lgl(link, checkVal)) %>%
  filter(is_uj) %>%
  mutate(ID = purrr::map_chr(link, getID)) %>%
  .$ID


############# BELOW NOT WORKING

# Lookup passes dashboard for each Utah Jazz player

rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]
remDr$navigate(link)
remDr$executeScript("document.querySelector('.Crom_table__p1iZz').scrollIntoView(true);")
html <- remDr$getPageSource()[[1]]

# Close the Selenium server
remDr$close()
rD[["server"]]$stop()

# Parse the HTML with rvest
table <- read_html(html) %>%
  html_elements("table") %>%
  html_table()

ID = uj_IDs[1]

link <- glue::glue("https://www.nba.com/stats/player/{ID}/passes-dash")

rvest::read_html(link) %>%
  rvest::html_elements("table") %>%
  rvest::html_table()



