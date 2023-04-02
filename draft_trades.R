library('tidyverse')
library('rvest')


all_transactions <- 
  tibble(
    date = lubridate::mdy('April 2, 2023'), 
    trade = TRUE,
    pick = TRUE, 
    full_transaction = "Lorem impsum"
  )
  

for(y in (sort(1:24, decreasing = TRUE) + 1999)) {
  url = glue::glue('https://www.basketball-reference.com/leagues/NBA_{y}_transactions.html')
  
  page_elements <-
    rvest::read_html(url) %>%
    rvest::html_elements('.page_index li') %>%
    rvest::html_text2()
  
  cleaned_data <-
    tibble(full_transaction = page_elements) %>%
    mutate(
      date = stringr::str_sub(full_transaction, 1, stringr::str_locate(full_transaction, '\n')[,1] - 1), 
      date = lubridate::mdy(date), 
      full_transaction = stringr::str_sub(full_transaction, stringr::str_locate(full_transaction, '\n')[,1])
    ) %>%
    tidyr::separate_longer_delim(full_transaction, '\n\n') %>%
    filter(full_transaction != "") %>%
    mutate(
      trade = stringr::str_detect(full_transaction, 'trade'), 
      pick = stringr::str_detect(full_transaction, 'pick')
    ) %>%
    select(date, trade, pick, full_transaction)
  
  
  all_transactions <- bind_rows(all_transactions, cleaned_data)
}


draft_dates <-
  tibble(
    date = c(
      'June 23, 2022', 'July 29, 2021', 'November 18, 2020', 'June 20, 2019', 'June 21, 2018', 
      'June 22, 2017', 'June 23, 2016', 'June 25, 2015', 'June 26, 2014', 'June 27, 2013', 
      'June 28, 2012', 'June 23, 2011', 'June 24, 2010', 'June 25, 2009', 'June 26, 2008', 'June 28, 2007', 
      'June 28, 2006', 'June 28, 2005', 'June 24, 2004', 'June 26, 2003', 
      'June 26, 2002', 'June 27, 2001', 'June 28, 2000', 'June 30, 1999'
    )
  ) %>%
  mutate(
    date = lubridate::mdy(date), 
    year = lubridate::year(date), 
    interval = lubridate::interval(
      date - 30, 
      date + 31
    )
  )


get_picks <- function(year) {
  url = glue::glue('https://www.basketball-reference.com/draft/NBA_{year}.html')

  page_tbl <-
    rvest::read_html(url) %>%
    rvest::html_element('#div_stats table') %>%
    rvest::html_table() %>%
    janitor::clean_names()

  players = page_tbl$round_1[2:31]
  
  unlist(stringr::str_extract_all(players, regex("[a-z0-9]+", TRUE)), use.names = FALSE)
}



show_trades <- function(year) {
  picks = get_picks(year)
  
  inter_sect <- function(list, picks) {
    return (intersect(picks, list[[1]]))
  }
  sub_trans = all_transactions[lubridate::year(all_transactions$date) == year,]
  z <- sub_trans %>%
    filter(date %within% draft_dates$interval[draft_dates$year == year] & trade == TRUE) %>%
    mutate(
      split_trans = stringr::str_extract_all(full_transaction, regex("[a-z0-9]+", TRUE))
    ) %>%
    mutate(
      # ISSUE IS RIGHT HERE
      common = purrr::map(split_trans, inter_sect, picks = picks) 
    )
    select(full_transaction) %>%
    unlist(use.names = FALSE)
}

print(show_trades(2022))
