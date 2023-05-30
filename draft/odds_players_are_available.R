library('tidyverse')

results = list()

interested = c("Jarace Walker", "Cam Whitmore", "Ausar Thompson", "Amen Thompson")

for(j in 1:1000) {
  picks = run_partial_draft(8)
  available = setdiff(interested, picks)
  if(length(available) > 0) {
    results[[length(results) + 1]] = available
  }
}

tibble(player = results) %>% nrow()

tibble(player = results) %>%
  tibble::rowid_to_column("sim_num") %>%
  unnest_longer(player) %>%
  group_by(player) %>%
  summarise(freq = n_distinct(sim_num)) %>%
  ungroup() %>%
  mutate(
    overall = sum(freq), 
    perc = freq / overall
  )
  

  
run_odds_draft <- function(targets) {
  unavailable = c()
  len = max(as.integer(names(targets)))
  
  for(i in 1:len) {
    
    left_over <-
      max_rank %>%
      filter(m_pick <= i & !(Player %in% unavailable))
    
    if(as.character(i) %in% names(targets)) {
      if(targets[[as.character(i)]] %in% unavailable) {
        return(FALSE)
      }
    }
    
    if(nrow(left_over) > 0) {
      unavailable[length(unavailable)+1] = left_over$Player[1]
    }
    else {
      unavailable[length(unavailable)+1] = makeSelection(i, unavailable)
    }
  }
  
  return(TRUE)
}

players <- list(
  "9" = "Taylor Hendricks", 
  "16" = "Jalen Hood-Schifino"#, 
  #"28" = "Dariq Whitehead"
)

results = c()

for(i in 1:100) {
  results[length(results) + 1] = run_odds_draft(players)
}

length(results[results])/length(results)


