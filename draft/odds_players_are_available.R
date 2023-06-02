library('tidyverse')

results = list()

interested = c("Cam Whitmore")

for(j in 1:100) {
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
      if(length(setdiff(targets[[as.character(i)]], unavailable)) == 0) {
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
  "9" = c("Ausar Thompson", "Cam Whitmore"), 
  "16" = c("Jalen Hood-Schifino", "GG Jackson"), 
  "28" = c("Dariq Whitehead", "Dereck Lively II")
)

results = c()

for(i in 1:500) {
  results[length(results) + 1] = run_odds_draft(players)
}

length(results[results])/length(results)



# Try to figure out a better way to estimate the probability players are available at a spot

prob_board %>% 
  unnest_longer(c(prospects, probabilities, min_rank)) %>%
  filter(prospects %in% c("Cason Wallace", "Jalen Hood-Schifino", "Kobe Bufkin")) %>%
  mutate(
    thresh = ifelse(Rank < 9, "Before", "After"), 
    thresh = factor(thresh, levels = c("Before", "After"))
  ) %>%
  group_by(thresh) %>%
  summarise(total_prob = sum(probabilities))
