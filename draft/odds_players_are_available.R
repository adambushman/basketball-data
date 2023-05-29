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
  tibble::rowid_to_column("sim_num") %>%
  unnest_longer(player) %>%
  group_by(player) %>%
  summarise(freq = n_distinct(sim_num)) %>%
  ungroup() %>%
  mutate(
    overall = sum(freq), 
    perc = freq / overall
  )
  
