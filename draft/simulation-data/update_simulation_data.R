library('tidyverse')
library('jsonlite')

my_data <- 
  read_csv(
    #url('https://raw.githubusercontent.com/adambushman/basketball-data/master/draft/2023_Industry_Boards.csv')
    "draft/2023_Industry_Boards.csv"
  )

# Long rankings
long_rank <- 
  my_data %>%
  pivot_longer(
    cols = -Rank, 
    names_to = "Source", 
    values_to = "Player"
  ) %>%
  mutate(Rank = as.integer(Rank))

# Rankings summary

summary <- long_rank %>% group_by(Player) %>% 
  summarise(avg = mean(Rank), min = min(Rank), max = max(Rank)) %>% 
  ungroup() %>% arrange(avg)

# Sources
sources <- 
  long_rank %>%
  select(Source) %>%
  distinct()

# Real rankings
real_rank <-
  long_rank %>%
  group_by(Rank, Player) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  filter(!is.na(Player))

# Full rankings
full_rank <-
  real_rank %>%
  group_by(Player) %>%
  reframe(
    Rank = seq(min(Rank), max(Rank)), 
    Default = 0.5
  ) %>%
  arrange(Rank) %>%
  select(Rank, Player, Default)

# Joined rankings
joined_rank <-
  left_join(
    full_rank, 
    real_rank,
    by = c("Rank", "Player")
  ) %>%
  mutate(
    F_Freq = coalesce(Default, 0) + coalesce(Freq, 0)
  ) %>%
  group_by(Rank) %>%
  mutate(
    R_Freq = F_Freq / sum(F_Freq)
  )

# Lowest rank
max_rank <-
  long_rank %>% 
  group_by(Player) %>%
  summarise(
    n_rank = n(), 
    rows = nrow(my_data), 
    maxx = max(Rank)
  ) %>% 
  ungroup() %>%
  rowwise() %>%
  mutate(
    m_pick = case_when(
      n_rank < 2 ~ rows, 
      TRUE ~ min(c(maxx, rows))
    )
  ) %>%
  arrange(m_pick)


prob_board <-
  joined_rank %>%
  group_by(Rank) %>%
  summarise(
    prospects = list(Player), 
    probabilities = list(R_Freq), 
    min_rank = list(min(Rank))
  ) %>%
  ungroup()

# Saving the average rank
write.csv(summary, "draft/simulation-data/2023_avg_board.csv", row.names = FALSE)
# Saving the sources
write.csv(sources, "draft/simulation-data/2023_sources.csv", row.names = FALSE)
# Saving the max rank
write.csv(max_rank, "draft/simulation-data/2023_max_rank.csv", row.names = FALSE)
# Saving the probability board
jsonlite::write_json(prob_board, "draft/simulation-data/2023_probability_board.json")

