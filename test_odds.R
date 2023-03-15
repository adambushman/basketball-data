library('tidyverse')
library('gt')

my_data <- 
  readxl::read_xlsx(
    'D:/Dropbox (HFC)/Jabber Jazz Podcast/Draft/2023_Industry_Boards.xlsx'
  )

# Real rankings
real_rank <- 
  my_data %>%
  pivot_longer(
    cols = -Rank, 
    names_to = "Source", 
    values_to = "Player"
  ) %>%
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

test <-
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


max_rank <-
  test %>% 
  group_by(Player) %>%
  summarise(m_pick = min(max(Rank)), nrow(my_data)) %>%
  arrange(m_pick)
  
  
prob_board <-
  test %>%
  summarise(
    prospects = list(Player), 
    probabilities = list(R_Freq), 
    min_rank = list(min(Rank))
  ) %>%
  ungroup()
  




makeSelection <- function(pick, db) {
  # Get available prospects & probs for pick
  to_remove = match(db, prob_board$prospects[pick][[1]])
  to_remove = to_remove[!is.na(to_remove)]
  if(length(to_remove) == 0) {
    to_remove = c()
  }
  
  extra_prob = sum(prob_board$probabilities[pick][[1]][to_remove])
  if(length(to_remove) != 0) {
    prosps = prob_board$prospects[pick][[1]][-to_remove]
    probs = prob_board$probabilities[pick][[1]][-to_remove]
  } 
  else {
    prosps = prob_board$prospects[pick][[1]]
    probs = prob_board$probabilities[pick][[1]]
  }
  
  if(length(prosps) != 0) {
    probs = probs + (extra_prob / length(probs))
    
    # Make the selection
    selection = sample(prosps, 1, prob = probs)
    
    return(selection)
  }
  else {
    return("EMPTY")
  }
}


runDraft <- function(to_pick) {
  draft_board = c()
  
  for(i in 1:to_pick) {
    
    left_over <-
      max_rank %>%
      filter(m_pick < i & !(Player %in% draft_board))
    
    for(p in left_over$Player) {
      draft_board[length(draft_board)+1] = p
    }
    
    draft_board[length(draft_board)+1] = makeSelection(i, draft_board)
    
    if(draft_board[length(draft_board)] == "EMPTY") {
      draft_board = draft_board[c(1, length(draft_board)-1)]
    }
    
    if(length(draft_board) == to_pick) {
      break;
    }
  }
  
  tibble(
    pick = seq(1, length(draft_board)), 
    team = c(
      "DET", "HOU", "SAS", "CHA", "ORL", "IND", "ORL", "POR", "WAS", "OKC", 
      "TOR", "UTA", "NOP", "LAL", "UTA", "GSW", "NYK", "ATL", "HOU", "MIA", 
      "BKN", "BKN", "POR", "MEM", "SAC", "IND", "UTA", "CHA", "IND", "LAC"
    )[1:length(draft_board)], 
    prospect = draft_board
  )
}


results = runDraft(10)

gt(results)
  
