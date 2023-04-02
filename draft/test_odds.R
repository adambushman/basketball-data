library('tidyverse')
library('gt')
library('gtExtras')

my_data <- 
  read_csv(
    url('https://raw.githubusercontent.com/adambushman/basketball-data/master/draft/2023_Industry_Boards.csv')
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

showPicked <- function(df) {
  gt(df)
}

showAvailable <- function(df) {
  gt(df)
}



runDraft <- function(selection_teams = c()) {
  
  board = tibble(
    pick = seq(1, 30), 
    team = c(
      "DET", "HOU", "SAS", "CHA", "POR", "ORL", "IND", "WAS", "UTA", "DAL", 
      "ORL", "OKC", "ATL", "UTA", "TOR", "NOP", "LAL", "HOU", "MIA", "GSW", 
      "BKN", "BKN", "POR", "SAC", "IND", "MEM", "CHA", "UTA", "IND", "LAC"
    ), 
    prospect = rep(as.character(NA), 30)
  )
  
  draft_order = board$team
  
  for(i in 1:nrow(board)) {
    
    unavailable = board$prospect[!is.na(board$prospect)]
    
    left_over <-
      max_rank %>%
      filter(m_pick < i & !(Player %in% unavailable))
    
    if(draft_order[i] %in% selection_teams) {
      # Pause for user entry
      available <- 
        max_rank %>% 
        filter(!(Player %in% unavailable)) %>%
        mutate(id = seq(1:nrow(.))) %>%
        select(id, Player)
      
      print(">>> Selected Picks <<<")
      print(board %>% filter(!is.na(prospect)))
      print(">>> Available Prospects <<<")
      available %>% arrange(-id) %>% print(n = nrow(.))
      
      print(glue::glue(">>> {draft_order[i]} is on the clock at #{i} <<<"))
      selection = readline("Make your selection. Enter a number corresponding to the player above:")
      
      board$prospect[length(unavailable)+1] = available$Player[as.integer(selection)]
    } 
    else if(nrow(left_over) > 0) {
      board$prospect[length(unavailable)+1] = left_over$Player[1]
    }
    else {
      board$prospect[length(unavailable)+1] = makeSelection(i, unavailable)
    }
  }
  
  gt(board) %>%
    tab_style(
      style = list(
        cell_fill(color = "#f6ee26"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = everything(),
        rows = team %in% selection_teams
      )
    )
}


runDraft(
  c("UTA")
)
