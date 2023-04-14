library('tidyverse')
library('gt')
library('gtExtras')
library('stringr')

my_data <- 
  read_csv(
    url('https://raw.githubusercontent.com/adambushman/basketball-data/master/draft/2023_Industry_Boards.csv')
  )

# Long rankings
long_rank <- 
  my_data %>%
  pivot_longer(
    cols = -Rank, 
    names_to = "Source", 
    values_to = "Player"
  ) %>%
  mutate(
    Update = str_sub(Source, str_locate(Source, "\\|")[,1] + 2, str_length(Source)), 
    Source = str_sub(Source, 1, str_locate(Source, "\\|")[,1] - 2)
  )

# Sources
sources <-
  long_rank %>%
  select(Source, Update) %>%
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


runDraft <- function(selection_teams = c(), partial_picks = c(), user) {
  
  pp_num = length(partial_picks)
  
  board = tibble(
    pick = seq(1, 30), 
    team = c(
      'DET','SAS','HOU','CHA','POR','ORL','WAS','IND','UTA','DAL','OKC','ORL','TOR','NOP','ATL',
      'UTA','LAL','MIA','HOU','GSW','BKN','BKN','POR','SAC','MEM','IND','CHA','UTA','IND','LAC'
    ), 
    prospect = c(partial_picks, rep(as.character(NA), 30 - pp_num))
  )
  
  draft_order = board$team
  
  for(i in 1:(nrow(board) - pp_num) + pp_num) {
    
    unavailable = board$prospect[!is.na(board$prospect)]
    
    left_over <-
      max_rank %>%
      filter(m_pick <= i & !(Player %in% unavailable))
    
    if(draft_order[i] %in% selection_teams) {
      # Pause for user entry
      available <- 
        max_rank %>% 
        filter(!(Player %in% unavailable)) %>%
        tibble::rowid_to_column("id") %>%
        select(id, Player)
      
      print(">>> Selected Picks <<<")
      board %>% filter(!is.na(prospect)) %>% print(n = nrow(.))
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
  
  styleTabl <- function(x) {
    gt(x) %>%
      tab_header(
        title = "2023 Draft Simulation | Round #1", 
        subtitle = glue::glue("Highlighted Picks Chosen By {user}")
      ) %>%
      cols_label(
        pick = "Pick #", 
        team = "Team", 
        prospect = "Prospect Name"
      ) %>%
      tab_source_note(source_note = "Simulation fed by five industry big boards") %>%
      tab_source_note(source_note = "Designed by @adam_bushman") %>%
      tab_style(
        style = list(
          cell_fill(color = "#f6ee26"),
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = everything(),
          rows = team %in% selection_teams
        )
      ) %>%
      tab_options(
        heading.background.color = "#050505", 
        column_labels.background.color = "#dbe2ea", 
        column_labels.font.weight = "bold", 
        source_notes.background.color = "#dbe2ea"
      ) 
  }
  
  gt_two_column_layout(
    gt_double_table(board, styleTabl, nrows = 15), 
    vwidth = 635, vheight = 765
  )
  
}

first_X = c(
  "Victor Wembanyama", "Scoot Henderson", "Brandon Miller", "Amen Thompson", "Ausar Thompson",
  "Jarace Walker", "Nick Smith Jr.", "Anthony Black"
)

###############################################

next_X = c(
  "Cam Whitmore", "Cason Wallace", "Keyonte George", "Jalen Hood-Schifino"
)

last_X = c(
  
)

runDraft(
  c("UTA")
  # , c(``
  #     #first_X
  #     # , next_X
  #     # , last_X
  #   )
  , user = "@adam_bushman"
)
