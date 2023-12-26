library('tidyverse')

test <- hoopR::load_nba_player_box()
passing <- read.csv('C:/Users/Adam Bushman/Documents/player_passing.csv')

players <- 
  test %>%
  filter(team_abbreviation == 'UTAH' & (!is.na(field_goals_attempted) | !is.na(free_throws_attempted))) %>%
  group_by(athlete_display_name, athlete_headshot_href) %>%
  summarise(
    TOT_FGA = sum(field_goals_attempted + (free_throws_attempted * 0.44))
  ) %>%
  ungroup()


all_data <- 
  inner_join(
    players, passing, 
    by = c("athlete_display_name" = "PLAYER")
  ) %>%
  filter(MIN > 100) %>%
  mutate(
    across(c(PASSES_MADE, AST, SECONDARY_AST, TOT_FGA), ~ round(. * 36 / MIN, 2))
  )


camcorder::gg_record(
  dir = "C:/Users/Adam Bushman/Camcorder", 
  device = "png", 
  width = 16, 
  height = 12, 
  units = "cm", 
  dpi = 300
)

ggplot(all_data) +
  nflplotR::geom_from_path(
    aes(
      x = PASSES_MADE, 
      y = TOT_FGA, 
      path = athlete_headshot_href
    ), 
    width = 0.12
  ) +
  scale_y_continuous(
    expand = rep(0.075,2)
  ) +
  scale_x_continuous(
    expand = rep(0.075,2)
  ) +
  labs(
    title = "The 'shoot/pass' balance for the Utah Jazz", 
    subtitle = "Roster members with 100+ total minutes | Adjusted to 36 minutes of action", 
    x = "Passes Made", 
    y = "True Shooting Attempts", 
    caption = "Stats via NBA.com\nDesigned by @adam_bushman"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(15, 15, 7, 15), 
    text = element_text(color = "#141414"), 
    axis.text = element_text(color = "#141414"),
    plot.title.position = "plot", 
    plot.title = ggtext::element_markdown(
      size = 18, face = "bold", margin = margin(0, 0, 7, 0)
    ), 
    plot.subtitle = ggtext::element_markdown(
      size = 12, face = "italic", margin = margin(0,0,7,0)
      ), 
    plot.background = element_rect(
      fill = "#FFFFFF", color = NA
    ), 
    panel.grid = element_line(color = "#DADADA"), 
    plot.caption = element_text(size = 6)
  )
