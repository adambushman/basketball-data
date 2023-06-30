## Expand the data
## Something like 200 attempts pre injury, since injury
## Maybe show a timeline view
## Distribution of shots
## Etc

## Make it a blog post: 5 ways to visualize John's 3P results

all_box <- hoopR::load_nba_player_box(seasons = c(2023, 2022, 2021, 2020))

league_avg <- 
  all_box %>%
  filter(!is.na(three_point_field_goals_made) & !is.na(three_point_field_goals_attempted)) %>%
  summarise(p3 = sum(three_point_field_goals_made) / sum(three_point_field_goals_attempted))

collins_head <- all_box %>% filter(athlete_display_name == "John Collins") %>% select(athlete_headshot_href) %>% distinct() %>% unlist(use.names = FALSE)

collins_box <- 
  all_box %>%
  arrange(game_date) %>%
  filter(athlete_display_name == "John Collins") %>%
  mutate(
    fgm3_roll = RcppRoll::roll_sum(
      three_point_field_goals_made, n = 10, fill = as.integer(NA), align = "right"
    ), 
    fga3_roll = RcppRoll::roll_sum(
      three_point_field_goals_attempted, n = 10, fill = as.integer(NA), align = "right"
    )
  ) %>%
  filter(season != 2020) %>%
  select(
    game_date, 
    fgm3 = three_point_field_goals_made, 
    fga3 = three_point_field_goals_attempted, 
    fgm3_roll, 
    fga3_roll
  ) %>%
  mutate(
    p3 = fgm3 / fga3, 
    p3_roll = fgm3_roll / fga3_roll
  ) %>%
  tibble::rowid_to_column("id")

time_stamps <- 
  collins_box %>%
  mutate(
    text = case_when(
      game_date == "2022-02-11" ~ "Foot/finger injuries", 
      TRUE ~ as.character(NA)
    ), 
    xs = id, 
    xe = id, 
    ys = case_when(
      game_date == "2022-02-11" ~ 0.12,
      TRUE ~ as.numeric(NA)
    ), 
    ye = case_when(
      game_date == "2022-02-11" ~ 0.6, 
      TRUE ~ as.numeric(NA)
    ), 
    t = case_when(
      game_date == "2022-02-11" ~ 0.6, 
      TRUE ~ as.numeric(NA)
    ), 
    t_size = case_when(
      game_date == "2022-02-11" ~ 3, 
      TRUE ~ as.numeric(NA)
    ), 
    color = case_when(
      game_date == "2022-02-11" ~ "#C8102E", 
      TRUE ~ as.character(NA)
    ), 
    t_color = case_when(
      game_date == "2022-02-11" ~ "#dbe2ea", 
      TRUE ~ as.character(NA)
    ), 
    opa = case_when(
      game_date == "2022-02-11" ~ 1, 
      TRUE ~ as.numeric(NA)
    )
  ) %>%
  filter(!is.na(text))

camcorder::gg_record(
  dir = "C:/Users/Adam Bushman/Camcorder", 
  device = "png", 
  width = 16, 
  height = 9, 
  units = "cm", 
  dpi = 300
)

ggplot(collins_box) +
  geom_point(
    aes(x = id, y = p3_roll), 
    color = "#FFFFFF", 
    size = 3, 
    alpha = 0.5
  ) +
  geom_label(
    aes(x = id, y = t, label = text, fill = color, color = t_color, size = t_size), 
    data = time_stamps, hjust = -0.05
  ) +
  geom_segment(
    aes(x=xs, xend=xe, y=ys, yend=ye, color = color, alpha = opa), 
    data = time_stamps, lwd = 1.25
  ) +
  geom_hline(
    yintercept = league_avg$p3, 
    color = "#FDB927", lwd = 1.5, linetype = "dotted"
  ) +
  annotate(
    nflplotR::GeomFromPath,
    x = 5,
    y = 0.16,
    path = collins_head, 
    width = 0.15
  ) +
  stat_smooth(aes(x = id, y = p3_roll), color = "#FFFFFF") + 
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_identity() + 
  scale_size_identity() + 
  labs(
    title = glue::glue(
      "John Collins' dip in shooting aligns with <i style='color: #C8102E;'>date of injury</i>"
    ), 
    subtitle = "10 game rolling average 3P% since start of 2020-21 season (including playoffs)<br><i style='color: #FDB927;'>League Avg 3P%</i>", 
    caption = "Data via NBA Stats API | Accessed via {hoopR}\nDesigned by Adam Bushman"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(15, 15, 7, 15), 
    text = element_text(color = "#FFFFFF"), 
    axis.text = element_text(color = "#FFFFFF"),
    plot.title.position = "plot", 
    axis.title = element_blank(), 
    axis.text.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    plot.title = ggtext::element_markdown(size = 16, face = "bold"), 
    plot.subtitle = ggtext::element_markdown(size = 10, face = "italic", lineheight = 1.2), 
    plot.background = element_rect(
      fill = "#141414", color = NA
    ), 
    panel.grid = element_line(color = "#292929"), 
    
    plot.caption = element_text(size = 5)
  )



data_filt <- 
  collins_box %>%
  mutate(
    makes = fgm3, 
    attempts = fga3, 
    perc = makes / attempts, 
    split = case_when(
      game_date < '2022-02-11' ~ "Pre-injury", 
      game_date >= '2022-02-11' & game_date < '2023-02-24' ~ "Post-injury", 
      game_date >= '2023-02-24' ~ "Post-All Star '23"
    ), 
    split = factor(split, levels = rev(c("Post-All Star '23", "Post-injury", "Pre-injury"))), 
    fill = case_when(
      split == "Pre-injury" ~ "#87674F", 
      split == "Post-injury" ~ "#C8102E", 
      split == "Post-All Star '23" ~ "#FDB927"
    )
  ) %>% 
  filter(fga3 != 0)


data_summ <- 
  collins_box %>%
  mutate(
    split = case_when(
      game_date < '2022-02-11' ~ "Pre-injury", 
      game_date >= '2022-02-11' & game_date < '2023-02-24' ~ "Post-injury", 
      game_date >= '2023-02-24' ~ "Post-All Star '23"
    ), 
    split = factor(split, levels = rev(c("Post-All Star '23", "Post-injury", "Pre-injury")))
  ) %>%
  dplyr::group_by(split) %>%
  dplyr::summarise(
    t_makes = sum(fgm3), 
    t_attempts = sum(fga3), 
    perc = t_makes / t_attempts
  ) %>%
  dplyr::ungroup() %>%
  mutate(
    text = glue::glue("{t_makes} / {t_attempts} = {round(perc * 100, 0)}%"), 
    fill = case_when(
      split == "Pre-injury" ~ "#87674F", 
      split == "Post-injury" ~ "#C8102E", 
      split == "Post-All Star '23" ~ "#FDB927"
    ), 
    x = 1.1, 
    y = 1.3
  )


data_dot <- 
  data_filt %>%
  mutate(
    t_alt = attempts / max(attempts * 2 / 3)
  )



ggplot(data_filt) +
  geom_density(aes(x = perc, fill = fill), color = NA) +
  geom_dotplot(aes(x = perc), data = data_dot, dotsize = 0.25, binwidth = 0.1, color = "#FFFFFF", fill = NA) +
  geom_label(aes(x, y, label = text, fill = fill), color = "#141414", size = 2.75, data_summ, hjust = 1) + 
  scale_fill_identity() +
  scale_x_continuous(labels = scales::label_percent()) +
  facet_wrap(~split, ncol = 3) +
  labs(
    title = "John Collin's late 3P improvement", 
    subtitle = glue::glue(
      "Distribution of **per game 3P%** during last season, comparing", 
      "<br><i style='color:#87674F'>**Pre-injury**</i>, ", 
      "<i style='color:#DB504A'>**Post-injury**</i> and, ", 
      "<i style='color:#E3B505'>**Post-All Star '23**</i> splits"
    ), 
    x = "Efficiency", 
    caption = "Data via NBA Stats API | Accessed via {hoopR}\nDesigned by Adam Bushman"
  ) +
  annotate(
    "point", x = 1, y = 1.5, size = 1.5, shape = 21, color = "#FFFFFF", fill = NA
  ) +
  annotate(
    "text", hjust = 1.05, x = 0.975, y = 1.5, size = 2, color = "#FFFFFF", label = "3PA distribution"
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "#FFFFFF"), 
    plot.title.position = "plot", 
    plot.margin = margin(15, 15, 7, 15), 
    plot.title = element_text(size = 18, face = "bold"), 
    plot.subtitle = ggtext::element_markdown(size = 10, face = "italic", lineheight = 1.2), 
    axis.text = element_text(color = "#FFFFFF"), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(), 
    strip.text = element_text(color = "#FFFFFF", face = "bold"), 
    strip.background = element_rect(fill = "#292929", color = NA), 
    plot.background = element_rect(fill = "#141414", color = NA), 
    panel.grid = element_line(color = "#292929"), 
    
    plot.caption = element_text(size = 5)
  )


shooting <- read.csv('C:/Users/Adam Bushman/Documents/R/basketball-data/john-collins-splits.csv') %>%
  mutate(
    fg3r = fg3a / fga, 
    split = factor(split, levels = c("Pre-injury", "Post-injury", "Post-All Star '23")), 
    difficulty = factor(difficulty, levels = c("Harder", "Easier")), 
    type = factor(type, levels = c("Corner", "C&S", "Open", "AB", "Pullup", "Guarded")), 
    fill = case_when(
      split == "Pre-injury" ~ "#87674F", 
      split == "Post-injury" ~ "#C8102E", 
      split == "Post-All Star '23" ~ "#FDB927"
    )
  )


ggplot(shooting) +
  geom_col(aes(x = split, y = fg3r, fill = fill)) +
  scale_fill_identity() + 
  scale_y_continuous(labels = scales::label_percent()) +
  coord_flip() +
  facet_wrap(~type) +
  labs(
    title = "John Collins got easier shots over time", 
    subtitle = glue::glue(
      "Total 3PA as percent of total FGA, comparing", 
      "<br><i style='color:#87674F'>**Pre-injury**</i>, ", 
      "<i style='color:#DB504A'>**Post-injury**</i> and, ", 
      "<i style='color:#E3B505'>**Post-All Star '23**</i> splits"
    ), 
    y = "Percent of Total FGA", 
    caption = "Data via NBA Stats API | Accessed via {hoopR}\nDesigned by Adam Bushman"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot", 
    text = element_text(color = "#FFFFFF"), 
    plot.margin = margin(15, 15, 7, 15), 
    plot.title = element_text(size = 18, face = "bold"), 
    plot.subtitle = ggtext::element_markdown(size = 10, face = "italic", lineheight = 1.2), 
    axis.text = element_text(color = "#FFFFFF"), 
    axis.title.x = element_text(size = 10, vjust = 0), 
    axis.title.y = element_blank(), 
    strip.text = element_text(color = "#FFFFFF", face = "bold"), 
    strip.background = element_rect(fill = "#292929", color = NA), 
    plot.background = element_rect(fill = "#141414", color = NA), 
    panel.grid = element_line(color = "#292929"), 
    
    plot.caption = element_text(size = 5)
  )
