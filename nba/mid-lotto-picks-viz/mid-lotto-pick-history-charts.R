test <-
  hoopR::nba_commonallplayers()

my_players <- read.csv(
  'C:/Users/Adam Bushman/Documents/pick8-10rookies.csv'
)


check <-
  my_players %>%
  mutate(
    Player = case_when(
      stringr::str_detect(Player, "Jakob P") ~ "Jakob Poeltl",
      stringr::str_detect(Player, "Sweetney") ~ "Michael Sweetney",
      Player == "Dennis Smith" ~ "Dennis Smith Jr.",
      Player == "Kevin Knox" ~ "Kevin Knox II",
      stringr::str_detect(Player, "Rafael A") ~ "Rafael Araujo",
      TRUE ~ Player
    )
  ) %>%
  inner_join(
    test[[1]] %>% filter(FROM_YEAR >= 1993),
    by = c("Player" = "DISPLAY_FIRST_LAST")
  )

nrow(check)
nrow(my_players)




player_list <- purrr::transpose(check)

eval <- function(id) {
  mp = hoopR::nba_playerprofilev2(player_id = id)[["SeasonTotalsRegularSeason"]]$MIN[1]
  as.integer(mp)
}


check_new <-
  check %>%
  mutate(ROOKIE_MP = purrr::map_int(PERSON_ID, eval))

real_data <-
  check_new %>%
  select(Year, TEAM_NAME, Player, Age, ROOKIE_MP)


camcorder::gg_record(
  'C:/Users/Adam Bushman/Camcorder',
  device = "png",
  width = 16,
  height = 16,
  units = "cm",
  dpi = 300
)

colors = adamb::make_color_obj(type = "Light", hue = 220)

ggplot(real_data, aes(x = "0", y = ROOKIE_MP)) +
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    justification = -.2,
    point_colour = NA,
    alpha = .7,
    fill = colors$core
  ) +
  geom_boxplot(
    width = .15,
    outlier.shape = NA,
    alpha = .7,
    color = colors$text,
    fill = colors$core
  ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    size = 3,
    side = "l",
    ## control range of jitter
    range_scale = .4,
    ## add some transparency
    alpha = .7,
    color = colors$core
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  annotate(
    "segment", y = 0, yend = 1000, color = "#7D1A2C",
    x = as.numeric("0") + 0.65, xend = as.numeric("0") + 0.65,
    arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))
  ) +
  annotate(
    "text", y = 0, color = "#7D1A2C", size = 2.5, vjust = 1, hjust = 0, x = as.numeric("0") + 0.6, fontface = "bold",
    label = "Of the 26 rookies who played <1000 NBA minutes\nonly 3 were from the last 5 years:\nObi Toppin, Jalen Smith, Johnny Davis"
  ) +
  annotate(
    "segment", y = 2000, yend = 3050, color = "#1A7D1E",
    x = as.numeric("0") + 0.65, xend = as.numeric("0") + 0.65,
    arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))
  ) +
  annotate(
    "text", y = 3050, color = "#1A7D1E", size = 2.5, vjust = 1, hjust = 1, x = as.numeric("0") + 0.6, fontface = "bold",
    label = "Of the 23 rookies who played >2000 NBA minutes\n5 were from the last 5 years:\nMikal Bridges, Franz Wagner,\nCollin Sexton, Davion Mitchell, Kevin Knox"
  ) +
  annotate(
    "text", y = 3050, color = colors$text, size = 5, vjust = 1, hjust = 1, x = as.numeric("0") + 1.7, fontface = "bold",
    lineheight = 2.3, label = "71% saw \nNBA minutes"
  ) +
  annotate(
    "text", y = 3050, color = colors$core, size = 9, hjust = 1, x = as.numeric("0") + 1.59, fontface = "bold",
    label = "+1,000"
  ) +
  labs(
    title = "Mid-lottery rookies play a lot",
    subtitle = "Past 30 years of #8, #9, #10 overall picks",
    caption = "Data from BBall Ref & NBA API\nDesigned by @adam_bushman",
    y = "Total NBA Minutes"
  ) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  coord_flip() +
  adamb::theme_adamb_cam(colors) +
  theme(
    plot.title = element_text(size = 27, margin = margin(10, 0, 0, 0, "pt")),
    plot.subtitle = element_text(margin = margin(0, 0, 10, 0, "pt")),
    plot.caption = element_text(size = 7),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )


camcorder::gg_record(
  'C:/Users/Adam Bushman/Camcorder',
  device = "png",
  width = 16,
  height = 9,
  units = "cm",
  dpi = 300
)

ggplot(
  real_data,
  aes(x = Year, y = ROOKIE_MP)
) +
  geom_jitter(
    color = colors$core,
    size = 2
  ) +
  stat_smooth(
    color = colors$text,
    fill = colors$lines
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  annotate(
    "segment", x = 2005, xend = 2005, y = 0, yend = 3050, color = "#7D1A2C"
  ) +
  annotate(
    "text", x = 2005.25, y = 3100, color = "#7D1A2C", hjust = 0, vjust = 1, size = 3,
    lineheight = 0.7, label = "G-League Expansion\nannounced"
  ) +
  annotate(
    "segment", x = 2015, xend = 2015, y = 0, yend = 3050, color = "#1A7D1E"
  ) +
  annotate(
    "text", x = 2015.25, y = 3100, color = "#1A7D1E", hjust = 0, vjust = 1, size = 3,
    lineheight = 0.7, label = "Single NBA\naffiliation achieved"
  ) +
  labs(
    title = "Mid-lottery rookies are not seeing\nless NBA mins since G-League",
    subtitle = "Total NBA minutes of past 30 years of #8, #9, #10 overall picks",
    caption = "Data from BBall Ref & NBA API\nDesigned by @adam_bushman",
    x = "Total NBA Minutes"
  ) +
  adamb::theme_adamb_cam(colors) +
  theme(
    plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
    plot.title = element_text(size = 15, margin = margin(10, 0, 5, 0, "pt")),
    plot.subtitle = element_text(size = 10, margin = margin(0, 0, 10, 0, "pt")),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(size = 7),
    panel.grid.major.y = element_blank(),
    axis.title = element_blank()
  )
