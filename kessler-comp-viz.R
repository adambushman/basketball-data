library('tidyverse')
library('ggtext')
library('glue')


camcorder::gg_record(
  dir = 'C:/Users/Adam Bushman/Pictures/_test', 
  device = 'png', 
  width = 16, 
  height = 16, 
  units = 'cm', 
  dpi = 300
)


data <- read.csv('C:/Users/Adam Bushman/Documents/kessler-stats.csv')

poss = data |> 
  filter(metric == 'Poss') |> 
  select(value) |>
  unlist(use.names = FALSE)

subT = "Key data points for games in which <i style='color:#ffffff'><strong>Kelly Olynyk sits</i><br>and those in which <i style='color:#f6ee26;font-weight:bold;'>Kelly Olynyk plays</i>"



ggplot(
  data |> 
    filter(metric != 'Poss') |>
    mutate(
      metric = factor(
        metric, 
        levels = rev(
          c('NRTG', 'eFG% adv', 'OREB% adv', 'TOV% adv', 'FTArate adv', 'On/Off Swing')
        )
      ), 
      new_col = ifelse(status == 'Kelly Played', '#f6ee26', '#ffffff')
    ), 
  aes(
    x = value, 
    y = metric
  )
) +
  geom_line(
    color = '#333333', 
    size = 1.5
  ) +
  geom_point(
    aes(color = new_col), 
    size = 3.5
  ) +
  scale_color_identity() + 
  geom_vline(
    xintercept = 0, 
    color = '#E5E5E5', 
    linetype = 'dashed', 
    linewidth = 0.5
  ) + 
  annotate(
    geom = 'text', 
    x = -0.5, 
    color = '#941C2F', 
    size = 3, 
    y = 'On/Off Swing', 
    hjust = 1, 
    vjust = 2, 
    label = 'Disadvantage'
  ) +
  annotate(
    geom = 'segment', 
    x = -0.5, xend = -2.5, 
    color = '#941C2F', 
    y = 'On/Off Swing', yend = 'On/Off Swing', 
    arrow = arrow(type = 'closed', length = unit(0.02, 'npc'))
  ) +
  annotate(
    geom = 'text', 
    x = 0.5, 
    color = '#018E42', 
    size = 3, 
    y = 'On/Off Swing', 
    hjust = 0, 
    vjust = 2, 
    label = 'Advantage'
  ) +
  annotate(
    geom = 'segment', 
    x = 0.5, xend = 2.5, 
    color = '#018E42', 
    y = 'On/Off Swing', yend = 'On/Off Swing', 
    arrow = arrow(type = 'closed', length = unit(0.02, 'npc'))
  ) +
  labs(
    title = 'Walker Kessler Impact', 
    subtitle = subT
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = '#050505', color = NA), 
    plot.title = element_text(face = 'bold', size = 20), 
    plot.subtitle = element_markdown(color = '#333333', face = 'italic', size = 12), 
    text = element_text(
      color = '#E5E5E5'
    ), 
    axis.title = element_blank(), 
    axis.text = element_text(
      color = '#E5E5E5', 
      face = 'bold'
    ), 
    panel.grid = element_line(
      color = '#333333'
    ), 
    plot.margin = margin(1, 0.75, 1, 0.5, unit = 'cm')
  )
