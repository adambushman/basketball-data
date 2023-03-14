camcorder::gg_record(
  dir = 'C:/Users/Adam Bushman/Pictures/_test', 
  device = 'png', 
  width = 16, 
  height = 16, 
  units = 'cm', 
  dpi = 300
)


data <-
  readxl::read_xlsx(
    'C:/Users/Adam Bushman/Documents/scoring.xlsx', sheet = 'Sheet1'
  ) |> 
  dplyr::inner_join(
    readxl::read_xlsx(
      'C:/Users/Adam Bushman/Documents/scoring.xlsx', sheet = 'Sheet2'
    ), 
    by = c('PLAYER' = 'PLAYER', 'TEAM' = 'TEAM')
  ) |> 
  janitor::clean_names() |> 
  dplyr::filter(fga >= 100) |> 
  dplyr::mutate(
    tsp = (pts / ((fga + (0.44 * fta)) * 2)) * 100, 
    tsp_rank = dplyr::dense_rank(dplyr::desc(tsp)), 
    ufga_rank = dplyr::dense_rank(dplyr::desc(fgm_percent_uast))
  )


data |>
  dplyr::mutate(
    new_lab = dplyr::case_when(
      (tsp_rank <= 8 | ufga_rank <= 8) ~ paste(
        stringr::str_sub(player, 1, 1), 
        '. ', 
        stringr::str_sub(
          player, 
          stringr::str_locate(player, ' ')[, 1] + 1, 
          stringr::str_length(player)
        ), 
        sep = ''
      ), 
      (tsp_rank <= 75 & ufga_rank <= 75) ~ paste(
        stringr::str_sub(player, 1, 1), 
        '. ', 
        stringr::str_sub(
          player, 
          stringr::str_locate(player, ' ')[, 1] + 1, 
          stringr::str_length(player)
        ), 
        sep = ''
      ), 
      TRUE ~ ''
    ), 
    new_col = ifelse(new_lab == '', 'gray', '#c8102a')
  ) |>
  
  ggplot2::ggplot(
    ggplot2::aes(
      x = fgm_percent_uast, 
      y = tsp, 
      label = new_lab
    )
  ) + 
  ggplot2::geom_point(
    ggplot2::aes(
      size = fga, 
      color = new_col
    ), 
    alpha = 0.6
  ) + 
  ggplot2::scale_color_identity() +
  ggrepel::geom_text_repel(
    size = 3, 
    max.overlaps = Inf, 
    box.padding = 0.5
  ) +
  ggplot2::labs(
    title = 'Landscape of NBA Player Self-Creation', 
    subtitle = 'A look into players who shine in efficiency (y), self-creation (x) \nor those who do both well (100+ FGAs)', 
    x = '% of FGM Unassisted', 
    y = 'True Shooting %', 
    size = 'Total FGAs'
  ) +
  ggplot2::geom_hline(
    yintercept = 57.6, 
    color = '#1d428a', 
    alpha = 0.8, 
    linewidth = 1, 
    linetype = 'dashed'
  ) +
  ggplot2::annotate(
    geom = 'text', x = 0, y = 58.6, size = 3, hjust = 0, 
    label = 'League Average TS', color = '#1d428a'
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = 'white', 
      color = NA
    ), 
    plot.title = ggplot2::element_text(
      face = 'bold', 
      size = 18, 
      color = '#c8102a'
    ), 
    plot.subtitle = ggplot2::element_text(
      face = 'italic', 
      size = 11, 
      color = 'darkgray'
    )
  )


data |>
  dplyr::mutate(
    new_lab = dplyr::case_when(
      team == 'UTA' ~ paste(
        stringr::str_sub(player, 1, 1), 
        '. ', 
        stringr::str_sub(
          player, 
          stringr::str_locate(player, ' ')[, 1] + 1, 
          stringr::str_length(player)
        ), 
        sep = ''
      ), 
      TRUE ~ ''
    ), 
    new_col = ifelse(new_lab == '', 'white', 'yellow'), 
    new_alf = ifelse(new_lab == '', 0.7, 1)
  ) |>
  
  ggplot2::ggplot(
    ggplot2::aes(
      x = fgm_percent_uast, 
      y = tsp, 
      label = new_lab, 
      alpha = new_alf
    )
  ) + 
  ggplot2::geom_point(
    ggplot2::aes(
      size = fga, 
      color = new_col
    ), 
  ) + 
  ggplot2::scale_color_identity() +
  ggrepel::geom_label_repel(
    box.padding = 0.5, 
    min.segment.length = 0, 
    max.overlaps = Inf, 
    size = 3, 
    color = 'white', 
    fill = NA
  ) +
  ggplot2::geom_hline(
    yintercept = 57.6, 
    color = 'yellow', 
    alpha = 0.8, 
    linewidth = 1, 
    linetype = 'dashed'
  ) +
  ggplot2::annotate(
    geom = 'text', x = 0, y = 58.6, size = 3, hjust = 0, 
    label = 'League Average TS', color = 'yellow'
  ) +
  ggplot2::labs(
    title = 'Landscape of NBA Player Self-Creation', 
    subtitle = 'A look into players who shine in efficiency (y), self-creation (x) \nor those who do both well (100+ FGAs)', 
    x = '% of FGM Unassisted', 
    y = 'True Shooting %', 
    size = 'Total FGAs'
  ) +
  ggplot2::guides(
    alpha = 'none', 
    size = ggplot2::guide_legend(
      override.aes = list(color = 'yellow')
    )
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    text = ggplot2::element_text(
      color = 'white'
    ), 
    axis.title = ggplot2::element_text(
      color = 'white'
    ), 
    axis.text = ggplot2::element_text(
      color = 'white'
    ), 
    plot.background = ggplot2::element_rect(
      fill = '#050505', 
      color = NA
    ), 
    plot.title = ggplot2::element_text(
      face = 'bold', 
      size = 18, 
      color = 'yellow'
    ), 
    plot.subtitle = ggplot2::element_text(
      face = 'italic', 
      size = 11, 
      color = 'darkgray'
    ), 
    panel.grid = ggplot2::element_line(
      color = '#333333'
    )
  )
