library('tidyverse')
library('rvest')

web = "https://overtimeelite.com/seasons/current#steals_per_game"

raw_data <- 
  rvest::read_html(web) %>%
  rvest::html_element("table") %>%
  rvest::html_table()

cleaned_data <- 
  raw_data %>%
  janitor::clean_names() %>%
  rename(player = x) %>%
  mutate(
    jersey_no = stringr::str_sub(
      player, 
      1, 
      stringr::str_locate(player, "[a-zA-z]")[,1]-1
    ), 
    player = stringr::str_sub(
      player, 
      stringr::str_locate(player, "[a-zA-z]")[,1], 
      stringr::str_length(player)
    )
  )

transformed <-
  cleaned_data %>%
  mutate(
    ast_30 = round(astast * 30 / minmin, 1), 
    reb_30 = round(rebreb * 30 / minmin, 1), 
    tov_30 = round(toto * 30 / minmin, 1), 
    blk_30 = round(blkblk * 30 / minmin, 1), 
    stl_30 = round(stlstl * 30 / minmin, 1), 
    fga_30 = round(fgafga * 30 / minmin, 1), 
    fta_30 = round(ftafta * 30 / minmin, 1), 
    x3pa_30 = round(x3pa3pa * 30 / minmin, 1)
  ) %>%
  mutate(
    ft_rt_30 = round(fta_30 / fga_30, 3), 
    x3pa_rt_30 = round(x3pa_30 / fga_30, 3), 
    blk_stl_30 = blk_30 + stl_30, 
    ast_to_30 = round(ast_30 / tov_30, 2)
  ) %>%
  mutate(
    ast_perc = percent_rank(apgapg), 
    reb_perc = percent_rank(rpgrpg),
    blk_stl_perc = percent_rank(blk_stl_30), 
    ast_to_perc = percent_rank(ast_to_30), 
    ft_rt_perc = percent_rank(ft_rt_30), 
    ftper_perc = percent_rank(ft_percent_ft_percent), 
    x3pa_rt_perc = percent_rank(x3pa_rt_30), 
    x3pa_per_perc = percent_rank(x3p_percent_3p_percent)
  )

# PASSING

transformed %>%
  select(player, apgapg, tpgtpg, ast_to_perc) %>%
  filter(stringr::str_detect(player, "Thompson"))

# REBOUNDING

transformed %>%
  select(player, reb_30, reb_perc) %>%
  filter(stringr::str_detect(player, "Thompson"))

# ACTIVITY

transformed %>%
  select(player, bpgbpg, spgspg, blk_stl_30, blk_stl_perc) %>%
  filter(stringr::str_detect(player, "Thompson"))

# SHOOTING

transformed %>%
  select(
    player, 
    # Free throws
    fta_30, ft_rt_30, ft_percent_ft_percent, ftper_perc, ft_rt_perc, 
    # Threes
    x3pa_30, x3pa_rt_30, x3p_percent_3p_percent, x3pa_per_perc, x3pa_rt_perc
  ) %>%
  filter(stringr::str_detect(player, "Thompson"))
