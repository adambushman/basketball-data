library('hoopR')
library('tidyverse')
library('stringr')


player_stats <- hoopR::load_mbb_player_box()

player_pbp <- hoopR::load_mbb_pbp()

glimpse(player_stats)

agg_stats <-
  player_stats %>%
  group_by(
    athlete_id, 
    athlete_display_name, 
    athlete_position_name, 
    team_name
  ) %>%
  summarise(
    gp = n(), 
    mp = round(sum(as.numeric(min)) / n(), 1), 
    a_reb = round(sum(as.numeric(reb)) / n(), 1), 
    a_tov = round(sum(as.numeric(to)) / n(), 1), 
    a_stl = round(sum(as.numeric(stl)) / n(), 1), 
    a_blk = round(sum(as.numeric(blk)) / n(), 1), 
    a_ast = round(sum(as.numeric(ast)) / n(), 1), 
    a_ft = round(sum(as.numeric(str_sub(ft, str_locate(ft, "-")[,1]+1, str_length(ft)))) / n(), 1), 
    a_fga = round(sum(as.numeric(str_sub(fg, str_locate(fg, "-")[,1]+1, str_length(fg)))) / n(), 1)
  ) %>%
  ungroup() %>%
  mutate(
    reb_30 = round(a_reb * 30 / mp, 3), 
    stl_30 = round(a_stl * 30 / mp, 3), 
    blk_30 = round(a_blk * 30 / mp, 3), 
    tov_30 = round(a_tov * 30 / mp, 3), 
    ast_30 = round(a_ast * 30 / mp, 3), 
    ft_30 = round(a_ft * 30 / mp, 3)
  ) %>%
  mutate(
    activity = a_stl + a_blk, 
    pass = a_ast / a_tov, 
    activity_30 = stl_30 + blk_30, 
    pass_30 = ast_30 / tov_30
  )

glimpse(agg_stats)

# SELF-CREATION

self_creation <- 
  player_pbp %>%
  mutate(
    assisted = stringr::str_detect(text, "Assisted")
  ) %>%
  filter(scoring_play == TRUE & !stringr::str_detect(text, "Free Throw")) %>%
  group_by(
    participants_0_athlete_id
  ) %>%
  summarise(
    unast_fgm = sum(case_when(assisted ~ 0, TRUE ~ 1)), 
    unast_freq = sum(case_when(assisted ~ 0, TRUE ~ 1)) / n()
  ) %>%
  inner_join(
    player_stats %>% 
      distinct(
        athlete_id, 
        athlete_display_name, 
        athlete_position_name, 
        team_name
      ), 
    by = c("participants_0_athlete_id" = "athlete_id")
  )


self_creation %>%
  filter(unast_fgm > 0) %>%
  group_by(athlete_position_name) %>%
  mutate(
    fgm_perc = percent_rank(unast_fgm), 
    freq_perc = percent_rank(unast_freq)
  ) %>%
  select(-participants_0_athlete_id) %>%
  filter(
    stringr::str_detect(athlete_display_name, "Maxwell Lewis") |
      #stringr::str_detect(athlete_display_name, "Jett Howard") |
      #stringr::str_detect(athlete_display_name, "Keyonte George") |
      stringr::str_detect(athlete_display_name, "Terquavion Smith")
  )

# DUNKS & LAYUPS ("FINISHING")

dunks_layups <- 
  player_pbp %>%
  mutate(
    dunk_layup = type_text %in% c("DunkShot", "LayUpShot")
  ) %>%
  filter(shooting_play == TRUE & !stringr::str_detect(text, "Free Throw")) %>%
  group_by(
    participants_0_athlete_id
  ) %>%
  summarise(
    dl_pts = sum(case_when(dunk_layup ~ ifelse(scoring_play, score_value, 0), TRUE ~ 0)), 
    dl_att = sum(case_when(dunk_layup ~ 1, TRUE ~ 0)), 
    fga = n(), 
    ppa = round(dl_pts / dl_att, 3)
  ) %>%
  inner_join(
    player_stats %>% 
      distinct(
        athlete_id, 
        athlete_display_name, 
        athlete_position_name, 
        team_name
      ), 
    by = c("participants_0_athlete_id" = "athlete_id")
  )

dunks_layups %>%
  filter(dl_att > 10) %>%
  group_by(athlete_position_name) %>%
  mutate(
    dl_freq = round(dl_att / fga, 3), 
    freq_perc = percent_rank(dl_freq), 
    ppa_perc = percent_rank(ppa)
  ) %>%
  ungroup() %>%
  select(-participants_0_athlete_id) %>%
  filter(
    stringr::str_detect(athlete_display_name, "Jalen Hood") |
      stringr::str_detect(athlete_display_name, "Dariq Whitehead") |
      #stringr::str_detect(athlete_display_name, "Keyonte George") |
      stringr::str_detect(athlete_display_name, "Terquavion Smith")
  )


# DEFENSIVE ACTIVITY

agg_stats %>%
  filter((gp * mp) > 100) %>%
  group_by(athlete_position_name) %>%
  mutate(
    activity_perc = percent_rank(activity), 
    blk_perc = percent_rank(a_blk), 
    stl_perc = percent_rank(a_stl)
  ) %>%
  ungroup() %>%
  select(
    athlete_display_name:mp, 
    a_blk, 
    blk_30, 
    a_stl, 
    stl_30, 
    activity, 
    activity_perc
  ) %>%
  filter(
    stringr::str_detect(athlete_display_name, "Brandon Miller") |
      stringr::str_detect(athlete_display_name, "Jarace Walker") |
      stringr::str_detect(athlete_display_name, "Gregory Jackson") |
      stringr::str_detect(athlete_display_name, "Cam Whitmore")
  )

# REBOUNDING

agg_stats %>%
  filter((gp * mp) > 100) %>%
  group_by(athlete_position_name) %>%
  mutate(
    reb_perc = percent_rank(a_reb)
  ) %>%
  ungroup() %>%
  select(
    athlete_display_name:mp, 
    a_reb, 
    reb_30, 
    reb_perc
  ) %>%
  filter(
    stringr::str_detect(athlete_display_name, "Brandon Miller") |
      stringr::str_detect(athlete_display_name, "Jarace Walker") |
      stringr::str_detect(athlete_display_name, "Gregory Jackson") |
      stringr::str_detect(athlete_display_name, "Cam Whitmore")
  )


# PASSING

agg_stats %>%
  filter((gp * mp) > 100) %>%
  group_by(athlete_position_name) %>%
  mutate(
    pass_perc = percent_rank(pass), 
    ast_perc = percent_rank(a_ast)
  ) %>%
  ungroup() %>%
  select(
    athlete_display_name:mp, 
    a_ast, 
    ast_30, 
    ast_perc, 
    pass, 
    pass_30, 
    pass_perc
  ) %>%
  filter(
    stringr::str_detect(athlete_display_name, "Jett Howard") |
      stringr::str_detect(athlete_display_name, "Gradey Dick") |
      stringr::str_detect(athlete_display_name, "Keyonte George") |
      stringr::str_detect(athlete_display_name, "Brice Sensabaugh")
  )

# FREE THROWS

agg_stats %>%
  filter((gp * mp) > 100) %>%
  mutate(
    a_ftR = round(a_ft / a_fga, 3)
  ) %>%
  group_by(athlete_position_name) %>%
  mutate(
    ftR_perc = percent_rank(a_ftR)
  ) %>%
  ungroup() %>%
  select(
    athlete_display_name:mp,
    a_ft, 
    ft_30, 
    a_ftR,
    ftR_perc
  ) %>%
  filter(
    stringr::str_detect(athlete_display_name, "Brandon Miller") |
      stringr::str_detect(athlete_display_name, "Jarace Walker") |
      stringr::str_detect(athlete_display_name, "Gregory Jackson") |
      stringr::str_detect(athlete_display_name, "Cam Whitmore")
  )
