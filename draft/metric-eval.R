library('hoopR')
library('tidyverse')
library('stringr')


player_stats <- hoopR::load_mbb_player_box()

player_pbp <- hoopR::load_mbb_pbp()

agg_stats <-
  player_stats %>%
  filter(!is.na(minutes)) %>%
  mutate(
    l_ftp = round(sum(free_throws_made) / sum(free_throws_attempted), 3), 
    l_3pp = round(sum(three_point_field_goals_made) / sum(three_point_field_goals_attempted), 3)
  ) %>%
  group_by(
    athlete_id, 
    athlete_display_name, 
    athlete_position_name, 
    team_name, 
    l_ftp, 
    l_3pp
  ) %>%
  summarise(
    gp = n(), 
    mp = sum(minutes), 
    a_reb = round(sum(rebounds) / n(), 1), 
    a_tov = round(sum(turnovers) / n(), 1), 
    a_stl = round(sum(steals) / n(), 1), 
    a_blk = round(sum(blocks) / n(), 1), 
    a_foul = round(sum(fouls) / n(), 1), 
    a_ast = round(sum(assists) / n(), 1), 
    a_ft = round(sum(free_throws_attempted) / n(), 1), 
    a_fga = round(sum(field_goals_attempted) / n(), 1), 
    a_3pa = round(sum(three_point_field_goals_attempted) / n(), 1), 
    
    a_ftp = round(sum(free_throws_made) / sum(free_throws_attempted), 3), 
    a_3pp = round(sum(three_point_field_goals_made) / sum(three_point_field_goals_attempted), 3)
  ) %>%
  ungroup() %>%
  mutate(
    reb_30 = round(a_reb * 30 / (mp / gp), 3), 
    stl_30 = round(a_stl * 30 / (mp / gp), 3), 
    blk_30 = round(a_blk * 30 / (mp / gp), 3), 
    foul_30 = round(a_foul * 30 / (mp / gp), 3), 
    tov_30 = round(a_tov * 30 / (mp / gp), 3), 
    ast_30 = round(a_ast * 30 / (mp / gp), 3), 
    ft_30 = round(a_ft * 30 / (mp / gp), 3)
  ) %>%
  mutate(
    activity = a_stl + a_blk - a_foul, 
    pass = a_ast / a_tov, 
    activity_30 = stl_30 + blk_30 - foul_30, 
    pass_30 = ast_30 / tov_30
  )

glimpse(agg_stats)

# SELF-CREATION

self_creation <- 
  player_pbp %>%
  mutate(
    assisted = stringr::str_detect(text, "Assisted")
  ) %>%
  filter(scoring_play == TRUE & !stringr::str_detect(text, "Free Throw") & !is.na(athlete_id_1)) %>%
  group_by(
    athlete_id_1
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
    by = c("athlete_id_1" = "athlete_id")
  )


self_creation %>%
  filter(unast_fgm > 0) %>%
  group_by(athlete_position_name) %>%
  mutate(
    fgm_perc = percent_rank(unast_fgm), 
    freq_perc = percent_rank(unast_freq)
  ) %>%
  select(athlete_display_name, athlete_position_name, team_name, unast_fgm, unast_freq, fgm_perc, freq_perc) %>%
  filter(
    athlete_display_name %in% c("Taylor Hendricks", "Keyonte George", "Brice Sensabaugh")
  )

# DUNKS & LAYUPS ("FINISHING")

dunks_layups <- 
  player_pbp %>%
  mutate(
    dunk_layup = type_text %in% c("DunkShot", "LayUpShot")
  ) %>%
  filter(shooting_play == TRUE & !stringr::str_detect(text, "Free Throw")) %>%
  group_by(
    athlete_id_1
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
    by = c("athlete_id_1" = "athlete_id")
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
  select(
    athlete_display_name, athlete_position_name, team_name, 
    dl_att, dl_pts, fga, ppa, dl_freq, freq_perc, ppa_perc
  ) %>%
  filter(
    athlete_display_name %in% c("Taylor Hendricks", "Keyonte George", "Brice Sensabaugh")
  )

# JUMP SHOTS

jumpshots <- 
  player_pbp %>%
  mutate(
    jumpshot = ifelse(type_text =="JumpShot", TRUE, FALSE)
  ) %>%
  filter(shooting_play == TRUE & !stringr::str_detect(text, "Free Throw")) %>%
  group_by(
    athlete_id_1
  ) %>%
  summarise(
    js_pts = sum(case_when(jumpshot ~ ifelse(scoring_play, score_value, 0), TRUE ~ 0)), 
    js_att = sum(case_when(jumpshot ~ 1, TRUE ~ 0)), 
    fga = n(), 
    ppa = round(js_pts / js_att, 3)
  ) %>%
  inner_join(
    player_stats %>% 
      distinct(
        athlete_id, 
        athlete_display_name, 
        athlete_position_name, 
        team_name
      ), 
    by = c("athlete_id_1" = "athlete_id")
  )

jumpshots %>%
  filter(js_att > 10) %>%
  group_by(athlete_position_name) %>%
  mutate(
    js_freq = round(js_att / fga, 3), 
    freq_perc = percent_rank(js_freq), 
    ppa_perc = percent_rank(ppa)
  ) %>%
  ungroup() %>%
  select(
    athlete_display_name, athlete_position_name, team_name, 
    js_att, js_pts, fga, ppa, js_freq, freq_perc, ppa_perc
  ) %>%
  filter(
    athlete_display_name %in% c("Taylor Hendricks", "Keyonte George", "Brice Sensabaugh")
  )

# THREES

agg_stats %>%
  filter((gp * mp) > 100) %>%
  group_by(athlete_position_name) %>%
  mutate(
    a_3pR = round(a_3pa / a_fga, 3), 
    x3pR_perc = percent_rank(a_3pR), 
    x3pp_perc = percent_rank(a_3pp), 
    dist_avg = round((l_3pp - a_3pp) * (a_3pa * gp), 1)
  ) %>%
  ungroup() %>%
  select(
    athlete_display_name:team_name, 
    gp:mp, 
    a_3pa, 
    a_3pR, 
    x3pR_perc, 
    a_3pp, 
    x3pp_perc, 
    dist_avg
  ) %>%
  filter(
    athlete_display_name %in% c("Taylor Hendricks", "Keyonte George", "Brice Sensabaugh")
  )


# DEFENSIVE ACTIVITY

agg_stats %>%
  filter((gp * mp) > 100) %>%
  group_by(athlete_position_name) %>%
  mutate(
    activity_perc = percent_rank(activity_30), 
    blk_perc = percent_rank(blk_30), 
    stl_perc = percent_rank(stl_30), 
    foul_perc = percent_rank(foul_30)
  ) %>%
  ungroup() %>%
  select(
    athlete_display_name:team_name, 
    gp:mp, 
    blk_30, 
    stl_30, 
    foul_30, 
    activity_30, 
    activity_perc
  ) %>%
  filter(
    athlete_display_name %in% c("Taylor Hendricks", "Keyonte George", "Brice Sensabaugh")
  )

# REBOUNDING

agg_stats %>%
  filter((gp * mp) > 100) %>%
  group_by(athlete_position_name) %>%
  mutate(
    reb_perc = percent_rank(reb_30)
  ) %>%
  ungroup() %>%
  select(
    athlete_display_name:team_name, 
    gp:mp, 
    reb_30, 
    reb_perc
  ) %>%
  filter(
    athlete_display_name %in% c("Taylor Hendricks", "Keyonte George", "Brice Sensabaugh")
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
    athlete_display_name:team_name, 
    gp:mp, 
    a_ast, 
    ast_30, 
    ast_perc, 
    pass, 
    pass_30, 
    pass_perc
  ) %>%
  filter(
    athlete_display_name %in% c("Taylor Hendricks", "Keyonte George", "Brice Sensabaugh")
  )

# FREE THROWS

agg_stats %>%
  filter((gp * mp) > 100) %>%
  mutate(
    a_ftR = round(a_ft / a_fga, 3), 
    ftR_perc = percent_rank(a_ftR), 
    ftp_perc = percent_rank(a_ftp), 
    dist_avg = round((l_ftp - a_ftp) * (a_ft * gp), 1)
  ) %>%
  group_by(athlete_position_name) %>%
  mutate(
    ftR_perc = percent_rank(a_ftR)
  ) %>%
  ungroup() %>%
  select(
    athlete_display_name:team_name, 
    gp:mp, 
    a_ft, 
    ft_30, 
    a_ftR,
    ftR_perc, 
    a_ftp, 
    dist_avg
  ) %>%
  filter(
    athlete_display_name %in% c("Taylor Hendricks", "Keyonte George", "Brice Sensabaugh")
  )
