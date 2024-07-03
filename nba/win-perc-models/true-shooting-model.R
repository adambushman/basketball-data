library('tidyverse')
library('nbastatR')

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

cols = c(test)
gameLogs = data.frame(test)


shootingData <- 
  nbastatR::game_logs(
    seasons = 2018:2023, 
    season_types = "Regular Season", 
    result_types = "team"
  ) %>%
  as_tibble() %>%
  select(yearSeason, idGame, slugTeam, slugOpponent, isWin, fgmTeam, fg3mTeam, fgaTeam, ftaTeam, orebTeam, drebTeam, tovTeam, ptsTeam) %>%
  inner_join(
    ., 
    ., 
    by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"), 
    suffix = c(".t", ".o")
  ) %>%
  rename(
    season = yearSeason.t, 
    team = slugTeam, 
    opponent = slugOpponent, 
    isTeamWin = isWin.t, 
  ) %>%
  mutate(
    poss = 0.5 * ((fgaTeam.t + (0.44 * ftaTeam.t) - (1.07 * (orebTeam.t / (orebTeam.t + drebTeam.o)) * (fgaTeam.t - fgmTeam.t)) + tovTeam.t) +
                  (fgaTeam.o + (0.44 * ftaTeam.o) - (1.07 * (orebTeam.o / (orebTeam.o + drebTeam.t)) * (fgaTeam.o - fgmTeam.o)) + tovTeam.o)), 
    tsa.t = fgaTeam.t + (0.44 * ftaTeam.t), 
    tsa.o = fgaTeam.o + (0.44 * ftaTeam.o)
  ) %>%
  group_by(
    season, team
  ) %>%
  summarise(
    winPct = sum(ifelse(isTeamWin == TRUE, 1, 0)) / n(), 
    nrtg = round((100.0 * sum(ptsTeam.t) / sum(poss)) - (100.0 * sum(ptsTeam.o) / sum(poss)), 3), 
    fgpAdv = round((sum(fgmTeam.t) / sum(fgaTeam.t)) - (sum(fgmTeam.o) / sum(fgaTeam.o)), 3),
    efgAdv = round(((sum(fgmTeam.t) + (0.5 * sum(fg3mTeam.t))) / sum(fgaTeam.t)) - ((sum(fgmTeam.o) + (0.5 * sum(fg3mTeam.o))) / sum(fgaTeam.o)), 3), 
    tsaAdv = round((sum(tsa.t) - sum(tsa.o)) / n(), 1), 
    tspAdv = round((sum(ptsTeam.t) / (sum(tsa.t) * 2)) - (sum(ptsTeam.o) / (sum(tsa.o) * 2)), 3), 
    .groups = "drop"
  )



####
# Model
####
model <- lm(winPct ~ tspAdv + tsaAdv, data = shootingData %>% filter(!(season %in% c(2022, 2023))))
summary(model)

winPctPred = predict(model, newdata = shootingData %>% filter(season == 2023))

data <- shootingData %>% 
  filter(season == 2023) %>%
  mutate(
    winPctPred,
    over_under = winPct - winPctPred, 
    split = ifelse(over_under > 0, "Over Performing", "Under Performing")
  ) %>%
  select(team, split, over_under, winPct, winPctPred) %>%
  pivot_longer(cols = c(winPct, winPctPred), names_to = 'type', values_to = 'win_rate')
  #mutate(team = factor(team, . %>% arrange(desc(over_under)) %>% .$team %>% unique(.)))
  # arrange(desc(over_under)) %>%
  # print(., n = 30)
  


data %>% mutate(team = afactor(., levels = . %>% arrange(desc(over_under)) %>% .$team %>% unique(.)))

lev = data %>% arrange(over_under) %>% .$team %>% unique(.)

data$team <- factor(data$team, levels = lev)

ggplot(
    data %>% filter(split == 'Over Performing'), 
    aes(win_rate, team)
  ) +
  geom_line() +
  geom_point(
    aes(color = type)
  ) +
  labs(
    title = "Test", 
    subtitle = "Test 2"
  )


get_age <- function(date) {
  days = as.integer(lubridate::as_date(Sys.Date()) - lubridate::as_date(date))
  
  return(paste(floor(days / 365.25), floor(10 * (days %% 365.25) / 365.25), sep = "."))
}


