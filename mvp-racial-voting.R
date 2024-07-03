library('tidyverse')

voting <- 
  bind_rows(
    readxl::read_xlsx('voting-results.xlsx', '2020-21'), 
    readxl::read_xlsx('voting-results.xlsx', '2021-22')
  ) %>%
  pivot_longer(
    -c(`Voter Name`, `Affiliation`), 
    names_to = "Finish", 
    values_to = "Selection"
  ) %>%
  janitor::clean_names() %>%
  filter(voter_name != 'Fan, Vote') %>%
  left_join(
    readxl::read_xlsx('voting-results.xlsx', 'Players') %>% 
      rename(player_caucasian = Caucasian) %>%
      mutate(player_caucasian = player_caucasian == 1), 
    by = c("selection" = "Player")
  ) %>%
  mutate(
    voter_caucasian = case_when(
      voter_name == 'Aldridge, David' ~ FALSE,
      voter_name == 'Amick, Sam' ~ TRUE, 
      voter_name == 'Anderson, Jason' ~ TRUE,
      voter_name == 'Anthony, Greg' ~ FALSE,
      voter_name == 'Arnovitz, Kevin' ~ TRUE, 
      voter_name == 'Aschburner, Steve' ~ TRUE, 
      voter_name == 'Bancharel, Antoine' ~ TRUE,
      voter_name == 'Bao, Renjun' ~ FALSE,
      voter_name == 'Beck, Howard' ~ TRUE,
      voter_name == 'Bontemps, Tim' ~ TRUE,
      voter_name == 'Boone, Rod' ~ FALSE,
      voter_name == 'Broussard, Chris' ~ FALSE,
      voter_name == 'Bucher, Ric' ~ TRUE,
      voter_name == 'Burke, Doris' ~ TRUE,
      voter_name == 'Caplan, Callie' ~ TRUE,
      voter_name == 'Charania, Shams' ~ FALSE,
      voter_name == 'Chiang, Anthony' ~ TRUE,
      voter_name == 'Chinellato, Davide' ~ as.logical(NA), # ?
      voter_name == 'Daimiel, Antoni' ~ as.logical(NA), # TRUE
      voter_name == 'Devine, Dan' ~ TRUE,
      voter_name == 'Edwards III, James' ~ as.logical(NA), # FALSE
      voter_name == 'Fedor, Chris' ~ TRUE,
      voter_name == 'Goodwill, Vince' ~ FALSE,
      voter_name == 'Grange, Michael' ~ TRUE,
      voter_name == 'Greenberg, Jared' ~ TRUE,
      voter_name == 'Greenberg, Mike' ~ TRUE,
      voter_name == 'Harlan, Kevin' ~ TRUE,
      voter_name == 'Harper, Zach' ~ FALSE,
      voter_name == 'Helin, Kurt' ~ TRUE,
      voter_name == 'Herring, Chris' ~ FALSE,
      voter_name == 'Hill, Drew' ~ TRUE,
      voter_name == 'Himmelsbach, Adam' ~ TRUE,
      voter_name == 'Hsu, Lisa' ~ FALSE,
      voter_name == 'Hubbarth, Cassidy' ~ FALSE,
      voter_name == 'Iko, Kelly' ~ FALSE,
      voter_name == 'Isola, Frank' ~ TRUE,
      voter_name == 'Jackson, Mark' ~ FALSE,
      voter_name == 'Johnson, Ernie' ~ TRUE,
      voter_name == 'Johnson, K.C.' ~ TRUE,
      voter_name == 'Kestecher, Marc' ~ TRUE,
      voter_name == 'Kirschner, Chris'~ FALSE,
      voter_name == 'Koreen, Eric' ~ TRUE,
      voter_name == 'Kushner, Scott' ~ TRUE,
      voter_name == 'Lowe, Zach' ~ TRUE,
      voter_name == 'Mahoney, Brian' ~ TRUE,
      voter_name == 'Mahoney, Rob' ~ TRUE,
      voter_name == 'Mannix, Chris' ~ TRUE,
      voter_name == 'Martinez, Diego' ~ as.logical(NA),
      voter_name == 'McDonald, Jeff' ~ TRUE,
      voter_name == 'McMenamin, Dave' ~ TRUE,
      voter_name == 'Medina, Mark' ~ TRUE,
      voter_name == 'Miyaji, Yoko' ~ FALSE,
      voter_name == 'Mussatto, Joe' ~ TRUE,
      voter_name == 'Nehm, Eric' ~ TRUE,
      voter_name == 'O\'Connor, Kevin' ~ TRUE,
      voter_name == 'Oram, Bill' ~ TRUE,
      voter_name == 'Pasch, Dave' ~ TRUE,
      voter_name == 'Pelton, Kevin' ~ TRUE,
      voter_name == 'Perkins, Kendrick' ~ FALSE,
      voter_name == 'Pina, Michael' ~ TRUE,
      voter_name == 'Pompey, Keith' ~ FALSE,
      voter_name == 'Popper, Steve' ~ TRUE,
      voter_name == 'Powell, Shaun' ~ FALSE,
      voter_name == 'Price, Khobi' ~ FALSE,
      voter_name == 'Quick, Jason' ~ TRUE,
      voter_name == 'Rankin, Duane' ~ FALSE,
      voter_name == 'Ready, Stephanie' ~ FALSE,
      voter_name == 'Reiter, Bill' ~ TRUE,
      voter_name == 'Reverchon, Remi' ~ TRUE,
      voter_name == 'Reynolds, Tim' ~ TRUE,
      voter_name == 'Robbins, Josh' ~ TRUE,
      voter_name == 'Rohlin, Melissa' ~ TRUE,
      voter_name == 'Rooks, Taylor' ~ FALSE,
      voter_name == 'Rose, Jalen' ~ FALSE,
      voter_name == 'Russillo, Ryen' ~ TRUE,
      voter_name == 'Schuhmann, John' ~ TRUE,
      voter_name == 'Schutz, Guillermo' ~ TRUE,
      voter_name == 'Scott, Dennis' ~ FALSE,
      voter_name == 'Shelburne, Ramona' ~ TRUE,
      voter_name == 'Simmons, Bill' ~ TRUE,
      voter_name == 'Singer, Mike' ~ TRUE,
      voter_name == 'Slater, Anthony' ~ TRUE,
      voter_name == 'Smith, Doug' ~ TRUE,
      voter_name == 'Smith, Stephen A.' ~ FALSE,
      voter_name == 'Smith, Steve' ~ FALSE,
      voter_name == 'Sohi, Seerat' ~ FALSE,
      voter_name == 'Stavrou, Harris' ~ TRUE,
      voter_name == 'Sugiura, Daisuke' ~ FALSE,
      voter_name == 'Termine, Justin' ~ TRUE,
      voter_name == 'Tsaltas, Christos' ~ TRUE,
      voter_name == 'Vardon, Joe' ~ TRUE,
      voter_name == 'Walden, Eric' ~ TRUE,
      voter_name == 'Washburn, Gary' ~ FALSE,
      voter_name == 'Wilbon, Michael' ~ FALSE,
      voter_name == 'Windhorst, Brian' ~ TRUE,
      voter_name == 'Winer, Matt' ~ TRUE,
      voter_name == 'Winfield, Kristian' ~ FALSE,
      voter_name == 'Wright, Michael C.' ~ FALSE,
      voter_name == 'Youngmisuk, Ohm' ~ FALSE,
      voter_name == 'Zillgitt, Jeff' ~ TRUE,
      voter_name == 'Albert, Marv' ~ TRUE,
      voter_name == 'Barnes, Evan' ~ FALSE,
      voter_name == 'Barry, Jon' ~ TRUE,
      voter_name == 'Bondy, Stefan' ~ TRUE,
      voter_name == 'Codocea, Mauricio' ~ as.logical(NA),
      voter_name == 'Collier, Jamal' ~ FALSE,
      voter_name == 'Freeman, Joe' ~ TRUE,
      voter_name == 'Goon, Kyle' ~ FALSE,
      voter_name == 'Haupt, Max' ~ as.logical(NA),
      voter_name == 'Haynes, Chris' ~ TRUE,
      voter_name == 'Jones, Jason' ~ FALSE,
      voter_name == 'Jones, Tony' ~ FALSE,
      voter_name == 'Katz, Fred' ~ TRUE,
      voter_name == 'Lewis, Brian' ~ FALSE,
      voter_name == 'MacMullan, Jackie' ~ TRUE,
      voter_name == 'Nichols, Rachel' ~ TRUE,
      voter_name == 'Owczarski, James' ~ TRUE,
      voter_name == 'Parker, Candace' ~ FALSE,
      voter_name == 'Parry, Roy' ~ TRUE,
      voter_name == 'Sandri, Simone' ~ TRUE,
      voter_name == 'Sankofa II, Omari' ~ FALSE,
      voter_name == 'Swanson, Mirjam' ~ TRUE,
      voter_name == 'Taylor, Maria' ~ FALSE,
      voter_name == 'Townsend, Brad' ~ TRUE,
      voter_name == 'Uluc, Olgun' ~ TRUE,
      voter_name == 'Wolstat, Ryan' ~ TRUE,
      voter_name == 'Young, Royce' ~ TRUE
    )
  ) %>%
  filter(!is.na(voter_caucasian))


# Just 1st place votes

fp_votes <- voting %>% filter(finish == '1st Place (10 points)')
fp_tabl <- table(fp_votes$voter_caucasian, fp_votes$player_caucasian, dnn = c("voter_caucasian", "player_caucasian"))
fp_results <- chisq.test(fp_tabl)

fp_results$p.value


# Just 1st + 2nd place votes

fsp_votes <- voting %>% filter(finish == '1st Place (10 points)' | finish == '2nd Place (7 points)')
fsp_tabl <- table(fsp_votes$voter_caucasian, fsp_votes$player_caucasian, dnn = c("voter_caucasian", "player_caucasian"))
fsp_results <- chisq.test(fsp_tabl)

fsp_results$p.value

# All votes

all_votes <- voting
all_tabl <- table(all_votes$voter_caucasian, all_votes$player_caucasian, dnn = c("voter_caucasian", "player_caucasian"))
all_results <- chisq.test(all_tabl)

all_results$p.value
