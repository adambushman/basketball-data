team_names <- sample(c('ATL', 'BOS', 'BKN', 'CHA', 'CHI', 'CLE', 'DAL', 'DEN', 'DET', 'GSW', 'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOP', 'NYK', 'OKC', 'ORL', 'PHI', 'PHX', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS'), 30)
win_totals <- sort(round(rnorm(30, 40, 12), 0))


run_lottery <- function(teams, wins) {
  # Official Odds
  odds <- as.integer(c(140, 140, 140, 125, 105, 90, 75, 60, 45, 30, 20, 15, 10, 5))

  # Lotto Tiebreakers
  ties = c()
  for(j in 2:14) {
    i = j - 1
    if(wins[i] == wins[j]) {
      ties[(length(ties)+1):(length(ties)+2)] = c(i, j)
    }
    else if (length(ties) > 0) {
      ties_u <- unique(ties)
      total = floor(sum(odds[ties_u]) / length(odds[ties_u]))

      # Fix the rounding ties
      if(sum(odds[ties_u]) %% length(odds[ties_u]) != 0) {
        odds[ties_u[1]] = total+1
        odds[ties_u[-1]] = total
      } else {
        odds[ties_u] = total
      }

      ties <- c()
    }
  }


  # Combinations
  combos <- combinat::combn(1:14, 4)

  # Assign combinations
  seqq <- 1:1000
  team_combos <- data.frame(matrix(nrow = 0, ncol = 2))
  for(i in 1:14) {
    indx <- sample(seqq, odds[i])
    team_combos <- rbind(team_combos, data.frame(teams[i], indx))
    seqq <- setdiff(seqq, indx)
  }

  # Run lotto (1-4)
  final_order <- c()
  while(length(final_order) < 4) {
    # Generate combo
    gen_combo <- sample(1:14, 4)
    while(paste(sort(gen_combo), collapse = ",") == combos[1:4, 1001]) {
      gen_combo <- sample(1:14, 4)
    }

    # Find team
    for(i in 1:1000) {
      if(paste(combos[1:4, i], collapse = ",") == paste(sort(gen_combo), collapse = ",")) {
        team <- team_combos$teams.i.[team_combos$indx == i]
        break
      }
    }

    # Top 4 order
    if(!(team %in% final_order)) {
      final_order[length(final_order)+1] <- team
      teams <- setdiff(teams, team)
    }
  }

  # Remaining 26 team order
  final_order[5:30] <- teams

  return(final_order)
}


results <- run_lottery(team_names, win_totals)

data.frame(results)


