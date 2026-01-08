# --------------------------
# Pure Monte Carlo functions
# --------------------------

# Function to simulate a single game
simulate_game_from_board <- function(board, player) {
  b <- board
  p <- player
  state <- check_state(b)
  
  while (is.null(state) && any(b == 0)) {
    empty_cells <- which(b == 0)
    i <- sample(empty_cells, 1)
    b[i] <- p
    p <- -p
    state <- check_state(b)
  }
  
  return(state)
}

# Function to run multiple simulations
pure_monte_carlo <- function(board, player, Nsim = 500) {
  replicate(Nsim, simulate_game_from_board(board, player))
}

pmc_results <- function(board, player, Nsims = 1000) {
  moves <- which(board == 0)
  df <- data.frame(
    cell = moves,
    win  = numeric(length(moves)),
    lose = numeric(length(moves)),
    draw = numeric(length(moves))
  )
  
  for (i in seq_along(moves)) {
    b <- board
    b[moves[i]] <- player
    
    results <- pure_monte_carlo(b, -player, Nsims) 
    
    df[i, -1] <- c(
      mean(results == player),
      mean(results == -player),
      mean(results == 0)
    )
  }
  
  return(df)
}

pmc_cell_prediction <- function(board, player, pmc_df) {
  # Priority: maximize wins, minimize losses, then draws
  
  # Find moves with maximum win probability
  max_win <- pmc_df[pmc_df$win == max(pmc_df$win), ]
  
  # If only one move has max win probability, choose it
  if (nrow(max_win) == 1) return(max_win$cell)
  
  # If tie on wins, choose move with minimum loss probability
  min_loss <- max_win[max_win$lose == min(max_win$lose), ]
  
  if (nrow(min_loss) == 1) return(min_loss$cell)
  
  # If still tied, choose move with maximum draw probability
  max_draw <- min_loss[min_loss$draw == max(min_loss$draw), ]
  
  # If still tied after all criteria, pick first one (or random)
  return(max_draw$cell[1])
}
