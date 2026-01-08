#Function that evaluates game state

Evaluate_game_state_matrix <- matrix(c(
  1, 0, 0, 1, 0, 0, 1, 0,
  0, 1, 0, 1, 0, 0, 0, 0,
  0, 0, 1, 1, 0, 0, 0, 1,
  1, 0, 0, 0, 1, 0, 0, 0,
  0, 1, 0, 0, 1, 0, 1, 1,
  0, 0, 1, 0, 1, 0, 0, 0,
  1, 0, 0, 0, 0, 1, 0, 1,
  0, 1, 0, 0, 0, 1, 0, 0,
  0, 0, 1, 0, 0, 1, 1, 0
), nrow = 9, ncol = 8, byrow = TRUE )


check_state <- function(b) {

  game_state <- t(b)%*%Evaluate_game_state_matrix
  
  # Win conditions
  if (any(game_state == -3)) return(-1)
  if (any(game_state ==  3)) return( 1)
  
  # Draw check
  if (sum(abs(b))==9) return(0)
  
  return(NULL)
}