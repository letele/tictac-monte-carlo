generate_random_board <- function(empty_spaces, max_attempts = 1000) {
  # Total cells in the board
  total_cells <- 9
  
  filled_spaces <- total_cells - empty_spaces
  
  # X always starts, so if filled_spaces is odd, X has one more move
  # If filled_spaces is even, both have equal moves
  x_moves <- ceiling(filled_spaces / 2)
  o_moves <- floor(filled_spaces / 2)
  
  # Try to generate a non-terminal board
  for (attempt in 1:max_attempts) {
    # Create a vector with the appropriate number of each value
    board <- c(rep(1, x_moves), rep(-1, o_moves), rep(0, empty_spaces))
    
    # Shuffle the board randomly
    board <- sample(board)
    
    # Check if the board is non-terminal
    state <- check_state(board)
    
    if (is.null(state)) {
      return(board)
    }
  }
  
  return(NULL)
}
