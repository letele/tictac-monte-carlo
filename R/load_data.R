boards_data <- read.csv(
  "data/boards.csv",
  header = FALSE,
  col.names = c(1:9, "PMC", "MCTS"),
  check.names = FALSE
)