# ---------------------------------
# Monte Carlo Tree Search functions
# ---------------------------------

monte_carlo_tree_search <- function(m, k = 1, alpha = 1) {
  
  BUF <- 500000
  # stats
  depth_s   <- integer(BUF)
  cell_s    <- integer(BUF)
  outcome_s <- integer(BUF)
  s_cnt <- 0
  
  # nodes
  x_n     <- numeric(BUF)
  y_n     <- numeric(BUF)
  depth_n <- integer(BUF)
  cell_n  <- integer(BUF)
  out_n   <- integer(BUF)
  term_n  <- logical(BUF)
  act_n   <- logical(BUF)
  n_cnt <- 0
  
  # edges
  x0_e <- numeric(BUF)
  y0_e <- numeric(BUF)
  x1_e <- numeric(BUF)
  y1_e <- numeric(BUF)
  act_e <- logical(BUF)
  e_cnt <- 0
  
  # ---- traversal ----
  traverse <- function(board, current_player,
                       x0, y0, d,
                       theta_min, theta_max,
                       cell, starting_player) {
    
    result <- check_state(board)
    
    # ---- terminal ----
    if (!is.null(result)) {
      
      active <- runif(1) < alpha
      
      if (active) {
        s_cnt <<- s_cnt + 1
        depth_s[s_cnt]   <<- d
        cell_s[s_cnt]    <<- cell
        outcome_s[s_cnt] <<- result * starting_player
      }
      
      n_cnt <<- n_cnt + 1
      x_n[n_cnt]     <<- x0
      y_n[n_cnt]     <<- y0
      depth_n[n_cnt] <<- d
      cell_n[n_cnt]  <<- cell
      out_n[n_cnt]   <<- result
      term_n[n_cnt]  <<- TRUE
      act_n[n_cnt]   <<- active
      
      return(active)
    }
    
    # ---- legal moves (no which) ----
    moves <- integer(9)
    nm <- 0
    for (i in 1:9) {
      if (board[i] == 0) {
        nm <- nm + 1
        moves[nm] <- i
      }
    }
    if (nm == 0) return(FALSE)
    
    angles <- seq(theta_min, theta_max, length.out = nm + 1)
    any_active <- FALSE
    
    for (i in 1:nm) {
      mid <- (angles[i] + angles[i + 1]) / 2
      x1 <- (d + 1) * cos(mid)
      y1 <- (d + 1) * sin(mid)
      
      move <- moves[i]
      board[move] <- current_player
      
      child_active <- traverse(
        board, -current_player,
        x1, y1,
        d + 1,
        angles[i], angles[i + 1],
        move, starting_player
      )
      
      board[move] <- 0
      any_active <- any_active || child_active
      
      e_cnt <<- e_cnt + 1
      x0_e[e_cnt] <<- x0
      y0_e[e_cnt] <<- y0
      x1_e[e_cnt] <<- x1
      y1_e[e_cnt] <<- y1
      act_e[e_cnt] <<- child_active
    }
    
    # ---- internal node ----
    n_cnt <<- n_cnt + 1
    x_n[n_cnt]     <<- x0
    y_n[n_cnt]     <<- y0
    depth_n[n_cnt] <<- d
    cell_n[n_cnt]  <<- cell
    out_n[n_cnt]   <<- NA
    term_n[n_cnt]  <<- FALSE
    act_n[n_cnt]   <<- any_active
    
    any_active
  }
  
  # ---- run ----
  traverse(m, k, 0, 0, 0, 0, 2*pi, 0, k)
  
  # ---- build outputs ONCE ----
  stats <- if (s_cnt > 0)
    data.frame(
      depth   = depth_s[1:s_cnt],
      cell    = cell_s[1:s_cnt],
      outcome = outcome_s[1:s_cnt]
    ) else data.frame()
  
  nodes <- if (n_cnt > 0)
    unique(data.frame(
      x = x_n[1:n_cnt],
      y = y_n[1:n_cnt],
      depth = depth_n[1:n_cnt],
      cell = cell_n[1:n_cnt],
      outcome = out_n[1:n_cnt],
      is_terminal = term_n[1:n_cnt],
      is_active = act_n[1:n_cnt]
    )) else data.frame()
  
  edges <- if (e_cnt > 0)
    unique(data.frame(
      x0 = x0_e[1:e_cnt],
      y0 = y0_e[1:e_cnt],
      x1 = x1_e[1:e_cnt],
      y1 = y1_e[1:e_cnt],
      is_active = act_e[1:e_cnt]
    )) else data.frame()
  
  list(
    stats = stats,
    nodes = nodes,
    edges = edges
  )
}

plot_tree <- function(tree_data) {
  nodes <- tree_data$nodes
  edges <- tree_data$edges
  
  if (nrow(nodes) == 0) {
    warning("No nodes to plot.")
    return(NULL)
  }
  
  p <- ggplot() + theme_void() + coord_fixed()
  
  
  if (!is.null(edges) && nrow(edges) > 0) {
    edge_idx <- !is.na(edges$x0) & !is.na(edges$y0) & !is.na(edges$x1) & !is.na(edges$y1)
    
    if (any(edge_idx)) {
      p_edges <- edges[edge_idx, ]
      p_edges$edge_col <- ifelse(p_edges$is_active, "#a3a3a3", "#a9a9a9")
      p_edges$edge_lty <- ifelse(p_edges$is_active, "solid", "dotted")
      p <- p + geom_segment(
        data = p_edges,
        aes(x = x0, y = y0, xend = x1, yend = y1, 
            color = edge_col, linetype = edge_lty),
        linewidth = 0.3
      )
    }
  }
  
  term_idx <- which(nodes$is_terminal)
  
  if (length(term_idx) > 0) {
    p_nodes <- nodes[term_idx, ]
    
    p_nodes$n_col <- "#d3d3d3"  
    p_nodes$n_lbl <- NA_character_
    p_nodes$n_size <- 1        
    
    
    act_idx <- which(p_nodes$is_active)
    if (length(act_idx) > 0) {
      outcomes <- p_nodes$outcome[act_idx]
      new_cols <- rep("#949494", length(act_idx)) 
      new_cols[outcomes == 1]  <- "purple"        
      new_cols[outcomes == -1] <- "orange"       
      p_nodes$n_col[act_idx] <- new_cols
    }
    
    p_nodes$n_lbl[p_nodes$outcome == 1]  <- "X"
    p_nodes$n_lbl[p_nodes$outcome == -1] <- "O"
    
    is_text <- p_nodes$outcome != 0
    text_data <- p_nodes[is_text, ]
    point_data <- p_nodes[!is_text, ]
    
    if (nrow(text_data) > 0) {
      p <- p + geom_text(
        data = text_data,
        aes(x = x, y = y, label = n_lbl, color = n_col),
        size = 4, fontface = "bold"
      )
    }
    
    if (nrow(point_data) > 0) {
      p <- p + geom_point(
        data = point_data,
        aes(x = x, y = y, color = n_col, size = n_size),
        shape = 19
      )
    }
  }
  
  p <- p + 
    scale_color_identity() + 
    scale_linetype_identity() + 
    scale_size_identity()
  
  print(p)
}

mcts_stats <- function(b, k = 1, alpha = 1) {
  
  # 1. Pre-allocate storage
  buffer_size <- 500000 
  depth_vec   <- integer(buffer_size)
  cell_vec    <- integer(buffer_size)
  outcome_vec <- integer(buffer_size)
  
  counter <- 0
  
  # 2. Define the traverser
  traverse <- function(board, current_player, d, last_cell) {
    
    result <- check_state(board)
    
    # --- Terminal Node ---
    if (!is.null(result)) {
      # Probabilistic logging (Alpha)
      if (runif(1) < alpha) {
        counter <<- counter + 1
        
        # Dynamic resizing if buffer is full
        if (counter > length(depth_vec)) {
          new_len <- length(depth_vec) * 2
          length(depth_vec) <<- new_len
          length(cell_vec) <<- new_len
          length(outcome_vec) <<- new_len
        }
        
        # Direct vector assignment (Super fast)
        depth_vec[counter]   <<- d
        cell_vec[counter]    <<- last_cell
        outcome_vec[counter] <<- result * k
      }
      return()
    }
    
    # --- Recursive Step ---
    legal_moves <- which(board == 0)
    
    for (move in legal_moves) {
      board[move] <- current_player
      
      # RECURSE
      traverse(board, -current_player, d + 1, move)
      
      # UNDO MOVE 
      board[move] <- 0
    }
  }
  
  # 3. Start the search
  traverse(b, k, 0, 0)
  
  # 4. Return formatted data
  if (counter == 0) return(NULL)
  
  idx <- 1:counter
  data.frame(
    depth = depth_vec[idx],
    cell = cell_vec[idx],
    outcome = outcome_vec[idx]
  )
}

mcts_df <- function(df) {
  if (nrow(df) == 0) {
    cols <- c("depth", "cell", "win", "lose", "draw")
    return(matrix(nrow = 0, ncol = 5, dimnames = list(NULL, cols)))
  }
  
  # Add win/lose/draw indicators using transform
  df <- transform(df,
                  win  = as.integer(outcome == 1),
                  lose = as.integer(outcome == -1),
                  draw = as.integer(outcome == 0))
  
  # Aggregate by depth and cell
  summary_df <- aggregate(cbind(win, lose, draw) ~ depth + cell, data = df, sum)
  
  summary_df <- summary_df[order(summary_df$depth, summary_df$cell), ]
  rownames(summary_df) <- NULL
  summary_df
}

mcts_predict_cell <- function(board,player, mcts_results){
  
  depths <- mcts_results[,"depth"]
  
  # Go through each depth
  for (d in depths) {
    # Filter data for current depth
    current_depth <- mcts_results[mcts_results[,"depth"] == d, ,drop=F]
    col <-  function(x) current_depth[,x]
    # Check for wins
    if (any(col("win") > 0)) {
      # Return Cell with maximum wins
      cell <- unname(col("cell")[which.max(col("win"))])
      return(cell)
    }
    # If no wins, check for loses
    if (any(col("lose") > 0)) {
      # Return Cell with minimum loses
      cell <- unname(col("cell")[which.min(col("lose"))])
      return(cell)
    }
  }
  
  # If no win/lose, return first draw 
  cell <- unname(mcts_results[mcts_results[, "draw"] != 0,"cell"][1])
  return(cell)
}
