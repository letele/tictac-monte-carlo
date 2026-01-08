
library(shiny)
library(shiny.fluent)
library(shiny.router) 
library(svglite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(irr)      
library(entropy) 


home <-  div(  
  h2( "Dashboard Overview"),
  div( class="flex-col gap-1", style="max-width:800px; margin-top: 1em;",
    
    p("This dashboard explores how ", strong("Pure Monte Carlo (PMC)")," and ",
      strong("Monte Carlo Tree Search (MCTS)")," generate game-state evaluations 
      through self-play."
       
    ),
    
    p("Using datasets produced entirely from simulated play, it analyses and 
    compares the statistical behaviour and structural properties of both 
    algorithms."),
    
    p(
      "This dashboard is part of a broader honours project exploring Monte Carlo methods in game-state evaluation. ",
      br(),"See the ",
      a(
        strong("Project Poster"),
        href = "https://drive.google.com/file/d/1s_nAA3W__txtyHq0PeTgrQhLhIoPx2SJ/view",
        target = "_blank"
      ),
      " for an overview of the project."
    )
    
    
    
  )
  
)

simulationSettingsUI <- function(
    drop_id, drop_value,slider_title,slider_text,
    slider_id, slider_value, slider_min, slider_max, slider_step,
    btn_id 
) {
  div(class = "bd-col1 bg-col1 bxs-1",
      div(class = "flex gap-05 align-c p-05 fs-09",
          Icon(iconName = "Settings", class = "col1"), "Simulation Settings"
      ),
      div(class = "p-075 bg-col2", style="width: 240px;",
          div(class = "flex align-c gap-05 mb-05",
              div("Empty Cells:"),
              Dropdown.shinyInput(
                drop_id,
                options = lapply(1:9, function(i)
                  list(key = i, text = as.character(i))
                ),
                value = drop_value
              )
          ),
          div(
            div(
              slider_title,
              strong(textOutput(slider_text, inline = TRUE))
            ),
            Slider.shinyInput(
              slider_id,
              value = slider_value,
              min = slider_min,
              max = slider_max,
              step = slider_step,
              showValue = FALSE
            ),
            PrimaryButton.shinyInput(
              btn_id,
              text = "\u25B6  \u00A0 Simulate Play",
              style = "width:100%;"
            )
          )
      )
  )
}


methodology <-  div(id="methodology", class="grid-row-auto-1fr h-100p gap-05", 
  div( class="flex gap-15",
    div(class="tab-link", `data-tab` = "terminal", "Terminal State"),
    div(class="tab-link", `data-tab` = "pms", "Pure Monte Carlo"),
    div(class="tab-link", `data-tab` = "mcts", "Monte Carlo Tree Search")
  ),
  
  div(id="terminal", class="tab-content ovy-auto p-05-075", 
      h3("Board Representation", style="margin-bottom: 1em;"),
      
      div(class="flex gap-2",
          div( id="terminal-board",
             tags$style(HTML("
              #terminal-board .board{
                width: 120px;
                height: 120px;
                display:flex;
                flex-wrap: wrap;
              }
              #terminal-board .cell:not(:nth-of-type(3n)){  
                border-right: 2px solid #aaa;
              }
              #terminal-board .cell:nth-of-type(-n + 6){  
                border-bottom: 2px solid #aaa;
              }
               #terminal-board  .cell{
                position: relative;
                font-size:1em;
                font-weight:500;
                display:flex;
                align-items: center;
                justify-content: center;
                width: 33%;
                height: 33%;
            }
          ")),
          h4("Board", style="margin-bottom: 0.5em; text-align:center;"),  
          div( class="board",
            div(class="cell",HTML("$$m_1$$")),
            div(class="cell",HTML("$$m_2$$")),
            div(class="cell",HTML("$$m_3$$")),
            div(class="cell",HTML("$$m_4$$")),
            div(class="cell",HTML("$$m_5$$")),
            div(class="cell",HTML("$$m_6$$")),
            div(class="cell",HTML("$$m_7$$")),
            div(class="cell",HTML("$$m_8$$")),
            div(class="cell",HTML("$$m_9$$")),
          )
        ),    
        div(
          style="height: 100%; align-self: center;",
          tags$img(src="arrow_right.svg", alt="arrow", style="height: 24px;")
        ),    
        div(
          h4("Vector Representation"),
          HTML("$$\\begin{gathered}
                \\mathbf{m} = [m_1 \\quad m_2 \\quad m_3 \\quad m_4 \\quad m_5 \\quad m_6 \\quad m_7 \\quad m_8 \\quad m_9] \\\\
                \\text{where } m_i = \\begin{cases} 0 & \\text{empty cell} \\\\ 1 & \\text{X} \\\\ -1 & \\text{O} \\end{cases}
                \\end{gathered}$$")
        )    
      ),
      
      h3("Terminal State", style="margin: 0.5em 0;"),
      
      p(style=" max-width:750px;","We multiply the board vector ", strong("m"), " 
        by the scoring matrix ", strong("S"), ", 
        which encodes all 8 possible winning lines (3 rows, 3 columns, 2 diagonals). 
        Each column of ", strong("S"), " represents one winning line, summing the values 
        of the three cells in that line. A terminal state occurs when any sum equals 
        3 (X wins), -3 (O wins), or when all 9 cells are filled (draw).
      "),
      div(style=" max-width:750px;",
          HTML("
            $$\\mathbf{m} \\mathbf{S} = [m_1 \\quad m_2 \\quad m_3 \\quad m_4 \\quad m_5 \\quad m_6 \\quad m_7 \\quad m_8 \\quad m_9] 
            \\begin{bmatrix} 
            1 & 0 & 0 & 1 & 0 & 0 & 1 & 0 \\\\ 
            1 & 0 & 0 & 0 & 1 & 0 & 0 & 0 \\\\ 
            1 & 0 & 0 & 0 & 0 & 1 & 0 & 1 \\\\ 
            0 & 1 & 0 & 1 & 0 & 0 & 0 & 0 \\\\ 
            0 & 1 & 0 & 0 & 1 & 0 & 1 & 1 \\\\ 
            0 & 1 & 0 & 0 & 0 & 1 & 0 & 0 \\\\ 
            0 & 0 & 1 & 1 & 0 & 0 & 0 & 1 \\\\ 
            0 & 0 & 1 & 0 & 1 & 0 & 0 & 0 \\\\ 
            0 & 0 & 1 & 0 & 0 & 1 & 1 & 0 
            \\end{bmatrix} = 
            \\begin{bmatrix} 
            m_1 + m_2 + m_3 \\\\ 
            m_4 + m_5 + m_6 \\\\ 
            m_7 + m_8 + m_9 \\\\ 
            m_1 + m_4 + m_7 \\\\ 
            m_2 + m_5 + m_8 \\\\ 
            m_3 + m_6 + m_9 \\\\ 
            m_1 + m_5 + m_9 \\\\ 
            m_3 + m_5 + m_7 
            \\end{bmatrix}$$
        ")
      )
  ),
 
  div(id="pms",  class="tab-content ovy-auto p-05-075", 
    div(class="flex gap-2",
      simulationSettingsUI(
        "pms_cells", 5,"Number of Simulations:", "n_sim_text","n_simulations", 
        1000, 1, 1000, 1, "pms_btn"
      ),
      div(class="flex align-c gap-2",
        div(id="pmc-init-board"),
        div(id="pmc-arrow"),
        div(id="pmc-final-board")
      ),
    ),
    div(
      h4(id="pmc-heading",style="margin: 1em 0 0.5em 0;"),
      div( class="flex gap-2",
        div(id="pmc-table"),
        div(id="pmc-board", class="align-s-c"),
      )
    )
  ),
  div(id="mcts", class="tab-content ovy-auto p-05-075 grid-row-auto-1fr h-100p",  
      div(class="flex gap-2", 
        simulationSettingsUI(
          "mcts_cells", 5,"Alpha rate:", "alpha_text","alpha_rate",
          1, 0.01, 1, 0.01, "mcts_btn"
        ),
        div(class="flex align-c gap-2",
          div(id="mcts-init-board"),
          div(id="mcts-arrow"),
          div(id="mcts-final-board")
        )
      ),
      div(class="grid-row-auto-1fr h-100p ov-hidden",
        h4(id="mcts-heading",style="margin: 1em 0 0.5em 0;"),
        div(class="flex gap-2 h-100p ov-hidden",
          div(id="mcts-table",class="grid-row-auto-1fr h-100p ovy-auto"),
          div(id="mcts-probabilities", class="h-100p" ),
          div(id="game-tree",class="h-100p")
        )
      )
    
  )
)

Analysis <- div(id="analysis", class="grid-row-auto-1fr h-100p gap-05",
   div( class="flex gap-15",
        div(class="tab-link", `data-tab` = "dataset", "Dataset"),
        div(class="tab-link", `data-tab` = "stats", "Statistics")
   ),
   div(id="dataset", class="tab-content flex gap-15 ovy-auto",
       div(class="grid-row-1fr-auto ovy-auto w-fcontent", style="border: 1px solid #8f8f8fff;",
          div(id="board-dataset",class="grid-row-auto-1fr ovy-auto"),
          div(
            class = "por flex align-c jus-btwn",
            style = "border-top: 1px solid #8f8f8fff; padding: 0.2em 0.7em;",

            div(id="total-rows"),

            div(
              id = "downloadBtn",
              style = "
                border: 1px solid #8f8f8fff; padding: 0.3em; cursor:pointer;
                font-size:12px; border-radius:3px;
              ",
              "Download Dataset"
            ),

            div(
              id = "downloadMenu",
              class = "poa flex-col gap-05",
              style = "background:white; bottom:100%; right:0;
             padding:0.5em; border:1px solid #8f8f8fff; width: 150px;
              border-right:none;
              ",

              tags$label( class="flex align-c gap-05",
                tags$input(
                  type = "checkbox",id = "check_boards",checked = TRUE,disabled = TRUE
                ),
                "Boards"
              ),

              tags$label(class="flex align-c gap-05",
                tags$input(type = "checkbox",id = "check_pms"),"PMS"
                ),
              tags$label(class="flex align-c gap-05",
                tags$input(type = "checkbox",id = "check_mcts"),"MCTS"
                ),

              downloadButton(
                outputId = "download_data",
                label = "Download",
                class = "btn btn-primary",
                style = "width:100%; background:#0078D4; color: #fff; padding:0.2em 0.5em;
                  text-align:center; border-radius:3px; font-weight: 600;
                "
              )
            )
          )
       ),
       div( class="flex-col gap-05",
         div(id="player_move"),
         div(id="pmc_value"),
         div(id="mcts_value"),
         div(id="board_view")
       )

    ),
   div(id="stats", class="tab-content ovy-auto"),
) 

navigation <- div( class="grid-nav",
  Nav(
    groups = list(
      list(links = list(
        list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
        list(name = 'Methodology', url = '#!/methodology', key = 'methodology', icon = 'DocumentSearch'),
        list(name = 'Analysis', url = '#!/analysis', key = 'analysis', icon = 'Table')
      ))
    )
  )
)

header <- div( id="header", class="grid-header flex jus-btwn p-05-5",
   h2(class="flex align-c fs-12 fw-600",
     div(
       span(style="color:#7a3cff;","Tic"),
       span(style="color:#f4a000;"," Tac "),
       span(style="color:#4b5563;","Monte Carlo"),
     )
  ),
   div( class="flex align-c gap-1",
        Link( href = "https://github.com/letele/tictac-monte-carlo", target = "_blank", icon("github"), class="fs-12", style="color:#333;"),
        Link(href = "https://letele.github.io/portfolio/", target = "_blank", icon("globe"), class="fs-12", style="color:#333;")
   )
)

router <- div(class="grid-main", router_ui(
    route("/", home),
    route("methodology", methodology),
    route("analysis", Analysis)
  )
)


ui <- fluentPage(
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet", type = "text/css")
  ),
  div(id="shiny-app",
      header,
      navigation,
      router
  ),
  withMathJax(),
  
  tags$script(src = "script.js")
)


# Static Data analysis
pmc_mcts_df <- boards_data[, c("PMC", "MCTS")]

data_long <- pmc_mcts_df %>%
  pivot_longer( cols = everything(),names_to = "Algorithm",values_to = "Cell"
)
counts <- data_long %>% count(Algorithm, Cell)

freq_plot <- ggplot(counts, aes(x = factor(Cell), y = n, fill = Algorithm)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_manual(values = c("#fc5e5e", "#4eb8f1ff")) +
  labs(x = "Cell Number",y = "Count") +
  theme_bw()

conf_matrix <- as.data.frame(table(pmc_mcts_df$PMC, pmc_mcts_df$MCTS))
colnames(conf_matrix) <- c("PMC", "MCTS", "Freq")

heatmap <- ggplot(conf_matrix, aes(x = PMC, y = MCTS, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "#f7fbff", high = "hotpink") +
  labs(x = "PMC Predicted Cell", y = "MCTS Predicted Cell") +
  theme_bw()


# Calculate agreement
df_summary <- pmc_mcts_df %>%
  mutate(Agreement = ifelse(.[,1] == .[,2], "Agree", "Disagree")) %>%
  summarise(
    Count = n(),
    Agree = sum(Agreement == "Agree"),
    Disagree = sum(Agreement == "Disagree"),
    Agreement_Rate = Agree / Count,
    Disagreement_Rate = Disagree / Count
  )

df_table <- tibble(
  outcome = c("agree", "disagree"),
  count = c(df_summary$Agree, df_summary$Disagree),
  rate = round(
    c(df_summary$Agreement_Rate, df_summary$Disagreement_Rate)*100,1)
)



# ---- Cohen's Kappa ----
kappa_res <- kappa2(pmc_mcts_df)
kappa_value <- kappa_res$value

# ---- Chi-squared test ----
conf_matrix <- table(pmc_mcts_df$PMC, pmc_mcts_df$MCTS)

chi_res <- suppressWarnings(chisq.test(conf_matrix))
  
# ---- Entropy ----
# Marginal entropy of PMC
pmc_entropy <- entropy(table(pmc_mcts_df$PMC), unit = "log2")
# Marginal entropy of MCTS
mcts_entropy <- entropy(table(pmc_mcts_df$MCTS), unit = "log2")
# Joint entropy (PMC,MCTS)
joint_entropy <- entropy(conf_matrix, unit = "log2")

# ---- Summary table ----
summary_table <- tibble(
  statistic = c(
    "Cohen's Kappa",
    "Chi-squared Statistic",
    "Chi-squared p-value",
    "PMC Entropy",
    "MCTS Entropy",
    "Joint Entropy"
  ),
  value = c(
    round(kappa_value, 3),
    round(chi_res$statistic, 0),
    chi_res$p.value,
    round(pmc_entropy, 3),
    round(mcts_entropy, 3),
    round(joint_entropy, 3)
  )
)


simplify_fraction <- function(numerator, denominator) {
  gcd <- function(a, b) if(b == 0) a else gcd(b, a %% b)
  d <- gcd(numerator, denominator)
  c(numerator / d, denominator / d)
}

server <- function(input, output, session) {
  router_server()

  # Reactive value to store simulation results
  simulation_results <- reactiveVal(NULL)
  
  # ==========================================
  # ========= Pure Monte Carlo ===============
  # ==========================================
  
  output$n_sim_text <- renderText({input$n_simulations})
  
  
  # Reactive value to store the current board
  pms_board <- reactiveVal(rep(0, 9))
  
  # Update board when moves_left changes
  observeEvent(input$pms_cells, {
    pms_cells <- as.numeric(input$pms_cells)
    
    # Reset simulation results when board changes
    simulation_results(NULL)
    
    if (pms_cells == 9) {
      # Empty board
      pms_board(rep(0, 9))
    } else {
      # Generate random non-terminal board with specified empty spaces
      new_board <- generate_random_board(pms_cells)
      if (!is.null(new_board)) {
        pms_board(new_board)
      }
    }
    
    session$sendCustomMessage("boardState", list(
      board = pms_board()
    ))
  })
  
  # Handle simulate button click
  observeEvent(input$pms_btn, {
    req(input$n_simulations)
    
    # Get current settings
    board <- pms_board()
    n_sims <- as.numeric(input$n_simulations)
    
    # Show notification
    showNotification("Running simulations...", type = "message", duration = 2)
    
    # Run simulations (generate frequencies)
    tryCatch({
      # player assumes x plays first
      player <- ifelse(sum(board != 0) %% 2 != 0, -1,1)
      results <- pmc_results(board, player, Nsims = n_sims)
      cell <- pmc_cell_prediction(board,player,results)
      simulation_results(list(
        board = board,
        player = player,
        pmcResults = round(results,3),
        cell = cell,
        newBoard = replace(board, cell ,player)
        
      ))
      showNotification("Simulation complete!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
   
    session$sendCustomMessage("pmsResults",  simulation_results())

  })
  
  # ==========================================
  # ========= Monte Carlo Tree Search ========
  # ==========================================
  
  mcts_board <- reactiveVal(rep(0, 9))
  output$alpha_text <- renderText({input$alpha_rate})
  
  # Update board when moves_left changes
  observeEvent(input$mcts_cells, {
    mcts_cells <- as.numeric(input$mcts_cells)
    
    # Reset simulation results when board changes
    simulation_results(NULL)
    
    if (mcts_cells == 9) {
      # Empty board
      mcts_board(rep(0, 9))
    } else {
      # Generate random non-terminal board with specified empty spaces
      new_board <- generate_random_board(mcts_cells)
      if (!is.null(new_board)) {
        mcts_board(new_board)
      }
    }
    
    session$sendCustomMessage("mctsBoardState", list(
      board = mcts_board()
    ))
  })
  
  # Handle simulate button click
  observeEvent(input$mcts_btn, {
    req(input$alpha_rate)
    
    # Get current settings
    board <- mcts_board()
    alpha_rate <- as.numeric(input$alpha_rate)
    mcts_cells <- as.numeric(input$mcts_cells)
    player <- ifelse(sum(board != 0) %% 2 != 0, -1, 1)
    
    # Show notification
    showNotification("Running simulations...", type = "message", duration = 2)
    
    # Run simulations
    tryCatch({
      if (mcts_cells %in% 3:6) {
        # Full tree search for smaller boards
        tree_data <- monte_carlo_tree_search(board, player, alpha_rate)
        mcts_results <- mcts_df(tree_data$stats)
        svg_string <- svgstring(width = 3, height = 3)
        print(plot_tree(tree_data))  
        tree_plot <- svg_string()
        dev.off()
        
      } else {
        # Stats-only approach for larger boards
        mcts_results <- mcts_df(mcts_stats(board, player, alpha_rate))
        tree_plot <- NULL
      }
      
      prediction <- mcts_predict_cell(board, player, mcts_results)
      
      s <- colSums(mcts_results[c("win", "lose", "draw")])
      total <-  sum(s)
      
      simulation_results(list(
        player = player,
        prediction = prediction,
        newBoard = replace(board,prediction,player),
        mctsResults = as.data.frame(mcts_results),
        probabilities = lapply(s[s > 0], function(x) simplify_fraction(x, total)),
        treePlot = tree_plot
      ))
      
      showNotification("Simulation complete!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
    
    session$sendCustomMessage("mctsResults", simulation_results())
  })
  
  # ==========================
  # ========= Dataset ========
  # ==========================
  
  observeEvent(get_page(), {
    if (get_page() != "analysis") return()
    
    svg_string <- svgstring(width =  7, height = 4)
    print(freq_plot)
    freq_plot_svg <- svg_string()
    dev.off()

    svg_string <- svgstring(width =  7, height = 5)
    print(heatmap)
    heatmap_svg <- svg_string()
    dev.off()

    session$sendCustomMessage("boardsData", list(
      boardsData = boards_data,
      freqPlot = freq_plot_svg,
      heatMap = heatmap_svg,
      agreeTable = df_table,
      statsSummary = summary_table
    ))
  })
  
  
  output$download_data <- downloadHandler(
    filename = "boards.csv",
    
    content = function(file) {
      
      mat <- boards_data
      
      keep_cols <- as.character(1:9)
      
      if (isTRUE(input$check_pms)) keep_cols <- c(keep_cols, "PMC")
      
      if (isTRUE(input$check_mcts)) keep_cols <- c(keep_cols, "MCTS")
      
      mat_out <- mat[, keep_cols, drop = FALSE]
      
      write.csv(mat_out, file, row.names = FALSE)
    }
  )
  

}

shinyApp(ui, server)







