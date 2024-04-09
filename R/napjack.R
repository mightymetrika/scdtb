napjack <- function(){
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("paper"),
    shiny::titlePanel("Nap Jack"),
    shiny::mainPanel(
      # show cards phase by phase
      shiny::uiOutput("card_display_1"),
      shiny::uiOutput("card_display_2"),
      shiny::uiOutput("card_display_3"),
      shiny::uiOutput("card_display_4"),

      # add swap card functionality beneath each phase
      shiny::uiOutput("swap_controls_ui"),

      # add buttons for dealing cards within phase and scoring game
      shiny::fluidRow(
        shiny::column(12, shiny::actionButton("deal_phase", "Deal Phase")),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::column(12, shiny::actionButton("score_game", "Score Game"))
        ),

      # show results of the game
      shiny::uiOutput("interpretation"),
      shiny::uiOutput("raw_plot_title"),
      shiny::plotOutput("raw_plot_out"),
      shiny::uiOutput("nap_title1"),
      shiny::verbatimTextOutput("nap_out1"),
      shiny::uiOutput("nap_title2"),
      shiny::verbatimTextOutput("nap_out2"),
      shiny::uiOutput("nap_titlerev"),
      shiny::verbatimTextOutput("nap_outrev"),
      shiny::uiOutput("mem_title"),
      shiny::uiOutput("mem_level_title"),
      shiny::plotOutput("mem_plot_out"),
      shiny::verbatimTextOutput("mem_sum"),
      shiny::verbatimTextOutput("mem_out")
      )
    )

  server <- function(input, output, session) {

    # set up reactive values
    deal_phase_rv <- shiny::reactiveVal(0)
    score_game_rv <- shiny::reactiveVal(NULL)
    game_deck <- shiny::reactiveVal(NULL)
    dealt_cards <- shiny::reactiveVal(NULL)
    phase_matrix <- shiny::reactiveVal(NULL)
    phase_open <- shiny::reactiveVal(FALSE)
    game_hand <- shiny::reactiveVal(NULL)
    game_table <- shiny::reactiveVal(NULL)
    point_total <- shiny::reactiveVal(0)

    # handle card dealing within phase
    shiny::observeEvent(input$deal_phase, {

      # reset reactive values
      score_game_rv(NULL)

      # limit number of phases within game
      shiny::req(deal_phase_rv() < 4)

      # get game deck
      if (deal_phase_rv() == 0){

        # use standard deck
        original_deck <- mmcards::i_deck(deck = mmcards::shuffle_deck(),
                        i_path = "www",
                        i_names = c("2_of_clubs", "2_of_diamonds", "2_of_hearts", "2_of_spades",
                                    "3_of_clubs", "3_of_diamonds", "3_of_hearts", "3_of_spades",
                                    "4_of_clubs", "4_of_diamonds", "4_of_hearts", "4_of_spades",
                                    "5_of_clubs", "5_of_diamonds", "5_of_hearts", "5_of_spades",
                                    "6_of_clubs", "6_of_diamonds", "6_of_hearts", "6_of_spades",
                                    "7_of_clubs", "7_of_diamonds", "7_of_hearts", "7_of_spades",
                                    "8_of_clubs", "8_of_diamonds", "8_of_hearts", "8_of_spades",
                                    "9_of_clubs", "9_of_diamonds", "9_of_hearts", "9_of_spades",
                                    "10_of_clubs", "10_of_diamonds", "10_of_hearts", "10_of_spades",
                                    "jack_of_clubs", "jack_of_diamonds", "jack_of_hearts", "jack_of_spades",
                                    "queen_of_clubs", "queen_of_diamonds", "queen_of_hearts", "queen_of_spades",
                                    "king_of_clubs", "king_of_diamonds", "king_of_hearts", "king_of_spades",
                                    "ace_of_clubs", "ace_of_diamonds", "ace_of_hearts", "ace_of_spades"
                        ))

        # add deck of cards to reactive value
        game_deck(original_deck)
      }

      # deal cards within a phase
      card_grid <- deal_phase(game_deck())

      # update phase matrix
      phase_matrix(card_grid$cards_matrix)

      # update phase counter
      deal_phase_rv(deal_phase_rv() + 1)

      # initialize a dataframe to store cards dealt within a phase
      dealts <- data.frame(card = character(),
                           rank = character(),
                           suit = character(),
                           value = numeric(),
                           icard = character(),
                           phase = numeric())

      # add cards from a phase to data frame
      lapply(1:6, function(x){
        .df <- as.data.frame(card_grid$dealt_cards[[x]])
        .df$phase <- deal_phase_rv()
        dealts <<- rbind(dealts, .df)
      })

      # store cards from phase to game_hand reactive value
      game_hand(dealts)

      # append cards from current phase to cards from previous phases
      game_table(rbind(game_table(), game_hand()))

      # render the card_display for each of the four phases
      if (deal_phase_rv() == 1){
        phase_matrix1 <- phase_matrix()
        output$card_display_1 <- shiny::renderUI({
          render_card_grid(phase_matrix1)
        })
      }

      if (deal_phase_rv() == 2){
        phase_matrix2 <- phase_matrix()
        output$card_display_2 <- shiny::renderUI({
          render_card_grid(phase_matrix2)
        })
      }

      if (deal_phase_rv() == 3){
        phase_matrix3 <- phase_matrix()
        output$card_display_3 <- shiny::renderUI({
          render_card_grid(phase_matrix3)
        })

      }

      if (deal_phase_rv() == 4){
        phase_matrix4 <- phase_matrix()
        output$card_display_4 <- shiny::renderUI({
          render_card_grid(phase_matrix4)
        })
      }

      #update deck reactive value
      game_deck(card_grid$updeck)

      # update phase open (to allow swapping cards within row)
      phase_open(TRUE)

    })

    # execute card swap within row functionality
    shiny::observeEvent(input$swap_inside_row, {

      # require that the phase is open
      shiny::req(phase_open())

      # extract current phases replication card grid from the reactive value
      card_matrix <- phase_matrix()

      # check for NULL values in user input and exit early if found
      if (is.na(input$swap_in_row_col1) || is.na(input$swap_in_row_col2))return(NULL)

      # check for out-of-bounds columns and exit early if found
      if(!(input$swap_in_row_col1 %in% 1:6 & input$swap_in_row_col2 %in% 1:6))return(NULL)


      # swap within the row using the swapper function
      tryCatch({

        # use swapper function to swap columns within row for current phase
        new_card_grid <- swapper(card_matrix,
                                 swap_in_row = c(input$swap_in_row_col1,
                                                 input$swap_in_row_col2))

        # update the reactive value to hold the new card grid
        phase_matrix(new_card_grid)

        # re-render the card_display for each of the four phases
        if (deal_phase_rv() == 1){
          phase_matrix1 <- phase_matrix()
          output$card_display_1 <- shiny::renderUI({
            render_card_grid(phase_matrix1)
          })
        }

        if (deal_phase_rv() == 2){
          phase_matrix2 <- phase_matrix()
          output$card_display_2 <- shiny::renderUI({
            render_card_grid(phase_matrix2)
          })
        }

        if (deal_phase_rv() == 3){
          phase_matrix3 <- phase_matrix()
          output$card_display_3 <- shiny::renderUI({
            render_card_grid(phase_matrix3)
          })

        }

        if (deal_phase_rv() == 4){
          phase_matrix4 <- phase_matrix()
          output$card_display_4 <- shiny::renderUI({
            render_card_grid(phase_matrix4)
          })
        }
      }, error = function(e) {
        # Handle the error by displaying a message
        shiny::showNotification(paste("An error occurred:", e$message), type = "error")
      })

      # initialize a dataframe to store cards dealt within a phase
      dealts <- data.frame(card = character(),
                           rank = character(),
                           suit = character(),
                           value = numeric(),
                           icard = character(),
                           phase = numeric())

      # add cards from a phase to data frame
      lapply(1:6, function(x){
        .df <- as.data.frame(new_card_grid[[x]])
        .df$phase <- deal_phase_rv()
        dealts <<- rbind(dealts, .df)
      })

      #update deck reactive value
      game_hand(dealts)

      # update table (i.e. full game) reactive value
      game_table(rbind(utils::head(game_table(), -6), game_hand()))

      # close phase to disallow more than one swap-per-phase
      phase_open(FALSE)
    })

    # render the swap controls while phase is open
    output$swap_controls_ui <- shiny::renderUI({
      if (phase_open()) {
        shiny::fluidRow(
          shiny::column(4,
                        shiny::numericInput("swap_in_row_col1", "Swap Column:", value = NULL)
                        ),
          shiny::column(4,
                        shiny::numericInput("swap_in_row_col2", "with Column:", value = NULL)
                        ),
          shiny::column(4,
                        shiny::actionButton("swap_inside_row", "Execute Inside Row Swap"))

        )
      }
    })

    # score Game
    shiny::observeEvent(input$score_game, {

      # ensure that cards have been dealt for all four phases
      shiny::req(deal_phase_rv() == 4)

      # get the final game table
      final_game_table <- game_table()

      # turn numeric phase variable to a factor
      final_game_table$phase <- factor(final_game_table$phase,
                                       levels = c(1, 2, 3, 4),
                                       labels = c("baseline 1", "treatment 1",
                                                  "baseline 2", "treatment 2"))

      # add a time variable to the data frame
      final_game_table$time <- 1:nrow(final_game_table)

      # Plot Raw Data

      # section title
      output$raw_plot_title <- shiny::renderUI({ shiny::tags$h2("Raw Data Plot") })

      # build and render raw data plot
      output$raw_plot_out <- shiny::renderPlot({
        raw_plot(.df = final_game_table, .out = "value",
                 .time = "time", .phase = "phase",
                 phase_levels = c("baseline 1", "treatment 1",
                                  "baseline 2", "treatment 2"))
        })

      # Run NAP Analysis

      # nap baseline 1

      # run nap analysis
      napo1 <- nap(.df = final_game_table, .y = "value", .phase = "phase",
                   .time = "time", type = "trend", last_m = 3,
                   phases = list("baseline 1"), improvement = "positive")

      # section title
      output$nap_title1 <- shiny::renderUI({ shiny::tags$h2("NAP Baseline 1") })

      # render nap value
      output$nap_out1 <- shiny::renderPrint( { napo1 } )

      # update point total based on nap
      if (napo1 <= 0.85){ point_total(point_total() + 1) }

      # nap baseline 2

      # run nap analysis
      napo2 <- nap(.df = final_game_table, .y = "value", .phase = "phase",
          .time = "time", type = "trend", last_m = 3,
          phases = list("baseline 2"), improvement = "positive")

      # section title
      output$nap_title2 <- shiny::renderUI({ shiny::tags$h2("NAP Baseline 2") })

      # render nap value
      output$nap_out2 <- shiny::renderPrint( { napo2 } )

      # update point total based on nap
      if (napo2 <= 0.85){ point_total(point_total() + 1) }

      # nap reversal

      # run nap analysis
      napor <- nap(.df = final_game_table, .y = "value", .phase = "phase",
                   .time = "time", type = "reversability",
                   phases = list("baseline 1", "baseline 2"), improvement = "positive")

      # section title
      output$nap_titlerev <- shiny::renderUI({ shiny::tags$h2("NAP Reversability") })

      # render nap value
      output$nap_outrev <- shiny::renderPrint( { napor } )

      # update point total based on nap
      if (napor <= 0.85){ point_total(point_total() + 1) }



      # Run Mixed Effects Analysis

      # run mixed effects model
      mem_res <- mixed_model_analysis(.df = final_game_table, .dv = "value",
                                      .time = "time", .phase = "phase",
                                      rev_time_in_phase = TRUE)

      # make 'baseline 2' reference level of phase to obtain other coefficients of
      # interest
      final_game_table$phase <- stats::relevel(final_game_table$phase, "baseline 2")

      # re-rerun mixed effects model 'baseline 2' as reference level for phase
      mem_res2 <- mixed_model_analysis(.df = final_game_table, .dv = "value",
                                      .time = "time", .phase = "phase",
                                      rev_time_in_phase = TRUE)

      # tidy the fitted model
      tidy_b1 <- broom.mixed::tidy(mem_res$fitted_mod, conf.int = TRUE)

      # extract rows that will be used to score game
      tidy_b1 <- tidy_b1[tidy_b1$term %in% c("phasetreatment 1",
                                             "time_in_phase:phasetreatment 1") , ]

      # reverse time:phase interaction to aide with interpretation
      tidy_b1$estimate[[2]] <- -tidy_b1$estimate[[2]]

      # rename terms for interpretability
      tidy_b1$term[[1]]<- "End of Treatment 1 vs End of Baseline 1"
      tidy_b1$term[[2]]<- "Rate of Change Treatment 1 vs Baseline 1"

      # add a reference level label to dataframe for interetability
      tidy_b1$ref <- "baseline 1"

      # update point total based on sign of estimate and p-value
      if (tidy_b1$estimate[[1]] > 0 & tidy_b1$p.value[[1]] < 0.05){
        point_total(point_total() + 1)}

      if (tidy_b1$estimate[[2]] > 0 & tidy_b1$p.value[[2]] < 0.05){
        point_total(point_total() + 1)}

      # tidy the releveled fitted model
      tidy_b2 <- broom.mixed::tidy(mem_res2$fitted_mod, conf.int = TRUE)

      # extract rows that will be used to score the game
      tidy_b2 <- tidy_b2[tidy_b2$term %in% c("phasetreatment 1",
                                             "phasetreatment 2",
                                             "time_in_phase:phasetreatment 1",
                                             "time_in_phase:phasetreatment 2"), ]

      # reverse time:phase interaction to aide with interpretation
      tidy_b2$estimate[[3]] <- -tidy_b2$estimate[[3]]
      tidy_b2$estimate[[4]] <- -tidy_b2$estimate[[4]]

      # rename terms for interpretability
      tidy_b2$term[[1]] <- "End of Treatment 1 vs End of Baseline 2"
      tidy_b2$term[[2]] <- "End of Treatment 2 vs End of Baseline 2"
      tidy_b2$term[[3]] <- "Rate of Change Treatment 1 vs Baseline 2"
      tidy_b2$term[[4]] <- "Rate of Change Treatment 2 vs Baseline 2"

      # add a reference level label to dataframe for interetability
      tidy_b2$ref <- "baseline 2"

      # update point total based on sign of estimate and p-value
      if (tidy_b2$estimate[[1]] > 0 & tidy_b2$p.value[[1]] < 0.05){
        point_total(point_total() + 1)}

      if (tidy_b2$estimate[[2]] > 0 & tidy_b2$p.value[[2]] < 0.05){
        point_total(point_total() + 1)}

      if (tidy_b2$estimate[[3]] > 0 & tidy_b2$p.value[[3]] < 0.05){
        point_total(point_total() + 1)}

      if (tidy_b2$estimate[[4]] > 0 & tidy_b2$p.value[[4]] < 0.05){
        point_total(point_total() + 1)}


      # bind tidy fitted model and tidy releveled fitted model
      tidy_all <- rbind(tidy_b1, tidy_b2)

      # section title
      output$mem_title <- shiny::renderUI({ shiny::tags$h2("Mixed Effects Analysis") })

      # tidied and edited fitted model
      output$mem_sum <- shiny::renderPrint({tidy_all})

      # plot model with data points
      output$mem_plot_out <- shiny::renderPlot({ mem_res$plot })

      # plot summary of fitted model (excluded releveled fitted model)
      output$mem_out <- shiny::renderPrint({ summary(mem_res$fitted_mod) })

      # render final score
      output$interpretation <- shiny::renderUI({
        if (point_total() > 7) {
          shiny::tags$div(
            shiny::tags$h1(paste0("You Win with ", point_total(), "/9 points."), style = "color: green; font-size: 48px;")

          )
        } else {
          shiny::tags$div(
            shiny::tags$h1(paste0("You Lose with ", point_total(), "/9 points"), style = "color: red; font-size: 48px;")
          )
        }
      })

    })
  }

  shiny::shinyApp(ui, server)
}
