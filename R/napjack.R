napjack <- function(){
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("paper"),
    shiny::titlePanel("Nap Jack"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Original study
        shiny::fluidRow(
          shiny::column(12, shiny::actionButton("deal_phase", "Deal Phase")),
        ),
        shiny::actionButton("score_game", "Score Game")
      ),

      shiny::mainPanel(
        shiny::uiOutput("card_display"),
      )
    )
  )

  server <- function(input, output, session) {

    # Set up reactive values
    deal_phase_rv <- shiny::reactiveVal(0)
    score_game_rv <- shiny::reactiveVal(NULL)
    game_deck <- shiny::reactiveVal(NULL)
    dealt_cards <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$deal_phase, {

      # Reset reactive values
      score_game_rv(NULL)

      # Get game deck
      if (deal_phase_rv() == 0){
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

        game_deck(original_deck)

      }

      # Deal cards
      card_grid <- deal_phase(game_deck())

      # Update deal phase
      deal_phase_rv(deal_phase_rv() + 1)

      # Return a data frame of dealt cards
      dealts <- data.frame(card = character(),
                           rank = character(),
                           suit = character(),
                           value = numeric(),
                           icard = character(),
                           phase = numeric())

      lapply(1:6, function(x){
        .df <- as.data.frame(card_grid$dealt_cards[[x]])
        .df$phase <- deal_phase_rv()
        dealts <<- rbind(dealts, .df)
      })

      # Rendering the UI for the card grid
      output$card_display <- shiny::renderUI({
        render_card_grid(card_grid$cards_matrix)
      })

      #Update deck
      game_deck(card_grid$updeck)

    })

    # shiny::observeEvent(input$swap_inside_row, {
    #   # Extract the replication card grid from the reactive value
    #   rep_cards <- replication_cards()
    #
    #   # Check for NULL values in user input and exit early if found
    #   if (is.null(input$swap_in_row_num) || is.null(input$swap_in_row_col1) || is.null(input$swap_in_row_col2)) return(NULL)
    #
    #   # Swap within the row using the swapper function
    #   tryCatch({
    #     new_card_grid <- swapper(rep_cards, swap_in_row = c(input$swap_in_row_num, input$swap_in_row_col1, input$swap_in_row_col2))
    #
    #     # Update the reactive value to hold the new card grid
    #     replication_cards(new_card_grid)
    #
    #     # Rerender the UI for the card grid
    #     output$rep_card_display <- shiny::renderUI({
    #       render_card_grid(new_card_grid)
    #     })
    #   }, error = function(e) {
    #     # Handle the error by displaying a message
    #     shiny::showNotification(paste("An error occurred:", e$message), type = "error")
    #   })
    # })
    # # Render the swap controls only when the replication study has been conducted
    # output$swap_controls_ui <- shiny::renderUI({
    #   if (replication_conducted()) {
    #     shiny::fluidRow(
    #       # Third column: Swap Inside Row
    #       shiny::column(4,
    #                     shiny::tagList(
    #                       shiny::numericInput("swap_in_row_num", "Row Number", value = NULL),
    #                       shiny::numericInput("swap_in_row_col1", "Swap Row Column 1", value = NULL),
    #                       shiny::numericInput("swap_in_row_col2", "Swap Row Column 2", value = NULL),
    #                       shiny::actionButton("swap_inside_row", "Execute Inside Row Swap")
    #                     )
    #       )
    #     )
    #   }
    # })

  }

  shiny::shinyApp(ui, server)
}
