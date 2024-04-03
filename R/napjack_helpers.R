deal_phase <- function(.deck) {


  # Deal 6 cards from the deck
  dealt_cards <- vector("list", 6)

  for(i in 1:6) {
    .deck <- mmcards::deal_card(.deck)
    dealt_cards[[i]] <- .deck$dealt_card
  }

  # Reshape the data frame to the desired format
  cards_matrix <- matrix(dealt_cards, nrow = 1, ncol = 6, byrow = TRUE)

  # Return results
  return(list(cards_matrix = cards_matrix, updeck = .deck$updated_deck, dealt_cards = dealt_cards))
  #return(cards_matrix = cards_matrix)
}

render_card_grid <- function(new_card_grid) {
  rep_card_images <- unlist(apply(new_card_grid, 1, function(row) sapply(row, function(card) {
    shiny::renderImage({
      list(src = system.file(card$icard, package = "scdtb"), contentType = "image/png", width = 200, height = "auto")
    }, deleteFile = FALSE)
  })))

  rep_matrix_layout <- matrix(rep_card_images, nrow = 1, byrow = TRUE)
  card_ui <- apply(rep_matrix_layout, 1, function(row) {
    shiny::fluidRow(lapply(row, shiny::column, width = floor(12/length(row))))
  })
  return(card_ui)
}

swapper <- function(cards_matrix, swap_in_row = NULL) {

  # Initialize move history
  if (!inherits(cards_matrix, "swapper")) {
    attr(cards_matrix, "swap_in_row_hist") <- 0
  }

  # Swap within row
  if (!is.null(swap_in_row) && length(swap_in_row) == 2) {
    col1 <- swap_in_row[1]
    col2 <- swap_in_row[2]

    if (attr(cards_matrix, "swap_in_row_hist") >= 1) {
      stop("You can't swap within phase more than once.")
    }

    temp <- cards_matrix[1, col1]
    cards_matrix[1, col1] <- cards_matrix[1, col2]
    cards_matrix[1, col2] <- temp

    attr(cards_matrix, "swap_in_row_hist") <- attr(cards_matrix, "swap_in_row1_hist") + 1

  }

  # Update the class
  class(cards_matrix) <- c("swapper", class(cards_matrix))

  return(cards_matrix)
}