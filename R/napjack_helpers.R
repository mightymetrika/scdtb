#' Deal a Phase of Cards
#'
#' This function deals a phase of cards from a given deck, consisting of 6 cards.
#' It returns a list containing the dealt cards as a matrix, the updated deck,
#' and the individual dealt cards.
#'
#' @param .deck A deck of cards created using the `mmcards` package.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{cards_matrix}{A matrix of the dealt cards, with 1 row and 6 columns.}
#'   \item{updeck}{The updated deck after dealing the cards.}
#'   \item{dealt_cards}{A list of the individual dealt cards.}
#' }
#'
#' @keywords internal
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
}

#' Render a Grid of Card Images
#'
#' This function takes a matrix of card objects and renders them as a grid of
#' card images using Shiny's `renderImage` function. The resulting grid is
#' returned as a Shiny UI component.
#'
#' @param new_card_grid A matrix of card objects, where each cell contains a
#'   card object with an `icard` property specifying the path to the card image.
#'
#' @return A Shiny UI component representing the grid of rendered card images.
#'
#' @details The function applies `renderImage` to each card object in the matrix,
#'   creating a grid of card images. The images are displayed in a fluid row
#'   layout, with each image occupying an equal portion of the available width.
#'
#' @keywords internal
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

#' Swap Cards Within a Row of a Card Matrix
#'
#' This function allows swapping of cards within a row of a card matrix. It takes a
#' card matrix and the indices of the columns to swap. The function modifies the
#' card matrix in-place and returns the updated matrix.
#'
#' @param cards_matrix A matrix of cards representing the current state of the game.
#' @param swap_in_row A vector of length 2 specifying the indices of the columns to
#'   swap within a row. If `NULL` (default), no swapping is performed.
#'
#' @return The updated card matrix with the specified cards swapped within a row.
#'
#' @details The function performs the following steps:
#'   1. Initializes a move history attribute if it doesn't exist.
#'   2. Checks if `swap_in_row` is provided and has a length of 2.
#'   3. Verifies that swapping within a phase has not been performed more than once.
#'   4. Swaps the cards at the specified column indices within the first row of the matrix.
#'   5. Updates the move history attribute.
#'   6. Adds the "swapper" class to the card matrix.
#'
#' @note The function modifies the card matrix in-place and returns the updated matrix.
#'
#' @keywords internal
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
