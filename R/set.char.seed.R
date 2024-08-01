#' Convert any character vector to an integer for setting a reproducible seed
#'
#' @description
#' \code{set.char.seed()} converts a character vector into an integer used as reproducible
#' seed
#'
#' @details
#' Each letter and number in the character vector is assigned a specific number. These
#' numbers are concatenated and divided by 2^39 - 1, and the remainder of this division
#' is used as the numeric reproducible seed.
#'
#' If you get a warning about 'loss of accuracy", consider using a more succint character
#' vector, otherwise you may get a seed of 0.
#'
#' @param char a character vector
#' @return \code{set.char.seed()} takes a character vector and returns a reproducible seed.
#' @export
#'
#' @author JP Monteagudo
#'
#'
#' @examples
#' set.char.seed("The Sticky Chicken")
#' set.char.seed("lanky tadpole")
#' set.char.seed("Manny said what?")
#'
#'
set.char.seed <- function(char) {
  temp <- c(1:26, 1:26, 0:9)
  names(temp) <- c(LETTERS, letters, 0:9)

  char <- gsub("[^0-9a-zA-Z]","",as.character(char))
  char_split <- temp[ strsplit(char,'')[[1]] ]
  char_split

  num_seed <- as.numeric(paste(char_split, collapse=""))
  num_seed

  seed <- as.integer( num_seed %% (2^31-1) )
  return(seed)

}

