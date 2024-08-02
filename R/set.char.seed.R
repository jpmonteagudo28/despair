# Code for this function modified from package `codename` by Steven Miller under license GPL -2.

#' Convert any character vector to an integer for setting a reproducible seed
#'
#' @description
#' \code{set.char.seed()} converts a character vector into an integer used as reproducible
#' seed
#'
#' @details
#' Each letter and number in the character vector is assigned a specific number. These
#' numbers are concatenated and divided by 2^30 - 1, and the remainder of this division
#' is used as the numeric reproducible seed.
#'
#' If you get a warning about 'loss of accuracy", consider using a more succinct character
#' vector, otherwise you will get a seed of 0.
#'
#' @param char a character vector
#' @return \code{set.char.seed()} takes a character vector and returns a reproducible seed.
#' @export
#'
#' @author JP Monteagudo
#'
#' @keywords internals
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
  split_it <- temp[ strsplit(char,'')[[1]] ]
  split_it

  num_seed <- as.numeric(paste(split_it, collapse=""))
  num_seed

  seed <- as.integer( num_seed %% (2^30-1) )
  return(seed)
}

