#' General motivation quotes for personal use
#'
#' @description
#' \code{motivate()} is a tool to lift up even the unmotivated individual.
#' Use with caution and remember— DO take it personally.
#'
#' @param cat a motivational category to choose from. The function defaults to "any".
#' @param seed a numeric or character seed for reproducible results. The function defaults to "NULL".
#'
#' @return \code{motivate()} takes a preferred category and an optional reproducible seed
#' to return a motivational quote for the user to feel better at any given time.
#' @export
#'
#'
#' @details
#'
#' When cat is 'any' (default), the function combines all category data frames and then
#' samples from the new,combined data frame.
#'
#' There are a total of five categories: psych, stoic, religion, lit(literature), and modern (history)
#' When a specific category is chosen the function will sample from a motivational quote data
#' frame and output a random quote for your spiritual and mental growth.
#'
#' @author JP Monteagudo
#'
#' @examples
#'
#' motivate("any",seed = "lethargic raccoon")
#' motivate("lit")
#' motivate("stoic", seed = 2899)

motivate <- function(cat = "any",
                     seed){

  valid_cats <- c("any","psych",
                  "religion","stoic",
                  "modern","lit")

  cat <- match.arg(cat,
                   valid_cats,
                   several.ok = FALSE)

  if(missing(seed)){

    set.seed(NULL)
  } else {
    if (is.numeric(seed)){
      set.seed(seed)
    }
    seed <- set.char.seed(seed)
    set.seed(seed)
  }

  if(cat == "any"){
    all_cats <- rbind(psych,religion,stoic,modern,lit)

    my_quote <- sample(all_cats$values,1)
  }
  if(cat == "psych"){
    my_quote <- sample(psych$values,1)
  }
  if(cat == "religion"){
    my_quote <- sample(religion$values,1)
  }
  if(cat == "stoic"){
    my_quote <- sample(stoic$values,1)
  }
  if(cat == "modern"){
    my_quote <- sample(modern$values,1)
  }
  if(cat == "lit"){
    my_quote <- sample(lit$values,1)
  }
  return(my_quote)
}
