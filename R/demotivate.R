#' Generate demotivational quotes for personal use.
#'
#' @description
#' \code{demotivate()} is a tool to demotivate even the highly motivated individual.
#' Use with caution and remember to not take it personally.
#'
#' @param cat a demotivational category to choose from. The function defaults to 'any'.
#' @param seed a numeric or character seed for reproducible results. The function defaults to 'NULL'.
#'
#' @return \code{demotivate()} takes a preferred category and an optional reproducible seed
#' to return a demotivational quote for the user to feel worse at any given time.
#' @export
#'
#' @details
#' When cat is 'any' (default), the function combines all category data frames and then
#' samples from the new,combined data frame.
#'
#' There are a total of six categories: family, life, work, science, TV, and rednecks.
#' When a specific category is chosen the function will sample from a demotivational quote data
#' frame and output a random quote for your spiritual and mental decay.
#'
#' @author JP Monteagudo
#'
#' @examples
#' demotivate("any",seed = "lanky puppy")
#' demotivate("work")

demotivate <- function(cat = "any",
                       seed){

  valid_cats <- c("any", "science",
                  "life", "work",
                  "tv", "rednecks",
                  "family")

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
    all_cats <- rbind(science,life,work,tv,rednecks,family)

    my_quote <- sample(all_cats$values,1)
  }
  if(cat == "science"){
    my_quote <- sample(science$values,1)
  }
  if(cat == "life"){
    my_quote <- sample(life$values,1)
  }
  if(cat == "work"){
    my_quote <- sample(work$values,1)
  }
  if(cat == "tv"){
    my_quote <- sample(tv$values,1)
  }
  if(cat == "rednecks"){
    my_quote <- sample(rednecks$values,1)
  }
  if(cat == "family"){
    my_quote <- sample(family$values,1)
  }
  return(my_quote)
}
