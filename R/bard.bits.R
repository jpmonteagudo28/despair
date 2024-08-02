#' Generate unique bard bits using Shakespeare's work and DSM-5 adjectives
#'
#' @description
#' \code{bard.bits()} generates bard bits using Shakespeare's works and the DSM-5 to use in personal projects. The term bard–bits comes from Shakespeare's title
#' 'The Bard of Avon' and the bits ( Shakespeare's characters, places, professions) used to come up with coherent combinations.
#'
#' @details
#' When the category is "any" (the default), the function combines Shakespeare's adjectives,
#' DSM-5 adjectives, colors and the characters, jobs, animals and things data frames
#' to then sample one adjective and one noun from each of the two combinations.
#'
#' When the category is "alliterate", the function combines the adjectives and colors
#' data frames and the animals and characters data frame. After an adjective is selected,
#' the first letter will be used to match against a noun.
#'
#' When the category is "character", the function will derive a bard bit from a data frame of Shakespeare's
#' characters and the adjectives data frame.
#'
#' When the category is "jobs", the function will derive a bard bit from the adjectives and colors data frame
#' and sample one value from the Shakespeare's jobs data frame.
#'
#' When the category is "dsm_5", the function will derive a bard bit from the combined jobs,
#' characters, animals and the DSM-5 adjectives data frame.
#'
#' @param cat a category to be used as bard–bit. Defaults to "any", but "character",
#' "jobs","alliterate", "dsm_5" are available.
#' @param seed an optional numeric or character seed for reproducible results.
#'
#' @return \code{bard.bits()} takes a category and an optional numeric or character seed
#' to produce a bard–bit.
#' @export
#'
#' @author JP Monteagudo
#'
#' @examples
#' bard.bits("any")
#' bard.bits("dsm_5", seed = 1234)
#' bard.bits("jobs", seed = "horrid antonio")
#'
bard.bits <- function(cat,
                       seed = NULL){

  valid_cats <- c("any","character",
                  "jobs","alliterate",
                  "dsm_5")

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
    adjs <- rbind(shake_adjectives,shake_clrs, dsm_5)
    one_adj <- sample(unique(adjs$values),1)

    nouns <- rbind(shake_animals,shake_jobs,shake_things,shake_chars)
    one_noun <- sample(unique(nouns$values),1)

    my_bit <- paste(one_adj,one_noun)
  }

  if(cat == "character"){
    one_adj <- sample(shake_adjectives$values,1)

    one_noun <- sample(shake_chars$values,1)

    my_bit <- paste(one_adj, one_noun)
  }

  if(cat == "jobs"){
    adjs <- rbind(shake_adjectives,shake_clrs)
    one_adj <- sample(unique(adjs$values),1)

    one_noun <- sample(shake_jobs$values,1)

    my_bit<- paste(one_adj,one_noun)
  }

  if(cat == "alliterate"){
    adjs <- rbind(shake_adjectives, shake_clrs)
    adjs <- subset(adjs, sapply(strsplit(adjs$values, " "), length) == 1)

    nouns <-rbind(shake_animals, shake_chars)
    one_noun <- sample(unique(nouns$values),1)
    one_st <- substring(one_noun,1,1)

    letter_match <- subset(adjs,substr(adjs$values,1,1)== one_st)
    one_adj <- sample(letter_match$values,1)

    my_bit <- paste(one_adj,one_noun)
  }

  if(cat == "dsm_5"){
    nouns <- rbind(shake_jobs,shake_chars,shake_animals)
    one_noun <- sample(unique(nouns$values),1)

    one_adj <- sample(dsm_5$values,1)

    my_bit <- paste(one_adj,one_noun)
  }

  return(my_bit)
}
