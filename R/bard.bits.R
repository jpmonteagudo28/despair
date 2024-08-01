bard.bits <- functions(cat,
                       seed = NULL){
  valid_cats <- c("any","character",
                  "jobs","alliterate")

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

  if(cat == "any"){}

  if(cat = "character"){}

  if(cat = "jobs"){}

  if(cat = "alliterate"){}

  return(my_bit)
}
