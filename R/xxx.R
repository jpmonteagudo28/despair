# CRAN Note avoidance.
# Inspiration: https://github.com/HughParsonage/grattan/blob/master/R/zzz.R
# Also:
if(getRversion() >= "2.15.1")
  utils::globalVariables(
    # underlying data used for behind-the-scenes handsomeness
    c("family", "life", "lit", "modern",
      "psych", "rednecks", "religion", "science", "stoic",
      "tv","work","shake_adjectives","shake_animals",
      "shake_things","shake_jobs","shake_chars","shake_clrs",
      "dsm_5")
  )
