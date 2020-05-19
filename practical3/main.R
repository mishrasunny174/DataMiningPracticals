setwd("../practical3/")
df <- read.csv(file = "wine.csv", sep = ";")

isNormalized <- function(df) {
  normalized <- TRUE
  for(i in 1:length(df)) {
    if (floor(mean(df[, i])) != 0 && sd(df[, i]) !=1) {
      normalized <- FALSE
    }
  }
  return(normalized)
}

normalizeTransformation <- function(x) {
  return ((x-mean(x))/sd(x))
}

if(isNormalized(df[, -12])) {
  cat("Dataset is normalized")
} else {
  cat("Dataset is not normalized")
  cat("\nNormalizing now")
  df.normalized <- data.frame(sapply(df[, -12], normalizeTransformation))
  df.normalized$quality <- df[, 12]
  if(isNormalized(df.normalized[, -12])) {
    cat("\nDataset is now normalized")
  } else {
    cat("\nNormalization failed")
  }
}


