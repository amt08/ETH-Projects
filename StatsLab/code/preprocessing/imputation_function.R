impute_DF <- function(x, soft = FALSE, higher_moments = FALSE) {
  
  ## Soft: Only imputing the values for research case IDs that do not have any readings for a
  # certain variable
  ## Higher moments: Imputing higher moment readings, i.e. setting all NAs in columns for
  # the standard deviation, skewness, etc. to 0
  
  a <- unique(x$research_case_id)
  x <- as.data.frame(x)
  
  if (soft) {
    for (i in a) {
      g <- x[x$research_case_id == i,]
      
      for (j in names(g)) {
        if (j %in% names(g)[names(g) %like% "_n|_sd|_skew|_kurtosis"]) {next}
        
        if (all(is.na(g[, j]))) {
          g[, j] <- rep(ff[ff$Variable == unlist(strsplit(j, "_"))[1], 3], nrow(g))
        }
      }
      x[x$research_case_id == i,] <- g
    }
  }
  
  
  else {
    for (k in names(x)) {
      if (k %in% names(x)[names(x) %like% "_n|_sd|_skew|_kurtosis"]) {
        
      }
      
      else if (!is.numeric(x[, k])) {
        
      }
      
      else if ((sum(!is.na(x[, k])) == 0)) {
        x[, k] <- rep(ff[ff$Variable == unlist(strsplit(k, "_"))[1], 3], nrow(x))
      }
      
      else if ((sum(!is.na(x[, k])) == 1)) {
        x[, k] <- rep(as.numeric(na.omit(unique(x[, k]))), length(x[, k]))
      }
      else {
        x[, k] <- na_interpolation(x[, k], "stine")
      }
    }
  }
  
  ## Optional: Imputing all higher moments: Setting all NAs to 0
  if (higher_moments) {
    
    for (k in names(x)) {
      if (k %in% names(x)[names(x) %like% "_n|_sd|_skew|_kurtosis"]) {
        x[is.na(x[, k]), k] <- 0
      }
    }
  }
  x <- as_tibble(x)
  return(x)
}
