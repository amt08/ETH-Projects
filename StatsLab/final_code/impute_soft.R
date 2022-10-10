impute_soft <- function(x, higher_moments = TRUE) {
  
  ## Soft: Only imputing the values for research case IDs that do not have any readings for a
  # certain variable
  ## Higher moments: Imputing higher moment readings, i.e. setting all NAs in columns for
  # the standard deviation, skewness, etc. to 0
  
  a <- unique(x$research_case_id)
  x <- as.data.frame(x)
  
  for (i in a) {
    
    g <- x[x$research_case_id == i, ]
    
    if (all(g[, "has_operation"] == 0)) {
      next
    }
    
    for (j in names(g)) {
      if (j %in% names(g)[names(g) %like% "_n|_sd|_skew|_kurtosis"]) {
        next
      }
      
      if (all(is.na(g[, j]))) {
        g[, j] <-
          rep(ff[ff$Variable == unlist(strsplit(j, "_"))[1], 3], nrow(g))
      }
    }
    x[x$research_case_id == i, ] <- g
  }
  
  ## Optional: Imputing all higher moments: Setting all NAs to 0
  if  (higher_moments) {
    
    for (k in names(x)) {
      if (k %in% names(x)[names(x) %like% "_n|_sd|_skew|_kurtosis"]) {
        x[is.na(x[, k]), k] <- 0
      }
    }
  }
  x <- as_tibble(x)
  return(x)
}