impute_hard <- function(x, ICU = FALSE) {
  
  a <- unique(x$research_case_id)
  x <- as.data.frame(x)
  
  for (i in a) {
    g <- x[x$research_case_id == i, ]
    
    for (j in names(g)) {
      if (j %in% names(g)[names(g) %like% "_n|_sd|_skew|_kurtosis"]) {
        g[is.na(g[, j]), j] <- 0
      }
      
      else if (!is.numeric(g[, j])) {
        
      }
      
      else if (all(is.na(g[, j]))) {
        if (ICU) {next}
        g[, j] <- rep(ff[ff$Variable == unlist(strsplit(j, "_"))[1], 3], nrow(g))
      }
      
      else if (sum(!is.na(g[, j])) == 1) {
        g[, j] <- rep(as.numeric(na.omit(unique(g[, j]))), length(g[, j]))
      }
      
      else if (sum(!is.na(g[, j])) > 1) {
        g[, j] <- na_interpolation(g[, j], "stine")
      }
      
      else {
        # left-overs: i.e. 1 data point that is not missing
        
      }
    }
    x[x$research_case_id == i, ] <- g
  }
  x <- as_tibble(x)
  x
}