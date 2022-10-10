# helper functions ---------
library(dplyr)
library(readr)

if (exists("E_Mapping_AIS")) {
  mapping <- E_Mapping_AIS
  } else {mapping <- read_csv("E_Mapping_AIS.csv")}

# ensure that all IDS are appropriate
case_id <- case_id <- mapping %>%
  select(research_case_id) %>%
  unique() %>%
  unlist() # 4692 research case IDs of severe trauma patients

filter_dataset <- function(data, ids = case_id){
  filtered <- data %>%
    filter(research_case_id %in% ids) %>%
    as_tibble()
  
  filtered
}

join_data <- function(d1, d2){
  merged <- d1 %>%
    left_join(d2, by = c('research_case_id')) %>%
    as_tibble()
  
  merged
}


# function to eliminate the variables that have missing values above a certain threshold in a dataset

eliminate_columns_with_missing <- function(data, threshold){
  
  # create copy of the incoming dataset
  data_replace_inf <- data
  
  # replace infinity with NA
  data_replace_inf[mapply(is.infinite, data_replace_inf)] <- NA
  
  columns_list <- apply(data_replace_inf, 2, function(x) {sum(is.na(x)) / length(x)})
  col_names <- names(columns_list)
  
  # extract column names with missing values above threshold
  list_to_eliminate <- col_names[columns_list > threshold]
  cat("The columns that need eliminating are: ", list_to_eliminate, fill = TRUE)
  
  # filter original dataset
  output <- data %>% select(-list_to_eliminate)
  
  return(output)
}

# example running the function eliminate_columns_with_missing(vitals_final, 0.6)



