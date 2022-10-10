library('renv')
library('tibble')
library('dplyr')
library('ggplot2')
library('visdat')
library('naniar')
library('lubridate')
library('readxl')
library('tidyr')
library('progress')


surgeries <- read.csv('polytrauma/data/raw_data/DCO_Defsurg.csv', stringsAsFactors = T)

surgeries <- surgeries %>% filter(Intervention %in%  c('DCO', 'DefSurg', 'Hemodynamic_support'))

## first filter the relevant columns

surgeries <- surgeries %>% select(research_case_id, Intervention, Therapy_dat)

# divide in 2 dataframes
dco_def_surgeries <- surgeries %>% filter(Intervention != 'Hemodynamic_support')
hemo_surgeries <- surgeries %>% filter(Intervention == 'Hemodynamic_support')


# solve problem of dco and definitive happening at the same time
new_surgeries <- data.frame()
for(patient in unique(dco_def_surgeries$research_case_id)){
  subset_patient <- subset(dco_def_surgeries, research_case_id == patient)
  
  for(dt in unique(subset_patient$Therapy_dat)){
    date_patient <- subset(subset_patient, Therapy_dat == dt)
    intervention_list <- unique(date_patient$Intervention)
    if(length(intervention_list) == 1){
      # single surgeries
      date_patient['multiple'] <- 0
    }else{
      if('DefSurg' %in% intervention_list){
        date_patient$Intervention <- "DefSurg"
      }
      date_patient['multiple'] <- 1
    }
    new_surgeries <- bind_rows(new_surgeries, date_patient)
  }
}

new_surgeries <- new_surgeries %>% distinct()

# dealing with hemodynamic support

# to be able to merge the 2 dataset
hemo_surgeries$multiple <- 0
hemo_surgeries <- hemo_surgeries %>% distinct()

surgeries_union <- rbind(new_surgeries, hemo_surgeries)


################################################

# cancelling if hemo is after def surgery
ranked_operations <- surgeries_union %>%
  group_by(research_case_id) %>%
  mutate(order = row_number(Therapy_dat)) %>%
  arrange(., research_case_id, order)

op_hemo <- data.frame()

for(patient in unique(ranked_operations$research_case_id)){
  
  subset_patient <- subset(ranked_operations, research_case_id == patient) %>% arrange(., order)
  subset_patient['hemo_is_involved'] <- 0
  order_max <- max(subset_patient$order)
  for(i in c(1:order_max)){
    if(subset_patient[i, 'Intervention'] == 'Hemodynamic_support'){
      if((i-1) > 0){
        subset_patient[i-1, 'hemo_is_involved'] <- 1
      }
    }
    
  }
  
  op_hemo <- bind_rows(op_hemo, subset_patient)
}

# checking if hemodynamic support happened in surgery
hemo_in_surgery <- op_hemo %>%
  group_by(research_case_id, Therapy_dat) %>%
  summarise(n = n()) %>%
  filter(n!= 1)

surgeries_new <- op_hemo

surgeries_new <- surgeries_new %>%
  filter(Intervention != 'Hemodynamic_support')

write.csv(surgeries_new, 'surgeries_new_processed_on_raw.csv', row.names = F)
