#pspiv 
#3/26/26

# Goal: To create a data file with each row representing a location
# and the cols representing conviction rates per attorney type
# and county characteristics

library(tidyverse)

widen_data_all <- function(data, measures) {
  conv_rate <- data %>%
    filter(measure_id == 120, #Conviction Rate Measure
           50 <= filter_id, filter_id <= 55) %>% #Filters for counsel type
    select(filter_id, location_id, value) %>% #Filter = Attorney Type, #Value = Conv Rate
    rename(counsel_type = filter_id,
           conviction_rate = value) %>%
    mutate(counsel_type = case_match(counsel_type, #Give good names to counsel type
                                     53 ~ "CR Court Appointed",
                                     55 ~ "CR Unknown",
                                     54 ~ "CR Other",
                                     51 ~ "CR Private",
                                     52 ~ "CR Public Defender",
                                     50 ~ "CR Self-Represented")) %>%
    #Pivots from (loc, attorney type) rows to loc rows with attorney type cols
    pivot_wider(names_from = counsel_type, values_from = conviction_rate)
  
  county_chars <- data %>%
    filter(measure_id %in% c(501, 504, 505, 511, 2511, #Population
                             514, 516, #Demographics
                             508, 512, 525, #Economics
                             527, 532, 534, 537, 548)) %>% #Crim Justice
    left_join(select(measures, name, id), join_by(measure_id == id)) %>% #Adds the names of the measures (eg. "Population" not 501)
    select(name, location_id, value) %>%
    pivot_wider(names_from = name, values_from = value)
  
  return(left_join(conv_rate, county_chars, by = join_by(location_id)))
}

pa_data <- read_csv("../raw/Pennsylvania_State_Data/data-2009-2013.csv")
pa_measures <- read_csv("../raw/Pennsylvania_State_Data/measures.csv")

fl_data <- read_csv("../raw/Florida_State_Data/data-2009-2013-fl.csv")
fl_measures <- read_csv("../raw/Florida_State_Data/measures.csv")

or_data <- read_csv("../raw/Oregon_State_Data/data-2009-2013-or.csv")
or_measures <- read_csv("../raw/Oregon_State_Data/measures.csv")


pa <- widen_data_all(pa_data, pa_measures)
fl <- widen_data_all(fl_data, fl_measures)
or <- widen_data_all(or_data, or_measures)

write_csv(pa, "../regr/PA.csv")
write_csv(fl, "../regr/FL.csv")
write_csv(or, "../regr/OR.csv")
