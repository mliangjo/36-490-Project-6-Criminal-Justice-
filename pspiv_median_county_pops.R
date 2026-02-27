library(tidyverse)

fl <- read_csv("data/raw/Florida_State_Data/data-2009-2013-fl.csv")
or <- read_csv("data/raw/Oregon_State_Data/data-2009-2013-or.csv")
pa <- read_csv("data/raw/Pennsylvania_State_Data/data-2009-2013.csv")

fl %>%
  # Measure 501 is population 
  # Location_id = FL gets rid of the statewide data
  filter(measure_id == 501, location_id != "FL") %>%
  summarize(med = median(value))

or %>%
  # Measure 501 is population 
  # Location_id = FL gets rid of the statewide data
  filter(measure_id == 501, location_id != "OR") %>%
  summarize(med = median(value))

pa %>%
  # Measure 501 is population 
  # Location_id = FL gets rid of the statewide data
  filter(measure_id == 501, location_id != "PA") %>%
  summarize(med = median(value))
