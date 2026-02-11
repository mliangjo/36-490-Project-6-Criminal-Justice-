# pspivack
# Feb 11, 2026
# 36490

library(tidyverse)

## Pennsylvania

#Read in each data frame
data_years <- read_csv("./data/Pennsylvania_State_Data/data-2009-2013.csv")
filters <- read_csv("./data/Pennsylvania_State_Data/filters.csv")
locations <- read_csv("./data/Pennsylvania_State_Data/locations.csv")
measures <- read_csv("./data/Pennsylvania_State_Data/measures.csv")

#Left join all of them
data_pa_comb <- data_years %>%
  left_join(x = ., y = filters, by = join_by(x$filter_id == y$id)) %>%
  left_join(x = ., y = locations, by = join_by(x$location_id == y$id)) %>%
  left_join(x = ., y = measures, by = join_by(x$measure_id == y$id))

#Export
write_csv(data_pa_comb, "data/Pennsylvania_comb.csv.gz")

## Oregon (Copy and pasted from PA with tweaks)

#Read in each data frame
data_years <- read_csv("./data/Oregon_State_Data/data-2009-2013-or.csv")
filters <- read_csv("./data/Oregon_State_Data/filters.csv")
locations <- read_csv("./data/Oregon_State_Data/locations-or.csv")
measures <- read_csv("./data/Pennsylvania_State_Data/measures.csv")

#Left join all of them
data_or_comb <- data_years %>%
  left_join(x = ., y = filters, by = join_by(x$filter_id == y$id)) %>%
  left_join(x = ., y = locations, by = join_by(x$location_id == y$id)) %>%
  left_join(x = ., y = measures, by = join_by(x$measure_id == y$id))

#Export
write_csv(data_or_comb, "data/Oregon_comb.csv.gz")

## Florida

#Read in each data frame
data_years <- read_csv("./data/Florida_State_Data/data-2009-2013-fl.csv")
filters <- read_csv("./data/Florida_State_Data/filters.csv")
locations <- read_csv("./data/Florida_State_Data/locations-fl.csv")
measures <- read_csv("./data/Florida_State_Data/measures.csv")

#Left join all of them
data_fl_comb <- data_years %>%
  left_join(x = ., y = filters, by = join_by(x$filter_id == y$id)) %>%
  left_join(x = ., y = locations, by = join_by(x$location_id == y$id)) %>%
  left_join(x = ., y = measures, by = join_by(x$measure_id == y$id))

#Export
write_csv(data_pa_comb, "data/Florida_comb.csv.gz")