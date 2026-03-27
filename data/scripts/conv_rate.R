library(tidyverse)

pa_comb <- read_csv("../combined/Pennsylvania_comb.csv.gz")
pa_filters <- read_csv("../raw/Pennsylvania_State_Data/filters.csv")
pa_measures <- read_csv("../raw/Pennsylvania_State_Data/measures.csv")
pa_loc <- read_csv("../raw/Pennsylvania_State_Data/locations.csv")
pa_data <- read_csv("../raw/Pennsylvania_State_Data/data-2009-2013.csv")

fu <- pa_data %>%
  filter(measure_id == 501) %>%
  head

table(pa_data$measure_id)

# Conviction Rate = Measure id 120
# Filters 50-55 are attorney types

library(tidyverse)

# 
conviction_data <- pa_data %>%
  filter(measure_id == 120, filter_id %in% 50:55)

# Pull county characteristics
county_characteristics <- pa_data %>%
  filter(measure_id > 500, filter_id == 1) %>%
  rename(
    characteristic_id    = measure_id,
    characteristic_value = value
  )

fu <- pa_data %>%
  filter(measure_id > 500)

# Join characteristics onto conviction data by location
pa_char <- conviction_data |>
  left_join(
    county_characteristics,
    by = "location_id",
    relationship = "many-to-many"
  )