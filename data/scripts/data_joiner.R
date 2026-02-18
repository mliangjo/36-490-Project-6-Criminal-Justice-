# pspivack
# Feb 16, 2026
# 36490

library(tidyverse)

# Transforms original MFJ data into combined left joined dataset
# Requires: setwd(path_to_git_folder)

# Original code by me. Also used Claude to transform into function
# Inputs (All strings)
## state_name Pennsylvania
## File Names:
### data_file
### locations_file
### filters_file
### measures_file
# Outputs (df)
## Combined dataset
process_state_data <- function(state_name, data_file, locations_file, 
                               filters_file = "filters.csv", 
                               measures_file = "measures.csv") {
  # Construct file paths 
  data_path <- file.path("./data/raw", paste0(state_name, "_State_Data"), data_file)
  locations_path <- file.path("./data/raw", paste0(state_name, "_State_Data"), locations_file)
  filters_path <- file.path("./data/raw", paste0(state_name, "_State_Data"), filters_file)
  measures_path <- file.path("./data/raw", paste0(state_name, "_State_Data"), measures_file)
  
  # Read in each data frame
  data_years <- read_csv(data_path)
  locations <- read_csv(locations_path)
  filters <- read_csv(filters_path)
  measures <- read_csv(measures_path)
  
  # Left join all of them
  data_comb <- data_years %>%
    left_join(locations, by = join_by(location_id == id)) %>%
    left_join(filters, by = join_by(filter_id == id)) %>%
    left_join(measures, by = join_by(measure_id == id)) %>%
  # Rename appropriate cols as well
    rename(
      location_type = type,
      location_name = name.x,
      measure_name = name.y,
      measure_format = format,
      demographics = description ## This is what encodes lawyer type
    ) %>%
  # Reorder ids to be next to descriptions
    ## Measure filter location
    select(
      measure_id, measure_name, measure_format, #Measures
      filter_id, demographics, #filters
      location_id, location_type, location_name, #locations
      everything() #Everything else
    )
  
  return(data_comb)
}

# Process each state
data_pa_comb <- process_state_data("Pennsylvania", "data-2009-2013.csv", "locations.csv")
data_or_comb <- process_state_data("Oregon", "data-2009-2013-or.csv", "locations-or.csv")
data_fl_comb <- process_state_data("Florida", "data-2009-2013-fl.csv", "locations-fl.csv")

# Export
# write_csv(data_pa_comb, "data/combined/Pennsylvania_comb.csv.gz")
# write_csv(data_or_comb, "data/combined/Oregon_comb.csv.gz")
# write_csv(data_fl_comb, "data/combined/Florida_comb.csv.gz")


###
###

# Transforms the combined dataset into a smaller dataset just with the filters for attorney type
# plus convenience updates

#Only keep filters which differentiate by attorney type
optimize_attorney_type <- function (comb_data){
  opti <- comb_data %>%
    filter(50 <= filter_id & filter_id <= 55) %>% #Keeps only the filters with attorney type
    rename(counsel_type = demographics) %>% #Better title
    mutate(counsel_type = fct_recode(counsel_type, #Shortens data names
    "Court Appointed" = "defendants represented by a court-appointed private attorney",
    "Unknown" = "defendants represented by an attorney of unknown type",
    "Other" = "defendants represented by an attorney of unknown type",
    "Private" = "defendants represented by private attorney",
    "Public Defender" = "defendants represented by public defender",
    "Self-Represented" = "defendants who self-represented"
    ))
}


data_pa_opti <- optimize_attorney_type(data_pa_comb)
data_or_opti <- optimize_attorney_type(data_or_comb)
data_fl_opti <- optimize_attorney_type(data_fl_comb)

#Export
# write_csv(data_pa_opti, "data/clean/PA.csv.gz")
# write_csv(data_or_opti, "data/clean/OR.csv.gz")
# write_csv(data_fl_opti, "data/clean/FL.csv.gz")