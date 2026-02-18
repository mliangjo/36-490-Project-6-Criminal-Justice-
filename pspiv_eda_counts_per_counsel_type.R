library(tidyverse)

# Determines which measures are common across all three datasets
# Creates bar plots which shows total number of cases (y) per counsel type (x)
# shaded by value (percentage of measure meeting criteria for PERCENTAGE, 
# median value for MEDIAN)

# Load data, keeping only state aggregates
pa <- read_csv("data/clean/PA.csv.gz") %>%
  dplyr::filter(location_type == "state")
or <- read_csv("data/clean/OR.csv.gz") %>%
  dplyr::filter(location_type == "state")
fl <- read_csv("data/clean/FL.csv.gz") %>%
  dplyr::filter(location_type == "state")

#Normal cleaning. PA format is eg. "MEDIAN" and OR/FL is "median"
or$measure_format <- str_to_upper(or$measure_format)
fl$measure_format <- str_to_upper(fl$measure_format)

# Create list of distinct measure names for each state
pa_measures <- pa %>% distinct(measure_name, measure_id, measure_format) %>%
  mutate(state = "PA")
or_measures <- or %>% distinct(measure_name, measure_id, measure_format) %>%
  mutate(state = "OR")
fl_measures <- fl %>% distinct(measure_name, measure_id, measure_format) %>%
  mutate(state = "FL")

#Creates nx6 matrix with the cols measure name, id, format, and then each state:
#TRUE in each cell if the measure is recorded for that state, NA o/w
all_measures <- bind_rows(pa_measures, or_measures, fl_measures) %>%
  mutate(present = T) %>%
  pivot_wider(names_from = state, values_from = present)

# Returns tibble of measures with data in all 3 states, by output type
# There's 8 "percentage" measures with data in all states
# There's 4 "median" measures with data in all states
all_measures %>% filter(PA, OR, FL, measure_format == "PERCENTAGE")
all_measures %>% filter(PA, OR, FL, measure_format == "MEDIAN")

# Function to plot count of PERCENTAGE measures per counsel type, 
# shaded by proportion of cases meeting measure criteria

# Inputs
## state: Correspondings to dataset names above
## m_id: Needs to match a measure id in the dat
## m_Name: Purely aesthetic for title of bar plot
bar_plot_percent_measures <- function(state, m_id, m_name){
  state %>%
    filter(measure_id == m_id) %>%
    ggplot(aes(x = counsel_type, y = denominator, fill = value)) +
    geom_col() +
    geom_text(aes(label = denominator, vjust = -0.5)) +
    labs(title = paste0("Barplot of ", m_name, " Shaded by Proportion of Measure Meeting Criteria"),
         x = "Counsel Type", y = "Total Number of Cases for Measure", 
         fill = "Prop of Measure Meeting Criteria")
}
  
# bar_plot_percent_measures(pa, 17, "Felony Cases 180 Days")
# bar_plot_percent_measures(pa, 18, "Misdem Cases 90 Days")


# Same code as above, labels and plot labels adjusted for median data
bar_plot_median_measures <- function(state, m_id, m_name){
  state %>%
    filter(measure_id == m_id) %>%
    ggplot(aes(x = counsel_type, y = count, fill = value)) +
    geom_col() +
    geom_text(aes(label = count, vjust = -0.5)) +
    labs(title = paste0("Barplot of ", m_name, " Shaded by Median Value"),
         x = "Counsel Type", y = "Total Number of Cases for Measure", 
         fill = "Median Value")
}

# bar_plot_median_measures(pa, 104, "time to disp felony")
# bar_plot_median_measures(pa, 105, "time to disp misdem")
