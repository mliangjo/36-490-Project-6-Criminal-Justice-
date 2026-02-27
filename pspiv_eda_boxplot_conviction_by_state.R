library(tidyverse)

pa <- read_csv("data/clean/PA.csv.gz") %>%
  dplyr::filter(location_type == "county", measure_id == 120) %>%
  mutate(state = "PA")
or <- read_csv("data/clean/OR.csv.gz") %>%
  dplyr::filter(location_type == "county", measure_id == 120) %>%
  mutate(state = "OR")
fl <- read_csv("data/clean/FL.csv.gz") %>%
  dplyr::filter(location_type == "county", measure_id == 120) %>%
  mutate(state = "FL")

combined <- bind_rows(pa, or, fl)

combined %>%
  filter(counsel_type %in% c("Court Appointed", "Private", "Public Defender", "Unknown")) %>%
  ggplot(aes(x = counsel_type, y = value, fill = state)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Cases Resulting in Conviction by Counsel Type",
       x = "Counsel Type", y = "Conviction (%)")