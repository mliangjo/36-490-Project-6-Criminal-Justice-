library(tidyverse)

fl <- read_csv("data/clean/FL.csv.gz")

fl <- fl %>%
  filter(location_type == "county",
         measure_id == 120) %>%
  group_by(location_id, measure_id) %>%
  mutate(
    total_cases = sum(denominator, na.rm = TRUE),
    SR = denominator[counsel_type == "Self-Represented"] / total_cases) %>%
  ungroup()

fl %>%
  distinct(location_id, SR, total_cases) %>%
  ggplot(aes(x = SR)) +
  geom_histogram(bins = 36, fill = "palevioletred") +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of Self-Representation Rates Across Counties",
    x = "Percent Self Represented", y = "Number of Counties")

fl %>%
  distinct(location_id, SR, total_cases) %>%
  summarize(
    NAs = sum(is.na(SR)),
    NaNs = sum(is.nan(SR))
  )