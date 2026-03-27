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
  labs(title = "Distribution of Self-Representation Rates Across Counties, FL",
    x = "Proportion Self Represented", y = "Number of Counties")

fl %>%
  distinct(location_id, SR, total_cases) %>%
  summarize(
    NAs = sum(is.na(SR)),
    NaNs = sum(is.nan(SR))
  )

###########################################################

pa <- read_csv("data/clean/PA.csv.gz")

pa <- pa %>%
  filter(location_type == "county",
         measure_id == 120) %>%
  group_by(location_id, measure_id) %>%
  mutate(
    total_cases = sum(denominator, na.rm = TRUE),
    SR = denominator[counsel_type == "Self-Represented"] / total_cases) %>%
  ungroup()

pa %>%
  distinct(location_id, SR, total_cases) %>%
  ggplot(aes(x = SR)) +
  geom_histogram(bins = 36, fill = "palevioletred") +
  # geom_density(alpha = 0.4) +
  labs(title = "Distribution of Self-Representation Rates Across Counties, PA",
       x = "Proportion Self Represented", y = "Number of Counties")

pa %>%
  distinct(location_id, SR, total_cases) %>%
  summarize(
    NAs = sum(is.na(SR)),
    NaNs = sum(is.nan(SR))
  )


###########################################################

or <- read_csv("data/clean/OR.csv.gz")

or <- or %>%
  filter(location_type == "county",
         measure_id == 120) %>%
  group_by(location_id, measure_id) %>%
  mutate(
    total_cases = sum(denominator, na.rm = TRUE),
    SR = denominator[counsel_type == "Self-Represented"] / total_cases) %>%
  ungroup()

or %>%
  distinct(location_id, SR, total_cases) %>%
  ggplot(aes(x = SR)) +
  geom_histogram(bins = 36, fill = "palevioletred") +
  # geom_density(alpha = 0.4) +
  labs(title = "Distribution of Self-Representation Rates Across Counties, OR",
       x = "Proportion Self Represented", y = "Number of Counties")

or %>%
  distinct(location_id, SR, total_cases) %>%
  summarize(
    NAs = sum(is.na(SR)),
    NaNs = sum(is.nan(SR))
  )


##############################################################


or <- read_csv("data/combined/Oregon_comb.csv.gz")

or2 <- or %>%
  filter(location_type == "county", measure_id == 120) %>%
  group_by(location_id) %>%
  mutate(
    percent_afr_am = denominator[demographics == "African American defendants"]
 / denominator[demographics== "all defendants"]
  )