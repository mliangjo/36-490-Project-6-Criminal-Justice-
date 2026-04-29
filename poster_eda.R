library(dplyr)
library(ggrepel)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(tidyr)

library(bestglm)
library(stargazer)
library(car)

library(DirichletReg)

FL_rep <- read.csv("Dirichlet/full_resp_FL2.csv")
PA_rep <- read.csv("Dirichlet/full_resp_PA2.csv")
OR_rep <- read.csv("Dirichlet/full_resp_OR2.csv")

PA_rep <- PA_rep %>% 
  mutate(Log.pop = log(Population)) %>%
  rename(
    private = percent_p,
    public_defender = percent_pd,
    appointed_counsel = percent_ca,
    self_represented = percent_sr,
    other = percent_o,
    unknown = percent_u
  ) %>%
  mutate(alternative = self_represented + other + unknown)

OR_rep <- OR_rep %>% 
  mutate(Log.pop = log(Population)) %>%
  rename(
    private = percent_p,
    public_defender = percent_pd,
    appointed_counsel = percent_ca,
    self_represented = percent_sr,
    other = percent_o,
    unknown = percent_u
  )%>%
  mutate(alternative = self_represented + other + unknown)

FL_rep <- FL_rep %>% 
  mutate(Log.pop = log(Population)) %>%
  rename(
    private = percent_p,
    public_defender = percent_pd,
    appointed_counsel = percent_ca,
    self_represented = percent_sr,
    other = percent_o,
    unknown = percent_u
  )%>%
  mutate(alternative = self_represented + other + unknown)

#Sums to 1, good
summary(PA_rep$alternative + PA_rep$appointed_counsel + PA_rep$public_defender + PA_rep$private)
summary(FL_rep$alternative + FL_rep$appointed_counsel + FL_rep$public_defender + FL_rep$private)
summary(OR_rep$alternative + OR_rep$appointed_counsel + OR_rep$public_defender + OR_rep$private)

PA_summary <- PA_rep %>%
  summarise(
    alternative = mean(alternative),
    appointed_counsel = mean(appointed_counsel),
    public_defender = mean(public_defender),
    private = mean(private)
  ) %>%
  pivot_longer(everything(), names_to = "counsel_type", values_to = "proportion") %>%
  mutate(state = "PA")

ggplot(PA_summary, aes(x = state, y = proportion, fill = counsel_type)) +
  geom_col(position = "stack", width = 0.6) +
  labs(
    x = "State",
    y = "Proportion of Cases",
    fill = "Counsel Type",
    title = "Proportion of Cases by Counsel Type (PA)"
  ) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "bottom")


PA_rep %>%
  select(alternative, appointed_counsel, public_defender, private) %>%
  head()


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Process and clean FL data
FL_summary <- FL_rep %>%
  # Filter out rows with 0 or NA that were skewing the mean
  filter(!is.na(alternative + appointed_counsel + public_defender + private)) %>%
  filter((alternative + appointed_counsel + public_defender + private) > 0.9) %>%
  summarise(
    `Alternative` = mean(alternative),
    `Appointed Counsel` = mean(appointed_counsel),
    `Public Defender` = mean(public_defender),
    `Private` = mean(private)
  ) %>%
  pivot_longer(everything(), names_to = "counsel_type", values_to = "proportion") %>%
  mutate(state = "FL")

# Do the same for PA and OR
PA_summary <- PA_rep %>%
  summarise(
    `Alternative` = mean(alternative),
    `Appointed Counsel` = mean(appointed_counsel),
    `Public Defender` = mean(public_defender),
    `Private` = mean(private)
  ) %>%
  pivot_longer(everything(), names_to = "counsel_type", values_to = "proportion") %>%
  mutate(state = "PA")

OR_summary <- OR_rep %>%
  summarise(
    `Alternative` = mean(alternative),
    `Appointed Counsel` = mean(appointed_counsel),
    `Public Defender` = mean(public_defender),
    `Private` = mean(private)
  ) %>%
  pivot_longer(everything(), names_to = "counsel_type", values_to = "proportion") %>%
  mutate(state = "OR")

# Combine and Plot
all_summary <- bind_rows(PA_summary, OR_summary, FL_summary)

ggplot(all_summary, aes(x = state, y = proportion, fill = counsel_type)) +
  geom_col(position = "stack", width = 0.6) +
  labs(
    x = "State",
    y = "Proportion of Cases",
    fill = "Counsel Type"
  ) +
  theme_minimal(base_size = 18)


# 1. Label and Combine Raw Data (not summaries)
FL_rep$state <- "FL"
PA_rep$state <- "PA"
OR_rep$state <- "OR"

# 2. Pivot to Long Format for ggplot
# We multiply by 100 to match the % scale of your reference image
plot_data_long <- bind_rows(FL_rep, PA_rep, OR_rep) %>%
  select(state, private, public_defender, appointed_counsel, alternative) %>%
  pivot_longer(
    cols = c(private, public_defender, appointed_counsel, alternative),
    names_to = "counsel_type",
    values_to = "percentage"
  ) %>%
  mutate(
    # percentage = percentage * 100, # Convert 0.75 to 75%
    counsel_type = case_when(
      counsel_type == "private" ~ "Private",
      counsel_type == "public_defender" ~ "Public Defender",
      counsel_type == "appointed_counsel" ~ "Appointed Counsel",
      counsel_type == "alternative" ~ "Alternative"
    )
  )

# 3. Create the Grouped Boxplot
ggplot(plot_data_long, aes(x = counsel_type, y = percentage, fill = state)) +
  geom_boxplot() +
  labs(
    # title = "Distribution of Counsel Type Proportions across Counties",
    x = "Counsel Type",
    y = "Proportion of Cases",
    fill = "State"
  ) +
  theme_minimal(base_size = 18)
