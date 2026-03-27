#pspivack
#36490
#Court Characteristics Joiner

library(tidyverse)

fl <- read_csv("../raw/Florida_State_Data/measures.csv")
pa <- read_csv("../raw/Pennsylvania_State_Data/measures.csv")
or <- read_csv("../raw/Oregon_State_Data/measures.csv")


pa_measures <- pa %>% distinct(name, id, format) %>%
  mutate(state = "PA",
         format = tolower(format)) %>%
  filter(id < 500)
or_measures <- or %>% distinct(name, id, format) %>%
  mutate(state = "OR") %>%
  filter(id < 500)
fl_measures <- fl %>% distinct(name, id, format) %>%
  mutate(state = "FL") %>%
  filter(id < 500)

all_measures <- bind_rows(pa_measures, or_measures, fl_measures) %>%
  mutate(present = T) %>%
  pivot_wider(names_from = state, values_from = present)

all_measures
write_excel_csv(all_measures, "fu.xlsx")