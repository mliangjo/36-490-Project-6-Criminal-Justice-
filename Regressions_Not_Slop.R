#pspivack
#4/1/26

library(tidyverse)

fl <- read_csv("data/regr/FL.csv")
or <- read_csv("data/regr/OR.csv")
pa <- read_csv("data/regr/PA.csv")

#Adds logit transformed cols
add_logit_cols <- function(df){
  df %>%
    mutate( #Div by 100 to get on 0-1 scale
      LOGIT_CR_Priv = qlogis((`CR Private`+0.5)/101), #Div by 100 for percents
      LOGIT_CR_PD = qlogis((`CR Public Defender`+0.5)/101), #Add some noise to get rid of Inf at exactly 0 or 1
      LOGIT_CR_AC = qlogis((`CR Court Appointed`+0.5)/101),
      Population = log(Population)
    ) %>%
    rename(log_Population = Population)
}

fl <- add_logit_cols(fl)
or <- add_logit_cols(or)
pa <- add_logit_cols(pa)


#Linear Model against covariates

fl_PD_reg <- lm(LOGIT_CR_PD ~ ., data = fl%>%
                  select(-location_id,
                  - `CR Self-Represented`,
                  - `CR Private`,
                  - `CR Public Defender`,
                  - `CR Court Appointed`,
                  - `CR Other`,
                  - `CR Unknown`,
                  - LOGIT_CR_Priv,
                  - LOGIT_CR_AC,
                  - `Rural Population` #= 1- Urban Pop, Multicollineraity
                  ) %>%
                  drop_na(LOGIT_CR_PD)) 
or_PD_reg <- lm(LOGIT_CR_PD ~ ., data = or%>%
                  select(-location_id,
                         - `CR Self-Represented`,
                         - `CR Private`,
                         - `CR Public Defender`,
                         - `CR Court Appointed`,
                         - `CR Other`,
                         - `CR Unknown`,
                         - LOGIT_CR_Priv,
                         - LOGIT_CR_AC,
                         - `Rural Population` # Multicolinearity
                  ) %>%
                  drop_na(LOGIT_CR_PD)) 
pa_PD_reg <- lm(LOGIT_CR_PD ~ ., data = pa%>%
                  select(-location_id,
                         - `CR Self-Represented`,
                         - `CR Private`,
                         - `CR Public Defender`,
                         - `CR Court Appointed`,
                         - `CR Other`,
                         - `CR Unknown`,
                         - LOGIT_CR_Priv,
                         - LOGIT_CR_AC,
                         - `Rural Population` # Multicollinearity
                  ) %>%
                  drop_na(LOGIT_CR_PD)) 

# Summaries & Diagnostics
summary(fl_PD_reg) #Note the observations removed due to missingness
summary(or_PD_reg)
summary(pa_PD_reg)
plot(fl_PD_reg, which = 1)
plot(or_PD_reg, which = 1)
plot(pa_PD_reg, which = 1)
