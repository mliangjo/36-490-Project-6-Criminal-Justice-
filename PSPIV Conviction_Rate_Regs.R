library(dplyr)
library(ggrepel)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(tidyr)

library(bestglm)
library(stargazer)
library(car)
library(ggrepel)
library(tidyr)

# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# VIF Based Covariate Selection
## Start with full linear model with all covariates, remove vars with high multicollinearity
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #


locations_pa <- read_csv("data/raw/Pennsylvania_State_Data/locations.csv")
locations_or <- read_csv("data/raw/Oregon_State_Data/locations-or.csv")
locations_fl <- read_csv("data/raw/Florida_State_Data/locations-fl.csv")

rdf<-read.csv("data/county_characteristics_conviction.csv")

rdf <- rdf %>% 
  mutate(Log.pop = log(Population),
         Log.Largest.Municipality.Population = log(Largest.Municipality.Population + 1))

rdf_PA <- rdf %>% 
  filter(state == "PA") %>% 
  left_join(locations_pa, by = c("location_id" = "id"))


rdf_OR <- rdf %>% 
  filter(state == "OR") %>% 
  left_join(locations_or, by = c("location_id" = "id"))

rdf_FL <-rdf %>% 
  filter(state == "FL") %>% 
  left_join(locations_fl, by = c("location_id" = "id"))

#

mod <- lm(
  car::logit(Public.Defender.conviction.rate) ~ 
    # Population
    Log.pop + Urban.Population + Young.Males.Population + Log.Largest.Municipality.Population + High.School.Graduates +
    # Racial/Ethnicity
    White.Population + African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate + Median.Household.Income +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = rdf_PA,
  na.action = na.omit)
#summary(mod)
vif(mod)

###### VIF IS REALLY HIGH #########
# VIF < 5 → fine
# VIF 5–10 → borderline
# VIF > 10 → drop or rethink


# We remove White.Population and Median.Household.Income
mod1 <- lm(
  car::logit(Public.Defender.conviction.rate) ~ 
    # Population
    Log.pop + Urban.Population + Young.Males.Population + Log.Largest.Municipality.Population + High.School.Graduates +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate + #Median.Household.Income +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = rdf_PA,
  na.action = na.omit)
#summary(mod)
vif(mod1)


# Better but not quite
# We remove Log.Largest.Municipality.Population and Urban.Population as they drop the Log.pop vif the most (to a reasonable value 6~)
mod2 <- lm(
  car::logit(Public.Defender.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Urban.Population + #Log.Largest.Municipality.Population + #High.School.Graduates +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = rdf_PA,
  na.action = na.omit)
#summary(mod)
vif(mod2)

# Police.Officers.per.100.000.Residents 
# Note moderate multicollinearity
# It represents a different concept (law enforcement capacity) so we leave it

##


# FL 
mod3 <- lm(
  car::logit(Public.Defender.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + #High.School.Graduates +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    #Number.of.Criminal.Court.Judges + 
    Police.Officers.per.100.000.Residents, #+ Number.of.Full.Time.Prosecutors, 
  data   = rdf_FL,
  na.action = na.omit)
#summary(mod)
vif(mod3)


# OR
mod4 <- lm(
  car::logit(Public.Defender.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + # Log.Largest.Municipality.Population + 
    #High.School.Graduates +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,#+
  # Criminal Justice
  #Number.of.Criminal.Court.Judges +Police.Officers.per.100.000.Residents, + Number.of.Full.Time.Prosecutors, 
  data   = rdf_OR,
  na.action = na.omit)
#summary(mod)
vif(mod4)


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Final Models
# Uses the covariates identified in the last section and generates conviction rate models per state
# LOGIT Public Def ~ Covariates
# LOGIT Private ~ Covariates
# LOGIT Court Appointed ~ Covariates
# LOGIT Self Rep ~ Covariates
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

# ---------------------------------Pennsylvania-------------------------------------------- #

lm_pd_PA <- lm(
  car::logit(Public.Defender.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = rdf_PA,
  na.action = na.omit)
#summary(lm_pd_PA)
#vif(lm_pd_PA)

lm_p_PA <- lm(
  car::logit(Private.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = rdf_PA,
  na.action = na.omit)
#summary(lm_p_PA)
#vif(lm_p_PA)

lm_ca_PA <- lm(
  car::logit(Court.Appointed.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = rdf_PA,
  na.action = na.omit)
#summary(lm_ca_PA)
#vif(lm_ca_PA)

lm_sr_PA <- lm(
  car::logit(Self.Represented.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = rdf_PA,
  na.action = na.omit)
#summary(lm_sr_PA)
#vif(lm_sr_PA)

# -----------------------------------Oregon------------------------------------------ #
lm_pd_OR<- lm(
  car::logit(Public.Defender.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
  # Criminal Justice
  # NONE
  data   = rdf_OR,
  na.action = na.omit)
#summary(lm_pd_OR)
#vif(lm_pd_OR)

lm_p_OR<- lm(
  car::logit(Private.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
  # Criminal Justice
  # NONE
  data   = rdf_OR,
  na.action = na.omit)
#summary(lm_p_OR)
#vif(lm_p_OR)


lm_ca_OR<- lm(
  car::logit(Court.Appointed.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
  # Criminal Justice
  # NONE
  data   = rdf_OR,
  na.action = na.omit)
#summary(lm_ca_OR)
#vif(lm_ca_OR)


lm_sr_OR <- lm(
  car::logit(Self.Represented.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
  # Criminal Justice
  # NONE
  data   = rdf_OR,
  na.action = na.omit)
#summary(lm_sr_OR)
#vif(lm_sr_OR)

# ---------------------------------Florida-------------------------------------------- #
lm_pd_FL<- lm(
  car::logit(Public.Defender.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Police.Officers.per.100.000.Residents,
  data   = rdf_FL,
  na.action = na.omit)
#summary(lm_pd_FL)
#vif(lm_pd_FL)

lm_p_FL <- lm(
  car::logit(Private.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Police.Officers.per.100.000.Residents,
  data   = rdf_FL,
  na.action = na.omit)
#summary(lm_p_FL)
#vif(lm_p_FL)


lm_ca_FL<- lm(
  car::logit(Court.Appointed.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Police.Officers.per.100.000.Residents,
  data   = rdf_FL,
  na.action = na.omit)
#summary(lm_ca_FL)
#vif(lm_ca_FL)

lm_sr_FL <- lm(
  car::logit(Self.Represented.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Police.Officers.per.100.000.Residents,
  data   = rdf_FL,
  na.action = na.omit)
#summary(lm_sr_FL)
#vif(lm_sr_FL)

# ----------------------------------------------------------------------------- #
## Regression Results Outputs
# ----------------------------------------------------------------------------- #

# Summary Tables

summary(lm_pd_PA)
summary(lm_p_PA)
summary(lm_ca_PA)
summary(lm_sr_PA)

summary(lm_pd_OR)
summary(lm_p_OR)
summary(lm_ca_OR)
summary(lm_sr_OR)

summary(lm_pd_FL)
summary(lm_p_FL)
summary(lm_ca_FL)
summary(lm_sr_FL)

# Coefficient Tables

library(stargazer)

state_coef_table <- function(state, lm_pd, lm_p, lm_ca, lm_sr, covar_labels, state_name) {
  stargazer(
    lm_pd,
    lm_p,
    lm_ca,
    lm_sr,
    
    type = "html",
    out = paste0("appendix/CR_Reg_Results/", tolower(state), ".html"),
    notes = NULL,
    notes.append = FALSE,
    # title = paste0("Conviction Rate Regression Results by Attorney Type (",
    #                state_name,")"),
    
    dep.var.labels.include = FALSE,
    dep.var.caption = "",#Regression Results for Conviction Rates by Counsel Type (Pennsylvania)",
    column.labels = c(
      "Public Defender",
      "Private",
      "Court Appointed",
      "Self-Represented"
    ),
    
    #dep.var.labels = "Logit(Conviction Rate)",
    
    digits = 3,
    report = "vc*s",
    omit.stat = c("f", "rsq", "ser"),
    
    covariate.labels = covar_labels,
    
    align = TRUE
  ) 
}


covar_labels_PA = c("Log Population",
                    "Proportion of Young Males",
                    "Proportion African American",
                    "Proportion Native American / Alaskan",
                    "Proportion Asian / Pacific Islander",
                    "Proportion Hispanic or Latino",
                    "Poverty Rate",
                    "Unemployment Rate",
                    "Number of Criminal Court Judges",
                    "Police Officers per 100,000 Residents",
                    "Number of Full-Time Prosecutors"
)

covar_labels_OR = c("Log Population",
                    "Proportion of Young Males",
                    "Proportion African American",
                    "Proportion Native American / Alaskan",
                    "Proportion Asian / Pacific Islander",
                    "Proportion Hispanic or Latino",
                    "Poverty Rate",
                    "Unemployment Rate"#,
                    #"Number of Criminal Court Judges",
                    #"Police Officers per 100,000 Residents",
                    #"Number of Full-Time Prosecutors"
)

covar_labels_FL = c("Log Population",
                    "Proportion of Young Males",
                    "Proportion African American",
                    "Proportion Native American / Alaskan",
                    "Proportion Asian / Pacific Islander",
                    "Proportion Hispanic or Latino",
                    "Poverty Rate",
                    "Unemployment Rate",
                    #"Number of Criminal Court Judges",
                    "Police Officers per 100,000 Residents"#,
                    #"Number of Full-Time Prosecutors"
)

# ~~ Publishable ~~

state_coef_table("pa", lm_pd_PA, lm_p_PA, lm_ca_PA, lm_sr_PA, covar_labels_PA, "Pennsylvania")
state_coef_table("or", lm_pd_OR, lm_p_OR, lm_ca_OR, lm_sr_OR, covar_labels_OR, "Oregon")
state_coef_table("fl", lm_pd_FL, lm_p_FL, lm_ca_FL, lm_sr_FL, covar_labels_FL, "Florida")

#Saves the R output as a PNG
library(webshot)
webshot("appendix/CR_Reg_Results/fl.html", "appendix/CR_Reg_Results/fl.png", 
  vwidth = 650, vheight = 700, zoom = 2)
webshot("appendix/CR_Reg_Results/or.html", "appendix/CR_Reg_Results/or.png", 
        vwidth = 650, vheight = 700, zoom = 2)
webshot("appendix/CR_Reg_Results/pa.html", "appendix/CR_Reg_Results/pa.png", 
        vwidth = 650, vheight = 700, zoom = 2)


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Regression Plots
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

reg_plots <- function(rdf_STATE, base_vals, state, lm_pd, lm_p, lm_ca, lm_sr) {  
  # Sequence of unemployment values for x-axis
  u_seq <- seq(
    min(rdf_STATE$Unemployment.Rate, na.rm = TRUE),
    max(rdf_STATE$Unemployment.Rate, na.rm = TRUE),
    length.out = 200
  )
  
  # Prediction grid
  pred_grid <- base_vals[rep(1, length(u_seq)), ]
  pred_grid$Unemployment.Rate <- u_seq
  
  # Function to get predictions + CI on response scale
  make_pred_df <- function(model, label, newdata) {
    pred <- predict(model, newdata = newdata, se.fit = TRUE)
    
    out <- newdata %>%
      mutate(
        # raw predictions
        fit_link = pred$fit,
        se_link = pred$se.fit,
        fit = plogis(fit_link), # convert to probability scale
        
        # 95% concidence intervals -> probability scale
        lwr = plogis(fit_link - 1.96 * se_link),
        upr = plogis(fit_link + 1.96 * se_link),
        Attorney.Type = label
      )
    
    out
  }
  
  pred_pd <- make_pred_df(lm_pd, "Public Defender", pred_grid)
  pred_p  <- make_pred_df(lm_p,  "Private Counsel", pred_grid)
  pred_ca <- make_pred_df(lm_ca, "Court Appointed", pred_grid)
  pred_sr <- make_pred_df(lm_sr, "Self-Represented", pred_grid)
  
  plot_df <- bind_rows(pred_pd, pred_p, pred_ca, pred_sr)
  
  points_df <- rdf_STATE %>%
    select(
      Unemployment.Rate,
      Private.conviction.rate,
      Public.Defender.conviction.rate,
      Court.Appointed.conviction.rate,
      Self.Represented.conviction.rate
    ) %>%
    pivot_longer(
      cols = -Unemployment.Rate,
      names_to = "Attorney.Type",
      values_to = "conviction_rate"
    ) %>%
    mutate(
      Attorney.Type = dplyr::recode(Attorney.Type,
                                    "Private.conviction.rate" = "Private Counsel",
                                    "Public.Defender.conviction.rate" = "Public Defender",
                                    "Court.Appointed.conviction.rate" = "Court Appointed",
                                    "Self.Represented.conviction.rate" = "Self-Represented"
      )
    ) %>%
    mutate(conviction_rate = conviction_rate / 100)
  
  # Plot
  loess_plt <- ggplot(plot_df, aes(x = Unemployment.Rate, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
    #geom_line(linewidth = 1, color = "red") + <--- for linear
    geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
    
    # Add actual data points
    geom_point(
      data = points_df,
      aes(x = Unemployment.Rate, y = conviction_rate),
      alpha = 0.6,
      size = 2
    ) +
    
    facet_wrap(~ Attorney.Type, ncol = 2) +
    labs(
      # title = paste0("Predicted Conviction Rate vs. Unemployment Rate in ", state, " -- LOESS"),
      x = "Unemployment Rate",
      y = "Predicted Conviction Rate"
    ) +
    theme_minimal(base_size = 12)
  
  
  # Plot
  linear_plt <- ggplot(plot_df, aes(x = Unemployment.Rate, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
    geom_line(linewidth = 1, color = "red") +
    #geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
    
    # Add actual data points
    geom_point(
      data = points_df,
      aes(x = Unemployment.Rate, y = conviction_rate),
      alpha = 0.6,
      size = 2
    ) +
    
    facet_wrap(~ Attorney.Type, ncol = 2) +
    labs(
      # title = paste0("Predicted Conviction Rate vs. Unemployment Rate in ", state, " Linear"),
      x = "Unemployment Rate",
      y = "Predicted Conviction Rate"
    ) +
    theme_minimal(base_size = 12)
  
  return(c(loess_plt = loess_plt, linear_plt = linear_plt))
}

# Common values to hold other predictors fixed at
base_vals_PA <- rdf_PA %>%
  summarise(
    Log.pop = mean(Log.pop, na.rm = TRUE),
    Young.Males.Population = mean(Young.Males.Population, na.rm = TRUE),
    African.American.Population = mean(African.American.Population, na.rm = TRUE),
    Native.American.Alaskan.Population = mean(Native.American.Alaskan.Population, na.rm = TRUE),
    Asian.or.Pacific.Islander.Population = mean(Asian.or.Pacific.Islander.Population, na.rm = TRUE),
    Hispanic.or.Latino.Population = mean(Hispanic.or.Latino.Population, na.rm = TRUE),
    Below.Poverty.Line = mean(Below.Poverty.Line, na.rm = TRUE),
    Number.of.Criminal.Court.Judges = mean(Number.of.Criminal.Court.Judges, na.rm = TRUE),
    Police.Officers.per.100.000.Residents = mean(Police.Officers.per.100.000.Residents, na.rm = TRUE),
    Number.of.Full.Time.Prosecutors = mean(Number.of.Full.Time.Prosecutors, na.rm = TRUE)
  )

base_vals_OR <- rdf_OR %>%
  summarise(
    Log.pop = mean(Log.pop, na.rm = TRUE),
    Young.Males.Population = mean(Young.Males.Population, na.rm = TRUE),
    African.American.Population = mean(African.American.Population, na.rm = TRUE),
    Native.American.Alaskan.Population = mean(Native.American.Alaskan.Population, na.rm = TRUE),
    Asian.or.Pacific.Islander.Population = mean(Asian.or.Pacific.Islander.Population, na.rm = TRUE),
    Hispanic.or.Latino.Population = mean(Hispanic.or.Latino.Population, na.rm = TRUE),
    Below.Poverty.Line = mean(Below.Poverty.Line, na.rm = TRUE),
    #Number.of.Criminal.Court.Judges = mean(Number.of.Criminal.Court.Judges, na.rm = TRUE),
    #Police.Officers.per.100.000.Residents = mean(Police.Officers.per.100.000.Residents, na.rm = TRUE),
    #Number.of.Full.Time.Prosecutors = mean(Number.of.Full.Time.Prosecutors, na.rm = TRUE)
  )

base_vals_FL <- rdf_FL %>%
  summarise(
    Log.pop = mean(Log.pop, na.rm = TRUE),
    Young.Males.Population = mean(Young.Males.Population, na.rm = TRUE),
    African.American.Population = mean(African.American.Population, na.rm = TRUE),
    Native.American.Alaskan.Population = mean(Native.American.Alaskan.Population, na.rm = TRUE),
    Asian.or.Pacific.Islander.Population = mean(Asian.or.Pacific.Islander.Population, na.rm = TRUE),
    Hispanic.or.Latino.Population = mean(Hispanic.or.Latino.Population, na.rm = TRUE),
    Below.Poverty.Line = mean(Below.Poverty.Line, na.rm = TRUE),
    #Number.of.Criminal.Court.Judges = mean(Number.of.Criminal.Court.Judges, na.rm = TRUE),
    Police.Officers.per.100.000.Residents = mean(Police.Officers.per.100.000.Residents, na.rm = TRUE),
    #Number.of.Full.Time.Prosecutors = mean(Number.of.Full.Time.Prosecutors, na.rm = TRUE)
  )


# Regression Plots -- LOESS

ggsave("Appendix/CR_Unemp_Plot/paLOESS.png", width = 10, height = 8, dpi = 300,
       reg_plots(rdf_PA, base_vals_PA, "Pennsylvania", lm_pd_PA, lm_p_PA, lm_ca_PA, lm_sr_PA)$loess_plt)
ggsave("Appendix/CR_Unemp_Plot/orLOESS.png", width = 10, height = 8, dpi = 300,
       reg_plots(rdf_OR, base_vals_OR, "Oregon33", lm_pd_OR, lm_p_OR, lm_ca_OR, lm_sr_OR)$loess_plt)
ggsave("Appendix/CR_Unemp_Plot/flLOESS.png", width = 10, height = 8, dpi = 300,
  reg_plots(rdf_FL, base_vals_FL, "Florida", lm_pd_FL, lm_p_FL, lm_ca_FL, lm_sr_FL)$loess_plt)


# Regression Plots -- Linear

ggsave("Appendix/CR_Unemp_Plot/paLIN.png", width = 10, height = 8, dpi = 300,
       reg_plots(rdf_PA, base_vals_PA, "Pennsylvania", lm_pd_PA, lm_p_PA, lm_ca_PA, lm_sr_PA)$linear_plt)
ggsave("Appendix/CR_Unemp_Plot/orLIN.png", width = 10, height = 8, dpi = 300,
       reg_plots(rdf_OR, base_vals_OR, "Oregon", lm_pd_OR, lm_p_OR, lm_ca_OR, lm_sr_OR)$linear_plt)
ggsave("Appendix/CR_Unemp_Plot/flLIN.png", width = 10, height = 8, dpi = 300,
       reg_plots(rdf_FL, base_vals_FL, "Florida", lm_pd_FL, lm_p_FL, lm_ca_FL, lm_sr_FL)$linear_plt)


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Residual Diagnostics (QQ)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

# residual_diag_plots <- function (lm_att_state) {
#   par(mfrow = c(2,1))
#   qqnorm(residuals(lm_att_state), main = "QQ Plot")
#   qqline(residuals(lm_att_state), col = "red")
#   
#   # 2. Residuals vs Fitted (linearity + homoskedasticity)
#   plot(fitted(lm_att_state), residuals(lm_att_state),
#        main = "Residuals vs Fitted",
#        xlab = "Fitted Values", ylab = "Residuals")
#   abline(h = 0, col = "red")
# }

## GGPlot version
library(patchwork)
residual_diag_plots <- function(lm_att_state) {
  df <- data.frame(
    fitted    = fitted(lm_att_state),
    residuals = residuals(lm_att_state)
  )
  
  qq_plt <- ggplot(df, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "QQ Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  resid_plt <- ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  combined <- qq_plt / resid_plt  # requires patchwork
  
  return(combined)
}


ggsave("Appendix/CR_Diagnostic/PApd.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_pd_PA))
ggsave("Appendix/CR_Diagnostic/PAp.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_p_PA))
ggsave("Appendix/CR_Diagnostic/PAca.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_ca_PA))
ggsave("Appendix/CR_Diagnostic/PAsr.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_sr_PA))

ggsave("Appendix/CR_Diagnostic/ORpd.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_pd_OR))
ggsave("Appendix/CR_Diagnostic/ORp.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_p_OR))
ggsave("Appendix/CR_Diagnostic/ORca.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_ca_OR))
ggsave("Appendix/CR_Diagnostic/ORsr.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_sr_OR))

ggsave("Appendix/CR_Diagnostic/FLpd.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_pd_FL))
ggsave("Appendix/CR_Diagnostic/FLp.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_p_FL))
ggsave("Appendix/CR_Diagnostic/FLca.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_ca_FL))
ggsave("Appendix/CR_Diagnostic/FLsr.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_sr_FL))

# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Influence / LOO
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

# Cook's Distanse Plot
cooks_plot <- function (mod, state, counsel, rdf_STATE) {
  # ---- Step 1: Identify outliers ----
  
  # Studentized residuals (outliers in response)
  rstud <- rstudent(mod)
  outliers <- which(abs(rstud) > 2)
  
  # Cook's distance (influential points)
  cooks <- cooks.distance(mod)
  threshold <- 4 / length(cooks)
  influential <- which(cooks > threshold)
  
  # Combine both
  # suspects <- unique(c(outliers, influential))
  # 
  # #cat("Potential influential observations:\n")
  # #print(suspects)
  # 
  used_data <- model.frame(mod)
  used_rows <- as.numeric(rownames(used_data))
  # 
  # influential_info <- data.frame(
  #   obs = influential,
  #   original_row = used_rows[influential],
  #   county_name = rdf_PA$name[used_rows[influential]],
  #   rstudent = round(rstud[influential], 3),
  #   cooks_d = round(cooks[influential], 3)
  # )
  # # Cook's Distance Plot
  # plot(cooks,
  #      pch = 19,
  #      #main = "Cook's Distance (PA Public Defender Model)",
  #      main = paste0("Cook's Distance (", state, " - ", counsel, ")"),
  #      xlab = "Observation",
  #      ylab = "Cook's Distance")
  # 
  # abline(h = threshold, col = "red", lty = 2)
  # 
  # # label flagged points
  # text(influential, cooks[influential],
  #      labels = influential_info$county_name,
  #      pos = 4,
  #    cex = 0.6,
  #    offset = 0.3,
  # col = "blue")
  #      #pos = 3, cex = 0.8)
  
  df <- data.frame(
    obs = 1:length(cooks),
    cooks = cooks,
    label = rdf_STATE$name[used_rows]
  )
  
  ggplot(df, aes(x = obs, y = cooks)) +
    geom_point(color = "grey70") +
    geom_hline(yintercept = threshold, color = "red", linetype = "dashed") +
    geom_text_repel(
      data = df[influential, ],
      aes(label = label),
      size = 3
    ) +
    labs(
      # title = paste0("Cook's Distance (", state, " - ", counsel, ") conviction rate"),
      x = "Observation",
      y = "Cook's Distance"
    ) +
    theme_minimal() +
    theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())
}

#PA
ggsave("Appendix/CR_Influence/PApd.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_pd_PA, "PA", "PD", rdf_PA))
ggsave("Appendix/CR_Influence/PAp.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_p_PA, "PA", "P", rdf_PA))
ggsave("Appendix/CR_Influence/PAca.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_ca_PA, "PA", "CA", rdf_PA))
ggsave("Appendix/CR_Influence/PAsr.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_sr_PA, "PA", "SR", rdf_PA))

#OR
ggsave("Appendix/CR_Influence/ORpd.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_pd_OR, "OR", "PD", rdf_OR))
ggsave("Appendix/CR_Influence/ORp.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_p_OR, "OR", "P", rdf_OR))
ggsave("Appendix/CR_Influence/ORca.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_ca_OR, "OR", "CA", rdf_OR))
ggsave("Appendix/CR_Influence/ORsr.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_sr_OR, "OR", "SR", rdf_OR))

#FL
ggsave("Appendix/CR_Influence/FLpd.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_pd_FL, "FL", "PD", rdf_FL))
ggsave("Appendix/CR_Influence/FLp.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_p_FL, "FL", "P", rdf_FL))
ggsave("Appendix/CR_Influence/FLca.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_ca_FL, "FL", "CA", rdf_FL))
ggsave("Appendix/CR_Influence/FLsr.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_sr_FL, "FL", "SR", rdf_FL))

## Don't know what's after here
## ?????????????????????????????????????????????????????????????????????????? ##

# OR - Private (Outliers Revmoved)
Removed all counties with cooks distance above the threshold
# Cook's distance (influential points)
cooks_p_OR <- cooks.distance(lm_p_OR)
threshold_p_OR <- 4 / length(cooks_p_OR)
influential_p_OR <- which(cooks_p_OR > threshold_p_OR)

used_data_p_OR <- model.frame(lm_p_OR)
used_rows_p_OR <- as.numeric(rownames(used_data_p_OR))

rdf_OR_clean_p_OR <- rdf_OR[-used_rows_p_OR[influential_p_OR], ]

lm_p_OR_clean <- lm(
  car::logit(Private.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
  # Criminal Justice
  # NONE
  data   = rdf_OR_clean_p_OR,
  na.action = na.omit)


residual_diag_plots(lm_p_OR_clean)
cooks_plot(lm_p_OR_clean, "FL", "P -- w/out influential", rdf_OR_clean_p_OR)
summary(lm_p_OR_clean)
```
```{r}
base_vals_OR_clean <- rdf_OR_clean_p_OR %>%
  summarise(
    Log.pop = mean(Log.pop, na.rm = TRUE),
    Young.Males.Population = mean(Young.Males.Population, na.rm = TRUE),
    African.American.Population = mean(African.American.Population, na.rm = TRUE),
    Native.American.Alaskan.Population = mean(Native.American.Alaskan.Population, na.rm = TRUE),
    Asian.or.Pacific.Islander.Population = mean(Asian.or.Pacific.Islander.Population, na.rm = TRUE),
    Hispanic.or.Latino.Population = mean(Hispanic.or.Latino.Population, na.rm = TRUE),
    Below.Poverty.Line = mean(Below.Poverty.Line, na.rm = TRUE),
    #Number.of.Criminal.Court.Judges = mean(Number.of.Criminal.Court.Judges, na.rm = TRUE),
    #Police.Officers.per.100.000.Residents = mean(Police.Officers.per.100.000.Residents, na.rm = TRUE),
    #Number.of.Full.Time.Prosecutors = mean(Number.of.Full.Time.Prosecutors, na.rm = TRUE)
  )

# Sequence of unemployment values for x-axis
u_seq_OR_clean <- seq(
  min(rdf_OR_clean_p_OR$Unemployment.Rate, na.rm = TRUE),
  max(rdf_OR_clean_p_OR$Unemployment.Rate, na.rm = TRUE),
  length.out = 200
)

# Prediction grid
pred_grid_OR_clean <- base_vals_OR_clean[rep(1, length(u_seq_OR_clean)), ]
pred_grid_OR_clean$Unemployment.Rate <- u_seq_OR_clean

# Function to get predictions + CI on response scale
make_pred_df <- function(model, label, newdata) {
  pred <- predict(model, newdata = newdata, se.fit = TRUE)
  
  out <- newdata %>%
    mutate(
      # raw predictions
      fit_link = pred$fit,
      se_link = pred$se.fit,
      fit = plogis(fit_link), # convert to probability scale
      
      # 95% concidence intervals -> probability scale
      lwr = plogis(fit_link - 1.96 * se_link),
      upr = plogis(fit_link + 1.96 * se_link),
      Attorney.Type = label
    )
  
  out
}

#pred_pd <- make_pred_df(lm_pd, "Public Defender", pred_grid)
pred_p_OR_clean  <- make_pred_df(lm_p_OR_clean,  "Private Counsel -- w/out Influential", pred_grid_OR_clean)
#pred_ca <- make_pred_df(lm_ca, "Court Appointed", pred_grid)
#pred_sr <- make_pred_df(lm_sr, "Self-Represented", pred_grid)

#plot_df <- bind_rows(pred_pd, pred_p, pred_ca, pred_sr)

points_df_OR_clean <- rdf_OR_clean_p_OR %>%
  select(
    Unemployment.Rate,
    Private.conviction.rate,
    Public.Defender.conviction.rate,
    Court.Appointed.conviction.rate,
    Self.Represented.conviction.rate
  ) %>%
  pivot_longer(
    cols = -Unemployment.Rate,
    names_to = "Attorney.Type",
    values_to = "conviction_rate"
  ) %>%
  mutate(
    Attorney.Type = dplyr::recode(Attorney.Type,
                                  "Private.conviction.rate" = "Private Counsel",
                                  "Public.Defender.conviction.rate" = "Public Defender",
                                  "Court.Appointed.conviction.rate" = "Court Appointed",
                                  "Self.Represented.conviction.rate" = "Self-Represented"
    )
  ) %>%
  mutate(conviction_rate = conviction_rate / 100)

# Plot
ggplot(pred_p_OR_clean, aes(x = Unemployment.Rate, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  #geom_line(linewidth = 1, color = "red") + <--- for linear
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
  
  # Add actual data points
  geom_point(
    data = points_df_OR_clean,
    aes(x = Unemployment.Rate, y = conviction_rate),
    alpha = 0.6,
    size = 2
  ) +
  
  #facet_wrap(~ Attorney.Type, ncol = 2) +
  labs(
    title = paste0("Predicted Conviction Rate vs. Unemployment Rate in OR -- LOESS"),
    x = "Unemployment Rate",
    y = "Predicted Conviction Rate"
  ) +
  theme_minimal(base_size = 12)

print("plot: OR - Private w/out Infuential points")
```

# OR - Private (Multnomah Revmoved)
```{r}
# Cook's distance (influential points)
#cooks_p_OR <- cooks.distance(lm_p_OR)
#threshold_p_OR <- 4 / length(cooks_p_OR)
#influential_p_OR <- which(cooks_p_OR > threshold_p_OR)
max_influential <- which.max(cooks_p_OR)

#used_data_p_OR <- model.frame(lm_p_OR)
#used_rows_p_OR <- as.numeric(rownames(used_data_p_OR))

rdf_OR_clean2_p_OR <- rdf_OR[-used_rows_p_OR[max_influential], ]

lm_p_OR_clean2 <- lm(
  car::logit(Private.conviction.rate) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
  # Criminal Justice
  # NONE
  data   = rdf_OR_clean_p_OR,
  na.action = na.omit)


residual_diag_plots(lm_p_OR_clean2)
cooks_plot(lm_p_OR_clean2, "FL", "P -- w/out influential", rdf_OR_clean2_p_OR)
summary(lm_p_OR_clean2)
```
```{r}
base_vals_OR_clean2 <- rdf_OR_clean2_p_OR %>%
  summarise(
    Log.pop = mean(Log.pop, na.rm = TRUE),
    Young.Males.Population = mean(Young.Males.Population, na.rm = TRUE),
    African.American.Population = mean(African.American.Population, na.rm = TRUE),
    Native.American.Alaskan.Population = mean(Native.American.Alaskan.Population, na.rm = TRUE),
    Asian.or.Pacific.Islander.Population = mean(Asian.or.Pacific.Islander.Population, na.rm = TRUE),
    Hispanic.or.Latino.Population = mean(Hispanic.or.Latino.Population, na.rm = TRUE),
    Below.Poverty.Line = mean(Below.Poverty.Line, na.rm = TRUE),
    #Number.of.Criminal.Court.Judges = mean(Number.of.Criminal.Court.Judges, na.rm = TRUE),
    #Police.Officers.per.100.000.Residents = mean(Police.Officers.per.100.000.Residents, na.rm = TRUE),
    #Number.of.Full.Time.Prosecutors = mean(Number.of.Full.Time.Prosecutors, na.rm = TRUE)
  )

# Sequence of unemployment values for x-axis
u_seq_OR_clean2 <- seq(
  min(rdf_OR_clean2_p_OR$Unemployment.Rate, na.rm = TRUE),
  max(rdf_OR_clean2_p_OR$Unemployment.Rate, na.rm = TRUE),
  length.out = 200
)

# Prediction grid
pred_grid_OR_clean2 <- base_vals_OR_clean2[rep(1, length(u_seq_OR_clean2)), ]
pred_grid_OR_clean2$Unemployment.Rate <- u_seq_OR_clean2

#pred_pd <- make_pred_df(lm_pd, "Public Defender", pred_grid)
pred_p_OR_clean2  <- make_pred_df(lm_p_OR_clean2,  "Private Counsel -- w/out Influential", pred_grid_OR_clean2)
#pred_ca <- make_pred_df(lm_ca, "Court Appointed", pred_grid)
#pred_sr <- make_pred_df(lm_sr, "Self-Represented", pred_grid)

#plot_df <- bind_rows(pred_pd, pred_p, pred_ca, pred_sr)

points_df_OR_clean2 <- rdf_OR_clean2_p_OR %>%
  select(
    Unemployment.Rate,
    Private.conviction.rate,
    Public.Defender.conviction.rate,
    Court.Appointed.conviction.rate,
    Self.Represented.conviction.rate
  ) %>%
  pivot_longer(
    cols = -Unemployment.Rate,
    names_to = "Attorney.Type",
    values_to = "conviction_rate"
  ) %>%
  mutate(
    Attorney.Type = dplyr::recode(Attorney.Type,
                                  "Private.conviction.rate" = "Private Counsel",
                                  "Public.Defender.conviction.rate" = "Public Defender",
                                  "Court.Appointed.conviction.rate" = "Court Appointed",
                                  "Self.Represented.conviction.rate" = "Self-Represented"
    )
  ) %>%
  mutate(conviction_rate = conviction_rate / 100)

# Plot
ggplot(pred_p_OR_clean2, aes(x = Unemployment.Rate, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  #geom_line(linewidth = 1, color = "red") + <--- for linear
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
  
  # Add actual data points
  geom_point(
    data = points_df_OR_clean2,
    aes(x = Unemployment.Rate, y = conviction_rate),
    alpha = 0.6,
    size = 2
  ) +
  
  #facet_wrap(~ Attorney.Type, ncol = 2) +
  labs(
    title = paste0("Predicted Conviction Rate vs. Unemployment Rate in OR -- LOESS"),
    x = "Unemployment Rate",
    y = "Predicted Conviction Rate"
  ) +
  theme_minimal(base_size = 12)

print("plot: OR - Private w/out Most Influential point (Multnomah)")
```