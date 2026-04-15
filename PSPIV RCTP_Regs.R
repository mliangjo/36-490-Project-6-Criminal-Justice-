library(dplyr)
library(ggrepel)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(tidyr)

library(bestglm)
library(stargazer)
library(car)

locations_pa <- read_csv("data/raw/Pennsylvania_State_Data/locations.csv")
locations_or <- read_csv("data/raw/Oregon_State_Data/locations-or.csv")
locations_fl <- read_csv("data/raw/Florida_State_Data/locations-fl.csv")

rdf<-read.csv("data/county_characteristics_conviction.csv")

rdf <- rdf %>% 
  mutate(Log.pop = log(Population),
         Log.Largest.Municipality.Population = log(Largest.Municipality.Population+1))

rdf_PA <- rdf %>% 
  filter(state == "PA") %>% 
  left_join(locations_pa, by = c("location_id" = "id"))
  
rdf_OR <- rdf %>% 
  filter(state == "OR") %>% 
  left_join(locations_or, by = c("location_id" = "id"))
  
rdf_FL <-rdf %>% 
  filter(state == "FL") %>% 
  left_join(locations_fl, by = c("location_id" = "id"))


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# First Analysis
## What are the different county variables that effect the conviction rates of different defendant types
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

process_state_data <- function(state_name, data_file, locations_file, 
                               filters_file = "filters.csv", 
                               measures_file = "measures.csv") {
  # Construct file paths 
  data_path <- file.path("data/raw", paste0(state_name, "_State_Data"), data_file)
  locations_path <- file.path("data/raw", paste0(state_name, "_State_Data"), locations_file)
  filters_path <- file.path("data/raw", paste0(state_name, "_State_Data"), filters_file)
  measures_path <- file.path("data/raw", paste0(state_name, "_State_Data"), measures_file)
  
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


percent_att_type_df <- function(data_comb) {
  data_comb %>% filter(location_type == "county", measure_id == 120) %>%
    group_by(location_id) %>%
    mutate(
      percent_p = denominator[demographics == "defendants represented by private attorney"]/
        denominator[demographics== "all defendants"],
      percent_pd = denominator[demographics == "defendants represented by public defender"]/
        denominator[demographics== "all defendants"],
      percent_ca = denominator[demographics == "defendants represented by a court-appointed private attorney"]/
        denominator[demographics== "all defendants"],
      percent_sr = denominator[demographics == "defendants who self-represented"]/
        denominator[demographics== "all defendants"]
      ) %>% 
    distinct(location_id, .keep_all = TRUE) %>% 
    select(location_id, percent_p, percent_pd, percent_ca, percent_sr)
}

pct_att_type_pa <- percent_att_type_df(data_pa_comb)
pct_att_type_or <- percent_att_type_df(data_or_comb)
pct_att_type_fl <- percent_att_type_df(data_fl_comb)

full_pct_df_PA <- inner_join(rdf_PA, pct_att_type_pa, by ="location_id")
full_pct_df_OR <- inner_join(rdf_OR, pct_att_type_or, by ="location_id")
full_pct_df_FL <- inner_join(rdf_FL, pct_att_type_fl, by ="location_id")


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Model Fitting
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

# -----------------------------------Pennsylvania------------------------------------- #
lm_pd_PA <- lm(
  car::logit(percent_pd) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = full_pct_df_PA,
  na.action = na.omit)
#summary(lm_pd_PA)
#vif(lm_pd_PA)

lm_p_PA <- lm(
  car::logit(percent_p) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = full_pct_df_PA,
  na.action = na.omit)
#summary(lm_p_PA)
#vif(lm_p_PA)

lm_ca_PA <- lm(
  car::logit(percent_ca) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = full_pct_df_PA,
  na.action = na.omit)
#summary(lm_ca_PA)
#vif(lm_ca_PA)

lm_sr_PA <- lm(
  car::logit(percent_sr) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Number.of.Criminal.Court.Judges + Police.Officers.per.100.000.Residents + Number.of.Full.Time.Prosecutors, 
  data   = full_pct_df_PA,
  na.action = na.omit)
#summary(lm_sr_PA)
#vif(lm_sr_PA)

# -----------------------------------Oregon------------------------------------------ #
lm_pd_OR<- lm(
  car::logit(percent_pd) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
    # Criminal Justice
    # NONE
  data   = full_pct_df_OR,
  na.action = na.omit)
#summary(lm_pd_OR)
#vif(lm_pd_OR)

lm_p_OR<- lm(
  car::logit(percent_p) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
    # Criminal Justice
    # NONE
  data   = full_pct_df_OR,
  na.action = na.omit)
#summary(lm_p_OR)
#vif(lm_p_OR)

lm_ca_OR<- lm(
  car::logit(percent_ca) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
    # Criminal Justice
    # NONE
  data   = full_pct_df_OR,
  na.action = na.omit)
#summary(lm_ca_OR)
#vif(lm_ca_OR)

lm_sr_OR <- lm(
  car::logit(percent_sr) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
    # Criminal Justice
    # NONE
  data   = full_pct_df_OR,
  na.action = na.omit)
#summary(lm_sr_OR)
#vif(lm_sr_OR)

# ---------------------------------Florida-------------------------------------------- #
lm_pd_FL<- lm(
  car::logit(percent_pd) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Police.Officers.per.100.000.Residents,
  data   = full_pct_df_FL,
  na.action = na.omit)
#summary(lm_pd_FL)
#vif(lm_pd_FL)

lm_p_FL <- lm(
  car::logit(percent_p) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Police.Officers.per.100.000.Residents,
  data   = full_pct_df_FL,
  na.action = na.omit)
#summary(lm_p_FL)
#vif(lm_p_FL)

lm_ca_FL<- lm(
  car::logit(percent_ca) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Police.Officers.per.100.000.Residents,
  data   = full_pct_df_FL,
  na.action = na.omit)
#summary(lm_ca_FL)
#vif(lm_ca_FL)

lm_sr_FL <- lm(
  car::logit(percent_sr) ~ 
    # Population
    Log.pop + Young.Males.Population + #Log.Largest.Municipality.Population + 
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate +
    # Criminal Justice
    Police.Officers.per.100.000.Residents,
  data   = full_pct_df_FL,
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
    out = paste0("Appendix/RCTP_Reg_Results/", tolower(state), ".html"),
    notes = NULL,
    notes.append = FALSE,
    
    dep.var.labels.include = FALSE,
    dep.var.caption = "",
    column.labels = c(
      "Public Defender",
      "Private",
      "Court Appointed",
      "Self-Represented"
    ),
    
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
webshot("Appendix/RCTP_Reg_Results/fl.html", "Appendix/RCTP_Reg_Results/fl.png", 
  vwidth = 650, vheight = 700, zoom = 2)
webshot("Appendix/RCTP_Reg_Results/or.html", "Appendix/RCTP_Reg_Results/or.png", 
        vwidth = 650, vheight = 700, zoom = 2)
webshot("Appendix/RCTP_Reg_Results/pa.html", "Appendix/RCTP_Reg_Results/pa.png", 
        vwidth = 650, vheight = 700, zoom = 2)


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Regression Plots
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

reg_plots <- function(full_pct_df_STATE, base_vals, state, lm_pd, lm_p, lm_ca, lm_sr) {  
  # Sequence of unemployment values for x-axis
  u_seq <- seq(
    min(full_pct_df_STATE$Unemployment.Rate, na.rm = TRUE),
    max(full_pct_df_STATE$Unemployment.Rate, na.rm = TRUE),
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
  
  points_df <- full_pct_df_STATE %>%
    select(
      Unemployment.Rate,
      percent_pd,
      percent_p,
      percent_ca,
      percent_sr
    ) %>%
    pivot_longer(
      cols = -Unemployment.Rate,
      names_to = "Attorney.Type",
      values_to = "pct_att_type"
    ) %>%
    mutate(
      Attorney.Type = dplyr::recode(Attorney.Type,
        "percent_p" = "Private Counsel",
        "percent_pd" = "Public Defender",
        "percent_ca" = "Court Appointed",
        "percent_sr" = "Self-Represented"
      )
    )
  
  # Plot
  loess_plt <- ggplot(plot_df, aes(x = Unemployment.Rate, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
    #geom_line(linewidth = 1, color = "red") + <--- for linear
    geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
    
    # Add actual data points
    geom_point(
      data = points_df,
      aes(x = Unemployment.Rate, y = pct_att_type),
      alpha = 0.6,
      size = 2
    ) +
    
    facet_wrap(~ Attorney.Type, ncol = 2) +
    labs(
      # title = paste0("Predicted Counsel Type Percentage vs. Unemployment Rate in ", state, " -- LOESS"),
      x = "Unemployment Rate",
      y = "Predicted Counsel Type Percentage"
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
      aes(x = Unemployment.Rate, y = pct_att_type),
      alpha = 0.6,
      size = 2
    ) +
    
    facet_wrap(~ Attorney.Type, ncol = 2) +
    labs(
      # title = paste0("Predicted Counsel Type Percentage vs. Unemployment Rate in ", state, " Linear"),
      x = "Unemployment Rate",
      y = "Predicted Counsel Type Percentage"
    ) +
    theme_minimal(base_size = 12)
  
  return(c(loess_plt = loess_plt, linear_plt = linear_plt))
}

# Common values to hold other predictors fixed at
base_vals_PA <- full_pct_df_PA %>%
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

base_vals_OR <- full_pct_df_OR %>%
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

base_vals_FL <- full_pct_df_FL %>%
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

ggsave("Appendix/RCTP_Unemp_Plot/paLOESS.png", width = 10, height = 8, dpi = 300,
       reg_plots(full_pct_df_PA, base_vals_PA, "Pennsylvania", lm_pd_PA, lm_p_PA, lm_ca_PA, lm_sr_PA)$loess_plt)
ggsave("Appendix/RCTP_Unemp_Plot/orLOESS.png", width = 10, height = 8, dpi = 300,
       reg_plots(full_pct_df_OR, base_vals_OR, "Oregon", lm_pd_OR, lm_p_OR, lm_ca_OR, lm_sr_OR)$loess_plt)
ggsave("Appendix/RCTP_Unemp_Plot/flLOESS.png", width = 10, height = 8, dpi = 300,
       reg_plots(full_pct_df_FL, base_vals_FL, "Florida", lm_pd_FL, lm_p_FL, lm_ca_FL, lm_sr_FL)$loess_plt)


# Regression Plots -- Linear

ggsave("Appendix/RCTP_Unemp_Plot/paLIN.png", width = 10, height = 8, dpi = 300,
       reg_plots(full_pct_df_PA, base_vals_PA, "Pennsylvania", lm_pd_PA, lm_p_PA, lm_ca_PA, lm_sr_PA)$linear_plt)
ggsave("Appendix/RCTP_Unemp_Plot/orLIN.png", width = 10, height = 8, dpi = 300,
       reg_plots(full_pct_df_OR, base_vals_OR, "Oregon", lm_pd_OR, lm_p_OR, lm_ca_OR, lm_sr_OR)$linear_plt)
ggsave("Appendix/RCTP_Unemp_Plot/flLIN.png", width = 10, height = 8, dpi = 300,
       reg_plots(full_pct_df_FL, base_vals_FL, "Florida", lm_pd_FL, lm_p_FL, lm_ca_FL, lm_sr_FL)$linear_plt)


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Residual Diagnostics (QQ)
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

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

ggsave("Appendix/RCTP_Diagnostic/PApd.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_pd_PA))
ggsave("Appendix/RCTP_Diagnostic/PAp.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_p_PA))
ggsave("Appendix/RCTP_Diagnostic/PAca.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_ca_PA))
ggsave("Appendix/RCTP_Diagnostic/PAsr.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_sr_PA))

ggsave("Appendix/RCTP_Diagnostic/ORpd.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_pd_OR))
ggsave("Appendix/RCTP_Diagnostic/ORp.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_p_OR))
ggsave("Appendix/RCTP_Diagnostic/ORca.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_ca_OR))
ggsave("Appendix/RCTP_Diagnostic/ORsr.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_sr_OR))

ggsave("Appendix/RCTP_Diagnostic/FLpd.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_pd_FL))
ggsave("Appendix/RCTP_Diagnostic/FLp.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_p_FL))
ggsave("Appendix/RCTP_Diagnostic/FLca.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_ca_FL))
ggsave("Appendix/RCTP_Diagnostic/FLsr.png", width = 10, height = 8, dpi = 300,
       residual_diag_plots(lm_sr_FL))


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# Influence / LOO
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #

# Cook's Distance Plot
cooks_plot <- function (mod, state, counsel, full_pct_df_STATE) {
  # ---- Step 1: Identify outliers ----
  
  # Studentized residuals (outliers in response)
  rstud <- rstudent(mod)
  outliers <- which(abs(rstud) > 2)
  
  # Cook's distance (influential points)
  cooks <- cooks.distance(mod)
  threshold <- 4 / length(cooks)
  influential <- which(cooks > threshold)
  
  used_data <- model.frame(mod)
  used_rows <- as.numeric(rownames(used_data))

  df <- data.frame(
    obs = 1:length(cooks),
    cooks = cooks,
    label = full_pct_df_STATE$name[used_rows]
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
      # title = paste0("Cook's Distance (", state, " - ", counsel, ") % Attourney Type"),
      x = "Observation",
      y = "Cook's Distance"
    ) +
    theme_minimal() +
    theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())
}

#PA
ggsave("Appendix/RCTP_Influence/PApd.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_pd_PA, "PA", "PD", full_pct_df_PA))
ggsave("Appendix/RCTP_Influence/PAp.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_p_PA, "PA", "P", full_pct_df_PA))
ggsave("Appendix/RCTP_Influence/PAca.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_ca_PA, "PA", "CA", full_pct_df_PA))
ggsave("Appendix/RCTP_Influence/PAsr.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_sr_PA, "PA", "SR", full_pct_df_PA))

#OR
ggsave("Appendix/RCTP_Influence/ORpd.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_pd_OR, "OR", "PD", full_pct_df_OR))
ggsave("Appendix/RCTP_Influence/ORp.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_p_OR, "OR", "P", full_pct_df_OR))
ggsave("Appendix/RCTP_Influence/ORca.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_ca_OR, "OR", "CA", full_pct_df_OR))
ggsave("Appendix/RCTP_Influence/ORsr.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_sr_OR, "OR", "SR", full_pct_df_OR))

#FL
ggsave("Appendix/RCTP_Influence/FLpd.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_pd_FL, "FL", "PD", full_pct_df_FL))
ggsave("Appendix/RCTP_Influence/FLp.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_p_FL, "FL", "P", full_pct_df_FL))
ggsave("Appendix/RCTP_Influence/FLca.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_ca_FL, "FL", "CA", full_pct_df_FL))
ggsave("Appendix/RCTP_Influence/FLsr.png", width = 10, height = 8, dpi = 300,
       cooks_plot(lm_sr_FL, "FL", "SR", full_pct_df_FL))


## ????????????????????????????????????????????????????????????????????????? ##

# OR - Private (Outliers Removed)
# Removed all counties with cooks distance above the threshold

# Cook's distance (influential points)
cooks_p_OR <- cooks.distance(lm_p_OR)
threshold_p_OR <- 4 / length(cooks_p_OR)
influential_p_OR <- which(cooks_p_OR > threshold_p_OR)

used_data_p_OR <- model.frame(lm_p_OR)
used_rows_p_OR <- as.numeric(rownames(used_data_p_OR))

rdf_OR_clean_p_OR <- full_pct_df_OR[-used_rows_p_OR[influential_p_OR], ]

lm_p_OR_clean <- lm(
  car::logit(percent_p) ~ 
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
cooks_plot(lm_p_OR_clean, "OR", "P -- w/out influential", rdf_OR_clean_p_OR)
summary(lm_p_OR_clean)


# OR - Private (Multnomah Removed)

# Cook's distance (influential points)
max_influential <- which.max(cooks_p_OR)

#used_data_p_OR <- model.frame(lm_p_OR)
#used_rows_p_OR <- as.numeric(rownames(used_data_p_OR))

rdf_OR_clean2_p_OR <- full_pct_df_OR[-used_rows_p_OR[max_influential], ]

lm_p_OR_clean2 <- lm(
  car::logit(percent_p) ~ 
    # Population
    Log.pop + Young.Males.Population +
    # Racial/Ethnicity
    African.American.Population + Native.American.Alaskan.Population + 
    Asian.or.Pacific.Islander.Population + Hispanic.or.Latino.Population +
    # Econ
    Below.Poverty.Line + Unemployment.Rate ,
    # Criminal Justice
    # NONE
  data   = rdf_OR_clean2_p_OR,
  na.action = na.omit)

residual_diag_plots(lm_p_OR_clean2)
cooks_plot(lm_p_OR_clean2, "OR", "P -- w/out influential", rdf_OR_clean2_p_OR)
summary(lm_p_OR_clean2)
