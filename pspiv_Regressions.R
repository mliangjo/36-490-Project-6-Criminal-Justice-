#Really horrible AI code just to get graphics to work

library(dplyr)
library(ggrepel)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(bestglm)

# ============================================================
# 1. LOAD DATA
# ============================================================
data_dir <- "data/regr" 

read_state <- function(state_abbr) {
  path <- file.path(data_dir, paste0(state_abbr, ".csv"))
  df   <- read.csv(path, check.names = FALSE)
  # Drop the state-level aggregate row
  df <- df %>% filter(location_id != state_abbr)
  # Derived log variables
  df <- df %>%
    mutate(
      Log.pop   = log(`Population`),
      Log.urban = log(`Urban Population` + 1),
      Log.rural = log(`Rural Population` + 1)
    )
  return(df)
}

rdf_PA <- read_state("PA")
rdf_OR <- read_state("OR")
rdf_FL <- read_state("FL")

# ============================================================
# 2. PA REGRESSIONS ??? full covariate set
# ============================================================

lm_pd_PA <- lm(
  `CR Public Defender` ~
    # Population
    Log.pop + `High School Graduates` + Log.urban +
    # Econ
    `Below Poverty Line` + `Unemployment Rate` + `Median Household Income` +
    # Criminal Justice
    `Number of Criminal Court Judges` + `Number of Full-Time Prosecutors` +
    `Total Number of Law Enforcement Agencies` + `Number of Part-Time Prosecutors` +
    `Police Officers per 100,000 Residents`,
  data = rdf_PA, na.action = na.omit)
summary(lm_pd_PA)

lm_p_PA <- lm(
  `CR Private` ~
    Log.pop + `High School Graduates` + Log.urban +
    `Below Poverty Line` + `Unemployment Rate` + `Median Household Income` +
    `Number of Criminal Court Judges` + `Number of Full-Time Prosecutors` +
    `Total Number of Law Enforcement Agencies` + `Number of Part-Time Prosecutors` +
    `Police Officers per 100,000 Residents`,
  data = rdf_PA, na.action = na.omit)
summary(lm_p_PA)

lm_ca_PA <- lm(
  `CR Court Appointed` ~
    Log.pop + `High School Graduates` + Log.urban +
    `Below Poverty Line` + `Unemployment Rate` + `Median Household Income` +
    `Number of Criminal Court Judges` + `Number of Full-Time Prosecutors` +
    `Total Number of Law Enforcement Agencies` + `Number of Part-Time Prosecutors` +
    `Police Officers per 100,000 Residents`,
  data = rdf_PA, na.action = na.omit)
summary(lm_ca_PA)

lm_sr_PA <- lm(
  `CR Self-Represented` ~
    Log.pop + `High School Graduates` + Log.urban +
    `Below Poverty Line` + `Unemployment Rate` + `Median Household Income` +
    `Number of Criminal Court Judges` + `Number of Full-Time Prosecutors` +
    `Total Number of Law Enforcement Agencies` + `Number of Part-Time Prosecutors` +
    `Police Officers per 100,000 Residents`,
  data = rdf_PA, na.action = na.omit)
summary(lm_sr_PA)

# ============================================================
# 3. PA VISUALIZATION ??? Conviction Rate vs Poverty
# ============================================================
rdf_PA %>%
  dplyr::select(
    `Below Poverty Line`,
    `CR Public Defender`,
    `CR Private`,
    `CR Court Appointed`,
    `CR Self-Represented`
  ) %>%
  pivot_longer(-`Below Poverty Line`, names_to = "Type", values_to = "Rate") %>%
  ggplot(aes(x = `Below Poverty Line`, y = Rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ Type, scales = "free_y") +
  labs(
    title = "Pennsylvania: Conviction Rate vs Poverty Rate",
    x     = "Below Poverty Line",
    y     = "Conviction Rate"
  )

# ============================================================
# 4. OR REGRESSIONS ??? reduced covariate set
# ============================================================

lm_pd_OR <- lm(
  `CR Public Defender` ~ Log.pop + `High School Graduates` + Log.urban + Log.rural,
  data = rdf_OR)
summary(lm_pd_OR)

lm_p_OR <- lm(
  `CR Private` ~ Log.pop + `High School Graduates` + Log.urban + Log.rural,
  data = rdf_OR)
summary(lm_p_OR)

lm_ca_OR <- lm(
  `CR Court Appointed` ~ Log.pop + `High School Graduates` + Log.urban + Log.rural,
  data = rdf_OR)
summary(lm_ca_OR)

lm_sr_OR <- lm(
  `CR Self-Represented` ~ Log.pop + `High School Graduates` + Log.urban + Log.rural,
  data = rdf_OR)
summary(lm_sr_OR)

# ============================================================
# 5. FL REGRESSIONS ??? reduced covariate set
# ============================================================

lm_pd_FL <- lm(
  `CR Public Defender` ~ Log.pop + `High School Graduates` + Log.urban + Log.rural,
  data = rdf_FL)
summary(lm_pd_FL)

lm_p_FL <- lm(
  `CR Private` ~ Log.pop + `High School Graduates` + Log.urban + Log.rural,
  data = rdf_FL)
summary(lm_p_FL)

lm_ca_FL <- lm(
  `CR Court Appointed` ~ Log.pop + `High School Graduates` + Log.urban + Log.rural,
  data = rdf_FL)
summary(lm_ca_FL)

lm_sr_FL <- lm(
  `CR Self-Represented` ~ Log.pop + `High School Graduates` + Log.urban + Log.rural,
  data = rdf_FL)
summary(lm_sr_FL)

# ============================================================
# 6. COMBINED ??? all three states stacked
# ============================================================
rdf_all <- bind_rows(
  mutate(rdf_PA, state = "PA"),
  mutate(rdf_OR, state = "OR"),
  mutate(rdf_FL, state = "FL")
)

lm_pd <- lm(
  `CR Public Defender` ~
    Log.pop + `High School Graduates` + Log.urban + Log.rural +
    `Below Poverty Line` + `Unemployment Rate` + `Median Household Income` +
    `Number of Criminal Court Judges` + `Number of Full-Time Prosecutors` +
    `Total Number of Law Enforcement Agencies` + `Number of Part-Time Prosecutors` +
    `Police Officers per 100,000 Residents`,
  data = rdf_all)
summary(lm_pd)

lm_p <- lm(
  `CR Private` ~
    Log.pop + `High School Graduates` + Log.urban + Log.rural +
    `Below Poverty Line` + `Unemployment Rate` + `Median Household Income` +
    `Number of Criminal Court Judges` + `Number of Full-Time Prosecutors` +
    `Total Number of Law Enforcement Agencies` + `Number of Part-Time Prosecutors` +
    `Police Officers per 100,000 Residents`,
  data = rdf_all)
summary(lm_p)

lm_ca <- lm(
  `CR Court Appointed` ~
    Log.pop + `High School Graduates` + Log.urban + Log.rural +
    `Below Poverty Line` + `Unemployment Rate` + `Median Household Income` +
    `Number of Criminal Court Judges` + `Number of Full-Time Prosecutors` +
    `Total Number of Law Enforcement Agencies` + `Number of Part-Time Prosecutors` +
    `Police Officers per 100,000 Residents`,
  data = rdf_all)
summary(lm_ca)

lm_sr <- lm(
  `CR Self-Represented` ~
    Log.pop + `High School Graduates` + Log.urban + Log.rural +
    `Below Poverty Line` + `Unemployment Rate` + `Median Household Income` +
    `Number of Criminal Court Judges` + `Number of Full-Time Prosecutors` +
    `Total Number of Law Enforcement Agencies` + `Number of Part-Time Prosecutors` +
    `Police Officers per 100,000 Residents`,
  data = rdf_all)
summary(lm_sr)

# ============================================================
# 7. BEST-SUBSET SELECTION (PA ??? Public Defender rate)
# ============================================================
df_bestglm <- rdf_FL %>%
  dplyr::select(
    Log.pop,
    `High School Graduates`,
    Log.urban,
    Log.rural,
    `Below Poverty Line`,
    `Unemployment Rate`,
    `Median Household Income`,
    # `Number of Criminal Court Judges`,
    # `Number of Full-Time Prosecutors`,
    `Total Number of Law Enforcement Agencies`,
    # `Number of Part-Time Prosecutors`,
    `Police Officers per 100,000 Residents`,
    y = `CR Public Defender`    # must be last column, named y
  ) %>%
  na.omit() %>%
  as.data.frame()

best_model <- bestglm(df_bestglm,
                      family = gaussian,
                      IC     = "AIC",
                      method = "exhaustive")
summary(best_model$BestModel)


# Helper to build the bestglm data frame and run selection
run_bestglm <- function(df, predictors, outcome, ic = "AIC") {
  df_bg <- df %>%
    dplyr::select(all_of(predictors), y = all_of(outcome)) %>%
    na.omit() %>%
    as.data.frame()
  best <- bestglm(df_bg, family = gaussian, IC = ic, method = "exhaustive")
  return(best)
}

# --- Full predictor set (used for PA) ---
predictors_full <- c(
  "Log.pop", "High School Graduates", "Log.urban", "Log.rural",
  "Below Poverty Line", "Unemployment Rate", "Median Household Income",
  "Number of Criminal Court Judges", "Number of Full-Time Prosecutors",
  "Total Number of Law Enforcement Agencies", "Number of Part-Time Prosecutors",
  "Police Officers per 100,000 Residents"
)

# --- Reduced predictor set (used for OR and FL) ---
predictors_reduced <- c(
  "Log.pop", "High School Graduates", "Log.urban", "Log.rural"
)

# PA ??? full set
cat("\n===== BEST GLM: PA =====\n")
best_PA <- run_bestglm(rdf_PA, predictors_full, "CR Public Defender")
summary(best_PA$BestModel)

# OR ??? reduced set
cat("\n===== BEST GLM: OR =====\n")
best_OR <- run_bestglm(rdf_OR, predictors_reduced, "CR Public Defender")
summary(best_OR$BestModel)

# FL ??? reduced set
cat("\n===== BEST GLM: FL =====\n")
best_FL <- run_bestglm(rdf_FL, predictors_reduced, "CR Public Defender")
summary(best_FL$BestModel)

# ============================================================
# 8. REGRESSION DIAGNOSTICS ??? combined models
# ============================================================

# QQ plots
par(mfrow = c(2, 2))
qqnorm(residuals(lm_pd), main = "Public Defender");  qqline(residuals(lm_pd), col = "red")
qqnorm(residuals(lm_p),  main = "Private");           qqline(residuals(lm_p),  col = "red")
qqnorm(residuals(lm_ca), main = "Court Appointed");   qqline(residuals(lm_ca), col = "red")
qqnorm(residuals(lm_sr), main = "Self Represented");  qqline(residuals(lm_sr), col = "red")

# Residuals vs Fitted
par(mfrow = c(2, 2))
plot(fitted(lm_pd), residuals(lm_pd), main = "Public Defender",
     xlab = "Fitted", ylab = "Residuals"); abline(h = 0, col = "red")
plot(fitted(lm_p),  residuals(lm_p),  main = "Private",
     xlab = "Fitted", ylab = "Residuals"); abline(h = 0, col = "red")
plot(fitted(lm_ca), residuals(lm_ca), main = "Court Appointed",
     xlab = "Fitted", ylab = "Residuals"); abline(h = 0, col = "red")
plot(fitted(lm_sr), residuals(lm_sr), main = "Self Represented",
     xlab = "Fitted", ylab = "Residuals"); abline(h = 0, col = "red")

# ============================================================
# 9. STARGAZER TABLES
# ============================================================

# PA models
stargazer(
  lm_p_PA, lm_pd_PA, lm_ca_PA, lm_sr_PA,
  type             = "text",
  font.size        = "small",
  column.sep.width = "1pt",
  covariate.labels = c(
    "Log Population", "HS Graduates", "Log Urban Pop",
    "Below Poverty", "Unemployment Rate", "Median HH Income",
    "Num. Criminal Court Judges", "Num. Full-Time Prosecutors",
    "Total Law Enforcement Agencies", "Num. Part-Time Prosecutors",
    "Police per 100k Residents"
  ),
  dep.var.labels = c("Private", "Public Defender", "Court-Appointed", "Self-Represented"),
  star.cutoffs   = c(0.05, 0.01, 0.001)
)

# Combined (all states) models
stargazer(
  lm_p, lm_pd, lm_ca, lm_sr,
  type             = "text",
  font.size        = "small",
  column.sep.width = "1pt",
  covariate.labels = c(
    "Log Population", "HS Graduates", "Log Urban Pop", "Log Rural Pop",
    "Below Poverty", "Unemployment Rate", "Median HH Income",
    "Num. Criminal Court Judges", "Num. Full-Time Prosecutors",
    "Total Law Enforcement Agencies", "Num. Part-Time Prosecutors",
    "Police per 100k Residents"
  ),
  dep.var.labels = c("Private", "Public Defender", "Court-Appointed", "Self-Represented"),
  star.cutoffs   = c(0.05, 0.01, 0.001)
)

## Pairs Plots


library(GGally)

pairs_vars <- c(
  "CR Public Defender",
  "Population",
  "Urban Population",
  "Rural Population",
  "High School Graduates",
  "Below Poverty Line",
  "Unemployment Rate",
  "Median Household Income",
  "African American Population",
  "White Population",
  "Number of Criminal Court Judges",
  "Number of Full-Time Prosecutors",
  "Number of Part-Time Prosecutors",
  "Total Number of Law Enforcement Agencies",
  "Police Officers per 100,000 Residents"
)

pairs_df <- rdf_PA %>%
  dplyr::select(all_of(pairs_vars)) %>%
  na.omit()

ggpairs(
  pairs_df,
  columnLabels = c(
    "CR\nPublic Def.",
    "Population",
    "Urban Pop.",
    "Rural Pop.",
    "HS Grads",
    "Below\nPoverty",
    "Unemploy.\nRate",
    "Median HH\nIncome",
    "African Am.\nPop.",
    "White\nPop.",
    "Court\nJudges",
    "FT\nProsecutors",
    "PT\nProsecutors",
    "Law Enf.\nAgencies",
    "Police\nper 100k"
  ),
  upper = list(continuous = wrap("cor", size = 2.5)),
  lower = list(continuous = wrap("points", alpha = 0.4, size = 0.6)),
  diag  = list(continuous = wrap("densityDiag")),
  progress = FALSE
) +
  labs(title = "Pennsylvania: Pairs Plot of County Covariates vs Public Defender Conviction Rate") +
  theme_bw(base_size = 7) +
  theme(
    strip.text       = element_text(size = 6),
    axis.text        = element_text(size = 5),
    plot.title       = element_text(size = 9, face = "bold")
  )

ggsave("pairs_plot_PA.png", width = 16, height = 14, dpi = 150)

library(patchwork)

## PA

plot_scatter <- function(xvar, xlabel) {
  ggplot(rdf_PA, aes(x = .data[[xvar]], y = `CR Public Defender`)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "red") +
    labs(x = xlabel, y = "CR Public Defender") +
    theme_bw()
}

p1 <- plot_scatter("Unemployment Rate",               "Unemployment Rate")
p2 <- plot_scatter("High School Graduates",                 "High School Graduates")
p3 <- plot_scatter("Number of Criminal Court Judges",           "Number of Criminal Court Judges")
p4 <- plot_scatter("Log.pop",                               "Log Population")

(p1 | p2) / (p3 | p4) +
  plot_annotation(title = "Pennsylvania: Public Defender Conviction Rate vs Key Covariates")

ggsave("key_covariates_PA.png", width = 10, height = 8, dpi = 150)

## OR

plot_scatter <- function(xvar, xlabel) {
  ggplot(rdf_PA, aes(x = .data[[xvar]], y = `CR Public Defender`)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "red") +
    labs(x = xlabel, y = "CR Public Defender") +
    theme_bw()
}

p1 <- plot_scatter("Unemployment Rate",               "Unemployment Rate")
p2 <- plot_scatter("High School Graduates",                 "High School Graduates")
p3 <- plot_scatter("Number of Criminal Court Judges",           "Number of Criminal Court Judges")
p4 <- plot_scatter("Log.pop",                               "Log Population")

(p1 | p2) / (p3 | p4) +
  plot_annotation(title = "Pennsylvania: Public Defender Conviction Rate vs Key Covariates")

ggsave("key_covariates_PA.png", width = 10, height = 8, dpi = 150)

## FL

plot_scatter <- function(xvar, xlabel) {
  ggplot(rdf_OR, aes(x = .data[[xvar]], y = `CR Public Defender`)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "red") +
    labs(x = xlabel, y = "CR Public Defender") +
    theme_bw()
}

p1 <- plot_scatter("Unemployment Rate",               "Unemployment Rate")
p2 <- plot_scatter("High School Graduates",                 "High School Graduates")
p3 <- plot_scatter("African American Population",           "African American Population")
p4 <- plot_scatter("Log.pop",                               "Log Population")

(p1 | p2) / (p3 | p4) +
  plot_annotation(title = "Oregon: Public Defender Conviction Rate vs Key Covariates")

ggsave("key_covariates_OR.png", width = 10, height = 8, dpi = 150)