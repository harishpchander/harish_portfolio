install.packages("plm")
install.packages("broom")
install.packages("readxl")
library(readxl)
library(plm)
library(broom)

#loading wdi dataset:
wdi_exp <- read_excel("C:/H drive/env_health/research_term_paper/WDI/wdi_data.xlsx", sheet = 4)
exp_df <- data.frame(wdi_exp)
exp_df$PMJAY <- as.factor(exp_df$PMJAY)

# standardizing the data:
exp_df$RCHE_pc_centered <- scale(exp_df$RCHE_pc, center = TRUE, scale = FALSE)
exp_df$ROOPE_pc_centered <- scale(exp_df$ROOPE_pc, center = TRUE, scale = FALSE)
exp_df$cdr_scaled <- scale(exp_df$crude_death_rate, center = TRUE, scale = FALSE)

# time series analysis: 
lm_exp <- lm(AE_total ~ RCHE_pc + ROOPE_pc + crude_death_rate + AE_PMJAY + PMJAY + covid_crude , data = exp_df)
summary(lm_exp)

# checking quality of the model:
library(car)
vif_model <- lm_exp
vif(vif_model)

# exporting results:
library(sjPlot)

tab_model(lm_exp, file = "C:/H drive/env_health/research_term_paper/WDI/wdi_results_.doc")

