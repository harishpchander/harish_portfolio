library(readxl)
library(broom)
library(plm)
library(dplyr)
library(car)

# importing data:
final_data <- read_excel("C:/H drive/appl_ecotrix_assgn/panel_assgn_dataset.xlsx", sheet=2)
sheet <- data.frame(final_data)


# structuring the df for a panel data:
sheet <- pdata.frame(sheet, index = c("id", "Year"))

# Assuming pdata_with_dummies is your data frame
pdata <- pdata.frame(pdata_with_dummies, index = c("id", "Year"))

# Create a model matrix for time dummies
time_dummies <- model.matrix(~ factor(Year) - 1, data = do_or_die_sheet)

# Convert the model matrix to a data frame
time_dummies_df <- as.data.frame(time_dummies)

# Combine the time dummies with the original data
pdata_with_dummies <- cbind(do_or_die_sheet, time_dummies_df)


# pooled OLS model without time_dummy:
pooled_ols <- plm(log_forest_area ~ log_land_area + popn_dens + log_total_CO2 + rur_popn + life_exp, model = "pooling", data = pdata)
summary(pooled_ols)

# pooled OLS model with time_dummy:
pooled_ols_dummy <- plm(log_forest_area ~ log_land_area + popn_dens + log_total_CO2 + rur_popn + life_exp + factor(Year), model = "pooling", data = pdata)
summary(pooled_ols_dummy)

# FE model without time dummy:
fe_model <- plm(log_forest_area ~ log_land_area + popn_dens + log_total_CO2 + rur_popn + life_exp, model = "within", effect = "individual", data = pdata)
summary(fe_model)

# FE model with time dummy:
fe_model_dummy <- plm(log_forest_area ~ log_land_area + popn_dens + log_total_CO2 + rur_popn + life_exp + factor(Year), model = "within", effect = "individual", data = pdata)
summary(fe_model_dummy)

# RE model without time duumy: 
re_model <- plm(log_forest_area ~ log_land_area + popn_dens + log_total_CO2 + rur_popn + life_exp, model = "random", effect = "individual", data = pdata)
summary(re_model)

re_model_dummy <- plm(log_forest_area ~ log_land_area + popn_dens + log_total_CO2 + rur_popn + life_exp  + factor(Year), model = "random", effect = "individual", data = pdata)
summary(re_model_dummy)

