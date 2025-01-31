library(readxl)
library(plm)
library(broom)
install.packages("glmnet")
library(glmnet)


#loading NHP dataset:
NHP <- read_excel("C:/H drive/env_health/research_term_paper/National Health Profile/finance indicators 2023.xlsx",sheet = 10)
nhp <- data.frame(NHP)

# transforming existing data for analysis:
nhp$state_percent_factor <- as.factor(nhp$states_percent)
nhp$log_GHE <- log(nhp$GHE)
nhp$log_prem <- log(nhp$PMJAY_prem_HH)
nhp$log_GSDP <- log(nhp$GSDP)
nhp$GGE <- as.numeric(nhp$GGE)nhp$log_GGE <- log(nhp$GGE)
nhp$GSDP <- as.numeric(nhp$GSDP)
nhp$UB <- as.numeric(nhp$UB)
nhp$log_HH <- log(nhp$PMJAY_HH)
nhp$log_ind <- log(nhp$PMJAY_ind)
nhp$log_THE <- log(nhp$THE)

# controlling for dummy variable trap:
nhp$state_percent_factor <- relevel(nhp$state_percent_factor, ref = "Low")

# Statewise regression analysis:
lm_nhp <- lm(THE ~ OOPE + GHE_pc + RUP + UBP + RBP  + TD, data = nhp)
summary(lm_nhp)


# Quality test fo the model: 
library(car)
vif_nhp <- lm_nhp
vif(vif_nhp)

# Following codes are for Extracting all the Results in pdf format:
library(sjPlot)
tab_model(lm_nhp, file = "C:/H drive/env_health/research_term_paper/National Health Profile/NHP_results.doc")

# Tidy up the results
model_results <- tidy(lm_nhp)

# Calculate percentage change for continuous variables (using coefficients)
model_results$percentage_change <- model_results$estimate * 100

# Round values for better display
model_results$percentage_change <- round(model_results$percentage_change, 2)

# Create a Word document
doc <- read_docx()

# Calculate Odds Ratios and determine sign
model_results <- tidy(lm_nhp) %>%
  mutate(estimate = model_results$estimate), 
         Sign = ifelse(estimate > 0, "Positive", "Negative"))

# Create Word document and add Odds Ratio table
doc <- read_docx() %>%
  body_add_table(value = model_results[, c("term",, "Sign", "p.value")], 
                 style = "table_template") %>%
  body_add_par("Odds Ratios and Signs of Coefficient Estimates", style = "heading 1")

# Get a summary of the model coefficients
model_summary <- as.data.frame(summary(logit_nfhs)$coefficients)
# Create a flextable from the model summary
ft <- flextable(model_summary)


# Create a new Word document and add the flextable
doc <- read_docx()
doc <- body_add_flextable(doc, ft)

# Save the Word document to your specified path
print(doc, target = "C:/H drive/env_health/research_term_paper/NFHS_urban_rural/nfhs_result.docx")

# the logit model and odds ratio:
library(broom)
library(officer)

# Assuming lm_nhp is your model
model_results <- tidy(lm_nhp) %>%
  mutate(estimate_sign = ifelse(estimate > 0, "+", "-"))
# Add "p-value" column
model_results$p_value <- model_results$p.value

# Create Word document and add Odds Ratio table
doc <- read_docx() %>%
  body_add_table(value = model_results[, c("term", "estimate_sign", "p_value")], 
                 style = "table_template") %>%
  body_add_par("Odds Ratios and Signs of Coefficient Estimates", style = "heading 1")

# Save the Word document
print(doc, target = "C:/H drive/env_health/research_term_paper/NFHS_urban_rural/nhp_resu.docx")


# Save the document
print(doc, target = "C:/H drive/env_health/research_term_paper/National Health Profile/nhp_resss.docx")

# Identifying the efficient variable using Machine Learning algorithm: 
#Key Adjustments:
# ridge and Lasso:
# Convert predictors to matrix form
x <- as.matrix(nhp[, c("OOPE", "TD", "UBP", "PMJAY_ind", "RUP", "UUP", "RBP", "state_percent_factor", "RGESIP","RPESIP", "UGESIP","UPESIP", "log_GSDP", "PMJAY_prem_HH", "state_percent_factor"  )])

# Convert response variable to vector
y <- nhp$THE

# Convert factor variables to dummy variables
x <- model.matrix(~ OOPE + TD + UBP + PMJAY_ind + RUP + UUP + RBP + state_percent_factor, data = nhp)[,-1]

# Ridge Regression with cross-validation
ridge_cv <- cv.glmnet(x, y, alpha = 0, nfolds = 5)  # alpha = 0 for Ridge

# Best lambda (penalty term)
best_lambda_ridge <- ridge_cv$lambda.min
print(best_lambda_ridge)

# Fit Ridge model with the best lambda
ridge_model <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)

# Coefficients of the Ridge model
coef(ridge_model)

#lasso:
# Lasso Regression with cross-validation
lasso_cv <- cv.glmnet(x, y, alpha = 1, nfolds = 5)  # alpha = 1 for Lasso

# Best lambda (penalty term)
best_lambda_lasso <- lasso_cv$lambda.min
print(best_lambda_lasso)

# Fit Lasso model with the best lambda
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)

# Coefficients of the Lasso model
coef(lasso_model)
