library(haven)
library(dplyr)
install.packages('glm')
library(glm)

# reading nfhs stata file:
nfhs <- read_dta("C:/H drive/env_health/research_term_paper/NFHS_urban_rural/nfhs_updated.dta")

# factorising dataset:
cols_to_factor <- c("All_other_Health_Insurance_Schem", "caste","Central_Government_Health_Scheme", "Community_Health_Insurance_Progr",
"district","employee_ins","Employees_State_Insurance_Scheme", "femalehead_noadult","head_edu","head_sex","HH_member_health_Insurance_cover","house",
"insurance","land","male","Medical_Reimbursement_from_Emplo", "nfhs","nfhs1","nfhs5","no_adult","no_male","Other_Health_Insurance_Coverage_",
"other_ins","pmjay","pmjay_dist", "private_ins","Privately_Purchased_Commercial_H", "quintile_rur","quintile_urb","religion","residence","roof","room",
"RSBY_coverage","social_combined","social_ins","State", "State_Health_Insurance_Scheme_Co","urban","Urban_Rural","wall", "Wealth_Index_Country", 
"Wealth_Index_Rural","Wealth_Index_Rural_within_State", "Wealth_Index_Rural_Urban", "Wealth_Index_Urban","Wealth_Index_Urban_within_State",
"Wealth_Index_within_State","wealth_index")                    


nfhs[cols_to_factor] <- lapply(nfhs[cols_to_factor], factor)

# Create a named vector for mapping
state_mapping <- c(
  "1" = "jammu & kashmir",
  "2" = "himachal pradesh",
  "3" = "punjab",
  "4" = "chandigarh",
  "5" = "uttarakhand",
  "6" = "haryana",
  "7" = "nct of delhi",
  "8" = "rajasthan",
  "9" = "uttar pradesh",
  "10" = "bihar",
  "11" = "sikkim",
  "12" = "arunachal pradesh",
  "13" = "nagaland",
  "14" = "manipur",
  "15" = "mizoram",
  "16" = "tripura",
  "17" = "meghalaya",
  "18" = "assam",
  "19" = "west bengal",
  "20" = "jharkhand",
  "21" = "odisha",
  "22" = "chhattisgarh",
  "23" = "madhya pradesh",
  "24" = "gujarat",
  "25" = "dadra & nagar haveli and daman & diu",
  "27" = "maharashtra",
  "28" = "andhra pradesh",
  "29" = "karnataka",
  "30" = "goa",
  "31" = "lakshadweep",
  "32" = "kerala",
  "33" = "tamil nadu",
  "34" = "puducherry",
  "35" = "andaman & nicobar islands",
  "36" = "telangana",
  "37" = "ladakh"
)
nfhs$State <- state_mapping[as.character(nfhs$State)]

# Replace all '8'(don't know) values with NA in the nfhs dataset
nfhs[nfhs == 8] <- NA

# factrozing:
nfhs$caste <- relevel(nfhs$caste, ref = "0")
nfhs$no_adult <- relevel(nfhs$no_adult, ref = "0")
nfhs$femalehead_noadult <- relevel(nfhs$femalehead_noadult, ref = "0")
nfhs$wall <- relevel(nfhs$wall, ref = "1")
nfhs$room <- relevel(nfhs$room, ref = "1")
nfhs$land <- relevel(nfhs$land, ref = "1")
nfhs$roof <- relevel(nfhs$roof, ref = "1")
nfhs$urban <- relevel(nfhs$urban, ref = "1")
nfhs$head_sex <- relevel(nfhs$head_sex, ref = "3")
nfhs$head_edu <- relevel(nfhs$head_edu, ref = "3")
nfhs$RSBY_coverage <- relevel(nfhs$RSBY_coverage, ref = "0")
nfhs$HH_member_health_Insurance_cover <- relevel(nfhs$HH_member_health_Insurance_cover, ref = "0")
nfhs$Wealth_Index_Country <- relevel(nfhs$Wealth_Index_Country, ref = "5")

# logit model for PMJAY:
logit_nfhs1 <- glm(HH_member_health_Insurance_cover  ~  no_adult + femalehead_noadult + land + factor(disable) + caste + urban + wall + roof + head_edu + pmjay + RSBY_coverage + Wealth_Index_Country , data = nfhs, family = binomial(link = 'logit'))
summary(logit_nfhs1)

# predicted probabilities of each observation in the data:
predicted_probabilities <- predict(logit_nfhs, newdata = nfhs, type = "response")


# trade-off in probabilities using odds-ratio: 
odds_ratios <- exp(coef(logit_nfhs))
print(odds_ratios)

# Quality test of Model:
library(car)
vif_nfhs <- logit_nfhs
vif(vif_nfhs)

# Following codes are for Extracting all the Results in pdf format:
library(stargazer)

stargazer(logit_nfhs, 
          type = "docx", 
          out = "C:/H drive/env_health/research_term_paper/NFHS_urban_rural/nfhs_result.docx")
library(broom)
library(officer)

# Calculate Odds Ratios and determining sign:
or_table1 <- tidy(logit_nfhs1) %>%
  mutate(Odds_Ratio = exp(estimate), 
         Sign = ifelse(estimate > 0, "Positive", "Negative"))

# Create Word document and add Odds Ratio table
doci <- read_docx() %>%
  body_add_table(value = or_table1[, c("term", "Odds_Ratio", "Sign")], 
                 style = "table_template") %>%
  body_add_par("Odds Ratios and Signs of Coefficient Estimates", style = "heading 1")

# Save the Word document
print(doci, target = "C:/H drive/env_health/research_term_paper/NFHS_urban_rural/ORRRRR.docx")

