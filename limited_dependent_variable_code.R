# (1) Binary Choice Model:
library(haven)
library(dplyr)
time_use <- read_dta("C:/H drive/appl_ecotrix_assgn/Assgn 2/ecotrixdf.dta")
time <- time_use[time_use$State == '24', ]

# splitting age groups:
time$age_grp <- ifelse(time$age >= 15 & time$age <= 17, 1,
                ifelse(time$age >= 18 & time$age <= 21, 2,
                ifelse(time$age >= 22 & time$age <= 25, 3,
                ifelse(time$age >= 26 & time$age <= 29, 4, 0))))

# factorising dataset:
cols_to_factor <- c("gender", "edu_level", "marital_status", "type_of_the_day", "cook_fuel", "lighting", 
                    "dwelling", "social_group", "religion", "Sector", "age_grp", "in_educ", "activity")
time[cols_to_factor] <- lapply(time[cols_to_factor], factor)


# logit model:
logit_educ <- glm(in_educ ~ age + gender + edu_level + marital_status + type_of_the_day + lnmpce + hhsize
                            + cook_fuel + lighting + dwelling + social_group + religion + Sector, 
                            data = time, family = binomial(link = 'logit'))
summary(logit_educ)

# data for predicted probability:
pred <- time %>%
  select(age, gender, in_educ)

logit_pred <- glm(in_educ ~ age + gender, data = pred, family = binomial(link = 'logit'))
summary(logit_pred) 

probit_pred <- glm(in_educ ~ age + gender, data = pred, family = binomial(link = 'probit'))
summary(probit_pred) 

pred$predicted_logit <- predict(logit_pred, pred, type = "response")
pred$predicted_probit <- predict(logit_pred, pred, type = "response")

# probit model:
probit_educ <- glm(in_educ ~ age + gender + edu_level + marital_status + type_of_the_day + lnmpce + hhsize
                  + cook_fuel + lighting + dwelling + social_group + religion + Sector, 
                  data = time, family = binomial(link = 'probit'))
summary(probit_educ)


# plotting predicted probabailities for logit and porbit:
library(ggplot2)
ggplot(pred, aes(x = age, y = predicted_logit, color = factor(gender, labels = c("Male", "Female")))) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Adds points at each age
  labs(title = "Predicted Probability of In Education by Age and Gender",
       x = "Age",
       y = "Predicted Probability of Being in Education",
       color = "Gender") +  # Renames legend title to 'Gender'
  scale_x_continuous(breaks = seq(min(pred$age), max(pred$age), by = 2)) +  # Sets x-axis breaks every 2 years
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))  

# probit vis:
ggplot(pred, aes(x = age, y = predicted_probit, color = factor(gender, labels = c("Male", "Female")))) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Adds points at each age
  labs(title = "Predicted Probability of In Education by Age and Gender",
       x = "Age",
       y = "Predicted Probability of Being in Education",
       color = "Gender") +  # Renames legend title to 'Gender'
  scale_x_continuous(breaks = seq(min(pred$age), max(pred$age), by = 2)) +  # Sets x-axis breaks every 2 years
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))

# 1 (a) table:

library(stargazer)
library(texreg)
library(officer)

# Calculate summary statistics by gender
# Convert columns to numeric if needed
selected <- time %>%
  mutate(across(c(edu_level, marital_status, type_of_the_day, cook_fuel, lighting, 
                  dwelling, social_group, religion, Sector, age, hhsize, gender, in_educ), 
                ~ as.numeric(as.character(.))))

# Check if the 'gender' column exists in the selected_data
if("gender" %in% colnames(selected_data)) {
  gender_summary <- selected %>%
    group_by(gender) %>%
    summarise(across(c(age, hhsize), 
                     list(
                       Obs. = ~n(),
                       Mean = ~mean(., na.rm = TRUE),
                       Std.Dev. = ~sd(., na.rm = TRUE),
                       Min = ~min(., na.rm = TRUE),
                       Max = ~max(., na.rm = TRUE)
                     ), 
                     .names = "{.col}_{.fn}"))
} else {
  stop("Column 'gender' does not exist in the selected data.")
}

# Calculate overall summary statistics for the total dataset
total_summary <- selected %>%
  summarise(across(c(age, hhsize), 
                   list(
                     Obs. = ~n(),
                     Mean = ~mean(., na.rm = TRUE),
                     Std.Dev. = ~sd(., na.rm = TRUE),
                     Min = ~min(., na.rm = TRUE),
                     Max = ~max(., na.rm = TRUE)
                   ), 
                   .names = "{.col}_{.fn}")) %>%
  mutate(gender = "Total")

# Combine gender and total summary statistics
combined <- bind_rows(gender_summary, total_summary)

# Print the combined summary
print(combined_summary)

# Write to Excel
write_xlsx(gender_summary, "C:/H drive/appl_ecotrix_assgn/Assgn 2/Q5.xlsx")

# List of factor columns to analyze
factor_columns <- c("edu_level", "marital_status", "type_of_the_day", "cook_fuel", 
                    "lighting", "dwelling", "social_group", "religion", "Sector", 
                    "age_grp", "in_educ")

# Create a new workbook
wb <- createWorkbook()

# Loop over each factor column and create summaries by gender, adding them to the workbook
for (factor_col in factor_columns) {
  # Create a summary for the current factor column
  subset_summary <- time %>%
    group_by(gender, !!sym(factor_col)) %>%
    summarise(Count = n(),
              Mean = mean(as.numeric(as.factor(!!sym(factor_col))), na.rm = TRUE),
              Std.Dev = sd(as.numeric(as.factor(!!sym(factor_col))), na.rm = TRUE),
              Min = min(as.numeric(as.factor(!!sym(factor_col))), na.rm = TRUE),
              Max = max(as.numeric(as.factor(!!sym(factor_col))), na.rm = TRUE)) %>%
    ungroup()
  
  # Add a worksheet for the current factor column
  addWorksheet(wb, sheetName = factor_col)
  
  # Write the subset summary to the sheet
  writeData(wb, sheet = factor_col, x = subset_summary)
}

# Save the workbook to an Excel file
saveWorkbook(wb, file = "C:/H drive/appl_ecotrix_assgn/Assgn 2/Qtn.xlsx", overwrite = TRUE)

# Print a message to indicate the file has been saved
print("Excel file with summary statistics has been saved.")


#2 multinomial models
multi_df <- subset(time, select = c("activity", "age", "gender", "edu_level", 
                                  "marital_status", "type_of_the_day", "lnmpce",
                                  "hhsize", "cook_fuel", "lighting", "dwelling",
                                  "social_group", "religion", "Sector"))

multi_df$activity <- relevel(multi_df$activity, ref = "2")

multinomial <- multinom(activity ~  age + gender + edu_level + marital_status + 
                          type_of_the_day + lnmpce + hhsize + cook_fuel + lighting +
                          dwelling + social_group + religion + Sector, data = multi_df)
#output for multinomial
stargazer(multinomial, type = "text", title = "Multinomial Logistic Regression Results", 
          keep.stat = c("n", "ll"), 
          summary = TRUE, 
          digits = 3, 
          dep.var.labels.include = FALSE, 
          covariate.labels = names(coef(multinomial)))

rrr <- exp(coef(multinomial))
#output for multinomial with RRR
stargazer(multinomial, type = "text", coef = list(rrr), p.auto = FALSE, keep.stat = c("n", "ll"))

#2c test for IIA
iia.data <- dfidx(multi_df, shape = "wide", varying = NULL, choice = "activity")

x1 <- mlogit(activity ~ 0 | age + gender + edu_level + marital_status + 
               type_of_the_day + lnmpce + hhsize + cook_fuel + lighting + dwelling +
               social_group + religion + Sector, data = iia.data, reflevel = "2")
x2 <- mlogit(activity ~ 0 | age + gender + edu_level + marital_status + 
               type_of_the_day + lnmpce + hhsize + cook_fuel + lighting + dwelling +
               social_group + religion + Sector, data = iia.data, reflevel = "2",
             alt.subset = c("2", "3"))

#Hausman McFadden Test
hmftest(x1,x2)

#2d
multi_df$activity_c <- ifelse(multi_df$activity %in% c("Edu 1", "NEET 3"), "Educ_NEET", multi_df$activity)
multinomial_c <- multinom(activity_c ~  age + gender + edu_level + marital_status + 
                            type_of_the_day + lnmpce + hhsize + cook_fuel + lighting +
                            dwelling + social_group + religion + Sector, data = multi_df)
#LR Test
lrtest(multinomial, multinomial_c)

#2e
chisq.test(multi_df$religion, multi_df$activity)

#3 tobit models
tobit_df <- subset(time, select = c("activity", "age", "gender", "edu_level", 
                                  "marital_status", "type_of_the_day", "lnmpce", "hhsize",
                                  "cook_fuel", "lighting", "dwelling", "social_group",
                                  "religion", "Sector", "time_learn", "sh_learn_time"))
tobit_df$activity <- relevel(tobit_df$activity, ref = "2")

# Assuming levels of gender are "Male" and "Female", and Sector has "Urban" and "Rural"
tobit_df$Male_Urban <- ifelse(tobit_df$gender == "1" & tobit_df$Sector == "Urban 2", 1, 0) * 1
tobit_df$Male_Rural <- ifelse(tobit_df$gender == "1" & tobit_df$Sector == "Rural 1", 1, 0) * 2
tobit_df$Female_Urban <- ifelse(tobit_df$gender == "2" & tobit_df$Sector == "Urban 2", 1, 0) * 3
tobit_df$Female_Rural <- ifelse(tobit_df$gender == "2" & tobit_df$Sector == "Rural 1", 1, 0) * 4

tobit_df$genderSector <- tobit_df$Male_Rural + tobit_df$Male_Urban + tobit_df$Female_Rural + tobit_df$Female_Urban
tobit_df$genderSector <- factor(tobit_df$genderSector, 
                                levels = c(1, 2, 3, 4), 
                                labels = c("male:rural", "male:urban", "female:rural", "female:urban"))

#3.i time_learn
tobit1 <- censReg(time_learn ~ age + edu_level + marital_status + type_of_the_day +
                    lnmpce + hhsize + cook_fuel + lighting + dwelling + social_group + 
                    religion + activity + gender*Sector, data = tobit_df)
summary(tobit1)

#3.ii log(time_learn)
tobit_df$log_time_learn <- log(tobit_df$time_learn)
#changes made as per cameron trivedi
tobit_df$lntime_learn <- ifelse(tobit_df$time_learn > 0, log(tobit_df$time_learn), NA)
gamma <- min(tobit_df$lntime_learn, na.rm = TRUE)
if (is.na(gamma) || gamma >= 0) gamma <- 0
tobit_df$lntime_learn[is.na(tobit_df$lntime_learn)] <- gamma - 0.0000001

tobit2 <- censReg(lntime_learn ~ age + edu_level + marital_status + type_of_the_day +
                    lnmpce + hhsize + cook_fuel + lighting + dwelling + social_group + 
                    religion + activity + gender*Sector, data = tobit_df)
summary(tobit2)

#3.iii sh_time_learn)
tobit3 <- censReg(sh_learn_time ~ age + edu_level + marital_status + type_of_the_day +
                    lnmpce + hhsize + cook_fuel + lighting + dwelling + social_group + 
                    religion + activity + gender*Sector, data = tobit_df)
summary(tobit3)

#3a
#i time_learn
ggplot(aes(x = time_learn), data = tobit_df) +
  geom_histogram(bins = 10, fill = "#E6E6FA", color = "purple") +
  labs(x = "Time(in min)",y = "Frequency") +
  ggtitle("Time spent on learning")

#ii log_time_learn
ggplot(aes(x = log_time_learn), data = tobit_df) +
  geom_histogram(bins = 10, color = "black") +
  labs(x = "Time(in min)",y = "Frequency") +
  ggtitle("Natural log of Time spent on learning")

#iii sh_time_learn
ggplot(aes(x = sh_learn_time), data = tobit_df) +
  geom_histogram(bins = 10, color = "dark blue", fill = "powder blue") +
  labs(x = "Time(in min)",y = "Frequency") +
  ggtitle("Share of time spent on learning")

#3b
stargazer(tobit1, tobit2, tobit3, type = "text", keep.stat = "n", header = FALSE)

#3c
#using lnmpce and hhsize as IV
first_stage <- multinom(activity ~ lnmpce + hhsize + social_group + Sector + age + gender + 
                          edu_level + marital_status + type_of_the_day + cook_fuel + lighting + 
                          dwelling + religion, data = tobit_df)
tobit_df$activity_pred <- predict(first_stage, type = "probs")[, "Emp 2"]

tobit.iv <- censReg(time_learn ~ activity_pred + age + gender + edu_level + marital_status + 
                      type_of_the_day + lnmpce + hhsize + cook_fuel + lighting + 
                      dwelling + social_group + religion + Sector, data = tobit_df)
stargazer(tobit.iv, type = "text", keep.stat = "n", header = FALSE)
