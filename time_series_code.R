#REQUISITE:
install.packages("plm")
install.packages("broom")
install.packages("readxl")
library(readxl)
library(plm)
library(broom)

#loading and reading:
data <- read_excel("C:/Users/haris/Downloads/Singapore_PCGDP_PPP_Data.xlsx")
df1 <- data.frame(data)
lm_model <- lm( "PCGDP_PPP ~ year ", data= df1)
summary(lm_model)

#plotting the lm_model
with(data, {
  plot(year, PCGDP_PPP, main = "Scatter Plot with Regression Line", xlab = "year", ylab = "PCGDP_PPP")
  lm_model <- lm(PCGDP_PPP ~ year, data = data)
  abline(lm_model, col = "blue")
})

# Set margin to accommodate the second axis
par(mar = c(5, 4, 4, 6)) 
plot(data$year,data$PCGDP_PPP, type = "l", col = "blue", lwd = 2,
     xlab = "Year", ylab = "Y", main = "Line Graph of Y and ln(Y) against Year")

# Creating a line graph with dual vertical axes
par(mar = c(5, 4, 4, 6))  # Adjusting margin to accommodate dual axes
plot(data$year, data$PCGDP_PPP, type = "l", col = "blue", lty = 1, lwd = 2,
     xlab = "Year", ylab = "Y", main = "Figure 1")

# Adding the second axis for ln(Y)
par(new = TRUE)
plot(data$year, log(data$PCGDP_PPP), type = "l", col = "red", lty = 2, lwd = 2,
     xlab = "", ylab = "", axes = FALSE)

# Adding axis labels for ln(Y)
axis(side = 4, at = pretty(log(data$PCGDP_PPP)), labels = format(pretty(log(data$PCGDP_PPP))))

# Adding legend
legend("bottomright", legend = c("Y", "ln(Y)"), col = c("blue", "red"), lty = c(1, 2), lwd = 2,)
data$ln_y <- log(data$PCGDP_PPP)

#converting to time trnd variable
data$time_trend <- seq_along(data$year)

#regressing lnY on time trend variable
lm_model2 <- lm( data$ln_y~ data$time_trend, data= data)
summary(lm_model2)

#listing table
data$t <- data$year - 2010
data$tln_y <- data$ln_y * data$t

#plotting lm_model2
with(data, {
  plot(time_trend, ln_y, main = "Scatter Plot with time trend", xlab = "time_trend", ylab = "ln_y")
  lm_model2<- lm(data$ln_y ~ data$time_trend, data = data)
  abline(lm_model2, col = "blue")
})

#fitted ln_y values:
data$obs_lny <- data$ln_y
data$fit_lny <- predict(lm_model2, data)
error_term <- data$obs_lny- data$fit_lny
data$error_term <- error_term

#finding RSS:
RSS <- sum(lm_model2$error_term^2)

#finding TSS:
TSS <- sum((data$ln_y - data$fit_lny)^2)

#finding ESS:
ESS <- sum((data$fit_lny)^2) - (sum(25 * mean(data$ln_y)^2))

#finding rsqrd:
r2 <- 1 - RSS/TSS

#plotting ln_y and and fit_lny:
figure2 <- plot(data$obs_lny, data$fit_lny, main = "figure 2", lty = 1, lwd = 1 , xlab = "ln_y", ylab = "fit_lny")

#
sxx <- sum((data$time_trend)^2) - (25 * mean(data$time_trend)^2)

#
sxy <- sum((data$time_trend) * (data$ln_y)) - 

#    
syy <- sum((data$ln_y)^2) - (sum(25 * mean(data$ln_y)^2))

#beta_hat:
beta_hat <- sxy/sxx

#alpha_hat:
alpha_hat <- mean(data$ln_y) - beta_hat * mean(data$time_trend)

#predicted mathematical gdp:
maths <- alpha_hat + beta_hat* 26


#plotting residual term against time_trend variable:
figure3 <- plot(data$time_trend, data$error_term, main = "figure 3",lty = 1, lwd = 1 , xlab = "time_trend", ylab = "residual")
abline(h = 0, col = "red", lty = 1)
lines(data$time_trend, data$error_term, type = "o", col = "blue")

#1 (a)regressing ln_y on t:
lm_model5 <- lm(data$ln_y ~ data$t, data = data)
summary(lm_model5)

#  (b) regressing ln_y on time_trend:
lm_model6 <- lm(data$ln_y ~ data$time_trend, data = data)
summary(lm_model6)                

#  (c) regressing ln_y on year:
lm_model7 <- lm(data$ln_y ~data$year, data = data)
summary(lm_model7)

#2 (b) regressing lnY* on time_trend:
data$ydollars <- data$PCGDP_PPP/1000 
data$lnYcap <- log(data$ydollars)
lm_model8 <- lm(data$lnYcap ~ data$time_trend)
summary(lm_model8)

#3 (a) regressing PCGDP_PPP on t:
lm_model9 <- lm(data$PCGDP_PPP ~ data$t)
summary(lm_model9)

#  (b) regressing PCGDP_PPP on year:
lm_model10 <- lm(data$PCGDP_PPP ~ data$year)
summary(lm_model10)

#  (c) regressing lnYcap on t:
lm_model11 <- lm(data$lnYcap ~ data$t)
summary(lm_model11)
