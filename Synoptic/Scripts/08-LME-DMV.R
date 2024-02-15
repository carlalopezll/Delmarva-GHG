# Linear mixed effects model analysis
# Carla Lopez Lloreda
# For Delmarva synoptic data
# Last updated 10/28/2022

library(lme4)
library(dplyr)
library(ggplot2)
library(methods) # for geom_stat
library(tidyverse)
library(viridis)
library(ggpubr) # for p values and r-squared on graph
library(emmeans)
library(lmerTest)
library(performance) # for collinearity checks
library(nlme)
library(lubridate)

citation("lme4")
citation("nlme")

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read in data
merge <- read.csv("Master spreadsheet.csv")

merge$Date_corrected <- as.Date(parse_date_time(merge$Date_corrected, c("mdy", "ymd")))

# SW <- wl_chem

SW <- filter(merge, Sample_Type == "SW")
SW <- filter(SW, Site_dry == "No")

theme <- theme_bw() + theme(axis.text = element_text(size = "14"), axis.title = element_text(size=18))


# Check distribution of problematic variables and test for normality
# Then log-transform if needed

shapiro.test(SW$CO2_uM)

# Select columns to test
columns_to_test <- SW[, 7:8]
columns_to_test <- SW[, 15:19]
columns_to_test <- SW[, 30:50]


# Apply Shapiro-Wilk test to each column
shapiro_results <- lapply(columns_to_test, shapiro.test)

# Display results
for (i in seq_along(shapiro_results)) {
  cat("Column:", names(columns_to_test)[i], "\n")
  cat("Test statistic:", shapiro_results[[i]]$statistic, "\n")
  cat("p-value:", shapiro_results[[i]]$p.value, "\n")
  cat("\n")
}

hist(SW$TDP)
SW$TDP_log <- log(SW$TDP)
hist(SW$TDP_log)

hist(SW$SO4)
SW$SO4_log <- log(SW$SO4)
hist(SW$SO4_log)
SW$Cl_log <- log(SW$Cl)

SW$CO2_log <- log10(SW$CO2_uM)
SW$CH4_log <- log10(SW$CH4_uM)


# Mixed effects model

# lmer(y ~ x + (1 | group), df) # with varying intercept
# lmer(y ~ x + (x | group), df) # correlated varying slope and intercept

# Mixed-effects model
  # 1. First model should have everything, then you can use step() to evaluate important variables
  # 2. Remove variables that are highly correlated
  # 3. Remove variables with few data points since this makes the model fail
  # 4. Re-scale variables that might be problematic


fm1_CO2 <- lmer(CO2_uM ~ Date_corrected + pH + DO_percent + Cl_log + CH4_uM + SpC + Temp_C + NPOC + d18O + TDN + TDP_log + SO4_log+ NO3 + (1 | Site), data = SW, REML = TRUE)

m1 <- summary(fm1_CO2)
m1

# Save objects
m1_residuals <- as.data.frame(m1$residuals)

write.csv(m1_residuals, "LMMs/CO2/FM1_CO2_residuals.csv", row.names = FALSE)

m1_coef <- as.data.frame(m1$coefficients)
write.csv(m1_coef, "LMMs/CO2/FM1_CO2_coefficients.csv")

# Evaluating multicollinearity in the model
check_collinearity(fm1_CO2)

# Plot results for collinearity
plot(check_collinearity(fm1_CO2))

# Look at model performance
check_model(fm1_CO2)

# Plot the fitted vs the residuals
plot(fm1_CO2)

# Identify significant variables
anova(fm1_CO2)
step(fm1_CO2, direction = "both")


# Use corCAR for uneven time-series (use corAR if equal time-series)
# Form = ~ Time | Grouping
# This includes and accomodates temporal autocorrelation

corr_resCO2_1 <- lme(
  CO2_uM ~ pH + DO_mgL + DO_percent + CH4_uM + SpC + Temp_C + NPOC + d18O + d2H + TDN + TDP_log + SO4_log + NO3 + NH3 + X54Fe + X55Mn + dly_mean_wtrlvl, method = "REML", 
  data = SW,
  random = ~ 1 | Site,
  correlation = corCAR1(form = ~ Date_corrected),
  na.action=na.exclude
)

summary(corr_resCO2_1)
anova(corr_resCO2_1)

# Evaluating multicollinearity in the model
check_collinearity(corr_resCO2_1)
plot(check_collinearity(corr_resCO2_1))

# Removed because of high collinearity
# How do you decide which of the variables to include?
# DO_percent, d18O, TDN

corr_resCO2_2 <- lme(
  CO2_uM ~ pH + DO_mgL + CH4_uM + SpC + Temp_C + NPOC + d18O + d2H + TDP_log + SO4_log + NO3 + NH3 + X54Fe + X55Mn + dly_mean_wtrlvl, method = "REML", 
  data = SW,
  random = ~ 1 | Site,
  correlation = corCAR1(form = ~ Date_corrected),
  na.action=na.exclude
)

summary(corr_resCO2_2)
anova(corr_resCO2_2)

# Remove non-significant variables

corr_resCO2_3 <- lme(
  CO2_uM ~ pH + DO_mgL + CH4_uM + Temp_C + dly_mean_wtrlvl, method = "REML", 
  data = SW,
  random = ~ 1 | Site,
  correlation = corCAR1(form = ~ Date_corrected),
  na.action=na.exclude
)

summary(corr_resCO2_3)
anova(corr_resCO2_3)
check_model(corr_resCO2_3)
model_performance(corr_resCO2_3)


# Plot predicted vs actual values: CO2

modeled_values_CO2 <- as.data.frame(predict(corr_resCO2_3))

predict_CO2 <- merge(SW, modeled_values_CO2, by = "row.names", all.x = TRUE)

ggplot(predict_CO2, aes(x= `predict(corr_resCO2_3)`, y = CO2_uM)) +
  geom_point() +
  geom_abline(a=1, b=0) +
  labs(x = "Predicted", y = "Actual", tag = "a)") +
  theme

# Getting r-squared of predicted vs actual
summary(lm(`predict(corr_resCO2_3)`~CO2_uM, predict_CO2))

ggsave("Graphs/MS/CO2 model performance.jpg")


# Calculating residuals

predict_CO2$residual <- predict_CO2$CO2_uM-predict_CO2$`predict(corr_resCO2_3)`

plot(corr_resCO2_3)
qqnorm(resid(corr_resCO2_3))

summary(predict_CO2$residual)


ggplot(predict_CO2, aes(x= `predict(corr_resCO2_3)`, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Predicted", y = "Residuals", tag = "c)") +
  theme


ggsave("Graphs/CO2 residuals.jpg")

# CH4 models with temporal autocorrelation

corr_resCH4_1 <- lme(
  CH4_uM ~ pH + DO_mgL + DO_percent + CO2_uM + SpC + Temp_C + NPOC + d18O + d2H + TDN + TDP_log + SO4_log + NO3 + NH3 + X54Fe + X55Mn + dly_mean_wtrlvl, method = "REML", 
  data = SW,
  random = ~ 1 | Site,
  correlation = corCAR1(form = ~ Date_corrected),
  na.action=na.exclude
)

# Evaluating multicollinearity in the model
check_collinearity(corr_resCH4_1)
plot(check_collinearity(corr_resCH4_1))

summary(corr_resCH4_1)
anova(corr_resCH4_1)


corr_resCH4_2 <- lme(
  CH4_uM ~ pH + DO_mgL + CO2_uM + SpC + Temp_C + NPOC + d18O + d2H + TDP_log + SO4_log + NO3 + NH3 + X54Fe + X55Mn + dly_mean_wtrlvl, method = "REML", 
  data = SW,
  random = ~ 1 | Site,
  correlation = corCAR1(form = ~ Date_corrected),
  na.action=na.exclude
)

summary(corr_resCH4_2)
anova(corr_resCH4_2)


corr_resCH4_3 <- lme(
  CH4_uM ~ pH + CO2_uM + NPOC + NH3, method = "REML", 
  data = SW,
  random = ~ 1 | Site,
  correlation = corCAR1(form = ~ Date_corrected),
  na.action=na.exclude
)

summary(corr_resCH4_3)
anova(corr_resCH4_3)

# Checking model performance

check_model(corr_resCH4_3)
model_performance(corr_resCH4_3)

# Checking residuals of the model

qqnorm(resid(corr_resCH4_3))
plot(corr_resCH4_3)


# Plot predicted vs actual values: CH4

modeled_values_CH4 <- as.data.frame(predict(corr_resCH4_3))

predict_CH4 <- merge(SW, modeled_values_CH4, by = "row.names", all.x = TRUE)

ggplot(predict_CH4, aes(x= `predict(corr_resCH4_3)`, y = CH4_uM)) +
  geom_point() +
  geom_abline(a=1, b=0) +
  labs(x = "Predicted", y = "Actual", tag = "b)") +
  theme

# Getting r-squared of predicted vs actual
summary(lm(`predict(corr_resCH4_3)`~CH4_uM, predict_CH4))

ggsave("Graphs/MS/CH4 model performance.jpg")


# Calculating residuals

predict_CH4$residual <- predict_CH4$CH4_uM-predict_CH4$`predict(corr_resCH4_3)`

plot(corr_resCH4_3)
qqnorm(resid(corr_resCH4_3))

summary(predict_CH4$residual)


ggplot(predict_CH4, aes(x= `predict(corr_resCH4_3)`, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Predicted", y = "Residuals", tag = "d)") +
  theme


ggsave("Graphs/CH4 residuals.jpg")





# Checking out potential correlations between variables

ggplot(SW, aes(x= SO4, y = SpC)) +
  geom_point()

ggplot(SW, aes(x= NO3, y = CO2_uM)) +
  geom_point()

ggplot(SW, aes(x= SO4, y = dly_mean_wtrlvl)) +
  geom_point()


ggplot(SW, aes(x= NH3, y = TDN)) +
  geom_point()

ggplot(SW, aes(x= DO_percent, y = DO_mgL)) +
  geom_point()

ggplot(SW, aes(x= pH, y = Temp_C)) +
  geom_point()

ggplot(SW, aes(x= d18O, y = d2H)) +
  geom_point()

ggplot(SW, aes(x= TDP, y = TDN)) +
  geom_point()

ggplot(SW, aes(x= NPOC, y = TDP_log)) +
  geom_point()

ggplot(SW, aes(x= NPOC, y = TDN)) +
  geom_point()

ggplot(SW, aes(x= NPOC, y = NO3)) +
  geom_point()

ggplot(SW, aes(x= d, y = CO2_uM)) +
  geom_point() +
  geom_smooth()

ggplot(SW, aes(x= d, y = CH4_uM)) +
  geom_point()

ggplot(SW, aes(x= TDN, y = CH4_uM)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(SW, aes(x= Date_corrected, y = d)) +
  geom_point()

# Getting the model coefficients
# This is what goes in the table
coef(fm1_CO2)
coef <- coef(summary(fm1_CO2))
coef


# Improving the first model by:
  # 1. Removing highly correlated variables
  # 2. Including significant variables

fm2_CO2 <- lmer(CO2_uM ~ DO_percent + CH4_uM + Temp_C +
                  (1 | Site), data = SW, REML = TRUE)

m2 <- summary(fm2_CO2)
m2

# Checking residuals of the model
qqnorm(resid(fm2_CO2))

sum(m2$residuals^2) # 51.414
anova(fm2_CO2)
step(fm2_CO2)

# Checking residuals of the model
qqnorm(resid(fm2_CO2))

# Save objects
m2_residuals <- as.data.frame(m2$residuals)
write.csv(m2_residuals, "LMMs/CO2/FM2_CO2_residuals.csv", row.names = FALSE)

m2_coef <- as.data.frame(m2$coefficients)
write.csv(m2_coef, "LMMs/CO2/FM2_CO2_coefficients.csv")

# Finalizing model by:
  # Finding the best combination of number of parameters + low AIC/BIC

fm3_CO2 <- lmer(CO2_uM ~ Date_corrected + DO_percent + CH4_uM + 
                  (1 | Site), SW, REML = FALSE)

m3 <- summary(fm3_CO2)
m3

anova(fm3_CO2)
step(fm3_CO2)

# Save objects
m3_residuals <- as.data.frame(m3$residuals)
write.csv(m3_residuals, "LMMs/CO2/FM3_CO2_residuals.csv", row.names = FALSE)

m3_coef <- as.data.frame(m3$coefficients)
write.csv(m3_coef, "LMMs/CO2/FM3_CO2_coefficients.csv")

# Checking residuals of the model
qqnorm(resid(fm3_CO2))

check_model(fm3_CO2)
model_performance(fm3_CO2)
plot(fm3_CO2)


# Why doesn't DOC show up as an important variable?
# Is it because the relationships by site are different?
 # YES!

ggplot(SW, aes(x= NPOC, y= CO2_uM, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Comparing model performance between the null (full model), and reduced models

AIC(fm1_CO2, fm2_CO2, fm3_CO2)
BIC(fm1_CO2, fm2_CO2, fm3_CO2)

myAIC <- AIC(fm1_CO2, fm2_CO2, fm3_CO2)
delta <- myAIC$AIC - min(myAIC$AIC)

L <- exp(-0.5 * delta)
w <- L/sum(L)


# Plot predicted vs actual values

modeled_values3 <- as.data.frame(predict(fm3_CO2))

predict3 <- merge(SW, modeled_values3, by = "row.names", all.x = TRUE)
  # need to make sure this merging is being done right

ggplot(predict3, aes(x= CO2_uM, y = `predict(fm3_CO2)`)) +
  geom_point() +
  geom_abline(a=1, b=0) +
  theme +
  xlim(0,1100) +
  labs(x = "Actual", y = "Predicted", tag = "a")

ggsave("Graphs/MS/CO2 model performance.jpg")



# Compare model performance and save this info
model_performance <- compare_performance(fm1_CO2, fm2_CO2, fm3_CO2)
write.csv(model_performance, "LMMs/CO2/Performance comparison.csv", row.names = FALSE)






# Creating a function that calculates estimated values

mCO2 <- function(Date, DO, CH4, SO4) {
  CO2 <- A* 7.36 + B * DO_estimate + C * CH4_estimate + D*SO4_estimate
}





#### CH4 ####

# Model selection for CH4 using step ()
lm_CH4 <- lm(CH4_uM ~ pH + DO_percent + CO2_uM + SpC + Temp_C, data = SW)

step(lm_CH4, direction = "both")
anova(lm_CH4)

# Mixed effects model with site as a random variable 
fm1_CH4 <- lmer(CH4_uM ~ Date_corrected + pH + DO_percent + CO2_uM + SpC + Temp_C + NPOC +
                  d18O + TDN + TDP_log + SO4_log + 
                  (1 | Site), data = SW, REML = FALSE)

summary(fm1_CH4)
anova(fm1_CH4)
step(fm1_CH4)

coef <- coef(summary(fm1_CH4))
coef(fm1_CH4)

plot(ranef(fm1_CH4))
coef(fm1_CO2)$'Site'

str(resid(fm1_CH4))

# Checking residuals of the model
qqnorm(resid(fm1))


# Getting the model coefficients
# This is what goes in the table
coef(fm1_CH4)
coef <- coef(summary(fm1_CH4))
coef


# Improving the first model by:
# 1. Removing highly correlated variables
# 2. Including significant variables

fm2_CH4 <- lmer(CH4_uM ~ Date_corrected + CO2_uM + Temp_C + TDN +
                  (1 | Site), data = SW, REML = FALSE)

m2 <- summary(fm2_CH4)
m2

step(fm2_CH4)

# Save objects
m2_residuals <- as.data.frame(m2$residuals)
write.csv(m2_residuals, "LMMs/CH4/FM2_CH4_residuals.csv", row.names = FALSE)

m2_coef <- as.data.frame(m2$coefficients)
write.csv(m2_coef, "LMMs/CH4/FM2_CH4_coefficients.csv")

# Checking residuals of the model
qqnorm(resid(fm2_CH4))

sum(m2$residuals^2) # 51.414
anova(fm2_CO2)
step(fm2_CO2)

# Checking residuals of the model
qqnorm(resid(fm2_CO2))

# Finalizing model by:
# Finding the best combination of number of parameters + low AIC/BIC

fm3_CH4 <- lmer(CH4_uM ~ Date_corrected + CO2_uM + Temp_C + TDN +
                  (1 | Site), SW, REML = FALSE)

m3 <- summary(fm3_CH4)
m3

step(fm3_CH4)

# Plot predicted vs actual values

modeled_values3 <- as.data.frame(predict(fm3_CH4))

predict3 <- merge(SW, modeled_values3, by = "row.names", all.x = TRUE)
# need to make sure this merging is being done right

ggplot(predict3, aes(x= CH4_uM, y = `predict(fm3_CH4)`)) +
  geom_point() +
  geom_abline(a=1, b=0) +
  theme +
  labs(x = "Actual", y = "Predicted", tag = "b")

ggsave("Graphs/MS/CH4 model performance.jpg")


# Checking residuals of the model
qqnorm(resid(fm2_CO2))

check_model(fm2_CO2)
model_performance(fm2_CO2)
plot(fm2_CO2)

# Why doesn't DOC show up as an important variable?
# Is it because the relationships by site are different?
# YES!

ggplot(SW, aes(x= NPOC, y= CO2_uM, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Same thing but reversed for TDN and CH4
# Why is TDN significant in the model but not super strong for regressions?

ggplot(SW, aes(x= TDN, y= CH4_uM, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Comparing model performance between the null (full model), and reduced models

AIC(fm1_CH4, fm2_CH4, fm3_CH4)
BIC(fm1_CO2, fm2_CO2, fm3_CO2)



#### Checking for temporal autocorrelation ####

library(car)
durbinWatsonTest(lm_CO2)

# Extract results

# Durbin Watson test
CO2_autocor <- durbinWatsonTest(lm_CO2)

# Breusch-Godfrey test 
library(lmtest)

unique_sites <- unique(SW$Site)

# Create an empty data frame to store the results
results <- data.frame(Site = character(0), p_value = numeric(0))

# Loop through the unique sites
for (site in unique_sites) {
  # Subset the data for the current site
  subset_data <- SW[SW$Site == site, ]
  
  # Fit a linear model
  lm_model <- lm(CO2 ~ Date_corrected + pH + DO_percent + CH4_uM + SpC + Temp_C + 
                   Cl + SO4 + NPOC + NO3, data = subset_data)
  
  # Perform the Breusch-Godfrey test
  bg_test <- bgtest(lm_model, order = 1, data = subset_data)
  
  # Extract the p-value
  p_value <- bg_test$p.value
  
  # Store the results in the data frame
  results <- rbind(results, data.frame(Site = site, p_value = p_value))
}


lm_CO2 <- lm(CO2_uM ~ Date_corrected + pH + DO_percent + CH4_uM + SpC + Temp_C + 
               Cl + SO4 + NPOC + NO3, data = SW[SW$Site == "DK", ])

bgtest((lm_CO2), order=1, data=SW[SW$Site == "DK", ])
bgtest((lm_CH4), order=1, data=SW)

# Ljung-Box test

Box.test(SW$pH, lag = 10, type = "Ljung-Box")


library(DHARMa)

# Standard use

fittedModel <- lm(CO2_uM ~ Date_corrected + pH + DO_percent + CH4_uM + SpC + Temp_C + 
                    Cl + SO4 + NPOC + NO3, data = SW [SW$Site == "DK", ])

res = simulateResiduals(fittedModel)

testTemporalAutocorrelation(res, time =  SW$Date_corrected)

# But...you actually have a lot of sites so you want to group by date

SW <- SW %>%
  mutate(yymm = format(ymd(Date_corrected), "%y-%m"))

str(SW$yymm)

simulationOutput1 <- recalculateResiduals(res, group=SW$yymm)
simulationOutput1$group

testTemporalAutocorrelation(simulationOutput1, time =  SW$yymm)

# testing only subgroup location 1, could do same with loc 2
res = recalculateResiduals(res, sel = SW$Site == "DK")
testTemporalAutocorrelation(res, time = unique(SW$Date_corrected))

length(unique(SW$Date_corrected))
length(res)



library(spacetime)
library(gstat)
library(lattice)

# Create an ACF plot
# No missing values

site_names <- unique(SW$Site)

for (i in 1:length(site_names)) {
  site <- get(site_names[i])
  
  # Create an ACF plot for the current site
  acf_plot <- autoplot(acf(site), main = paste("ACF Plot for", site_names[i]))
  
  # Print the ACF plot for the current site
  print(acf_plot)
}

library()

pacf

