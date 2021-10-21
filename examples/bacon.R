# analysis of bacon consumption in the national health interview survey.
# open nhis dta and summarize.
library(haven)
nhis_raw <- read_dta("https://github.com/tvogl/econ121/raw/main/data/nhis2000.dta")

summary(nhis_raw)

# drop observations with bacon missing.
# generate a variable that equals one if any bacon consumption, zero otherwise.
nhis <- nhis_raw[!is.na(nhis_raw$bacon),]
table(nhis$bacon)
nhis$anybacon <- ifelse(nhis$bacon > 0, 1, 0)

# generate some covariates.
# gender.
table(nhis$sex)
nhis$male <- 2-nhis$sex

# marital status
table(nhis$marstat)
nhis$married <- ifelse(nhis$marstat < 20, 1, 0)

# COMPARING OLS, LOGIT, AND PROBIT

# estimate a linear probability model, a logit, and a probit.
# then generate predicted probabilities for each of these approaches.
# then compare the predicted probabilities. note that we use
# R's Generalized Linear Model function glm() to estimate the logit and 
# probit models. this package does not allow for robust standard errors,
# so we use concentional standard errors for the probit and logit.
library(estimatr)
ols_model <- lm_robust(anybacon ~ edyrs + age + male + black + hisp + other + married,data = nhis)
summary(ols_model)
nhis$p_ols <- predict(ols_model, nhis, type="response")

probit_model <- glm(anybacon ~ edyrs + age + male + black + hisp + other + married, data = nhis, family=binomial(link="probit"))
summary(probit_model)
nhis$p_probit <- predict(probit_model, nhis, type="response")

logit_model <- glm(anybacon ~ edyrs + age + male + black + hisp + other + married, data = nhis, family=binomial(link="logit"))
summary(logit_model)
nhis$p_logit <- predict(logit_model, nhis, type="response")

predictions <- na.omit(nhis[c("p_ols", "p_probit", "p_logit")]) #collect predictions in a new data frame with no missing values
summary(predictions) #means and quantiles of the predictions
cor(predictions) #correlation matrix for the predictions

# MARGINAL EFFECTS

# now let's compute marginal effects at the means of the independent
# variables. we use the mfx package, which does allow robust SEs, so
# we use them. uncomment below to install.
#install.packages("mfx")
library(mfx)

# marginal effects at the means of the independent variables
logitmfx(anybacon ~ edyrs + age + male + black + hisp + other + married, data = nhis, atmean = TRUE, robust = TRUE)
probitmfx(anybacon ~ edyrs + age + male + black + hisp + other + married, data = nhis, atmean = TRUE, robust = TRUE)

#average of the individual marginal effects
logitmfx(anybacon ~ edyrs + age + male + black + hisp + other + married, data = nhis, atmean = FALSE, robust = TRUE)
probitmfx(anybacon ~ edyrs + age + male + black + hisp + other + married, data = nhis, atmean = FALSE, robust = TRUE)

# ODDS RATIOS

# finally, we can also estimate odds ratios in the logit setting.
# the mfx package also allows estimation of odds ratios:
logitor(anybacon ~ edyrs + age + male + black + hisp + other + married, data = nhis, robust = TRUE)
# these are especially convenient for binary independent variables.
# for instance, men have 48% higher odds of eating bacon than women.