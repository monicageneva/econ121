# This do file uses data from the National Logitudinal Study
# of Young Men to study the returns to schooling. The key
# idea, following Card (1993), is to instrument for years of
# schooling using the distance to the nearest college as the
# instrument. We now understand that this instrument is 
# probably not valid -- distance to college is correlated 
# with many determinants of earnings, violating the exogeneity
# assumption. Nonetheless, it is a useful example to understand
# the mechanics of instrumental variables.
library(haven)
nlsym <- read_dta("https://github.com/tvogl/econ121/raw/main/data/card_college.dta")

# a new way to generate new variables in R, using the dplyr package
library(dplyr)
nlsym <- mutate(nlsym, age2 = age^2)

# a new way to drop observations with missing values in R, using the dplyr package
nlsym <- filter(nlsym, !is.na(lwage))
  
# summarize data
summary(nlsym)

# OLS estimates of the return to schooling
library(estimatr)
lm_robust(lwage ~ edyrs + age + age2 + black + smsa, data = nlsym)

# OLS with some background characteristics
lm_robust(lwage ~ edyrs + age + age2 + black + smsa + daded + momed + sinmom14 + south, data = nlsym)

# From now on, leave out current SMSA residence, which is endogenous to education
# and therefore a "bad control."

# First stage
lm_robust(edyrs ~ nearc2 + nearc4 + age + age2 + black + daded + momed + sinmom14 + south, data = nlsym)
lm_robust(edyrs ~ nearc2 + nearc4a + nearc4b + age + age2 + black + daded + momed + sinmom14 + south, data = nlsym)
lm_robust(edyrs ~ nearc4 + age + age2 + black + daded + momed + sinmom14 + south, data = nlsym)

# Reduced form
lm_robust(lwage ~ nearc4 + age + age2 + black, data = nlsym)
lm_robust(lwage ~ nearc4 + age + age2 + black + daded + momed + sinmom14 + south, data = nlsym)

# Compute IV coefficient as ratio of reduced form to first stage
# no background covariates
lm_robust(edyrs ~ nearc4 + age + age2 + black, data = nlsym)
lm_robust(lwage ~ nearc4 + age + age2 + black, data = nlsym)
0.1262711/0.7304538

# background covariates
lm_robust(edyrs ~ nearc4 + age + age2 + black + daded + momed + sinmom14 + south, data = nlsym)
lm_robust(lwage ~ nearc4 + age + age2 + black + daded + momed + sinmom14 + south, data = nlsym)
0.0891009/0.4456637

# 2SLS estimates of the return to schooling
# We use the ivreg() function from the package AER (stands for Applied Econometrics with R)
# install.packages("AER")
library(AER)
# no background covariates
ivmodel1 <- ivreg(lwage ~ edyrs + age + age2 + black | nearc4 + age + age2 + black, data = nlsym)
coeftest(ivmodel1, vcov = vcovHC(ivmodel1, type="HC2")) # to get robust standard errors

# background covariates
ivmodel2 <- ivreg(lwage ~ edyrs + age + age2 + black + daded + momed + sinmom14 + south | 
                  nearc4 + age + age2 + black + daded + momed + sinmom14 + south,
                  data = nlsym)
coeftest(ivmodel2, vcov = vcovHC(ivmodel2, type="HC2")) # to get robust standard errors

# Is the instrument valid?
# Ideally, would not be correlated with background characteristics.
# It is, which should make us doubt its validity.
lm_robust(nearc4 ~ age + age2 + black + daded + momed + sinmom14 + south, data=nlsym)
library(mfx)
probitmfx(nearc4 ~ age + age2 + black + daded + momed + sinmom14 + south, 
          data=nlsym, atmean = TRUE, robust = TRUE)


