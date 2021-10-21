# This R script presents solutions to ECON 121 Problem Set 1

#############
##Problem 1##
#############
  
# If education and experience are exogenous, then beta1 represents
# the causal effect of education on log wages. The quantitative
# interpretation is that each additional year of education raises
# wages by 100*beta1 percent.

# The squared term in experience allows for wages to vary non-
# linearly with experience. For instance, we might expect wages
# to rise with experience at a decreasing rate. In this case, 
# beta3 would be positive and beta4 would be negative.

#############
##Problem 2##
#############

# We open the CPS dataset, then describe and summarize the data
library(haven)
cps18_raw <- read_dta("https://github.com/tvogl/econ121/raw/main/data/cps_18.dta")

summary(cps18_raw)

# Drop observations with <50 weeks or <35 hours or missing hours 
# or 0 income (which will drop when we take logs anyway) -> new data frame cps18
cps18 <- cps18_raw[which(cps18_raw$uhrsworkt >= 35 & cps18_raw$uhrsworkt <= 170 &
                         cps18_raw$incwage>0 & cps18_raw$wkswork1>=50),]

# Generate new variables
cps18$lnw <- log(cps18$incwage/(cps18$wkswork1*cps18$uhrsworkt))

cps18$black <- ifelse(cps18$race == 200, 1, 0)
cps18$other <- ifelse(cps18$race > 200, 1, 0)

table(cps18$educ) # assign midpoints of multigrade categories, 
                  # which are visible in the value labels in Stata
library(dplyr)
cps18$edyrs <- case_when((cps18$educ == 2)                           ~ 0,
                         (cps18$educ == 10)                          ~ 2.5,
                         (cps18$educ == 20)                          ~ 5.5,
                         (cps18$educ == 30)                          ~ 7.5,
                         (cps18$educ == 40)                          ~ 9,
                         (cps18$educ == 50)                          ~ 10,
                         (cps18$educ == 60)                          ~ 11,
                         (cps18$educ >= 71 & cps18$educ <= 73)       ~ 12,
                         (cps18$educ >= 81 & cps18$educ <= 92)       ~ 14,
                         (cps18$educ == 111)                         ~ 16,
                         (cps18$educ == 123)                         ~ 18,
                         (cps18$educ == 124)                         ~ 18.5,
                         (cps18$educ == 125)                         ~ 20)
table(cps18$edyrs)

cps18$exper <- cps18$age - cps18$edyrs - 5
cps18$exper2 <- cps18$exper^2

cps18$female <- ifelse(cps18$sex == 2, 1, 0)

# Summarize the new variables. The mean log wage is 3.13, or 
# approximately $23/hour. Education averages 14 years, with a 
# standard deviation of 2.7. Experience averages 24 years, with
# a standard deviation of 11. 12 percent of the sample is black.
library(Rmisc)
summarySE(cps18,"lnw")
summarySE(cps18,"edyrs")
summarySE(cps18,"exper")
summarySE(cps18,"black")
summarySE(cps18,"other")
summarySE(cps18,"female")

#############
##Problem 3##
#############
  
# Estimate the Mincerian regression. The estimated return is
# 0.113, or an 11 percent wage increase per year of education.
library(estimatr)
lm_robust(lnw ~ edyrs + exper + exper2,data = cps18)

#############
##Problem 4##
#############

# The extended Mincerian regression yields an estimated 
# return of 0.117, which is similar but slightly larger
# than the original estimate.
lm_robust(lnw ~ edyrs + exper + exper2 + female + black + other,data = cps18)

#############
##Problem 5##
#############

# The female-male wage gap is 0.272 log points, while the
# black white gap is -0.167 log points, leading to a 
# difference of 0.106, which is significant at the 0.1% level.
library(multcomp)
extended <- lm_robust(lnw ~ edyrs + exper + exper2 + female + black + other,data = cps18)
summary(glht(extended, linfct = c("black - female = 0")))

#############
##Problem 6##
#############

# If we run separate regressions for men and women, we
# find a return of 0.111 for men and 0.126 for women, 
# implying a difference in returns of 0.0145 log points
# or 1.45 percent.
models_by_sex <- 
  cps18 %>% 
  group_by(female) %>% 
  do(model = lm_robust(lnw ~ edyrs + exper + exper2 + black + other, data = .))
models_by_sex$model
# To assess whether the difference is significant, we
# can compute the t-statistic using the coefficients and
# standard errors. That's because the male and female samples
# are independent, so the coefficients have no covariance.
# The t-statistic is 5.84, so the difference in coefficients
# is significant at the 5% level.
(.1256933-.1111288)/sqrt(.0019207^2+.0015886^2)

#############
##Problem 7##
#############

# To match the approach in Problem 6, we need to allow
# ALL of the coefficients to vary by gender, so we need
# many interaction terms. The coefficient on the interaction
# term between education and female is 0.0145, with a 
# t-statistic of 5.84, just as in Problem 6!
cps18$edyrs_f <- cps18$edyrs*cps18$female
cps18$exper_f <- cps18$exper*cps18$female
cps18$exper2_f <- cps18$exper2*cps18$female
cps18$black_f <- cps18$black*cps18$female
cps18$other_f <- cps18$other*cps18$female
interacted <- lm_robust(lnw ~ edyrs + edyrs_f + exper + exper_f + exper2 + exper2_f +
                              black + black_f + other + other_f + female
                        ,data = cps18)
interacted

# Using the delta method to estimate the ratio of returns, we
# find a female/male ratio of 1.13. The 95% confidence interval
# starts at 1.085, so we can reject the null hypothesis that 
# the ratio is 1 at the 5% level.
library(car)
deltaMethod(interacted,"(edyrs+edyrs_f)/edyrs",rhs=1)

# Alternatively, we could have used the bootstrap, which
# leads to the same answer.
ratio <- function(x) (coef(x)[2] + coef(x)[3])/coef(x)[2]
Boot(interacted,f = ratio,method = c("case"),R = 99)

#############
##Problem 8##
#############

# Open the NLSY dataset
# nlsy79 <- read_dta("https://github.com/tvogl/econ121/raw/main/data/nlsy79")
nlsy79_raw <- read_dta("/Users/tvogl/Dropbox/courses/econ121/problem sets/nlsy - ps 1/nlsy79.dta")

# The sample is 25% black and 16% Hispanic, but when we use 
# sampling weights, those shares fall to 14% black and 6% 
# Hispanic. Because the sampling weights undo the NLSY's over-
# sampling, the latter estimates are representatives of US
# adults who were teenagers residing in the United States
# in 1979. The weighted statistics provide unbiased estimates
# of the population racial composition, since they restore
# representativeness in the sample.
mean(nlsy79_raw$black)
weighted.mean(nlsy79_raw$black,nlsy79_raw$perweight)
mean(nlsy79_raw$hisp)
weighted.mean(nlsy79_raw$hisp,nlsy79_raw$perweight)

#############
##Problem 9##
#############

# Keep full time workers with positive, non-missing earnings
nlsy79 <- nlsy79_raw[which(nlsy79_raw$hours07 >= 35*50 & nlsy79_raw$laborinc07>0),]

# Generate new variables
nlsy79$lnw <- log(nlsy79$laborinc07/nlsy79$hours07)

nlsy79$exper <- nlsy79$age79 + (2007-1979) - nlsy79$educ - 5
nlsy79$exper2 <- nlsy79$exper^2

# Estimates of the Mincerian return to education are quite 
# similar using OLS (0.115) and WLS (0.119). Since the 
# unweighted OLS estimates are more precise (consistent 
# with the Guass Markov theorem), I will continue the 
# analysis with unweighted regressions. (I could have
# also said that I prefer to have results that are 
# representative of the coefficient I would obtain in
# the full population, which would have led me to run
# weighted regressions for the rest of the analysis.)
lm_robust(lnw ~ educ + exper + exper2 + black + hisp + male,data = nlsy79)
lm_robust(lnw ~ educ + exper + exper2 + black + hisp + male,weight = perweight,data = nlsy79)

# For later, let's also note the sample size for these regressions.
nobs(lm_robust(lnw ~ educ + exper + exper2 + black + hisp + male,data = nlsy79))

##############
##Problem 10##
##############

# The extended Mincerian regression yields an estimated 
# return to education of 12 percent, similar to the CPS.
# However, while the CPS had a significant positive coef-
# ficient on exper and a significant negative coefficient
# on exper2, these coefficients are insignificantly 
# different from zero in the NLSY. This difference is 
# likely due to the fact that all NLSY respondents are 
# middle-aged in 2007, with substantial potential experience.
# The CPS indicated that wages rise with experience at labor
# market entry but then flatten out (due to the negative 
# squared term). Since the NLSY in 2007 only had mature 
# workers, the dataset is not well-suited for estimating
# the returns to experience.

##############
##Problem 11##
##############

# It seems unlikely that the coefficient on education 
# represents the causal effect of education. Education
# and wages are likely to be correlated with a number of
# omitted variables, such as innate ability and parental
# socioeconomic status.

##############
##Problem 12##
##############

# To address the concerns above, we can control for 
# childhood background characteristics and cognitive
# test scores. Doing so reduces the estimated return to 
# education substantially, to 6 percent.
lm_robust(lnw ~ educ + exper + exper2 + black + hisp + male +
                foreign + urban14 + mag14 + news14 + lib14 +
                educ_mom + educ_dad + numsibs + afqt81,data = nlsy79)

# However, note that the sample size changed because
# we do not have all control variables for all 
# observations. To see this, let's re-estimate the model
# using lm() and save it, so that we can get the sample
# size and estimation sample.
extended <- lm(lnw ~ educ + exper + exper2 + black + hisp + male +
                      foreign + urban14 + mag14 + news14 + lib14 +
                      educ_mom + educ_dad + numsibs + afqt81,data = nlsy79)
nobs(extended)

# We should reestimate the "short" model
# to make sure that the change in coefficients is not
# due to sample composition. (This was not essential
# for full credit, but it is good practice.)

esample <- rownames(as.matrix(resid(extended)))
nlsy79_extended <- nlsy79[esample,]
lm_robust(lnw ~ educ + exper + exper2 + black + hisp + male,data = nlsy79_extended)

#check the sample size
nobs(lm_robust(lnw ~ educ + exper + exper2 + black + hisp + male,data = nlsy79_extended))

# Why did the return fall when we included additional
# covariates? It appears that urban residence, paternal
# education and AFQT scores (a measure of innate ability)
# are all positively related with wages, and it is likely
# that they also predict higher education. (You can check this.)





