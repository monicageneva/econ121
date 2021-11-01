# This R script presents solutions to ECON 121 Problem Set 2.

# Note: You may have noticed that the dataset includes a
#       primary sampling unit identifier and a sampling 
#       weight. It would be reasonable to cluster standard
#       errors at the PSU level and weight using sampling
#       weights, but these details were not the focus of the
#       problem set and are therefore not required. You may
#       have also noticed slight overlap in the white, black,
#       and hisp variables, which were supposed to be mutually
#       exclusive. This nuance was also not the point of the
#       problem set, so you were not required to correct it.  
#       My code ignores both these issues.

 # # # # # #
# Problem 1 # 
 # # # # # #
  
# Summary statistics appear below. 14 percent of the sample 
# reports being in fair or poor health, and 8 percent died 
# died within 5 years of the survey. The sample includes
# adults 25 and up, with a median age of 46. (The mean age
# is less meaningful because age was top-coded at 85. You
# did not need to notice this.) 57 percent of the sample
# is female, perhaps surprisingly. This gender imbalance
# has two sources. First, men and women responded to the 
# survey at different rates, so the gender imbalance shrinks
# when we use the sampling weights. Second, men die at 
# higher rates than women, so the gender imbalance grows
# with age.

library(haven)
nhis <- read_dta("https://github.com/tvogl/econ121/raw/main/data/nhis2000.dta")

prop.table(table(nhis$health)) # prop.table converts frequencies into proportions
nhis$fpoor <- ifelse(nhis$health > 3, 1, 0)

summary(nhis)

prop.table(table(nhis$age)) 

prop.table(table(nhis$sex)) # 57% female
mean(nhis$sex) # 57% female
weighted.mean(nhis$sex,w = nhis$sampweight) # 52% female once we use weights

 # # # # # #
# Problem 2 # 
 # # # # # #
  
# I draw local linear regressions with bandwidths of 1 year.
# 5-year mortality is higher for people with fair/poor health
# than for people with good/very good/excellent health. In
# both groups, 5-year mortality rises non-linearly with age.
# Since local linear regression fits a linear regression within
# the bandwidth around each grid point, this method is the non-
# parametric version of the linear probability model.
poorhealth <- subset(nhis, fpoor==1 & !is.na(mort5), select=c(age, mort5))
notpoorhealth <- subset(nhis, fpoor==0 & !is.na(mort5), select=c(age, mort5))
library(KernSmooth)
poorreg <- data.frame(locpoly(x=poorhealth$age, y=poorhealth$mort5, bandwidth=1, degree=1))
notpoorreg <- data.frame(locpoly(x=notpoorhealth$age, y=notpoorhealth$mort5, bandwidth=1, degree=1))
library(ggplot2)
ggplot() +
  geom_line(data = poorreg, aes(x = x, y = y,color = "Fair/poor")) +
  geom_line(data = notpoorreg, aes(x = x, y = y,color = "Good/Very Good/Excellent")) +
  labs(x="age", y="mort5", color = "Legend") +
  scale_color_manual(values = c("Fair/poor" = "black", "Good/Very Good/Excellent" = "grey"))

 # # # # # #
# Problem 3 # 
 # # # # # #

# Rates of mortality and fair/poor health decline with
# family income. This pattern holds for the full sample
# as well as by gender. The same general pattern holds
# for education as well, although individuals with 
# post-graduate education do not appear to be in worse 
# health than college graduates.

# Generate family income category variable
library(dplyr)
nhis$faminc <- case_when((nhis$faminc_gt75 == 0&nhis$faminc_20t75==0) ~ 1,
                         (nhis$faminc_gt75 == 0&nhis$faminc_20t75==1) ~ 2,
                         (nhis$faminc_gt75 == 1&nhis$faminc_20t75==0) ~ 3)
prop.table(table(nhis$faminc))

# Mortality by income
ggplot(data=nhis, mapping=aes(x=faminc, y=mort5)) +
  stat_summary(fun.data=mean_sdl, geom="bar")

# Mortality by income by sex
ggplot(data=nhis, mapping=aes(x=faminc, y=mort5)) +
  stat_summary(fun.data=mean_sdl, geom="bar") +
  facet_wrap(~sex)

# Health by income
ggplot(data=nhis, mapping=aes(x=faminc, y=fpoor)) +
  stat_summary(fun.data=mean_sdl, geom="bar")

# Health by income by sex
ggplot(data=nhis, mapping=aes(x=faminc, y=fpoor)) +
  stat_summary(fun.data=mean_sdl, geom="bar") +
  facet_wrap(~sex)

# Generate education level
nhis$edlev <- case_when((nhis$edyrs<12)                   ~ 1,
                         (nhis$edyrs==12)                 ~ 2,
                         (nhis$edyrs>=13 & nhis$edyrs<15) ~ 3,
                         (nhis$edyrs==16)                 ~ 4,
                         (nhis$edyrs>=17 & nhis$edyrs<20) ~ 5)
prop.table(table(nhis$edlev))

# Mortality by education
ggplot(data=nhis, mapping=aes(x=edlev, y=mort5)) +
  stat_summary(fun.data=mean_sdl, geom="bar")

# Mortality by education by sex
ggplot(data=nhis, mapping=aes(x=edlev, y=mort5)) +
  stat_summary(fun.data=mean_sdl, geom="bar") +
  facet_wrap(~sex)

# Health by education
ggplot(data=nhis, mapping=aes(x=edlev, y=fpoor)) +
  stat_summary(fun.data=mean_sdl, geom="bar")

# Health by education by sex
ggplot(data=nhis, mapping=aes(x=edlev, y=fpoor)) +
  stat_summary(fun.data=mean_sdl, geom="bar") +
  facet_wrap(~sex)

 # # # # # #
# Problem 4 # 
 # # # # # #

# Because age and education have non-linear relation-
# ships with health, I include a series of dummy 
# variables for categories. I use the education cate-
# gories from above, and 10-year age intervals.

# For both outcomes and for all three models, the
# results show that mortality and fair/poor health 
# decline with income, decline with education, and 
# rise with age. One surprising result is that 
# conditional on the socioeconomic variables, racial
# gaps in mortality are small and insignificant.
# There are larger racial gaps in fair/poor health.
# Another surprising result is that Hispanics have
# low mortality risk (conditional on the other
# covariates).

# The linear probability results are similar to the
# probit and logit average marginal effects, although 
# the similarity is much stronger for fair/poor health
# than for mortality. You did not need to comment
# on the reason in your response, but the larger 
# difference in the case of mortality is probably
# due to the fact that mortality risk is exceptionally
# low across much of the age distribution, so that
# the marginal effect is calculated in the flatter part 
# of the CDF.

# Generate factor variables for education and age (in decades)
# With a factor variable, R automatically converts it into a series of dummies
nhis$edlev.f <- factor(nhis$edlev)
nhis$agecat.f <- factor(floor(nhis$age/10)*10)

# Mortality analyses
library(estimatr)
lm_robust(mort5 ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis)
probit_model <- glm(mort5 ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis, family=binomial(link="probit"))
summary(probit_model)
logit_model <- glm(mort5 ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis, family=binomial(link="logit"))
summary(logit_model)
# Marginal effects for the probit and logit models
library(mfx)
probitmfx(mort5 ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis, atmean = TRUE, robust = TRUE)
logitmfx(mort5 ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis, atmean = TRUE, robust = TRUE)

# Fair/poor health analyses
lm_robust(fpoor ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis)
probit_model <- glm(fpoor ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis, family=binomial(link="probit"))
summary(probit_model)
logit_model <- glm(fpoor ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis, family=binomial(link="logit"))
summary(logit_model)
# Marginal effects for the probit and logit models
library(mfx)
probitmfx(fpoor ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis, atmean = TRUE, robust = TRUE)
logitmfx(fpoor ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis, atmean = TRUE, robust = TRUE)

 # # # # # #
# Problem 5 # 
 # # # # # #

# I used the logit model above for this test. In
# that model, the difference between high income 
# African Americans and low income whites is the
# black coefficient plus the high income coefficient
# minus the white coefficient. The results suggest
# that high income blacks have significantly lower
# mortality rates than low income whites. You
# did not need to comment on the size of the gap,
# but the exponentiated difference (in the log odds) 
# is 0.59755, suggesting that high income blacks
# have 40 percent lower odds of 5-year mortality 
# than low income whites.

# It's likely that this model is not the best for 
# testing differences between these groups. It would
# be better to include interactions of race and income.

logit_model <- glm(mort5 ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, data = nhis, family=binomial(link="logit"))
summary(logit_model)
library(multcomp)
summary(glht(logit_model, linfct = c("black + faminc_gt75 - white = 0")))
exp(-.5149)

 # # # # # #
# Problem 6 # 
 # # # # # #

# We probably should not interpret these results as causal.
# One problem is that there are many confounding variables
# that we do not observe but may jointly determine health
# and income, for instance place of birth. Another problem
# is that there may be reverse causality, i.e. health may
# affect income.

 # # # # # #
# Problem 7 # 
 # # # # # #

# I used the logit model again, and I exponentiated the 
# coefficients for interpretability. I control for
# insurance status, smoking status, exercise, bacon
# consumption, and obesity. To keep the samples the same
# in the regressions with and without the additional
# control variables, I run the long regression
# and the short regression on the same sample, which
# required subsetting the data first. This was not required.

# Mortality results:
# Insurance status, bacon consumption, and obesity were
# not significantly associated with 5-year mortality risk.
# Smoking and exercise were highly associated with mortality
# risk: ever smoking raised the odds of death twofold, and
# weekly exercise halved the odds. These patterns explain part
# of the socioeconomic gradient in health. Upon controlling for
# these variables, the odds ratio on the high income dummy rose
# from 0.52 to 0.57 and the odds ratio on the >16 years of 
# education dummy rose from 0.46 to 0.59. Thus, health behavior
# explains a larger share of the education-mortality relationship
# than the income-mortality relationship.

# Fair/poor health results:
# Smoking, exercise, and obesity were positively associated
# with being in fair/poor health. Being uninsured and eating
# bacon were (surprisingly) negatively associated with
# being in fair/poor health -- likely because of reverse 
# causality. These patterns again explain part of the 
# socioeconomic gradient in health. The odds ratio on high
# income rises from 0.23 to 0.26, while that on >16 years
# rises from 0.21 to 0.29.

# Recode behavior variables as 0/1 dummies
table(nhis$uninsured)
nhis$uninsured <- 2-nhis$uninsured
table(nhis$smokev)
nhis$smokev <- 0.1*(nhis$smokev)-1
table(nhis$vig10fwk)
nhis$vig10fwk <- ifelse(nhis$vig10fwk > 0, 1, 0)
nhis$obese <- ifelse(nhis$bmi >= 30, 1, 0)

# Mortality analyses
nhis_subset <- na.omit(nhis[c("mort5","faminc_20t75","faminc_gt75",
                              "edlev.f","agecat.f","white","black","hisp",
                              "uninsured","smokev","vig10fwk","bacon","obese")])
logitor(mort5 ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, 
        data = nhis_subset, robust = TRUE)
logitor(mort5 ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp +
                uninsured + smokev + vig10fwk + obese, 
        data = nhis_subset, robust = TRUE)

# Fair/poor analyses
nhis_subset <- na.omit(nhis[c("fpoor","faminc_20t75","faminc_gt75",
                              "edlev.f","agecat.f","white","black","hisp",
                              "uninsured","smokev","vig10fwk","bacon","obese")])
logitor(fpoor ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp, 
        data = nhis_subset, robust = TRUE)
logitor(fpoor ~ faminc_20t75 + faminc_gt75 + edlev.f + agecat.f + white + black + hisp +
                uninsured + smokev + vig10fwk + obese, 
        data = nhis_subset, robust = TRUE)

