# this script continues analysing the relationship between age and  
# asset ownership in India, using the National Family Health Survey

# input Stata file (need haven library for importing Stata file)
library(haven)
nfhs_raw <- read_dta("https://github.com/tvogl/econ121/raw/main/data/nfhs4.dta")

# subset data to include only 20-80 year olds
nfhs <- nfhs_raw[which(nfhs_raw$age >= 20
                      & nfhs_raw$age <= 80),]

# we left off with estimates of a single slope,
# as a summary of a population pattern. recall 
# that we used sampling weights, to ensure that
# the estimates are representative of the 
# estimate we would obtain for the full population.
library(estimatr)
lm_robust(assets ~ age,data = nfhs,clusters = clustnum,weights=weight,se_type = "stata")

# heterogeneity between urban and rural areas
library(dplyr)
bygroup = nfhs %>% group_by(rural) %>% do(regs = lm_robust(assets ~ age,data = .,clusters = clustnum,weights = weight,se_type = "stata"))
bygroup$regs

# the urban/rural subsamples are independent, so we can compute
# a t-statistic for the difference in slopes "by hand"
(.006709-.0100517)/sqrt(.0001704^2+.000329^2)

# alternatively, we can use an interaction term.
#first generate the interaction term
nfhs$ageXrural <- nfhs$age * nfhs$rural
# now run the model and save the results as model1 for later
model1 <- lm_robust(assets ~ age + rural + ageXrural,data = nfhs,clusters = clustnum,weights = weight,se_type = "stata")
model1
# same t-statistic! assets rise less steeply with age in rural areas.
# we could also do this more elegantly using R's functionality for
# interaction terms, substituting "age * rural" for "age + rural + ageXrural".
# but it is easier to work with the coefficients afterwards if we do it the
# more cumbersome way.

# if we wanted to obtain the slope for rural areas from
# the interacted model, we would take a linear combination
# of the coefficient on age plus coefficient on the ruralXage 
# interaction term. we use the glht function from the multcomp
# package to do this.
library(multcomp)
summary(glht(model1, linfct = c("age + ageXrural = 0")))
# same as we got when we estimated a separate regression for 
# rural areas!

# note: in this sample, we could not compute the t-statistic 
# ourselves if we were interested in male/female differences in
# slopes. the primary sampling units are clusters, so the male
# and female subsamples are not independent. our only option
# here is to use an interaction term. here we will take advantage
# of R's elegant functionality for interaction terms:
lm_robust(assets ~ age*male,data = nfhs,clusters = clustnum,weights = weight,se_type = "stata")
# no significant difference in slopes between men and women!

# now let's suppose we are interested in the ratio of the urban
# slope to the rural slope. we have two options.

# option 1: delta method (using deltaMethod in the car package).
# we use the interacted model we named model1.
library(car)
deltaMethod(model1,"age / (age + ageXrural)",rhs=1)
# this is nice for interpretation. it's hard to interpret the 
# difference of 0.003 between the urban and rural slopes. but
# now we can say that the slope in urban areas is 50% higher than
# the slope in rural areas, and that proportional difference is
# statistically significant (because the 95% CI for the ratio
# excludes 1).

# option 2: bootstrap the interacted model.
# let's start by just bootstrapping the model without calculating
# the ratio, just to get used to the bootstrap command. we'll do
# 99 bootstrap replications. we will use the bootcov function in 
# the rms package. this requires us to use ols() (also in the rms
# package) instead of lm() or lm_robust(). so we first re-estimate
# the model and save it as model1a.
library(rms)
model1a <- ols(assets ~ age + rural + ageXrural,data = nfhs, x=TRUE, y=TRUE)
# note that we did not use the survey weights. that's because the
# bootstrap function ignores them anyway. it is difficult to incorporate 
# weights into a bootstrap procedure, since the procedure emulates simple 
# random sampling. a weighted sample is not a simple random sample.
# now for the bootstrap.
bootcov(model1a,B=99)
# this command produces stardard errors that are robust to 
# heteroskedasticity, but they do not account for the clustered
# sampling in this survey, which is important. we can use the
# "block bootstrap" procedure, which resamples clusters (villages)
# instead of individuals:
bootcov(model1a,cluster=nfhs$clustnum,B=99)
# compare these with the analytical results we get from the
# unweighted regression with clustered SEs:
lm_robust(assets ~ age*rural,data = nfhs,clusters = clustnum,se_type = "stata")
# very similar! now we would like to bootstrap the ratio of urban to 
# rural slopes, using a block bootstrap with no weighting as in the
# the Stata code. to my knowledge, existing R packages do not allow
# this. we cannot pass a function of coefficients to bootcov. an 
# alternative bootstrap function, Boot(), does allow us to pass a
# function of coefficients, but it does not allow block bootstrapping.
# so we would need to write an original block bootstrap program, which
# is beyond the scope of this course. let me show you how to bootstrap
# the ratio assuming a simple random sample (instead of a cluster sample):
ratio <- function(x) coef(x)[2]/(coef(x)[2] + coef(x)[4])
Boot(model1,f = ratio,method = c("case"),R = 99)
