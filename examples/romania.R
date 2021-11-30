# this R script analyzes data from romanian schools. the data include
# students who live in towns with two high schools. high school admissions
# are based on an entry exam. we will use the admissions cutoff to
# the town's better high school in a fuzzy regression discontinuity design.

library(haven)
romania <- read_dta("https://github.com/tvogl/econ121/raw/main/data/romania_schools.dta")

# variables in the dataset
ls(romania)
# distance = score - cutoff
# better = went to better school in town
# ptile = percentile on high school exit exam

# summarize distance to the town-specific cutoff
summary(romania$distance)

# graph of first stage relationship --> effect of being above town's cutoff on going to the better school
library(KernSmooth)
library(ggplot2)
romania_below <- subset(romania, distance < 0)
romania_above <- subset(romania, distance >= 0)
yhat_below <- data.frame(locpoly(x=romania_below$distance, y=romania_below$better, bandwidth=.2, degree=1))
yhat_above <- data.frame(locpoly(x=romania_above$distance, y=romania_above$better, bandwidth=.2, degree=1))
ggplot() +
  geom_line(data = yhat_below, aes(x = x, y = y)) +
  geom_line(data = yhat_above, aes(x = x, y = y)) +
  labs(x="distance", y="pr[better school]")

# graph of reduced form relationship --> effect of being above town's cutoff on exit exam percentile
yhat_below <- data.frame(locpoly(x=romania_below$distance, y=romania_below$ptile, bandwidth=.2, degree=1))
yhat_above <- data.frame(locpoly(x=romania_above$distance, y=romania_above$ptile, bandwidth=.2, degree=1))
ggplot() +
  geom_line(data = yhat_below, aes(x = x, y = y)) +
  geom_line(data = yhat_above, aes(x = x, y = y)) +
  labs(x="distance", y="exit exam percentile")
	   
# first stage local linear regression with rectangular kernel and bw = 0.05
library(dplyr)
library(estimatr)
romania <- mutate(romania, above=ifelse(distance>=0,1,0), aboveXdistance=above*distance)
lm_robust(better ~ above + distance + aboveXdistance, data=romania, subset=(abs(distance)<=0.2))

# reduced form local linear regression with rectangular kernel and bw = 0.05
lm_robust(ptile ~ above + distance + aboveXdistance, data=romania, subset=(abs(distance)<=0.2))

# ratio of reduced form to first stage --> effect of going to the town's better school on exit exam percentile
3.95468/.6369932

# estimate by two stage least squares
library(AER)
ivmodel <- ivreg(ptile ~ better + distance + aboveXdistance | above + distance + aboveXdistance, data=romania, subset=(abs(distance)<=0.2))
coeftest(ivmodel, vcov = vcovHC(ivmodel, type="HC2")) # to get robust standard errors