# uncomment if these packages are not installed
# install.packages(c('tidyverse','estimatr','radiant.data','ggpubr'))

# this script estimates the relationship between age and asset 
# ownership in India, using the National Family Health Survey

# input Stata file (need haven library for importing Stata file)
library(haven)
nfhs_raw <- read_dta("https://github.com/tvogl/econ121/raw/main/data/nfhs4.dta")

# subset data to include only 20-80 year olds
nfhs <- nfhs_raw[which(nfhs_raw$age >= 20
                      & nfhs_raw$age <= 80),]

# note that the survey has sampling weights. these are to
# adjust for the fact that the sample was not a simple
# random sample. subpopulations that were undersampled receive
# larger sampling weights, so that weighted statistics provide
# unbiased estimates of population parameters. if p is the
# probability that a household was included in the sample,
# then that household's weight is proportional to 1/p. the
# actual number is (1/p)*(sample size)*(1 million), but since
# all weighted statistics rescale by the sum of the weights,
# it is conceptually the same as weighting by 1/p.
summary(nfhs$weight)
hist(nfhs$weight)

# let's aggregate the data to age bins and plot average assets by age.
# note that we are estimating weighted averages using the survey weights.
# we use summaryBy from the doBy package to collapse the data by age.
library(dplyr) #for collapsing by age
library(radiant.data) #for weighted sd function
nfhs_collapsed <- nfhs %>% group_by(age) %>% 
  summarize(mean=weighted.mean(assets,w=weight),
            sd = weighted.sd(assets,w=weight),
            n=n())

# plot the data by age
library(ggplot2) #for plots
library(ggpubr) #to combine plots
plot1 <- ggplot(nfhs_collapsed, aes(x=age, y=mean, size=n)) + 
          geom_point(shape=21, show.legend=FALSE)
plot2 <- ggplot(nfhs_collapsed, aes(x=age, y=sd), size=n) + 
           geom_point(shape=21, show.legend=FALSE)
ggarrange(plot1, plot2, ncol = 1, nrow = 2)

# now we are going to run a series of OLS regressions on the grouped data.
# since we have unequal group sizes, this may be a good scenario to weight
# by the number of observations in each group. however, that method works
# when the same model holds for everyone (i.e., beta is not heterogeneous)
# and the individual-level model is homoskedastic. we already know that
# the individual-level model is heteroskedastic, so we may not end up
# improving the precision of the estimator (i.e., shrinking the SEs).

# before we run the unweighted and weighted regressions, let's have a look
# at the group sizes by age:
ggplot(nfhs_collapsed, aes(x=age, y=n)) + geom_point(shape=21)

# linear regression using grouped data, with and without weighting by N.
# get different coefficients because of linearity. not clear that the 
# weighted regression is preferred, since it is changing the coefficient
# rather than improving precision
library(estimatr)
lm_robust(mean ~ age,data = nfhs_collapsed)
lm_robust(mean ~ age,data = nfhs_collapsed,weights=n)

# quadratic regression using grouped data, with and without weighting by N.
# get very similar coefficients now, but smaller SEs in unweighted model,
# not as predicted under correct functional form, homoskedasticity, and 
# constant coefficients
lm_robust(mean ~ age + I(age^2),data = nfhs_collapsed)
lm_robust(mean ~ age + I(age^2),data = nfhs_collapsed,weights=n)

# back to the micro data
# estimate quadratic functional form, with and without weighting by 
# sampling weight. get slightly diffent coefficients, larger SEs.
# so the weighted estimate is NOT more efficient. it is also not
# necessarily the average coefficient in the population. it IS
# an estimate of the answer we would get from a census of the population.
lm_robust(assets ~ age + I(age^2),data = nfhs)
lm_robust(assets ~ age + I(age^2),data = nfhs,weights=weight)

# should actually estimate this taking the clustered sampling into
# account. villages were sampled, and then individuals within them.
# need cluster-robust SEs. the SEs become larger, consistent with
# positive intra-cluster correlations.
lm_robust(assets ~ age + I(age^2),data = nfhs,clusters = clustnum,weights=weight,se_type = "stata")
# se_type uses the Stata std. err. formula. otherwise, with this large a dataset, R crashes!

# even though the relationship is non-linear, it may still be
# useful to use a single slope as a summary of a population pattern.
# here it probably makes sense to weight.
lm_robust(assets ~ age,data = nfhs,clusters = clustnum,weights=weight,se_type = "stata")
