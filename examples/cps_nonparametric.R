# this script studies the relationship or gender, race, and age 
# with income in the united states, using the current population 
# survey. it relies on nonparametric estimators.

# I will focus only on Black-white differences. If you are 
# in other racial/ethnic groups, I encourage you to modify 
# the do file in your spare time.

# input Stata file
library(haven)
cps18_raw <- read_dta("https://github.com/tvogl/econ121/raw/main/data/cps_18.dta")

summary(cps18_raw)

# tables of race and sex - note: Stata labels have been removed
table(cps18_raw$race) #100 is white, 200 is black
table(cps18_raw$sex) #1 is male, 2 is female

# from the summary() results above, we know incwage has many 0s.
# over 25% of sample has 0s! restrict to workers who work 40+ hrs/wk 
# with positive income. since we are just analyzing black/white diffs
# drop all other racial categories.
table(cps18$uhrsworkt) #997 and 999 are top codes
cps18 <- cps18_raw[which(cps18_raw$uhrsworkt >= 40 & cps18_raw$uhrsworkt <= 170 &
                         cps18_raw$incwage>0 & cps18_raw$race<=200),]
# cps18 is our analysis dataset, excluding people who work < 40 hours and 
# people who are missing hours data.

# generate log income
cps18$lninc <- log(cps18$incwage)

# what is the sd of lninc?
sd(cps18$lninc)

# for the kernel density estimation, we can rely on elegant ggplot
# syntax, which allows us to estimate by race/sex category.
# first, let's create a single race/sex variable:
cps18$racesex <- as.factor(ifelse(cps18$race == 200 & cps18$sex == 2, 'Black women',
                           ifelse(cps18$race == 100 & cps18$sex == 2, 'White women',
                           ifelse(cps18$race == 200 & cps18$sex == 1, 'Black men',  
                           ifelse(cps18$race == 100 & cps18$sex == 1, 'White men', 'Other')))))
# kernel density estimates of log income by race and sex
# let's use .25 as the bandwidth, about a quarter of the sd of lninc.
library(tidyverse)
ggplot(data=cps18, aes(x=lninc, group=racesex, fill=racesex)) +
  geom_density(bw=.25,alpha=.4) #alpha specifies the transparency of the colors

# now let's try a bandwidth of .1 -> higher variance but more detail
ggplot(data=cps18, aes(x=lninc, group=racesex, fill=racesex)) +
  geom_density(bw=.1,alpha=.4)

# for local linear regression, our syntax will be less elegant,
# since ggplot does not have a local linear regression function.
# many analysts use # geom_smooth() from ggplot2 to smooth data, 
# but it is not local linear regression. we will use the locpoly()
# function from the KernSmooth package. we will need to estimate
# four local linear regressions separately and then plot them
# using ggplot.

# first, generate log income in four separate data frames, for each
# race/gender group. drop obs with 0 income, to avoid missing data issues.
whitemen <- subset(cps18, race==100 & sex==1, select=c(age, lninc))
whitewomen <- subset(cps18, race==100 & sex==2, select=c(age, lninc))
blackmen <- subset(cps18, race==200 & sex==1, select=c(age, lninc))
blackwomen <- subset(cps18, race==200 & sex==2, select=c(age, lninc))
# now let's track racial differences in log wages over the lifecycle
# using local linear regression. set the degree to 1 because we are
# estimating a local LINEAR regression (not quadratic, cubic, etc.).
# and start with a bandwidth of 1 year of age.  
library(KernSmooth)
wm <- data.frame(locpoly(x=whitemen$age, y=whitemen$lninc, bandwidth=1, degree=1))
ww <- data.frame(locpoly(x=whitewomen$age, y=whitewomen$lninc, bandwidth=1, degree=1))
bm <- data.frame(locpoly(x=blackmen$age, y=blackmen$lninc, bandwidth=1, degree=1))
bw <- data.frame(locpoly(x=blackwomen$age, y=blackwomen$lninc, bandwidth=1, degree=1))
ggplot() +
  geom_line(data = wm, aes(x = x, y = y,color = "WM")) +
  geom_line(data = ww, aes(x = x, y = y,color = "WW")) +
  geom_line(data = bm, aes(x = x, y = y,color = "BM")) +
  geom_line(data = bw, aes(x = x, y = y,color = "BW")) +
  labs(x="age", y="lninc", color = "Legend") +
  scale_color_manual(values = c("WM" = "blue", "WW" = "red","BM" = "black", "BW" = "grey"))
                               
# what happens if we expand the bandwidth to 2 years? smoother curves.
wm <- data.frame(locpoly(x=whitemen$age, y=whitemen$lninc, bandwidth=1, degree=2))
ww <- data.frame(locpoly(x=whitewomen$age, y=whitewomen$lninc, bandwidth=1, degree=2))
bm <- data.frame(locpoly(x=blackmen$age, y=blackmen$lninc, bandwidth=1, degree=2))
bw <- data.frame(locpoly(x=blackwomen$age, y=blackwomen$lninc, bandwidth=1, degree=2))
ggplot() +
  geom_line(data = wm, aes(x = x, y = y,color = "WM")) +
  geom_line(data = ww, aes(x = x, y = y,color = "WW")) +
  geom_line(data = bm, aes(x = x, y = y,color = "BM")) +
  geom_line(data = bw, aes(x = x, y = y,color = "BW")) +
  labs(x="age", y="lninc", color = "Legend") +
  scale_color_manual(values = c("WM" = "blue", "WW" = "red","BM" = "black", "BW" = "grey"))