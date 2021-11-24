# this R script estimates the political party incumbency
# advantage in US House elections

library(haven)
elections <- read_dta("https://github.com/tvogl/econ121/raw/main/data/elections.dta")

# the running variable in this RD analysis is the
# democratic vote margin of victory: the vote share
# of the democrat minus the vote share of the other
# top-two candidate. the democrat wins for all values
# greater than zero

# the dependent variable is the democratic vote share
# in the next election

# we also have two predetermined variables: the
# democratic vote share in the previous election and 
# the democrat's years of political experience

summary(elections)

# histogram of the democratic vote margin of victory
# we make sure that the bars don't overlap zero
library(ggplot2)
ggplot(elections, aes(x=difdemshare)) + 
  geom_histogram(binwidth=0.025, boundary=-1, alpha=.7)

# drop uncontested elections
library(dplyr)
elections <- elections %>% filter(abs(difdemshare)!=1)

# now generate a binning variable for the figures
# each bin will be 0.05 wide
elections <- mutate(elections, bin = floor(difdemshare*20)/20+0.025)
prop.table(table(elections$bin))

# now generate local means for each of the bins
local_means <- elections %>%
                group_by(bin) %>%
                summarise_at(vars(demsharenext, demshareprev, demofficeexp), list(mean))

# now let's run our main RD, using a global 4th order polynomial
# to approximate the conditional expectation function. we allow
# the shape of the polynomial to be different above and below
# the victory threshold. following david lee, we will cluster
# by district-year
library(estimatr)
poly <- lm_robust(demsharenext ~ right + difdemshare + difdemshare2 + difdemshare3 + difdemshare4
                               + rdifdemshare + rdifdemshare2 + rdifdemshare3 + rdifdemshare4,
                  data = elections, cluster=statedisdec)
summary(poly)
# so a democratic victory now causes a 7 point increase in
# the next election's democratic vote share
# let's generate a predicted value
elections$demsharenext_hat_poly <- predict(poly, elections)

# let's check whether this result is robust to including the 
# predetermined variables as controls
poly_control <- lm_robust(demsharenext ~ right + difdemshare + difdemshare2 + difdemshare3 + difdemshare4
                          + rdifdemshare + rdifdemshare2 + rdifdemshare3 + rdifdemshare4
                          + demshareprev + demofficeexp,
                          data = elections, cluster=statedisdec)
summary(poly_control)
# doesn't seem to matter much, which is promising
# let's generate a predicted value, holding
# demshareprev demofficeexp at their means
demshareprev.mean <- mean(elections$demshareprev)
demofficeexp.mean <- mean(elections$demofficeexp)
elections_temp <- mutate(elections,demshareprev = demshareprev.mean,
                                   demofficeexp = demofficeexp.mean)
elections$demsharenext_hat_poly_control <- predict(poly_control, elections_temp)
rm(elections_temp, demshareprev.mean, demofficeexp.mean)

# the above result suggests that demshareprev demofficeexp 
# don't change discontinuously at zero, but we can also check
# directly
lm_robust(demshareprev ~ right + difdemshare + difdemshare2 + difdemshare3 + difdemshare4
                       + rdifdemshare + rdifdemshare2 + rdifdemshare3 + rdifdemshare4,
          data = elections, cluster=statedisdec)
lm_robust(demofficeexp ~ right + difdemshare + difdemshare2 + difdemshare3 + difdemshare4
          + rdifdemshare + rdifdemshare2 + rdifdemshare3 + rdifdemshare4,
          data = elections, cluster=statedisdec)

# the global polynomial might be misleading, let's check for 
# local mean differences and also run a local linear regression
# we use a bandwidth of 0.1
lmean <- lm_robust(demsharenext ~ right,
                     data = elections, subset = abs(difdemshare)<0.1, cluster=statedisdec)
summary(lmean)
lpoly <- lm_robust(demsharenext ~ right + difdemshare + rdifdemshare,
                   data = elections, subset = abs(difdemshare)<0.1, cluster=statedisdec)
summary(lpoly)
# let's generate a predicted value
elections$demsharenext_hat_lpoly <- predict(lpoly, elections)

# let's plot the data and our various predictions
# add a local linear regression over all the data
ggplot(elections, aes(difdemshare, demsharenext)) +
  geom_point(data = local_means, aes(bin, demsharenext)) +
  geom_line(aes(difdemshare, demsharenext_hat_poly), color = "blue") +
  geom_line(aes(difdemshare, demsharenext_hat_poly_control), color = "navy") +
  geom_line(data = subset(elections,abs(difdemshare)<.1), aes(difdemshare, demsharenext_hat_lpoly), color = "tomato")

# let's also plot the local means for the predetermined variables
library(patchwork) # to combine ggplots
plot1 <- ggplot(local_means, aes(bin, demshareprev)) +
           geom_point()
plot2 <- ggplot(local_means, aes(bin, demofficeexp)) +
           geom_point()
plot1 + plot2
      
       
                
                    



