# This do file analyzes a panel of states over time
# to measure the effect of seat belt laws on motor vehicle
# fatality rates and seat belt usage between 1983 and 1997.

# Import dataset
library(haven)
sb <- read_dta("https://github.com/tvogl/econ121/raw/main/data/SeatBelts.dta")

# Generate log income
sb$lnincome <- log(sb$income)

# Summarize
summary(sb)

##############################
# VISUALIZE POLICY VARIATION #
##############################
library(ggplot2)
ggplot() +
  geom_line(data = sb, aes(x = year,y = primary, color = "Primary")) +
  geom_line(data = sb, aes(x = year,y = secondary, color = "Secondary"), linetype = 2) +
  labs(x = "Year", y = "Seat belts law") +
  facet_wrap(~ state) +
  scale_color_manual(name="Type of law", values = c("Primary" = "purple", "Secondary" = "orange")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

# Load plm package for estimating panel data models
library(plm)

#####################################
# BASELINE MODEL WITH NO COVARIATES #
#####################################
# two-way fixed effects model
twfe <- plm(fatalityrate ~ primary + secondary, data = sb, 
            index = c("fips","year"), model = "within", effect = "twoways")
summary(twfe,vcov=vcovHC(twfe, method="white1")) # heteroskedasticity-robust, not cluster-robust
summary(twfe,vcov=vcovHC(twfe)) # heteroskedasticity- and cluster- robust

# first difference model
sb$year.f <- factor(sb$year) # factor variable version of year, for inclusion as covariate
fd <- plm(fatalityrate ~ primary + secondary + year.f, data = sb, 
          index = c("fips","year"), model = "fd")
summary(fd,vcov=vcovHC(fd, method="white1")) # heteroskedasticity-robust, not cluster-robust
summary(fd,vcov=vcovHC(fd)) # heteroskedasticity- and cluster- robust

################################################
# INCLUDE TIME-VARYING, STATE-LEVEL COVARIATES #
################################################
# two-way fixed effects model with clustering
twfe_controls <- plm(fatalityrate ~ primary + secondary + speed65 + speed70 + 
                                    drinkage21 + ba08 + lnincome + age, data = sb, 
                     index = c("fips","year"), model = "within", effect = "twoways")
summary(twfe_controls,vcov=vcovHC(twfe_controls))

# first difference model with clustering
fd_controls <- plm(fatalityrate ~ primary + secondary + speed65 + speed70 + 
                                  drinkage21 + ba08 + lnincome + age + year.f, data = sb, 
                   index = c("fips","year"), model = "fd")
summary(fd_controls,vcov=vcovHC(fd_controls))


