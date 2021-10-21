*this do-file continues analyzing the relationship 
*between age and asset ownership in India, using 
*the National Family Health Survey.

use "https://github.com/tvogl/econ121/raw/main/data/nfhs4.dta" if age>=20&age<=80,clear

*we left off with estimates of a single slope,
*as a summary of a population pattern. recall 
*that we used sampling weights, to ensure that
*the estimates are representative of the 
*estimate we would obtain for the full population.
reg assets age [aw=weight],cluster(clustnum)

*heterogeneity between urban and rural areas
bysort rural: reg assets age [aw=weight],cluster(clustnum)

*the urban/rural subsamples are independent, so we can compute
*a t-statistic for the difference in slopes "by hand"
di (.006709-.0100517)/sqrt(.0001704^2+.000329^2)

*alternatively, we can use an interaction term
gen ruralXage = rural*age
reg assets age rural ruralXage [aw=weight],cluster(clustnum)
*same t-statistic! assets rise less steeply with age in rural areas.

*if we wanted to obtain the slope for rural areas from
*the interacted model, we would take a linear combination
*of the coefficient on age plus coefficient on the ruralXage 
*interaction term.
lincom age+ruralXage
*same as we got when we estimated a separate regression for 
*rural areas!

*note: in this sample, we could not compute the t-statistic 
*ourselves if we were interested in male/female differences in
*slopes. the primary sampling units are clusters, so the male
*and female subsamples are not independent. our only option
*here is to use an interaction term:
gen maleXage = male*age
reg assets age male maleXage [aw=weight],cluster(clustnum)
*no significant difference in slopes between men and women!

*now let's suppose we are interested in the ratio of the urban
*slope to the rural slope. we have two options.

*option 1: delta method.
*start by estimating the interacted model.
reg assets age rural ruralXage [aw=weight],cluster(clustnum)
*then use the nlcom command.
nlcom _b[age]/(_b[age]+_b[ruralXage])
*this is nice for interpretation. it's hard to interpret the 
*difference of 0.003 between the urban and rural slopes. but
*now we can say that the slope in urban areas is 50% higher than
*the slope in rural areas, and that proportional difference is
*statistically significant (because the 95% CI for the ratio
*excludes 1).

*option 2: bootstrap the interacted model.
*let's start by just bootstrapping the model without calculating
*the ratio, just to get used to the bootstrap command. we'll do
*99 bootstrap replications. the simplest case is:
bootstrap,reps(99): reg assets age rural ruralXage
*this command produces stardard errors that are robust to 
*heteroskedasticity, but they do not account for the clustered
*sampling in this survey, which is important. we can use the
*"block bootstrap" procedure, which resamples clusters (villages)
*instead of individuals:
bootstrap,reps(99) cluster(clustnum): reg assets age rural ruralXage
*compare these with the analytical standard errors:
reg assets age rural ruralXage,cluster(clustnum)
*very similar! note that we have not used survey weights here.
*it is difficult to incorporate weights into a bootstrap procedure,
*since the procedure emulates simple random sampling. a weighted
*sample is not a simple random sample. so let's bootstrap the ratio
*of urban to rural slopes, using a block bootstrap with no weighting.
bootstrap ratio=(_b[age]/(_b[age]+_b[ruralXage])),reps(99) cluster(clustnum): reg assets age rural ruralXage






