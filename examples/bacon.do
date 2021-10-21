*analysis of bacon consumption in the national health interview survey.
*open nhis dta, describe and summarize.
use "https://github.com/tvogl/econ121/raw/main/data/nhis2000.dta",clear

d
sum

*generate a variable that equals one if any bacon consumption, zero otherwise.
*stata counts missing values as very high values, so let's drop observations with
*bacon missing.
tab bacon,miss
drop if bacon==.
gen anybacon = (bacon>0)

*generate some covariates.
*gender.
tab sex
tab sex,nol
gen male = 2-sex

*marital status: dummy for marriage
tab marstat
tab marstat,nol
gen married = (marstat<20) if marstat<.

**COMPARING OLS, LOGIT, AND PROBIT

*estimate a linear probability model, a logit, and a probit.
*then generate predicted probabilities for each of these approaches.
*then compare the predicted probabilities
reg anybacon edyrs age male black hisp other married,robust
predict p_ols

probit anybacon edyrs age male black hisp other married,robust
predict p_probit

logit anybacon edyrs age male black hisp other married,robust
predict p_logit

sum p_*
corr p_*

**MARGINAL EFFECTS

*now let's compute marginal effects at the means of the independent
*variables. one way to do this is to run a logit or a probit and
*to then use the mfx compute command:
logit anybacon edyrs age male black hisp other married,robust
mfx compute

probit anybacon edyrs age male black hisp other married,robust
mfx compute

*alternatively, we can use margins to compute avergae 
*marginal effects. here we do it for the probit above:
margins,dydx(*)

*we can also calculate marginal effects in the probit model
*using the dprobit command:
dprobit anybacon edyrs age male black hisp other married,robust
*note that the marginal effects are the same as those computed
*by mfx compute. but they are NOT the same as those computed
*by margins. that's because mfx and dprobit evaluate marginal
*effects at the means of the independent variables, while
*margins evaluates the average marginal effect.

*stata does not provide a similar command for logits,
*but you can download one from the internet if you so
*desire. type "net search dlogit2" in stata; follow the 
*instructions.

**ODDS RATIOS

*finally, we can also estimate odds ratios in the logit setting:
logit anybacon edyrs age male black hisp other married,robust or
logistic anybacon edyrs age male black hisp other married,robust

*these are especially convenient for binary independent variables.
*for instance, men have 48% higher odds of eating bacon than women.
