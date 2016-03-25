#######################################################
### Illustrated code to obtain robust and clustered ###
###  standard errors from OLS in R                  ###
### Johannes Karreth                                ###
### 03/25/2016                                      ###
#######################################################

# NOTE: This code is provided for teaching purposes to students in my seminar
# RPOS/RPAD 417, Quantitative Research Methods, at the University at Albany.
# Please refer to the accompanying course materials for this course for
# more information at <http://www.jkarreth.net/rpos517.html>.

# Robust standard errors were proposed by White (1980) in order to 
# calculate standard errors when the errors are heteroskedastic but 
# still independent (for more, see Fox pp. 275-277 (2nd ed.)).

# Clustered SEs correct for the clustering of observations in groups.
# If you don't want to assume that observations coming from the same cluster are still
# independent (i.e. they have a different error structure than observations
# from a different cluster), clustered SEs adjust SE estimates for this data
# structure. 

# But note that neither robust nor clustered SEs adjust your coefficient estimates.
# They also do not give you much useful information about the quality of your model
# and the actual structure of your data. 

# Functions for this code:
# robSE <github>
# clSE <github>

### 1. Robust standard errors

# Load example data from John Fox's book
# See ?car::SLID for more information on these data

slid_dat <- read.csv("http://www.jkarreth.net/files/car_SLID.csv")
summary(slid_dat)

# Fit OLS as usual: outcome are individuals' wages

slid_mod <- lm(wages ~ education + age + sex + language, data = slid_dat)
summary(slid_mod)

# Load robSE function from my github repository

devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/robSE.R")

# Create a vector with robust SEs 

slid_mod.rse <- robSE(mod = slid_mod)

# and the associated p-values, if needed

slid_mod.rse.pval <- (1 - pnorm(abs(coef(slid_mod) / slid_mod.rse))) * 2

# Combine conventional and robust SEs in one regression table, using screenreg()

library(texreg)
screenreg(list(slid_mod, slid_mod), 
          override.se = list(coef(summary(slid_mod))[, 2], slid_mod.rse),
          override.pval = list(coef(summary(slid_mod))[, 4], slid_mod.rse.pval),
          custom.model.names = c("Conventional SEs", "Robust SEs"),
          digits = 3)


### 2. Clustered standard errors

# This function requires complete data (no missing observations) on all variables
# in the model PLUS the clustering variable. It will return an error if there are 
# missing values on the clustering variable. 

devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/clSE.R")

# Load example data from Alvarez, Garrett, and Lange 1991 (APSR 85 (2), 539-556)
# Online at Alvarez's website <http://people.hss.caltech.edu/~rma/replication.html>
# or download a copy from my website <http://www.jkarreth.net/files/growth.dta>
# Unit of observation: country-year (i.e. multiple obs. per country)

agl_dat <- rio::import("http://www.hss.caltech.edu/~rma/agl_verified.dta")
summary(agl_dat)

# Rescale some variables
agl_dat$opengdp2 <- agl_dat$opengdp / 100
agl_dat$openex2 <- agl_dat$openex / 100
agl_dat$openimp2 <- agl_dat$openimp / 100
agl_dat$leftc2 <- agl_dat$leftc / 100

# Fit OLS as usual: outcome is economic growth

agl_mod <- lm(growth ~ opengdp2 + openex2 + openimp2 + leftc2 + central, data = agl_dat)

# Create a vector with SEs clustered on countries
# clSE() takes three arguments: the dataframe (df), the lm() object (mod),
# and the name of the clustering variable (cluster)

agl_mod.cse <- clSE(df = agl_dat, mod = agl_mod, cluster = "country")

# associated p-values, if needed

agl_mod.cse.pval <- (1 - pnorm(abs(coef(agl_mod) / agl_mod.cse))) * 2

# Combine conventional and robust SEs in one regression table, using screenreg()

library(texreg)
screenreg(list(agl_mod, agl_mod), 
          override.se = list(coef(summary(agl_mod))[, 2], agl_mod.cse),
          override.pval = list(coef(summary(agl_mod))[, 4], agl_mod.cse.pval),
          custom.model.names = c("Conventional SEs", "Clustered SEs"),
          digits = 3)