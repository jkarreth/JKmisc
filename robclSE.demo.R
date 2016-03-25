#######################################################
### Illustrated code to obtain robust and clustered ###
###  standard errors after OLS in R                 ###
### Johannes Karreth                                ###
### 30/03/2015                                      ###
#######################################################

# NOTE: Robust and clustered standard errors are not the same, 
# and "correct" for very different structures in the data. 

# Robust standard errors were proposed by White (1980) in order to 
# calculate standard errors when the errors are heteroskedastic but 
# still independent (for more, see Fox pp. 275-277).

# Clustered SEs are sometimes used when observations come from "clusters".
# If you want to assume that observations coming from the same cluster are not 
# independent (i.e. they have a different error structure than observations
# from a different cluster), clustered SEs adjust SE estimates for this data
# structure. 

# But note that neither robust nor clustered SEs adjust your coefficinet estimates.
# They also do not give you much useful information about the quality of your model
# and the actual structure of your data. For more on this, see King and Roberts (2014).

# Functions for this code:
# robSE <github>
# clSE <github>

### Load example data from John Fox's book

slid.dat <- read.csv("http://www.jkarreth.net/files/car_SLID.csv")

# Before advancing, make sure to check that there are no missing observations
# for outcomes, covariates, and cluster variables
summary(slid.dat)

### Fit OLS as usual

mod <- lm(wages ~ education + age + sex, data = slid.dat)

### Read in clSE and robSE functions from my GitHub repository "JKmisc".
### To do this, use the source_url function from the devtools package

library(devtools)

# source("/Users/johanneskarreth/R/JKmisc/clSE.R")
source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/clSE.R")
# source("/Users/johanneskarreth/R/Source/JKmisc/robSE.R")
source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/robSE.R")

### Robust SEs

# Create a vector with robust SEs and the associated p-values, if needed

mod.rse <- robSE(mod)
mod.rse.pval <- (1 - pnorm(abs(coef(mod) / mod.rse))) * 2

### Clustered SEs

# If there are missing observations and you want to use clustered standard errors, 
# create a new dataset where all missing observations on the relevant variables are dropped.
# If you want to estimate a model of "wages" with the following predictors:
# "education", "age", "sex"
# and you assume that your errors are not independent across subjects' languages, 
# then your cluster variable is "language" and you would use the following command:
slid.dat.noNa <- na.omit(slid.dat[, c("wages", "education", "age", "sex", "language")])
summary(slid.dat.noNa)

# Fit the model on the data without missing observations
mod.noNa <- lm(wages ~ education + age + sex, data = slid.dat.noNa)

# Create a vector with clustered SEs and the associated p-values, if needed

mod.cse <- clSE(data = slid.dat.noNa, model = mod.noNa, cluster = "language")
mod.cse.pval <- (1 - pnorm(abs(coef(mod) / mod.cse))) * 2


### Create regression table for Word or LaTeX
# Note that in this table, the N for each model differs because the model with 
# clustered SEs had to drop observations for which we don't have info on the 
# clustering variable ("language"). If the models were fit to the exactly same sample,
# the coefficient estimates would be identical. Only the SEs would differ.

library(texreg)

screenreg(list(mod, mod.noNa), 
            override.se = list(mod.rse, mod.cse),
            override.pval = list(mod.rse.pval, mod.cse.pval),
            custom.model.names = c("Robust SEs", "Clustered SEs"),
            digits = 3
  )

htmlreg(list(mod, mod.noNa), 
          override.se = list(mod.rse, mod.cse),
          override.pval = list(mod.rse.pval, mod.cse.pval),
          custom.model.names = c("Robust SEs", "Clustered SEs"),
          digits = 3,
          caption = "OLS estimates of wages",
          caption.above = TRUE,
          file = "wagesreg.doc"
)

texreg(list(mod, mod.noNa), 
        override.se = list(mod.rse, mod.cse),
        override.pval = list(mod.rse.pval, mod.cse.pval),
        custom.model.names = c("Robust SEs", "Clustered SEs"),
        digits = 3,
        caption = "OLS estimates of wages",
        caption.above = TRUE,
        file = "wagesreg.tex"
)