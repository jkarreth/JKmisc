# Code for calculating cluster-robust standard errors for OLS
# Johannes Karreth

# Purpose: create a vector of cluster-robust SEs
# that can be inserted into a regression table

# This function is used mainly for teaching. For more advanced functions
# to calculate culstered SEs, see, for instance, the following posts:
# <http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/>
# <http://rforpublichealth.blogspot.com/2014/10/easy-clustered-standard-errors-in-r.html>

# Credit to Mahmood Arai's technical brief at 
# <http://www.researchgate.net/publication/251965897_Cluster-robust_standard_errors_using_R>
# The code below is based on that post.

# For this function to work, you need to install the "lmtest" and "sandwich" packages.
# install.packages(c("lmtest", "sandwich"))

clSE  <- function(data, model, cluster){  # data frame (no missing obs!), model object, name of the cluster variable (in "")
           require(sandwich, quietly = TRUE)  
           require(lmtest, quietly = TRUE)
           cl <- data[, cluster]  # new vector with clustering variable  
           M <- length(unique(cl))  # Number of clusters
           N <- length(cl)  # Number of observations
           K <- model$rank  # Number of covariates 
           dfc <- (M / (M-1)) * ((N-1) / (N-K)) # Degrees of freedom correction
           uj  <- apply(estfun(model), 2, function(x) tapply(x, cl, sum)) # extract estimation function
           vcovCL <- dfc * sandwich(model, meat = crossprod(uj) / N) # use sandwich shortcut for the corrected vcov matrix
           # coeftest(model, vcovCL) 
           clSE <- sqrt(diag(vcovCL))
  		   return(clSE)
       }

# texreg users: You can then use the override.se argument in screenreg, texreg, etc. to use these clustered SEs when making tables.