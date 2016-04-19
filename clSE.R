# R function for calculating cluster-robust standard errors for OLS
# Johannes Karreth

# Purpose: create a vector of cluster-robust SEs
# that can be inserted into a regression table

# Credit to Mahmood Arai's technical brief at 
# <http://www.researchgate.net/publication/251965897_Cluster-robust_standard_errors_using_R>
# The code below is taken from his paper and only modified slightly.

# This function is used mainly for teaching. For more advanced functions
# to calculate culstered SEs, see, for instance, the following posts:
# <http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/>
# <http://rforpublichealth.blogspot.com/2014/10/easy-clustered-standard-errors-in-r.html>

# For this function to work, you need to install the "lmtest" and "sandwich" packages.
# install.packages(c("lmtest", "sandwich"))

# df: data frame passed to the lm() object, must contain the cluster IDs)
# model: lm() object, must be fit to only the observations for which cluster IDs exist!
# cluster: name of the cluster variable (as a character)

clSE  <- function(mod, df, cluster){  
           
  require(sandwich, quietly = TRUE)  
  require(lmtest, quietly = TRUE)
  
  y_name <- names(mod$model)[1]
  x_names <- c(names(mod$model)[2:ncol(mod$model)], cluster)
  data_formula <- as.formula(paste(y_name, paste(x_names, collapse = " + "), sep = " ~ "))
  data <- model.frame(formula = data_formula, data = df)
  
  cl <- data[, paste(cluster)]  # new vector with clustering variable  
  m <- length(unique(cl))  # Number of clusters
  n <- length(cl)  # Number of observations
  k <- mod$rank  # Number of covariates 
  
  uj  <- apply(estfun(mod), 2, function(x) tapply(x, cl, sum)) # uj = X_j*e_j for each cluster j. 
  
  dfc <- (m / (m - 1)) * ((n - 1) / (n - k)) # Degrees of freedom correction
  
  vcovCL <- dfc * sandwich(x = mod, meat = crossprod(uj) / n) # use sandwich() shortcut for the corrected vcov matrix
 
  clSE <- sqrt(diag(vcovCL))  # SEs are the square root of the diagonal of the vcov matrix
  
  return(clSE)

}

# texreg users: You can then use the override.se argument in screenreg, texreg, etc. to use these clustered SEs when making tables.