# Code for calculating robust standard errors for OLS
# Johannes Karreth

# Purpose: create a vector of heteroskedastcity-robust SEs
# that can be inserted into a regression table

# This function is used mainly for teaching. For more advanced functions
# to calculate robust SEs, see, for instance, the following post:
# <http://stats.stackexchange.com/questions/117052/replicating-statas-robust-option-in-r>

# Credit to Alan Fernihough's blog post at 
# <https://diffuseprior.wordpress.com/2012/06/15/standard-robust-and-clustered-standard-errors-computed-in-r/>
# The code below is based on that post.

# For this function to work, you need to install the "lmtest" package.
# install.packages("lmtest")

robSE <- function(mod){

	require(lmtest, quiet = TRUE)

	X.mat <- model.matrix(mod) # note that X includes 1 for the intercept
	y <- mod$mod[1]	# Response variable
	e <- resid(mod)	# Vector of residuals
	n <- dim(X.mat)[1] # Number of observations
	k <- dim(X.mat)[2] # Number of predictors (including intercept)
	
	vcov.mat <- vcov(mod)	# Variance-covariance matrix
	SE <- sqrt(diag(vcov.mat)) # Standard errors
	E.mat <- matrix(0, ncol = n, nrow = n)	# n-by-n Matrix of residuals
	diag(E.mat) <- e^2  # Squared residuals in the diagonal
	
	sum.mat <- t(X.mat) %*% E.mat %*% X.mat
	
	vcov.mat.rob <- abs(solve(t(X.mat) %*% X.mat) %*% sum.mat %*% solve(t(X.mat) %*% X.mat))
	
	rob.SE <- sqrt(diag(vcov.mat.rob))
	## Add the degrees-of-freedom correction
	dfc <- sqrt(nrow(X.mat)) / sqrt(nrow(X.mat) - ncol(X.mat))
	rob.SE.dfc <- dfc * sqrt(diag(vcov.mat.rob))
	return(rob.SE.dfc)

	}

# texreg users: You can then use the override.se argument in screenreg, texreg, etc. to use these robust SEs when making tables.