# This is a modification of texreg::extract
# to pass results from panelAR() to texreg
# Johannes Karreth

extract.panelAR <- function (model, include.rsquared = TRUE, 
          include.nobs = TRUE, include.panels = TRUE,
          ...) 
{
  s <- summary(model, ...)
  names <- rownames(s$coefficients)
  co <- s$coefficients[, 1]
  se <- s$coefficients[, 2]
  pval <- s$coefficients[, 4]
  rs <- ifelse(!is.null(s$r2), s$r2, NA)
  n <- s$panelStructure$N
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.rsquared == TRUE) {
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R$^2$")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.panels == TRUE) {
    panels <- s$panelStructure$N.panel
    panel.names <- "Num. groups"
    gof <- c(gof, panels)
    gof.names <- c(gof.names, panel.names)
    gof.decimal <- c(gof.decimal, rep(FALSE, length(panels)))
  }
  tr <- createTexreg(coef.names = names, coef = co, se = se, 
                     pvalues = pval, gof.names = gof.names, gof = gof, gof.decimal = gof.decimal)
  return(tr)
}