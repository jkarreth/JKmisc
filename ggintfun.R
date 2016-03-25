ggintfun <- function (obj, varnames, varlabs, 
                      title = FALSE, 
                      rug = FALSE, 
                      twoways = FALSE, 
                      rugsize = 0.1, 
                      jitter_factor = 0){
  
  # This function was inspired by David A. Armstrong's DAintfun2
  #    function (in the DAmisc package)
  # It uses ggplot2, adds custom variable names for axis labels (varlabs)
  #    and puts the conditional effect of X1/X2 in the subplot title to save space
  # varlabs: character vector of length 2 with the desired variable names for
  #   x1 and x2 to show up as labels in the plot.

  require(ggplot2); require(gridExtra)
   
  if(class(obj) == "lm"){
    
    if (!("model" %in% names(obj))) {
      obj <- update(obj, model = T)
    }
    
    v1 <- varnames[1]
    v2 <- varnames[2]
    vlab1 <- varlabs[1]
    vlab2 <- varlabs[2]
    
    if(length(unique(model.matrix(obj)[, paste(v1)])) == 2){
      s1 <- c(min(model.matrix(obj)[, paste(v1)]), max(model.matrix(obj)[, paste(v1)]))
    }
    
    if(length(unique(model.matrix(obj)[, paste(v1)])) > 2){
      s1 <- seq(from = min(model.matrix(obj)[, paste(v1)]), to = max(model.matrix(obj)[, paste(v1)]), length.out = 25)
    }

    if(length(unique(model.matrix(obj)[, paste(v2)])) == 2){
      s2 <- c(min(model.matrix(obj)[, paste(v2)]), max(model.matrix(obj)[, paste(v2)]))
    }
    
    if(length(unique(model.matrix(obj)[, paste(v2)])) > 2){
      s2 <- seq(from = min(model.matrix(obj)[, v2]), to = max(model.matrix(obj)[, v2]), length.out = 25)
    }


   
    b1.pos <- grep(pattern = v1, x = names(coef(obj)), fixed = TRUE)[1]
    b3.pos <- grep(pattern = v1, x = names(coef(obj)), fixed = TRUE)[2]
    b1 <- as.numeric(obj$coef[b1.pos])
    b3 <- as.numeric(obj$coef[b3.pos])
    var1 <- vcov(obj)[b1.pos, b1.pos]
    var3 <- vcov(obj)[b3.pos, b3.pos]
    cov13 <- vcov(obj)[b1.pos, b3.pos]

    eff1 <- b1 + b3 * s2
    var.eff1 <- var1 + s2^2 * var3 + 2 * s2 * cov13
    se.eff1 <- sqrt(var.eff1)
    low1 <- eff1 - 1.96 * se.eff1
    up1 <- eff1 + 1.96 * se.eff1
    
    b2.pos <- grep(pattern = v2, x = names(coef(obj)), fixed = TRUE)[1]
    b3.pos <- grep(pattern = v2, x = names(coef(obj)), fixed = TRUE)[2]
    b2 <- as.numeric(obj$coef[b2.pos])
    b3 <- as.numeric(obj$coef[b3.pos])
    var2 <- vcov(obj)[b2.pos, b2.pos]
    var3 <- vcov(obj)[b3.pos, b3.pos]
    cov23 <- vcov(obj)[b2.pos, b3.pos]

    eff2 <- b2 + b3 * s1
    var.eff2 <- var2 + s1^2 * var3 + 2 * s1 * cov23
    se.eff2 <- sqrt(var.eff2)
    low2 <- eff2 - 1.96 * se.eff2
    up2 <- eff2 + 1.96 * se.eff2
    
    rug2.dat <- data.frame(obj$model[v2])
    rug1.dat <- data.frame(obj$model[v1])
    
  }
  
  if(class(obj) == "lmerMod"){
    
    # Model matrix always included as obj@frame
    
    v1 <- varnames[1]
    v2 <- varnames[2]
    vlab1 <- varlabs[1]
    vlab2 <- varlabs[2]
    
    b1.pos <- grep(v1, names(fixef(obj)))[1]
    b3.pos <- grep(v1, names(fixef(obj)))[2]
    b1 <- as.numeric(fixef(obj)[b1.pos])
    b3 <- as.numeric(fixef(obj)[b3.pos])
    var1 <- vcov(obj)[b1.pos, b1.pos]
    var3 <- vcov(obj)[b3.pos, b3.pos]
    cov13 <- vcov(obj)[b1.pos, b3.pos]
    
    s1 <- rseq(obj@frame[v1])
    s2 <- rseq(obj@frame[v2])
    
    eff1 <- b1 + b3 * s2
    var.eff1 <- var1 + s2^2 * var3 + 2 * s2 * cov13
    se.eff1 <- sqrt(var.eff1)
    low1 <- eff1 - 1.96 * se.eff1
    up1 <- eff1 + 1.96 * se.eff1
    
    b2.pos <- grep(v2, names(fixef(obj)))[1]
    b3.pos <- grep(v2, names(fixef(obj)))[2]
    b2 <- as.numeric(fixef(obj)[b2.pos])
    b3 <- as.numeric(fixef(obj)[b3.pos])
    var2 <- vcov(obj)[b2.pos, b2.pos]
    var3 <- vcov(obj)[b3.pos, b3.pos]
    cov23 <- vcov(obj)[b2.pos, b3.pos]
    
    eff2 <- b2 + b3 * s1
    var.eff2 <- var2 + s1^2 * var3 + 2 * s1 * cov23
    se.eff2 <- sqrt(var.eff2)
    low2 <- eff2 - 1.96 * se.eff2
    up2 <- eff2 + 1.96 * se.eff2
    
    rug2.dat <- data.frame(obj@frame[v2])
    rug1.dat <- data.frame(obj@frame[v1])
    
  }
  
  plot1.dat <- data.frame(s2, eff1, se.eff1, low1, up1)

  plot2.dat <- data.frame(s1, eff2, se.eff2, low2, up2)  

  if (twoways == FALSE) {

    if(length(unique(model.matrix(obj)[, paste(v2)])) == 2){

    p1 <- ggplot(data = plot1.dat, aes(x = factor(s2), y = eff1), environment = environment()) +     # env. important for rug
      geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
      geom_segment(aes(x = factor(s2), xend = factor(s2), y = low1, yend = up1), color = "black") +
      geom_point() + 
      xlab(paste(vlab2)) + ylab(paste("Effect of ", vlab1, sep = ""))
    }

    if(length(unique(model.matrix(obj)[, paste(v2)])) > 2){
    
   p1 <- ggplot(data = plot1.dat, aes(x = s2, y = eff1), environment = environment()) +     # env. important for rug
      geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
      geom_ribbon(aes(x = s2, ymin = low1, ymax = up1), alpha = 0.25, color = NA) +
      geom_line() + 
      xlab(paste(vlab2)) + ylab(paste("Effect of ", vlab1, sep = ""))
    p1 <- p1 + ggtitle(paste("Conditional effect of \n", vlab1, sep = ""))
    }
            
    if(title == TRUE){
      p1 <- p1 + ggtitle(paste("Conditional effect of \n", vlab1, sep = ""))
    }  
    
    if(rug == TRUE & length(unique(model.matrix(obj)[, paste(v2)])) > 2){
      p1 <- p1 + geom_rug(data = rug2.dat, aes(x = jitter(rug2.dat[, 1], factor = jitter_factor), y = 0), sides = "b", size = rugsize)
    }     
    
    p1 <- p1 + theme_bw()
    return(p1)
  }
  
  if (twoways == TRUE) {
    if(length(unique(model.matrix(obj)[, paste(v2)])) == 2){
     p1 <- ggplot(data = plot1.dat, aes(x = factor(s2), y = eff1), environment = environment()) +     # env. important for rug
      geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
      geom_segment(aes(x = factor(s2), xend = factor(s2), y = low1, yend = up1), color = "black") +
      geom_point() + 
      xlab(paste(vlab2)) + ylab(paste("Effect of ", vlab1, sep = ""))
    }

    if(length(unique(model.matrix(obj)[, paste(v2)])) > 2){
     p1 <- ggplot(data = plot1.dat, aes(x = s2, y = eff1), environment = environment()) +     # env. important for rug
      geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
      geom_ribbon(aes(x = s2, ymin = low1, ymax = up1), alpha = 0.25, color = NA) +
      geom_line() + 
      xlab(paste(vlab2)) + ylab(paste("Effect of ", vlab1, sep = ""))
    }   
    
    p1 <- p1 + ggtitle(paste("Conditional effect of \n", vlab1, sep = "")) + theme_bw()

    if(rug == TRUE & length(unique(model.matrix(obj)[, paste(v2)])) > 2){
      p1 <- p1 + geom_rug(data = rug2.dat, aes(x = jitter(rug2.dat[, 1], factor = jitter_factor), y = 0), sides = "b", size = rugsize)
    }     
    
    if(length(unique(model.matrix(obj)[, paste(v1)])) == 2){
     p2 <- ggplot(data = plot2.dat, aes(x = factor(s1), y = eff2), environment = environment()) +     # env. important for rug
      geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
      geom_segment(aes(x = factor(s1), xend = factor(s1), y = low2, yend = up2), color = "black") +
      geom_point() + 
      xlab(paste(vlab1)) + ylab(paste("Effect of ", vlab2, sep = ""))
    }

    if(length(unique(model.matrix(obj)[, paste(v1)])) > 2){
     p2 <- ggplot(data = plot2.dat, aes(x = s1, y = eff2), environment = environment()) +     # env. important for rug
      geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
      geom_ribbon(aes(x = s1, ymin = low2, ymax = up2), alpha = 0.25, color = NA) +
      geom_line() + 
      xlab(paste(vlab1)) + ylab(paste("Effect of ", vlab2, sep = ""))
    
    }  
    
    p2 <- p2 + ggtitle(paste("Conditional effect of \n", vlab2, sep = "")) + theme_bw()
    
    if(rug == TRUE & length(unique(model.matrix(obj)[, paste(v1)])) > 2){
      p2 <- p2 + geom_rug(data = rug1.dat, aes(x = jitter(rug1.dat[, 1], factor = jitter_factor), y = 0), sides = "b", size = rugsize)
    }     
    
    grid.arrange(p1, p2, ncol = 2)
  }
}