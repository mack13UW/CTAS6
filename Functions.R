#function to collate fit indices from each sample
core_fitmeasures <- function(fit, fitindices = v$fitindices) {
  x <- fitmeasures(fit)
  return(round(x[fitindices], digits=5))
}

#function to collate robust fit indices from each sample
core_fitmeasures_robust <- function(fit, fitindices = v$fitindices_robust) {
  x <- fitmeasures(fit)
  return(round(x[fitindices], digits=5))
}

#function to extract parameter estimates 
get_est <- function(fit) {
  x <- standardizedsolution(fit)
  lambda <- round(x$est.std[which(x$op == "=~")], digits=3)
  se <- round(x$se[which(x$op == "=~")], digits=3)
  r2 <- as.vector(round(as.numeric(inspect(fit, "r2")), digits=3))
  return(cbind(lambda, se, r2))
}

#function to extract parameter estimates 
get_est_mg <- function(fit) {
  # Standardized factor loadings
  lambda1 <- as.vector(as.numeric(inspect(fit,"std")$'Male'$'lambda')) 
  lambda2 <- as.vector(as.numeric(inspect(fit,"std")$'Female'$'lambda'))
  
  # R^2
  r21 <- as.vector(as.numeric(inspect(fit, "r2")$'Male'))
  r22 <- as.vector(as.numeric(inspect(fit, "r2")$'Female'))
  
  # Combine
  out <- cbind(lambda1, r21, lambda2, r22)
  return(out)
}

#function to extract threshold estimates 
get_threshold_mg <- function(fit, df) {
  
  # Output thresholds
  std.tau1 <- as.vector(as.numeric(inspect(fit,what="std")$'Male'$tau)) 
  std.tau2 <- as.vector(as.numeric(inspect(fit,"std")$'Female'$tau))
  
  pe <-parameterEstimates(fit)
  k <- which(colnames(df) == "ctar_q1_1")
  n1 <- length(df$gender[which(df$gender=="Female")])
  n2 <- length(df$gender[which(df$gender=="Male")])
  thres1 <- NULL
  thres2 <- NULL
  var1 <- NULL
  var2 <- NULL
  for (i in 1:17){
    item <- item <- names(df)[[k-1+i]]
    
    # Thresholds
    t1 <- pe[pe$lhs==item & pe$op=="|" & pe$group=="1", "est"]
    thres1 <- c(thres1, t1) 
    t2 <- pe[pe$lhs==item & pe$op=="|" & pe$group=="2", "est"]
    thres2 <- c(thres2, t2) 
    
    # Variances
    v1 <- pe[pe$lhs==item & pe$op=="~~" & pe$rhs==item & pe$group=="1", "est"]
    var1 <- c(var1, v1)
    v2 <- pe[pe$lhs==item & pe$op=="~~" & pe$rhs==item & pe$group=="2", "est"]
    var2 <- c(var2, v2)
  }
  # Cohen's d
  d <- (thres1-thres2)/ sqrt(((n1-1)*v1+(n2-1)*v2)/(n1+n2-2))
  
  # Combine
  out <- cbind(std.tau1, std.tau2, thres1, thres2, rep(var1,each=3), rep(var2,each=3), abs(d))
  return(out)
}

#function to extract threshold estimates 
get_residual_mg <- function(fit) {
  # Standardized error variances
  theta1 <- as.data.frame(as.numeric(inspect(fit,"std")$'Male'$theta))
  theta2 <- as.data.frame(as.numeric(inspect(fit,"std")$'Female'$theta))
  
  # Combine
  out <- cbind(theta1, theta2)
  return(out)
}

#function to extract modification indices
get_modind <- function(mod) {
  x <- mod[1,]
  xx <- paste(x$lhs, x$op, x$rhs, collapse = " ")
  index <- round(x$mi, digits=2)
  return(cbind(xx, index))
}
