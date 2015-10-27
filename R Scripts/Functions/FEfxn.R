FEfxn <- function(data,voteshare2009, voteshare2014,dep_matrix){
  X.FE <- cbind(1,data$treatany, data[,voteshare2009],data$num_eligible1,data$num_eligible2)
  Y.FE <- data[,voteshare2014]
  FE.beta_hat <- solve(t(X.FE)%*%X.FE)%*%(t(X.FE)%*%Y.FE)
  FE.resid <- Y.FE - X.FE%*%FE.beta_hat
  FE.omega_hat <- dep_matrix*(FE.resid %*% t(FE.resid))
  FE.vhat <- solve(t(X.FE)%*%X.FE) %*% (t(X.FE) %*% FE.omega_hat %*% X.FE) %*% solve(t(X.FE)%*%X.FE) 
  FE.se_hat <- sqrt(diag(FE.vhat))
  FE.p = 1 - pnorm(abs(FE.beta_hat[2]/FE.se_hat[2]))
  FE.sst <- sum((Y.FE - mean(Y.FE))^2)
  FE.rsq <- 1 - sum(FE.resid^2)/FE.sst
  values <- c(FE.beta_hat[2],FE.se_hat[2],FE.p, FE.rsq)
  names(values) <- c("ATE from FE", "SE from FE", "p from FE", "Rsq from FE")
  return(values)
}