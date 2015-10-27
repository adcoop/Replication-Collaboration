IPW.FE.fxn <- function(data,voteshare2009, voteshare2014,dep_matrix,round=2){
  w <- sqrt(data$wgt_treatany)
  X.IPW <- cbind(w, data$treatany*w, data[,voteshare2009]*w)
  Y.IPW <- data[,voteshare2014]*w
  IPW.beta_hat <- solve(t(X.IPW)%*%X.IPW)%*%(t(X.IPW)%*%Y.IPW)
  IPW.resid <- Y.IPW - X.IPW%*%IPW.beta_hat
  IPW.omega_hat <- dep_matrix*(IPW.resid %*% t(IPW.resid))
  IPW.vhat <- solve(t(X.IPW)%*%X.IPW) %*% (t(X.IPW) %*% IPW.omega_hat %*% X.IPW) %*% solve(t(X.IPW)%*%X.IPW) 
  IPW.se_hat <- sqrt(diag(IPW.vhat))
  IPW.p <- 1 - pnorm(abs(IPW.beta_hat[2]/IPW.se_hat[2]))
  IPW.sst <- sum((Y.IPW - mean(Y.IPW))^2)
  IPW.rsq <- 1 - sum(IPW.resid^2)/IPW.sst
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
  values.IPW <- c(IPW.beta_hat[2],IPW.se_hat[2],IPW.p, IPW.rsq)
  values.FE <- c(FE.beta_hat[2],FE.se_hat[2],FE.p, FE.rsq)
  values <- rbind(values.IPW, values.FE)
  colnames(values) <- c("ATE", "SE", "p", "Rsq")
  rownames(values) <- c("IPW", "FE")
  values <- round(values,round)
  return(t(values))
}