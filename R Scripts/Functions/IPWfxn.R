IPWfxn <- function(data,voteshare2009, voteshare2014,dep_matrix){
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
  values <- c(IPW.beta_hat[2],IPW.se_hat[2],IPW.p, IPW.rsq)
  names(values) <- c("ATE from IPW", "SE from IPW", "p from IPW", "Rsq from IPW")
  return(values)
}