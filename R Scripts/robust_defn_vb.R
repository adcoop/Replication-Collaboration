
##########
##########
## Robustness to Other Defns of Vote Buyers
##########
##########


rm(list=ls())

library(foreign)
library(dplyr)

## set to your local clone
setwd('~/Desktop/Replication Collaboration Clone')

## read in RI function
source("R Scripts/Functions/ri_fxn.R")

## read in my new data and their original data and merge
dat1 <- read.csv("Data/voteshare1.csv")
dat1o <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Matlab Data/voteshare1.csv")
dat1o <- data.frame('voteshare_spec1_2014o' = dat1o$voteshare_spec1_2014, 
                    'voteshare_spec1_2009o' = dat1o$voteshare_spec1_2009, 
                    'state_name' = dat1o$state_name, 'ac_num' = dat1o$ac_num)
dat1 <- left_join(dat1, dat1o, by = c('state_name', 'ac_num'))

dat2 <- read.csv("Data/voteshare2.csv")
dat2o <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Matlab Data/voteshare2.csv")
dat2o <- data.frame('voteshare_spec2_2014o' = dat2o$voteshare_spec2_2014, 
                    'voteshare_spec2_2009o' = dat2o$voteshare_spec2_2009, 
                    'state_name' = dat2o$state_name, 'ac_num' = dat2o$ac_num)
dat2 <- left_join(dat2, dat2o, by = c('state_name', 'ac_num'))

dat3 <- read.csv("Data/voteshare3.csv")
dat3o <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Matlab Data/voteshare3.csv")
dat3o <- data.frame('voteshare_spec3_2014o' = dat3o$voteshare_spec3_2014, 
                    'voteshare_spec3_2009o' = dat3o$voteshare_spec3_2009, 
                    'state_name' = dat3o$state_name, 'ac_num' = dat3o$ac_num)
dat3 <- left_join(dat3, dat3o, by = c('state_name', 'ac_num'))


## visualize the differences between the defns that are supposed to be the same
pdf("Figures/new_vb_defn_0.pdf")
par(mar=c(5,5,3,2))
plot(dat1$voteshare_spec1_2014o, dat1$voteshare_spec1_2014,
     xlab = 'Original Vote Buyer Vote Share', ylab = "New Vote Buyer Vote Share",
     main = 'Identified by Any Journalists', cex.main = 2, cex.lab = 2, cex.axis = 2)
dev.off()

plot(dat1$voteshare_spec1_2009o, dat1$voteshare_spec1_2009,
     xlab = 'Original Vote Buyer Vote Share', ylab = "New Vote Buyer Vote Share",
     main = 'Identified by Any Journalists', cex.main = 2, cex.lab = 2, cex.axis = 2)

  ## there is something wrong with dat2 - state and poll date
plot(dat2$voteshare_spec2_2014o, dat2$voteshare_spec2_2014,
     xlab = 'Original Vote Buyer Vote Share', ylab = "New Vote Buyer Vote Share",
     main = 'Identified by Any Journalists', cex.main = 2, cex.lab = 2, cex.axis = 2)

plot(dat2$voteshare_spec2_2009o, dat2$voteshare_spec2_2009,
     xlab = 'Original Vote Buyer Vote Share', ylab = "New Vote Buyer Vote Share",
     main = 'Identified by Any Journalists', cex.main = 2, cex.lab = 2, cex.axis = 2)

plot(dat3$voteshare_spec3_2014o, dat3$voteshare_spec3_2014,
     xlab = 'Original Vote Buyer Vote Share', ylab = "New Vote Buyer Vote Share",
     main = 'Identified by Any Journalists', cex.main = 2, cex.lab = 2, cex.axis = 2)

plot(dat3$voteshare_spec3_2009o, dat3$voteshare_spec3_2009,
     xlab = 'Original Vote Buyer Vote Share', ylab = "New Vote Buyer Vote Share",
     main = 'Identified by Any Journalists', cex.main = 2, cex.lab = 2, cex.axis = 2)


## check the subset of dat2 that didn't merge correctly
t <- select(dat2, state_name, pc_name, poll_date, ac_name, pct_nda.spec2, pct_upa.spec2, pct_oth.spec2, voteshare_spec2_2014, voteshare_spec2_2014o)
t$voteshare_spec2_2014 <- round(t$voteshare_spec2_2014, 3)
t$voteshare_spec2_2014o <- round(t$voteshare_spec2_2014o, 3)
t <- filter(t, voteshare_spec2_2014!=voteshare_spec2_2014o)
View(t)


## output figures plotting the old and new defns against each other
for (i in 1:9){
  pdf(paste0("Figures/new_vb_defn_", i, ".pdf"))
  par(mar=c(5,5,3,2))
  plot(dat1$voteshare_spec1_2014o, dat1[,paste0('voteshare_spec1_2014_0.', i)],
       xlab = 'Original Vote Buyer Vote Share', ylab = "New Vote Buyer Vote Share",
       main = paste0('Identified by >= ', i*10, '% of Journalists'),
       cex.main = 2, cex.lab = 2, cex.axis = 2)
  dev.off()
}


## run regressions and output into a matrix for plotting

out <- data.frame('coef' = rep(NA, 10),
                  'se' = rep(NA, 10),
                  'p.bar' = rep(NA, 10),
                  'p.ri' = rep(NA, 10))
outs <- c('voteshare_spec1_2014o', 'voteshare_spec1_2014', paste0('voteshare_spec1_2014_0.', seq(1:9)))
pres <- c('voteshare_spec1_2009o', 'voteshare_spec1_2009', paste0('voteshare_spec1_2009_0.', seq(1:9)))


## set up omega matrix

N <- dim(dat1)[1]
station1 <- dat1[,'station_id1']
station2 <- dat1[,'station_id2']
station3 <- dat1[,'station_id3']
dep_matrix <- matrix(0,N,N) 
for (i in 1:N){
  for (j in 1:N){
    dep_matrix[i,j] <- ifelse(station1[i] == station1[j],1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station1[i] == station2[j],1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station1[i] == station3[j],1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station2[i] == station1[j] & station2[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station2[i] == station2[j] & station2[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station2[i] == station3[j] & station2[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station3[i] == station1[j] & station3[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station3[i] == station2[j] & station3[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station3[i] == station3[j] & station3[i] != -9,1,dep_matrix[i,j])
  }
}
num_terms <- sum(dep_matrix)

# IPW - Spec 1 (Table 6 Col 1)
for (i in 1:10){
  N <- dim(dat1)[1]
  dat1$out <- dat1[,outs[i]]
  dat1$pre <- dat1[,pres[i]]
  IPW1 = (lm(out ~ treatany + pre, data=dat1, weights=wgt_treatany))
  w <- sqrt(dat1$wgt_treatany)
  X.IPW1 <- cbind(w, dat1$treatany*w, dat1$pre*w)
  Y.IPW1 <- dat1$out*w
  IPW1.beta_hat <- solve(t(X.IPW1)%*%X.IPW1)%*%(t(X.IPW1)%*%Y.IPW1)
  IPW1.resid <- Y.IPW1 - X.IPW1%*%IPW1.beta_hat
  IPW1.vhat.notrobust <- (1/(N-ncol(X.IPW1)))*as.numeric((t(IPW1.resid) %*% IPW1.resid))*solve(t(X.IPW1)%*%X.IPW1)
  IPW1.omega_hat <- dep_matrix*(IPW1.resid %*% t(IPW1.resid))
  IPW1.vhat <- solve(t(X.IPW1)%*%X.IPW1) %*% (t(X.IPW1) %*% IPW1.omega_hat %*% X.IPW1) %*% solve(t(X.IPW1)%*%X.IPW1) 
  IPW1.se_hat <- sqrt(diag(IPW1.vhat))
  IPW1.p = 1 - pnorm(abs(IPW1.beta_hat[2]/IPW1.se_hat[2]))
  out$coef[i] <- summary(IPW1)$coefficient['treatany',1]
  out$se[i] <- IPW1.se_hat[2]
  out$p.bar[i] <- IPW1.p
  
  ## get p-value from RI
  ri.out <- ri(dat1, Z = 'treatany', Y = 'out', cov = 'pre', prob = 'prob_treatany', iter = 10000)
  out$p.ri[i] <- ri.out$p
  
}

out$up90 <- out$coef + 1.64*out$se
out$lo90 <- out$coef - 1.64*out$se
out$up95 <- out$coef + 1.96*out$se
out$lo95 <- out$coef - 1.96*out$se

pdf("Figures/coef_journo_defn.pdf")
par(mar=c(5,5,3,3))
plot(x = seq(1,10), y = out$coef, cex = 2, col = 'red', pch = 16, 
     cex.axis = 1.8, cex.lab = 1.8, cex.main = 2,
     mar=c(2,2,2,2), ylim = c(min(out$lo95), max(out$up95)),
     ylab = 'Average Treatment Effect',
     xaxt = "n", xlab = "Proportion Journalists Identifying as Vote Buyer")
axis(side = 1, at = seq(1,10,1), cex.axis = 1.4,
     labels = c('any', '>10%', '>20%', '>30%', '>40%', ">50%", '>60%', '>70%', '>80%', '>90%'))
for (i in 1:10){
  lines(x = rep(i, 2), y = c(out$up90[i], out$lo90[i]), lwd = 6, col = 'red')
  lines(x = rep(i, 2), y = c(out$up95[i], out$lo95[i]), lwd = 4, col = 'red')
}
abline(h=0, lty = 2)
dev.off()

pdf("Figures/p_journo_defn.pdf")
plot(x = seq(1:10), y = out$p.bar, type = "l", lty = 3, col = 'blue', lwd = 3, 
     ylim = c(0,0.6), xaxt = "n", ylab = 'p-value', 
     xlab = 'Proportion Journalists Identifying as Vote Buyer', cex.lab = 1.8, cex.axis = 1.8)
lines(x = seq(1:10), y = out$p.ri, lty = 4, col = 'green', lwd = 3)
abline(h = .1, lty = 2)
axis(side = 1, at = seq(1,10,1), cex.axis = 1.4, cex.lab = 2,
     labels = c('any', '>10%', '>20%', '>30%', '>40%', ">50%", '>60%', '>70%', '>80%', '>90%'))
legend('topleft', lty = c(3,4), lwd =3, col = c('blue', 'green'), 
       legend = c('Barrios', 'RI'), cex = 2)
dev.off()



