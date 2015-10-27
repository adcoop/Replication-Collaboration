
##########
##########
## Voteshare Regressions
##########
##########


rm(list=ls())

library(foreign)
library(dplyr)

## set to your local clone
setwd('~/Desktop/Replication Collaboration Clone')

dat1 <- read.csv("Data/voteshare1.csv")
# dat1o <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Matlab Data/voteshare1.csv")

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
IPW1 = (lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009, data=dat1, weights=wgt_treatany))
summary(IPW1)
w <- sqrt(dat1$wgt_treatany)
X.IPW1 <- cbind(w, dat1$treatany*w, dat1$voteshare_spec1_2009*w)
Y.IPW1 <- dat1$voteshare_spec1_2014*w
IPW1.beta_hat <- solve(t(X.IPW1)%*%X.IPW1)%*%(t(X.IPW1)%*%Y.IPW1)
IPW1.resid <- Y.IPW1 - X.IPW1%*%IPW1.beta_hat
IPW1.vhat.notrobust <- (1/(N-ncol(X.IPW1)))*as.numeric((t(IPW1.resid) %*% IPW1.resid))*solve(t(X.IPW1)%*%X.IPW1)
sqrt(diag(IPW1.vhat.notrobust))
IPW1.omega_hat <- dep_matrix*(IPW1.resid %*% t(IPW1.resid))
IPW1.vhat <- solve(t(X.IPW1)%*%X.IPW1) %*% (t(X.IPW1) %*% IPW1.omega_hat %*% X.IPW1) %*% solve(t(X.IPW1)%*%X.IPW1) 
IPW1.sd_hat <- sqrt(diag(IPW1.vhat))
IPW1.sd_hat
IPW1.p <- 1 - pnorm(abs(IPW1.beta_hat[2]/IPW1.sd_hat[2]))
IPW1.p

# FE - Spec 1 (Table 6 Column 2)
FE1 = (lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1))
summary(FE1)
ATE.FE1 <- coefficients(FE1)[2]
print(ATE.FE1)
X.FE1 <- cbind(1,dat1$treatany, dat1$voteshare_spec1_2009,dat1$num_eligible1,dat1$num_eligible2)
Y.FE1 <- dat1$voteshare_spec1_2014
FE1.beta_hat <- solve(t(X.FE1)%*%X.FE1)%*%(t(X.FE1)%*%Y.FE1)
FE1.resid <- Y.FE1 - X.FE1%*%FE1.beta_hat
FE1.vhat.notrobust <- (1/(N-ncol(X.FE1)))*as.numeric((t(FE1.resid) %*% FE1.resid))*solve(t(X.FE1)%*%X.FE1)
sqrt(diag(FE1.vhat.notrobust))
FE1.omega_hat <- dep_matrix*(FE1.resid %*% t(FE1.resid))
FE1.vhat <- solve(t(X.FE1)%*%X.FE1) %*% (t(X.FE1) %*% FE1.omega_hat %*% X.FE1) %*% solve(t(X.FE1)%*%X.FE1) 
FE1.sd_hat <- sqrt(diag(FE1.vhat))
FE1.sd_hat
FE1.p = 1 - pnorm(abs(FE1.beta_hat[2]/FE1.sd_hat[2]))

# Heterogeneous treatment effects by rural (they drop the state names in the Stata - need to use a new version to get back)
dat1$rur90.dum[dat1$rural_pc>90] <- 1
dat1$rur90.dum[dat1$rural_pc<=90] <- 0
dat1$rur80.dum[dat1$rural_pc>80] <- 1
dat1$rur80.dum[dat1$rural_pc<=80] <- 0

hist(dat1$rural_pc, breaks=20)
dat1$rur.cat<-NULL
dat1$rur.cat[dat1$rural_pc<=60] <- "rur60less"
dat1$rur.cat[dat1$rural_pc<=80 & dat1$rural_pc>60] <- "rur60.80"
dat1$rur.cat[dat1$rural_pc<=90 & dat1$rural_pc>80] <- "rur80.90"
dat1$rur.cat[dat1$rural_pc<=100& dat1$rural_pc>90] <- "rur90.100"
dat1$rur.cat <- as.factor(dat1$rur.cat)
dat1 <- within(dat1, rur.cat <- relevel(rur.cat, ref = "rur60less"))
table(dat1$rur.cat)

FE <- lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1)
FE.rur <- lm(voteshare_spec1_2014 ~ treatany*rur90.dum + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1)
FE.rur.cat <- lm(voteshare_spec1_2014 ~ treatany*rur.cat + voteshare_spec1_2009 + num_eligible1 + num_eligible2 , data=dat1)
summary(FE.rur)
summary(FE.rur.cat)
hist(dat1$rural_pc)

# Heterogeneous treatment effects by SC/ST
hist(dat1$scst_pc, breaks=20)
dat1$sc.dum40 <-0
dat1$sc.dum40[dat1$scst_pc>40] <- 1
dat1$sc.cat<-NULL
dat1$sc.cat[dat1$scst_pc<=20] <- "sc20less"
dat1$sc.cat[dat1$scst_pc<=40 & dat1$scst_pc>20] <- "sc20.40"
dat1$sc.cat[dat1$scst_pc<=60 & dat1$scst_pc>40] <- "sc40.60"
dat1$sc.cat[dat1$scst_pc<=100& dat1$scst_pc>60] <- "sc60.100"
dat1$sc.cat <- as.factor(dat1$sc.cat)
dat1 <- within(dat1, sc.cat <- relevel(sc.cat, ref = "sc60.100"))
table(dat1$sc.cat)

FE <- lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1)
FE.sc.cat <- lm(voteshare_spec1_2014 ~ treatany*sc.cat + rural_pc + voteshare_spec1_2009 + num_eligible1 + num_eligible2 , data=dat1)
summary(FE)
summary(FE.sc.cat)
