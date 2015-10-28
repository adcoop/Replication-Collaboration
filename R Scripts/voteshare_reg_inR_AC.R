
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
dat2o <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Matlab Data/voteshare2.csv")
dat3o <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Matlab Data/voteshare3.csv")
dat1 <- read.csv("Data/voteshare1.csv")
dat2 <- read.csv("Data/voteshare2.csv")
dat3 <- read.csv("Data/voteshare3.csv")

source("R Scripts/Functions/omegamatrix.R")
source("R Scripts/Functions/IPW and FE function.R")
<<<<<<< HEAD
source("R Scripts/Functions/ri_fxn.R")
=======
>>>>>>> origin/master

dep_matrix1 <- omegamatrix(dat1)
dep_matrix2 <- omegamatrix(dat2)
dep_matrix2o <- omegamatrix(dat2o)
dep_matrix3 <- omegamatrix(dat3)
dep_matrix3o <- omegamatrix(dat3o)

Spec1 <- IPW.FE.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014", dep_matrix1)
Spec2 <- IPW.FE.fxn(dat2, "voteshare_spec2_2009", "voteshare_spec2_2014", dep_matrix2)
Spec3 <- IPW.FE.fxn(dat3, "voteshare_spec3_2009", "voteshare_spec3_2014", dep_matrix3)

<<<<<<< HEAD
Spec1.ipw.ri <- ri(dat1, Z = 'treatany', Y = 'voteshare_spec1_2014',
               cov = 'voteshare_spec1_2009', prob = 'prob_treatany', iter = 10000)
Spec2.ipw.ri <- ri(dat2, Z = 'treatany', Y = 'voteshare_spec2_2014',
               cov = 'voteshare_spec2_2009', prob = 'prob_treatany', iter = 10000)
Spec3.ipw.ri <- ri(dat3, Z = 'treatany', Y = 'voteshare_spec3_2014',
               cov = 'voteshare_spec3_2009', prob = 'prob_treatany', iter = 10000)
Spec1.fe.ri <- ri(dat1, Z = 'treatany', Y = 'voteshare_spec1_2014',
                  cov = c('voteshare_spec1_2009','num_eligible1','num_eligible2'),
                  iter = 10000, prob = 'prob_treatany', ipw = F)
Spec2.fe.ri <- ri(dat2, Z = 'treatany', Y = 'voteshare_spec2_2014',
                  cov = c('voteshare_spec2_2009','num_eligible1','num_eligible2'),
                  iter = 10000, prob = 'prob_treatany')
Spec3.fe.ri <- ri(dat3, Z = 'treatany', Y = 'voteshare_spec3_2014',
                   cov = c('voteshare_spec3_2009','num_eligible1','num_eligible2'),
                  iter = 10000, prob = 'prob_treatany', ipw = F)

Spectable <- cbind(rbind(Spec1, 'p.ri' = c(Spec1.ipw.ri$p, Spec1.fe.ri$p)), 
                   rbind(Spec2, 'p.ri' = c(Spec2.ipw.ri$p, Spec2.fe.ri$p)),
                   rbind(Spec3, 'p.ri' = c(Spec3.ipw.ri$p, Spec3.fe.ri$p)))
=======
Spectable <- cbind(Spec1, Spec2, Spec3)
Spectable

# Heterogeneous Effects ---------------------------------------------------
# Heterogeneous treatment effects by rural
hist(dat1$rural_pc, breaks=20, xlab="Rural Perc. in AC", main="Histogram of Percent Rural in AC")
dat1$rur.greater90[dat1$rural_pc>90] <- 1
dat1$rur.greater90[dat1$rural_pc<=90] <- 0
dat1$rur.greater80[dat1$rural_pc>80] <- 1
dat1$rur.greater80[dat1$rural_pc<=80] <- 0

#FE.rur <- lm(voteshare_spec1_2014 ~ treatany*rural_pc + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1)
#FE.rur90 <- lm(voteshare_spec1_2014 ~ treatany*rur90.dum + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1)
#summary(FE.rur90)

FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014"
           ,interaction = "rur.greater90", dep_matrix1, labels=c("Treat","Rural >90 pc","Treat:Rural90"))
FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014"
           ,interaction = "rur.greater80", dep_matrix1, labels=c("Treat","Rural >80 pc","Treat:Rural80"))
FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014"
           ,interaction = "rural_pc", dep_matrix1,labels=c("Treat","Rural pc","Treat:Rural pc"))

>>>>>>> origin/master



