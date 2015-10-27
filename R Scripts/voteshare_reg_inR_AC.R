
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

dep_matrix1 <- omegamatrix(dat1)
dep_matrix2 <- omegamatrix(dat2)
dep_matrix2o <- omegamatrix(dat2o)
dep_matrix3 <- omegamatrix(dat3)
dep_matrix3o <- omegamatrix(dat3o)

Spec1 <- IPW.FE.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014", dep_matrix1)
Spec2 <- IPW.FE.fxn(dat2, "voteshare_spec2_2009", "voteshare_spec2_2014", dep_matrix2)
Spec3 <- IPW.FE.fxn(dat3, "voteshare_spec3_2009", "voteshare_spec3_2014", dep_matrix3)

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
