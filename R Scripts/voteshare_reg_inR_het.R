
##########
##########
## Voteshare Regressions
##########
##########


rm(list=ls())

library(foreign)
library(dplyr)
library(xtable)
library(stargazer)

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
#dep_matrix2o <- omegamatrix(dat2o)
dep_matrix3 <- omegamatrix(dat3)

# Heterogeneous Effects ---------------------------------------------------
# Heterogeneous treatment effects by rural
pdf(file = "~/Desktop/Replication Collaboration Clone/Figures/histrurpc.pdf",width=5,height=5)
hist(dat1$rural_pc, breaks=20, xlab="Rural Perc. in AC", main="Histogram of Percent Rural in AC")
dev.off()
dat1$rur.greater90[dat1$rural_pc>90] <- 1
dat1$rur.greater90[dat1$rural_pc<=90] <- 0
dat1$rur.greater80[dat1$rural_pc>80] <- 1
dat1$rur.greater80[dat1$rural_pc<=80] <- 0
#FE.rur <- lm(voteshare_spec1_2014 ~ treatany*rural_pc + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1)
#FE.rur90 <- lm(voteshare_spec1_2014 ~ treatany*rur90.dum + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1)
#summary(FE.rur90)
het.rur90 <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014"
           ,interaction = "rur.greater90", dep_matrix1, labels=c("Treat","Rural >90 pc","Treat:Rural90"))
het.rur80 <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014"
           ,interaction = "rur.greater80", dep_matrix1, labels=c("Treat","Rural >80 pc","Treat:Rural80"))
het.rurpc <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014"
           ,interaction = "rural_pc", dep_matrix1,labels=c("Treat","Rural pc","Treat:Rural pc"))
cat(print(xtable(het.rur90),floating = F), file="Figures/hetrur90.tex", sep="\n")
cat(print(xtable(het.rur80),floating = F), file="Figures/hetrur80.tex", sep="\n")
cat(print(xtable(het.rurpc),floating = F), file="Figures/hetrurpc.tex", sep="\n")

# Heterogeneous treatment effects by SC/ST
pdf(file = "~/Desktop/Replication Collaboration Clone/Figures/histscstpc.pdf",width=5,height=5)
hist(dat1$scst_pc, breaks=20, xlab="SC/ST Perc. in AC", main="Histogram of Percent SC/ST in AC")
dev.off()
dat1$sc.greaterthan50 <-0
dat1$sc.greaterthan50[dat1$scst_pc>50] <- 1
dat1$sc.greaterthan25 <-0
dat1$sc.greaterthan25[dat1$scst_pc>25] <- 1
het.sc50 <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014"
                        ,interaction = "sc.greaterthan50", dep_matrix1, labels=c("Treat","SC/ST >50 pc","Treat:SC/ST50"))
het.sc25 <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014"
                        ,interaction = "sc.greaterthan25", dep_matrix1, labels=c("Treat","SC/ST >25 pc","Treat:SC/ST25"))
het.scstpc <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014"
                        ,interaction = "scst_pc", dep_matrix1,labels=c("Treat","ST/SC pc","Treat:SC/ST pc"))
cat(print(xtable(het.sc50),floating = F), file="Figures/hetsc50.tex", sep="\n")
cat(print(xtable(het.sc25),floating = F), file="Figures/hetsc25.tex", sep="\n")
cat(print(xtable(het.scstpc),floating = F), file="Figures/hetscstpc.tex", sep="\n")

# Heterogeneous treatment effects by State
table(dat1$treatany,dat1$state_name)
state.table <- t(table(dat1$treatany,dat1$state_name))
colnames(state.table)<- c("Control AC", "Treated AC")
cat(print(xtable(state.table),floating = F), file="Figures/statetreatment.tex", sep="\n")

# dat1.state <- cbind(dat1,model.matrix( ~ state_name-1, data=dat1))
# state.dummies <- colnames(model.matrix( ~ state_name-1, data=dat1))
# het.state <- FE.het.multi.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014" ,interaction = state.dummies[c(1,3:9)], dep_matrix1, round=3)
# Excludes dummy for Bihar, because Bihar had no treated ACs

FE.state <- lm(voteshare_spec1_2014 ~ treatany*state_name + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1)

covlabels <- c("Treat", "State Bihar", "State Chattisgarh", "State Jharkhand", "State Karnataka", "State Madhya Pradesh", "State Maharashtra", "State Orissa", "State Rajasthan", "State Uttar Pradesh", "Vote Share of Vote Buying Parties 2009", "Num Radio 1", "Num Radio 2", "Treat:Bihar", "Treat:Chattisgarh", "Treat:Jharkhand", "Treat:Karnataka", "Treat:Madhya Pradesh", "Treat:Maharashtra", "Treat:Orissa", "Treat:Rajasthan", "Treat:Uttar Pradesh", "Constant")

cat(stargazer(FE.state, single.row = T, covariate.labels = covlabels, dep.var.labels = "Vote Share of Vote Buying Parties 2014"), file="Figures/statehet.tex", sep="\n")

