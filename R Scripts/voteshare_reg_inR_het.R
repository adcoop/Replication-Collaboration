
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
dep_matrix2o <- omegamatrix(dat2o)
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
het.rur90 <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014",interaction = "rur.greater90", dep_matrix1, labels=c("Treat","Rural >90 pc","Treat:Rural90"))
het.rur80 <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014",interaction = "rur.greater80", dep_matrix1, labels=c("Treat","Rural >80 pc","Treat:Rural80"))
het.rurpc <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014",interaction = "rural_pc", dep_matrix1,labels=c("Treat","Rural pc","Treat:Rural pc"))
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
het.sc50 <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014",interaction = "sc.greaterthan50", dep_matrix1, labels=c("Treat","SC/ST >50 pc","Treat:SC/ST50"))
het.sc25 <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014",interaction = "sc.greaterthan25", dep_matrix1, labels=c("Treat","SC/ST >25 pc","Treat:SC/ST25"))
het.scstpc <- FE.het.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014",interaction = "scst_pc", dep_matrix1,labels=c("Treat","ST/SC pc","Treat:SC/ST pc"))
cat(print(xtable(het.sc50),floating = F), file="Figures/hetsc50.tex", sep="\n")
cat(print(xtable(het.sc25),floating = F), file="Figures/hetsc25.tex", sep="\n")
cat(print(xtable(het.scstpc),floating = F), file="Figures/hetscstpc.tex", sep="\n")

# Heterogeneous treatment effects by State
table(dat1$treatany,dat1$state_name)
state.table <- t(table(dat1$treatany,dat1$state_name))
colnames(state.table)<- c("Control AC", "Treated AC")
cat(print(xtable(state.table),floating = F), file="Figures/statetreatment.tex", sep="\n")
FE.state <- lm(voteshare_spec1_2014 ~ treatany*state_name + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1)
covlabels <- c("Treat", "State Bihar", "State Chattisgarh", "State Jharkhand", "State Karnataka", "State Madhya Pradesh", "State Maharashtra", "State Orissa", "State Rajasthan", "State Uttar Pradesh", "Vote Share of Vote Buying Parties 2009", "Num Radio 1", "Num Radio 2", "Treat:Bihar", "Treat:Chattisgarh", "Treat:Jharkhand", "Treat:Karnataka", "Treat:Madhya Pradesh", "Treat:Maharashtra", "Treat:Orissa", "Treat:Rajasthan", "Treat:Uttar Pradesh", "Constant")
cat(stargazer(FE.state, single.row = T, covariate.labels = covlabels, dep.var.labels = "Vote Share of Vote Buying Parties 2014"), file="Figures/statehet.tex", sep="\n")
# dat1.state <- cbind(dat1,model.matrix( ~ state_name-1, data=dat1))
# state.dummies <- colnames(model.matrix( ~ state_name-1, data=dat1))
# het.state <- FE.het.multi.fxn(dat1, "voteshare_spec1_2009", "voteshare_spec1_2014" ,interaction = state.dummies[c(1,3:9)], dep_matrix1, round=3)
# Excludes dummy for Bihar, because Bihar had no treated ACs

# Heterogeneous treatment effects by Poll Date
treat.poll <- t(table(dat1$treatany, dat1$poll_date))
colnames(treat.poll) <- c("C", "T")
cat(print(xtable(treat.poll),floating = F), file="Figures/treatpoll.tex", sep="\n")
dat1poll <- subset(dat1, poll_date!="2014-05-12")
FE.poll <- lm(voteshare_spec1_2014 ~ treatany*poll_date + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=dat1poll)
covlabels <- c("Treat", "Poll 2014-04-17", "Poll 2014-04-24", "Poll 2014-04-30", "Poll 2014-05-07", "Vote Share VB 2009", "Num Radio 1", "Num Radio 2", "Treat:Poll 2014-04-17", "Treat:Poll 2014-04-24", "Treat:Poll 2014-04-30", "Treat:Poll 2014-05-07",  "Constant")
cat(stargazer(FE.poll, single.row = T,  dep.var.labels = "Vote Share VB 2014", notes = "Omitted Date 2014-04-10, Excludes 2014-05-12", notes.append = T,
              covariate.labels = covlabels), file="Figures/pollhet.tex", sep="\n")


cat(print(xtable(table(dat1$poll_date, dat1$state_name)),floating = F), file="Figures/pollstate.tex", sep="\n")

# Competition -------------------------------------------------------------
elec <- read.csv("Data/marginvictory_data.csv")
names(elec)
elec <- left_join(dat1, 
                  select(elec, state_name, ac_num, ac_name,
                         margin_2009, margin_2014, winner_party2009, 
                         winner_party2014,runnerup_party2009, runnerup_party2014, winner_votes2014, runnerup_votes2014, winner_votes2009, runnerup_votes2009, total_ac_votes2009, total_ac_votes2014 ),
                  by = c('state_name', 'ac_num', 'ac_name'))

hist(elec$margin_2009)
hist(elec$margin_2014)
pdf(file = "~/Desktop/Replication Collaboration Clone/Figures/histcomp.pdf",width=5,height=5)
hist(elec$margin_2009, breaks=20, xlab="Margin Victory 2009", main="Histogram of Margin of Victory in 2009")
dev.off()
FE.margin2009 <- lm(voteshare_spec1_2014 ~ treatany*margin_2009 + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=elec)
summary(FE.margin2009)
elec$margin_2009.cat[elec$margin_2009<=5] <- 1
elec$margin_2009.cat[elec$margin_2009>5 & elec$margin_2009<=10] <- 2
elec$margin_2009.cat[elec$margin_2009>10 & elec$margin_2009<=20] <- 3
elec$margin_2009.cat[elec$margin_2009>20 & elec$margin_2009<=30] <- 4
elec$margin_2009.cat[elec$margin_2009>30] <- 5
table(elec$margin_2009.cat)
FE.margin2009.cat <- lm(voteshare_spec1_2014 ~ treatany*as.factor(margin_2009.cat) + voteshare_spec1_2009 + num_eligible1 + num_eligible2, data=elec)
summary(FE.margin2009.cat)
covlabels <- c("Treat", "Margin 5-10", "Margin 10-20", "Margin 20-30", "Margin 30+", "VB share 2009", "1 Station", "2 Stations", "Treat:Margin 5-10", "Treat:Margin 10-20", "Treat:Margin 20-30", "Treat:Margin 30+", "Constant")
cat(stargazer(FE.margin2009.cat, single.row = T, covariate.labels = covlabels, dep.var.labels = "VB Share 2014"), file="Figures/comphet.tex", sep="\n")
## No heterogeneous effects with continuous or categorical margin of victory

## Other analysis could see what happened in those places with close races in 2009 (say <10pp margin)
## If we assume the constant average treatment effect, did it lead to a switch in who won?
comp10 <- subset(elec, margin_2009<10)
table(comp10$state_name)
table(comp10$treatany)
dep_matrixcomp <- omegamatrix(comp10)
IPW.FE.fxn(comp10, "voteshare_spec1_2009", "voteshare_spec1_2014", dep_matrixcomp)

table(comp10$winner_party2009)
table(comp10$winner_party2014, comp10$state_name)
table(as.character(comp10$winner_party2009)==as.character(comp10$winner_party2014))
# 179 ACs of 289 competitive ACs in 2009 switched parties

pdf(file = "~/Desktop/Replication Collaboration Clone/Figures/VBcomp.pdf",width=10,height=5)
par(mfrow=c(1,2))
hist(comp10$voteshare_spec1_2009, main="Histogram of VB Share in \nCompetitive ACs (<10pp in 2009) 2009", xlab="Vote Share of VB parties 2009")
hist(comp10$voteshare_spec1_2014, main="Histogram of VB Share in \nCompetitive ACs (<10pp in 2009) 2014", xlab="Vote Share of VB parties 2014")
dev.off()
# Of the competitive ACs in 2009, many had really high share of vote-buying in 2009, 
# and even higher share of vote-buying in 2014

comp10[1:20,c("state_name", "ac_name", "winner_party2014", "runnerup_party2014", "margin_2009", "margin_2014")]
# Now need to check if the winner or runnerup parties in 2014 were considered vote-buying


par(mfrow=c(1,1))
hist(comp10$winner_votes2014/comp10$total_ac_votes2014, main="Hist of Winner Vote Share 2014 in \nCompetitive ACs (<10pp in 2009)", xlab="Winner Vote Share 2014")

