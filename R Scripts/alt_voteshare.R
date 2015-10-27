
#####
## Output data with varying defns of what is a vote buying party
#####

rm(list=ls())

library(dplyr)
library(foreign)
library(readstata13)
library(data.table)

# read Journalist Data
# votebuyers.spec1 <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec1.dta")
# votebuyers.spec2 <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec2.dta")
# votebuyers.spec3 <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec3.dta")
# journalist.data.nopii.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/journalist_data_nopii_clean.dta")
journalist.data.nopii <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Input Data/journalist_data_nopii.dta")

# Read Electoral Data
results.10states2014.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/results_10states2014_clean.dta")
results.10states2009.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/results_10states2009_clean.dta")
ECI.sched.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/ECI_sched_clean.dta")
PC.results <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/PC_results.dta")

# Read Sample Data
AC.expt.sample <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/3. Sample Data/AC_expt_sample.dta")

# Read Analysis Data
# election2014.data <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Stata Data/election2014_data.dta")
# voteshare.data <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Stata Data/voteshare_data.dta")
# turnout.data <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Stata Data/turnout_data.dta")



# # election2014_vars.do ----------------------------------------------------
# #*******************************************2014 Variables************************************
# # *Number votebuyers, number parties, number candidates, votebuyer voteshare, independent voteshare, margin, turnoutrate and electors
# elec <- results.10states2014.clean
# elec$state_name <- as.factor(elec$state_name)
# elec <- subset(elec, ac_num>0) # dropping postal ballot
# # *Candidates
# elec <- elec %>% group_by(state_name, ac_num) %>% mutate(num_cand_2014 = n())
# # *Independents
# elec$ind[elec$cand_party=="IND"] <- elec$cand_votes[elec$cand_party=="IND"]
# elec <- elec %>% group_by(state_name, ac_num) %>% mutate(indvoteshare_2014 = sum(ind, na.rm=T))
# # elec$indvoteshare_2014[is.na(elec$ind)] <- NA # why are we getting rid of this var for non-ind rows? this is an ac-level variable
# elec$indvoteshare_2014 <- 100*elec$indvoteshare_2014/elec$total_ac_votes
# elec <- elec %>% group_by(state_name, ac_num) %>% mutate(num_ind_2014 = sum(!is.na(ind)))
# # *Margin
# elec <- elec %>% group_by(state_name, ac_num) %>% mutate(rank = dense_rank(-cand_votes))
# elec <- elec %>% group_by(state_name, ac_num) %>% mutate(winner_votes = cand_votes[rank==1])
# elec <- elec %>% group_by(state_name, ac_num) %>% mutate(runnerup_votes = cand_votes[rank==2])
# elec$margin_2014 <- 100*(elec$winner_votes-elec$runnerup_votes)/elec$total_ac_votes
# # elec$state_name <- as.character(elec$state_name)
# 
# merge <- inner_join(elec, AC.expt.sample, by=c("state_name", "ac_num")) 
# # Stata code only retains treatany variable from the AC.expt.sample dataset. I kept all here.
# # Stata code only does keep(matched) so I used inner_join
# merge <- inner_join(merge,voteshare.data, by=c("state_name", "ac_num"))
# # Isn't voteshare.data an output of this code? Why are we using this rather than elec? Isn't this circular?
# # Stata code only retains voteshare_spec?_2014 num_spec?_2014 variables from the voteshare.data dataset. I kept all here.
# # Stata code only does keep(matched) so I used inner_join
# merge <- left_join(merge,turnout.data, by=c("state_name", "ac_num") )
# # Stata code only retains turnoutrate_2014 variable from the turnoutshare.data dataset. I kept all here.
# # Stata code only does keep(matched) so I used inner_join
# merge$electors_2014 <- merge$ac_electors
# # Stata code drops a lot of variables. I keep them in for now.
# # Saves this dataset as "4. Analysis/Stata Data/election2014_data.dta"
# save(merge, file="~/Desktop/Replication Collaboration Clone/R Scripts/election2014_data.Rdata")

# votebuyer_specs.do -------------------------------------------------------

# the journalist.data.nopii is the uncleaned results from the journalists
# code to create journalist.data.nopii.clean
journalist <- left_join(journalist.data.nopii, PC.results, by=c("state_name", "pc_name"))
PC.sample <- AC.expt.sample[,c("state_name", "pc_name", "ac_num")]
PC.sample <- PC.sample %>% group_by(state_name, pc_name) %>% mutate(n = dense_rank(ac_num))
PC.sample <- subset(PC.sample,n==1)
PC.sample <- PC.sample[,c("state_name", "pc_name")]
PC.sample$in_sample <- 1
journalist <- left_join(journalist,PC.sample, by=c("state_name", "pc_name"))
journalist <- left_join(journalist,ECI.sched.clean, by=c("state_name", "pc_name"))
journalist$win_2[journalist$win_1==journalist$win_2] <- ""
journalist$resp_secret[journalist$secret_1==""] <- 0
journalist$resp_secret[journalist$secret_1!=""] <- 1
# journalist here is the same as journalist.data.nopii.clean

# To export the votebuyers.spec1, etc.
journalist.sample <- subset(journalist, in_sample==1)
write.csv(journalist.sample, '~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec1.csv')
journalist.sample <- subset(journalist.sample, resp_secret==1)
write.csv(journalist.sample, '~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec2.csv')
# journalist.sample <- journalist.sample %>% group_by(state_name, pc_num) %>% mutate(num_resp = n())
journalist.sample <- journalist.sample %>% group_by(pc_num) %>% mutate(num_resp = n())
write.csv(journalist.sample, '~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec3.csv')
table(journalist.sample$state_name, journalist.sample$pc_num)


# voteshare_data.do -------------------------------------------------------
votebuyers.spec1 <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec1.csv")
votebuyers.spec2 <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec2.csv")
votebuyers.spec3 <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec3.csv")

elec2014 <- results.10states2014.clean
elec2014 <- subset(elec2014, ac_num>0)
elec2014 <- left_join(elec2014,ECI.sched.clean, by=c("state_name", "pc_num") )

# *Standardizing party/alliance names
elec2014$party_name <- elec2014$cand_party
elec2014$party_name[elec2014$party_name %in% c("BJP","TDP","SHS","SWP","RPI(A)","RSP","LJP","AD")] <- "NDA"
elec2014$party_name[elec2014$party_name %in% c("INC","NCP","RJD","RLD","JMM")] <- "UPA"

# *Merging in vote-buyer specifications
votebuyers.spec1$votebuyer_1 <- 1
votebuyers.spec2$votebuyer_2 <- 1
votebuyers.spec3$votebuyer_3 <- 1
elec2014$poll_date <- as.character(elec2014$poll_date)
votebuyers.spec2$poll_date <- as.character(votebuyers.spec2$poll_date)
elec2014 <- left_join(elec2014,votebuyers.spec1,by=c("state_name", "pc_num", "party_name"))
elec2014$poll_date <- elec2014$poll_date.x
elec2014 <- left_join(elec2014,votebuyers.spec2,by=c("state_name", "poll_date", "party_name"))
elec2014 <- left_join(elec2014,votebuyers.spec3,by=c("state_name", "party_name"))
# *Number of vote-buying parties and vote-buyer vote-shares
votebuyer1 <- subset(elec2014, votebuyer_1==1)
votebuyer1 <- votebuyer1 %>% group_by(state_name, ac_num) %>% mutate(votes = sum(cand_votes))
votebuyer1 <- votebuyer1 %>% group_by(state_name, ac_num) %>% mutate(num_spec1_2014 = sum(votebuyer_1))
votebuyer1$voteshare_spec1_2014 <- 100*votebuyer1$votes/votebuyer1$total_ac_votes
votebuyer1 <- votebuyer1 %>% group_by(state_name, ac_num) %>% mutate(n = dense_rank(cand_votes))
View(votebuyer1[,c("state_name", "ac_num", "votes", "cand_votes", "n")])
votebuyer1 <- subset(votebuyer1, n==1)
votebuyer1 <- votebuyer1[,c("state_name", "pc_name", "pc_num", "ac_name", "ac_num", "voteshare_spec1_2014", "num_spec1_2014")]
votebuyer2 <- subset(elec2014, votebuyer_2==1)
votebuyer2 <- votebuyer2 %>% group_by(state_name, ac_num) %>% mutate(votes = sum(cand_votes))
votebuyer2 <- votebuyer2 %>% group_by(state_name, ac_num) %>% mutate(num_spec2_2014 = sum(votebuyer_2))
# This is different from their code. From stata: "bys state_name ac_num: egen num_spec2_2014 = count(votebuyer_1)"
# Doesn't make sense to count up the number of votebuyer_1 per ac_num -- instead, want to know the number of parties in each ac_num identified via spec2
# If stata includes NA in the count, then their code works.
View(votebuyer2[,c("state_name", "ac_num", "votes", "cand_votes","votebuyer_1", "votebuyer_2", "party_name")])
votebuyer2$voteshare_spec2_2014 <- 100*votebuyer2$votes/votebuyer2$total_ac_votes
votebuyer2 <- votebuyer2 %>% group_by(state_name, ac_num) %>% mutate(n = dense_rank(cand_votes))
View(votebuyer2[,c("state_name", "ac_num", "votes", "cand_votes", "n", "voteshare_spec2_2014", "num_spec2_2014")])
votebuyer2 <- subset(votebuyer2, n==1)
votebuyer2 <- votebuyer2[,c("state_name", "pc_name", "pc_num", "ac_name", "ac_num", "voteshare_spec2_2014", "num_spec2_2014")]
votebuyer3 <- subset(elec2014, votebuyer_3==1)
votebuyer3 <- votebuyer3 %>% group_by(state_name, ac_num) %>% mutate(votes = sum(cand_votes))
votebuyer3 <- votebuyer3 %>% group_by(state_name, ac_num) %>% mutate(num_spec3_2014 = sum(votebuyer_3))
# This is different from their code. From stata: "bys state_name ac_num: egen num_spec3_2014 = count(votebuyer_1)"
# Doesn't make sense to sum up the number of votebuyer_1 per ac_num -- instead, want to know the number of parties in each ac_num identified via spec3
# If stata includes NA in the count, then their code works.
votebuyer3$voteshare_spec3_2014 <- 100*votebuyer3$votes/votebuyer3$total_ac_votes
votebuyer3 <- votebuyer3 %>% group_by(state_name, ac_num) %>% mutate(n = dense_rank(cand_votes))
View(votebuyer3[,c("state_name", "ac_num", "votes", "cand_votes", "n", "voteshare_spec3_2014", "num_spec3_2014")])
votebuyer3 <- subset(votebuyer3, n==1)
votebuyer3 <- votebuyer3[,c("state_name", "pc_name", "pc_num", "ac_name", "ac_num", "voteshare_spec3_2014", "num_spec3_2014")]
data2014 <- inner_join(votebuyer1,votebuyer2,by=c("state_name", "ac_num"))
data2014 <- inner_join(data2014,votebuyer3,by=c("state_name", "ac_num"))

###### same for 2009
elec2009 <- results.10states2009.clean
elec2009 <- subset(elec2009, ac_num>0)
elec2009 <- left_join(elec2009,ECI.sched.clean, by=c("state_name", "pc_num") )
# *Standardizing party/alliance names
elec2009$party_name <- elec2009$cand_party
elec2009$party_name[elec2009$party_name %in% c("BJP","TDP","SHS","SWP","RPI(A)","RSP","LJP","AD")] <- "NDA"
elec2009$party_name[elec2009$party_name %in% c("INC","NCP","RJD","RLD","JMM")] <- "UPA"
#View(elec2009[,c("party_name", "cand_party")])
# *Merging in vote-buyer specifications
elec2009$poll_date <- as.character(elec2009$poll_date)
elec2009 <- left_join(elec2009,votebuyers.spec1,by=c("state_name", "pc_num", "party_name"))
elec2009$poll_date <- elec2009$poll_date.x
elec2009 <- left_join(elec2009,votebuyers.spec2,by=c("state_name", "poll_date", "party_name"))
elec2009 <- left_join(elec2009,votebuyers.spec3,by=c("state_name", "party_name"))
# *Number of vote-buying parties and vote-buyer vote-shares
votebuyer1 <- subset(elec2009, votebuyer_1==1)
votebuyer1 <- votebuyer1 %>% group_by(state_name, ac_num) %>% mutate(votes = sum(cand_votes))
votebuyer1 <- votebuyer1 %>% group_by(state_name, ac_num) %>% mutate(num_spec1_2009 = sum(votebuyer_1))
votebuyer1$voteshare_spec1_2009 <- 100*votebuyer1$votes/votebuyer1$total_ac_votes
votebuyer1 <- votebuyer1 %>% group_by(state_name, ac_num) %>% mutate(n = dense_rank(cand_votes))
votebuyer1 <- subset(votebuyer1, n==1)
votebuyer1 <- votebuyer1[,c("state_name", "pc_name", "pc_num", "ac_name", "ac_num", "voteshare_spec1_2009", "num_spec1_2009")]
votebuyer2 <- subset(elec2009, votebuyer_2==1)
votebuyer2 <- votebuyer2 %>% group_by(state_name, ac_num) %>% mutate(votes = sum(cand_votes))
votebuyer2 <- votebuyer2 %>% group_by(state_name, ac_num) %>% mutate(num_spec2_2009 = sum(votebuyer_2))
votebuyer2$voteshare_spec2_2009 <- 100*votebuyer2$votes/votebuyer2$total_ac_votes
votebuyer2 <- votebuyer2 %>% group_by(state_name, ac_num) %>% mutate(n = dense_rank(cand_votes))
votebuyer2 <- subset(votebuyer2, n==1)
votebuyer2 <- votebuyer2[,c("state_name", "pc_name", "pc_num", "ac_name", "ac_num", "voteshare_spec2_2009", "num_spec2_2009")]
votebuyer3 <- subset(elec2009, votebuyer_3==1)
votebuyer3 <- votebuyer3 %>% group_by(state_name, ac_num) %>% mutate(votes = sum(cand_votes))
votebuyer3 <- votebuyer3 %>% group_by(state_name, ac_num) %>% mutate(num_spec3_2009 = sum(votebuyer_3))
votebuyer3$voteshare_spec3_2009 <- 100*votebuyer3$votes/votebuyer3$total_ac_votes
votebuyer3 <- votebuyer3 %>% group_by(state_name, ac_num) %>% mutate(n = dense_rank(cand_votes))
votebuyer3 <- subset(votebuyer3, n==1)
votebuyer3 <- votebuyer3[,c("state_name", "pc_name", "pc_num", "ac_name", "ac_num", "voteshare_spec3_2009", "num_spec3_2009")]
data2009 <- inner_join(votebuyer1,votebuyer2,by=c("state_name", "ac_num"))
data2009 <- inner_join(data2009,votebuyer3,by=c("state_name", "ac_num"))

voteshare_data <- left_join(data2014, data2009, by=c("state_name", "ac_num"))
voteshare_data$num_spec1_2009[is.na(voteshare_data$num_spec1_2009)] <- 0
voteshare_data$num_spec2_2009[is.na(voteshare_data$num_spec2_2009)] <- 0
voteshare_data$num_spec3_2009[is.na(voteshare_data$num_spec3_2009)] <- 0
# Reproduction of their voteshare_data.dta. 
# Does not include parties, etc. Just sum of voteshare for each votebuying party for each AC.

