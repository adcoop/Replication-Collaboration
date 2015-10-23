rm(list=ls())
library(dplyr)
library(foreign)
library(haven)

#*******************************************2014 Variables************************************
#  *Number votebuyers, number parties, number candidates, votebuyer voteshare, independent voteshare, margin, turnoutrate and electors
elec <- read_dta("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/results_10states2014_clean.dta")
names(elec)
elec$state_name <- as.factor(elec$state_name)
elec$state_name <- as.factor(elec$state_name)
table(elec$ac_num)
elec <- subset(elec, ac_num>0) # dropping postal ballot
elec <- as.data.frame(elec)
# *Candidates
elec <- elec %>% group_by(state_name, ac_num) %>% mutate(num_cand_2014 = n())

# *Independents
elec$ind[elec$cand_party=="IND"] <- elec$cand_votes[elec$cand_party=="IND"]
elec <- elec %>% group_by(state_name, ac_num) %>% mutate(indvoteshare_2014 = sum(ind, na.rm=T))
elec$indvoteshare_2014[is.na(elec$ind)]<- NA
elec$indvoteshare_2014 <- 100*elec$indvoteshare_2014/elec$total_ac_votes
elec <- elec %>% group_by(state_name, ac_num) %>% mutate(num_ind_2014 = sum(!is.na(ind)))

# *Margin
elec <- elec %>% group_by(state_name, ac_num) %>% mutate(rank = dense_rank(-cand_votes))
#View(elec[,c("state_name", "ac_num", "cand_votes", "cand_num", "rank")])

elec <- elec %>% group_by(state_name, ac_num) %>% mutate(winner_votes = cand_votes[rank==1])
elec <- elec %>% group_by(state_name, ac_num) %>% mutate(runnerup_votes = cand_votes[rank==2])
elec$margin_2014 <- 100*(elec$winner_votes-elec$runnerup_votes)/elec$total_ac_votes


AC.expt.sample <- read_dta("~/Dropbox/Green and Vasudevan (2015) replication/3. Sample Data/AC_expt_sample.dta")
electiontotal <- read_stata("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Stata Data/election2014_data.dta")

