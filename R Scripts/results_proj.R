
#####
## bounds on effect on results
#####

setwd('~/Desktop/Replication Collaboration Clone')

library(dplyr)
library(data.table)
library(foreign)
library(readstata13)


#####
## build dataset
#####

## read in vote buyers lists
votebuyers.spec1 <- read.csv('Data/votebuyers_spec1.csv')

## prep election results
elec2014 <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/results_10states2014_clean.dta")
ECI.sched.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/ECI_sched_clean.dta")

elec2014 <- subset(elec2014, ac_num>0)
elec2014 <- left_join(elec2014,ECI.sched.clean, by=c("state_name", 'pc_name', "pc_num") )

## Standardizing party/alliance names
elec2014$party_name <- elec2014$cand_party
elec2014$party_name[elec2014$party_name %in% c("BJP","TDP","SHS","SWP","RPI(A)","RSP","LJP","AD")] <- "NDA"
elec2014$party_name[elec2014$party_name %in% c("INC","NCP","RJD","RLD","JMM")] <- "UPA"

## create dummies for vote buying parties at diff levels of jouranlist concordance
cuts <- seq(0,0.9,0.1)
for (i in 1:10){
  votebuyers.spec1$vb[votebuyers.spec1$pct_nda >= cuts[i] & votebuyers.spec1$party_ally=="NDA"] <- 1
  votebuyers.spec1$vb[votebuyers.spec1$pct_upa >= cuts[i] & votebuyers.spec1$party_ally=="UPA"] <- 1
  votebuyers.spec1$vb[votebuyers.spec1$pct_oth >= cuts[i] & votebuyers.spec1$party_ally!="NDA" & votebuyers.spec1$party_ally!="UPA"] <- 1
  setnames(votebuyers.spec1, 'vb', paste0('votebuyer_1_', cuts[i]))
}

## merge journalist data into election outcomes
elec2014$poll_date <- as.character(elec2014$poll_date)
setnames(votebuyers.spec1, 'party_ally', 'party_name')
votebuyers.spec1 <- select(votebuyers.spec1, -poll_date, -pc_name, -X)
elec2014 <- left_join(elec2014, votebuyers.spec1, by=c("state_name", "pc_num", "party_name"))

## merge in experimental sample
AC.expt.sample <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/3. Sample Data/AC_expt_sample.dta")
elec2014 <- inner_join(elec2014, 
                             select(AC.expt.sample, -pc_name, -ac_name, -schedule_num, -poll_date, -pc_type, -pc_num, -state_code),
                             by = c('state_name', 'ac_num'))

#####
## upper bound - take from winning vote buyer and give to non-buying runner up
#####

## sort by vote buyer status and votes in 2014
elec2014 <- arrange(elec2014, state_name, ac_num, votebuyer_1_0, desc(cand_votes))

## generate y_0 under assumption of constant treatment effects
elec2014$y0 <- elec2014$cand_votes
elec2014$ate <- elec2014$total_valid_votes*(-0.0586)

## make indicators for max and min votebuyers and non
elec2014 <- elec2014 %>% group_by(state_name, ac_num, votebuyer_1_0) %>% mutate(vb_rank = dense_rank(-cand_votes))

## generate y_1 under assumption that ate taken from winning votebuyer and given to runner up non
elec2014$votebuyer_1_0[is.na(elec2014$votebuyer_1_0)] <- 0
elec2014$y1_max[elec2014$votebuyer_1_0==1 & elec2014$vb_rank==1] <- elec2014$y0[elec2014$votebuyer_1_0==1 & elec2014$vb_rank==1] + elec2014$ate[elec2014$votebuyer_1_0==1 & elec2014$vb_rank==1]
elec2014$y1_max[elec2014$votebuyer_1_0==0 & elec2014$vb_rank==1] <- elec2014$y0[elec2014$votebuyer_1_0==0 & elec2014$vb_rank==1] - elec2014$ate[elec2014$votebuyer_1_0==0 & elec2014$vb_rank==1]

## generate y_1 under assumption that ate taken from lowest votebuyer and given to lowest runner up
elec2014 <- elec2014 %>% group_by(state_name, ac_num, votebuyer_1_0) %>% mutate(vb_min = max(vb_rank))

elec2014$y1_min[elec2014$votebuyer_1_0==1 & elec2014$vb_rank==elec2014$vb_min] <- elec2014$y0[elec2014$votebuyer_1_0==1 & elec2014$vb_rank==elec2014$vb_min] + elec2014$ate[elec2014$votebuyer_1_0==1 & elec2014$vb_rank==elec2014$vb_min]
elec2014$y1_min[elec2014$votebuyer_1_0==0 & elec2014$vb_rank==elec2014$vb_min] <- elec2014$y0[elec2014$votebuyer_1_0==0 & elec2014$vb_rank==elec2014$vb_min] - elec2014$ate[elec2014$votebuyer_1_0==0 & elec2014$vb_rank==elec2014$vb_min]

## calculate winning party
elec2014 <- elec2014 %>% group_by(state_name, ac_num) %>% 
  mutate(win_actual = dense_rank(cand_votes), win_upper = dense_rank(y1_max), win_lower = dense_rank(y1_min))
elec2014 <- filter(elec2014, win_actual==1 | win_upper==1 | win_lower==1)
elec2014 <- select(elec2014, state_name, ac_name, ac_num, party_name, win_actual, win_upper, win_lower)

elec2014$y0[elec2014$treatany==1] <- 


