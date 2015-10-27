
##########
##########
## Building datasets for analysis
##########
##########


rm(list=ls())

library(dplyr)
library(foreign)
library(readstata13)
library(tidyr)
library(data.table)

## set to your local clone
setwd('~/Desktop/Replication Collaboration Clone')

# read Journalist Data
journalist.data.nopii <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Input Data/journalist_data_nopii.dta")

# Read Electoral Data
results.10states2014.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/results_10states2014_clean.dta")
results.10states2009.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/results_10states2009_clean.dta")
ECI.sched.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/ECI_sched_clean.dta")
PC.results <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/PC_results.dta")

# Read Sample Data
AC.expt.sample <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/3. Sample Data/AC_expt_sample.dta")


##########
## build defn of vote buyers 
## adapted from votebuyer_specs.do
## main output: journalist.data.nopii.clean
##########

## merge journalist data and PC-level election results
journalist <- left_join(journalist.data.nopii, PC.results, by=c("state_name", "pc_name"))

## create list of PCs in the AC sample
PC.sample <- select(AC.expt.sample, state_name, pc_name, ac_num)
PC.sample <- PC.sample %>% group_by(state_name, pc_name) %>% mutate(n = dense_rank(ac_num))
PC.sample <- subset(PC.sample,n==1)
PC.sample <- PC.sample[,c("state_name", "pc_name")]
PC.sample$in_sample <- 1

## merge PC sample list into journalist data
journalist <- left_join(journalist,PC.sample, by=c("state_name", "pc_name"))

## merge election schedule into journalist data
journalist <- left_join(journalist,ECI.sched.clean, by=c("state_name", "pc_name"))
journalist$win_2[journalist$win_1==journalist$win_2] <- ""

## create dummy for whether journalist thinks any parties are vote buying
journalist$resp_secret[journalist$secret_1==""] <- 0
journalist$resp_secret[journalist$secret_1!=""] <- 1

## output journalist.data.nopii.clean
write.csv(journalist, "Data/journalist.data.nopii.clean.csv")


# create affiliations
ally <- data.frame('party_ally' = c(rep('NDA', 8), rep('UPA', 5)),
                   'party_name' = c("BJP","TDP","SHS","SWP","RPI(A)","RSP","LJP","AD",
                                    "INC","NCP","RJD","RLD","JMM"),
                   stringsAsFactors=F)
setnames(ally, c('party_ally', 'party_name'), c('secret_1_ally', 'secret_1'))
journalist <- left_join(journalist, ally, by=c("secret_1"))
setnames(ally, c('secret_1_ally', 'secret_1'), c('secret_2_ally', 'secret_2'))
journalist <- left_join(journalist, ally, by=c("secret_2"))
setnames(ally, c('secret_2_ally', 'secret_2'), c('secret_3_ally', 'secret_3'))
journalist <- left_join(journalist, ally, by=c("secret_3"))
setnames(ally, c('secret_3_ally', 'secret_3'), c('secret_4_ally', 'secret_4'))
journalist <- left_join(journalist, ally, by=c("secret_4"))
journalist$secret_1_ally[is.na(journalist$secret_1_ally)] <- journalist$secret_1[is.na(journalist$secret_1_ally)]
journalist$secret_2_ally[is.na(journalist$secret_2_ally)] <- journalist$secret_2[is.na(journalist$secret_2_ally)]
journalist$secret_3_ally[is.na(journalist$secret_3_ally)] <- journalist$secret_3[is.na(journalist$secret_3_ally)]
journalist$secret_4_ally[is.na(journalist$secret_4_ally)] <- journalist$secret_4[is.na(journalist$secret_4_ally)]

## create dummies for vote buying parties
journalist$vb <- paste(journalist$secret_1_ally, journalist$secret_2_ally, journalist$secret_3_ally, journalist$secret_4_ally, sep = ",")
journalist$nda_vb <- ifelse(grepl("NDA", journalist$vb), 1, 0)
journalist$upa_vb <- ifelse(grepl("UPA", journalist$vb), 1, 0)
journalist$oth_vb <- ifelse(grepl("AAAP|AJSUP|BJD|BSP|BVA|JBSP|JD(S)|JKP|JVM|SP|TRS|YSRCP", journalist$vb), 1, 0)


#####
## by PC - Spec 1
#####

## subset the journalist data to create a list of votebuyers by PC
journalist.sample <- filter(journalist, in_sample==1)

## create measure of pct of journos who name major parties by PC
journalist.sample <- journalist.sample %>% 
  group_by(state_name, pc_name) %>% 
  mutate(pct_nda = mean(nda_vb, na.rm=T), pct_upa = mean(upa_vb, na.rm=T), pct_oth = mean(oth_vb, na.rm=T))

## reshape to create list of votebuyers by PC
journalist.sample <- journalist.sample %>% 
  select(state_name, pc_name, pc_num, pct_nda:pct_oth, poll_date, secret_1_ally:secret_4_ally) %>% 
  gather(secret, party_ally, -pc_name, -state_name, -pc_num, -poll_date, -pct_nda, -pct_upa, -pct_oth)
journalist.sample <- filter(journalist.sample, party_ally!="") %>% 
  select(-secret) %>% 
  arrange(state_name, pc_name, party_ally)
journalist.sample <- unique(journalist.sample)

## write to csv
write.csv(journalist.sample, "Data/votebuyers_spec1.csv")


#####
## by state - Spec 2
#####

## subset the journalist data to create a list of votebuyers by state
journalist.sample <- filter(journalist, in_sample==1)

## create measure of pct of journos who name major parties by state
journalist.sample <- journalist.sample %>% 
  group_by(state_name) %>% 
  mutate(pct_nda = mean(nda_vb, na.rm=T), pct_upa = mean(upa_vb, na.rm=T), pct_oth = mean(oth_vb, na.rm=T))

## reshape to create list of votebuyers by PC
journalist.sample <- journalist.sample %>% 
  select(state_name, pct_nda:pct_oth, poll_date, secret_1_ally:secret_4_ally) %>% 
  gather(secret, party_ally, -state_name, -poll_date, -pct_nda, -pct_upa, -pct_oth)
journalist.sample <- filter(journalist.sample, party_ally!="") %>% 
  select(-secret) %>% 
  arrange(state_name, party_ally)
journalist.sample <- unique(journalist.sample)

## write to csv
write.csv(journalist.sample, "Data/votebuyers_spec2.csv")


#####
## by state - Spec 3
#####

## subset the journalist data to create a list of votebuyers by state
journalist.sample <- filter(journalist, in_sample==1)

## create measure of pct of journos who name major parties by state
journalist.sample <- journalist.sample %>% 
  group_by(state_name) %>% 
  mutate(pct_nda = mean(nda_vb, na.rm=T), pct_upa = mean(upa_vb, na.rm=T), pct_oth = mean(oth_vb, na.rm=T))

## reshape to create list of votebuyers by PC
journalist.sample <- journalist.sample %>% 
  select(state_name, pct_nda:pct_oth, secret_1_ally:secret_4_ally) %>% 
  gather(secret, party_ally, -state_name, -pct_nda, -pct_upa, -pct_oth)
journalist.sample <- filter(journalist.sample, party_ally!="") %>% 
  select(-secret) %>% 
  arrange(state_name, party_ally)
journalist.sample <- unique(journalist.sample)

## write to csv
write.csv(journalist.sample, "Data/votebuyers_spec3.csv")


##########
## build vote share vars
## adapted from voteshare_data.do
## main output voteshare_data.dta
##########

## read in vote buyers lists
votebuyers.spec1 <- read.csv('Data/votebuyers_spec1.csv')
votebuyers.spec2 <- read.csv('Data/votebuyers_spec2.csv')
votebuyers.spec3 <- read.csv('Data/votebuyers_spec3.csv')

## prep election results
elec2014 <- results.10states2014.clean
elec2014 <- subset(elec2014, ac_num>0)
elec2014 <- left_join(elec2014,ECI.sched.clean, by=c("state_name", 'pc_name', "pc_num") )

## Standardizing party/alliance names
elec2014$party_name <- elec2014$cand_party
elec2014$party_name[elec2014$party_name %in% c("BJP","TDP","SHS","SWP","RPI(A)","RSP","LJP","AD")] <- "NDA"
elec2014$party_name[elec2014$party_name %in% c("INC","NCP","RJD","RLD","JMM")] <- "UPA"

## create dummies for vote buying parties at diff levels of jouranlist concordance
cuts <- seq(0,0.9,0.1)
for (i in 1:10){
  votebuyers.spec1$vb[votebuyers.spec1$pct_nda > cuts[i] & votebuyers.spec1$party_ally=="NDA"] <- 1
  votebuyers.spec1$vb[votebuyers.spec1$pct_upa > cuts[i] & votebuyers.spec1$party_ally=="UPA"] <- 1
  votebuyers.spec1$vb[votebuyers.spec1$pct_oth > cuts[i] & votebuyers.spec1$party_ally!="NDA" & votebuyers.spec1$party_ally!="UPA"] <- 1
  setnames(votebuyers.spec1, 'vb', paste0('votebuyer_1_', cuts[i]))
  votebuyers.spec2$vb[votebuyers.spec2$pct_nda > cuts[i] & votebuyers.spec2$party_ally=="NDA"] <- 1
  votebuyers.spec2$vb[votebuyers.spec2$pct_upa > cuts[i] & votebuyers.spec2$party_ally=="UPA"] <- 1
  votebuyers.spec2$vb[votebuyers.spec2$pct_oth > cuts[i] & votebuyers.spec2$party_ally!="NDA" & votebuyers.spec2$party_ally!="UPA"] <- 1
  setnames(votebuyers.spec2, 'vb', paste0('votebuyer_2_', cuts[i]))
  votebuyers.spec3$vb[votebuyers.spec3$pct_nda > cuts[i] & votebuyers.spec3$party_ally=="NDA"] <- 1
  votebuyers.spec3$vb[votebuyers.spec3$pct_upa > cuts[i] & votebuyers.spec3$party_ally=="UPA"] <- 1
  votebuyers.spec3$vb[votebuyers.spec3$pct_oth > cuts[i] & votebuyers.spec3$party_ally!="NDA" & votebuyers.spec3$party_ally!="UPA"] <- 1
  setnames(votebuyers.spec3, 'vb', paste0('votebuyer_3_', cuts[i]))
}

## merge journalist data into election outcomes
elec2014$poll_date <- as.character(elec2014$poll_date)
setnames(votebuyers.spec1, 'party_ally', 'party_name')
setnames(votebuyers.spec2, 'party_ally', 'party_name')
setnames(votebuyers.spec3, 'party_ally', 'party_name')
votebuyers.spec1 <- select(votebuyers.spec1, -poll_date, -pc_name, -X)
votebuyers.spec2 <- select(votebuyers.spec2, state_name, poll_date:votebuyer_2_0.9)
votebuyers.spec3 <- select(votebuyers.spec3, state_name, party_name:votebuyer_3_0.9)
elec2014 <- left_join(elec2014, votebuyers.spec1, by=c("state_name", "pc_num", "party_name"))
elec2014 <- left_join(elec2014, votebuyers.spec2, by=c("state_name", "poll_date", "party_name"))
elec2014 <- left_join(elec2014, votebuyers.spec3, by=c("state_name", "party_name"))

## Number of vote-buying parties and vote-buyer vote-shares - Spec 1
votebuyer1 <- elec2014 %>% 
  group_by(state_name, pc_name, ac_name, ac_num) %>%
  summarize(votes_0 = sum(cand_votes[votebuyer_1_0==1], na.rm=T),
            votes_0.1 = sum(cand_votes[votebuyer_1_0.1==1], na.rm=T),
            votes_0.2 = sum(cand_votes[votebuyer_1_0.2==1], na.rm=T),
            votes_0.3 = sum(cand_votes[votebuyer_1_0.3==1], na.rm=T),
            votes_0.4 = sum(cand_votes[votebuyer_1_0.4==1], na.rm=T),
            votes_0.5 = sum(cand_votes[votebuyer_1_0.5==1], na.rm=T),
            votes_0.6 = sum(cand_votes[votebuyer_1_0.6==1], na.rm=T),
            votes_0.7 = sum(cand_votes[votebuyer_1_0.7==1], na.rm=T),
            votes_0.8 = sum(cand_votes[votebuyer_1_0.8==1], na.rm=T),
            votes_0.9 = sum(cand_votes[votebuyer_1_0.9==1], na.rm=T),
            num_spec1_2014 = sum(votebuyer_1_0, na.rm=T),
            total_ac_votes = unique(total_ac_votes))
votebuyer1$voteshare_spec1_2014 <- 100*votebuyer1$votes_0/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2014_0.1 <- 100*votebuyer1$votes_0.1/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2014_0.2 <- 100*votebuyer1$votes_0.2/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2014_0.3 <- 100*votebuyer1$votes_0.3/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2014_0.4 <- 100*votebuyer1$votes_0.4/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2014_0.5 <- 100*votebuyer1$votes_0.5/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2014_0.6 <- 100*votebuyer1$votes_0.6/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2014_0.7 <- 100*votebuyer1$votes_0.7/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2014_0.8 <- 100*votebuyer1$votes_0.8/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2014_0.9 <- 100*votebuyer1$votes_0.9/votebuyer1$total_ac_votes

## Number of vote-buying parties and vote-buyer vote-shares - Spec 2
votebuyer2 <- elec2014 %>% 
  group_by(state_name, pc_name, ac_name, ac_num) %>%
  summarize(votes_0 = sum(cand_votes[votebuyer_2_0==1], na.rm=T),
            votes_0.1 = sum(cand_votes[votebuyer_2_0.1==1], na.rm=T),
            votes_0.2 = sum(cand_votes[votebuyer_2_0.2==1], na.rm=T),
            votes_0.3 = sum(cand_votes[votebuyer_2_0.3==1], na.rm=T),
            votes_0.4 = sum(cand_votes[votebuyer_2_0.4==1], na.rm=T),
            votes_0.5 = sum(cand_votes[votebuyer_2_0.5==1], na.rm=T),
            votes_0.6 = sum(cand_votes[votebuyer_2_0.6==1], na.rm=T),
            votes_0.7 = sum(cand_votes[votebuyer_2_0.7==1], na.rm=T),
            votes_0.8 = sum(cand_votes[votebuyer_2_0.8==1], na.rm=T),
            votes_0.9 = sum(cand_votes[votebuyer_2_0.9==1], na.rm=T),
            num_spec2_2014 = sum(votebuyer_2_0, na.rm=T),
            total_ac_votes = unique(total_ac_votes))
votebuyer2$voteshare_spec2_2014 <- 100*votebuyer2$votes_0/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2014_0.1 <- 100*votebuyer2$votes_0.1/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2014_0.2 <- 100*votebuyer2$votes_0.2/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2014_0.3 <- 100*votebuyer2$votes_0.3/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2014_0.4 <- 100*votebuyer2$votes_0.4/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2014_0.5 <- 100*votebuyer2$votes_0.5/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2014_0.6 <- 100*votebuyer2$votes_0.6/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2014_0.7 <- 100*votebuyer2$votes_0.7/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2014_0.8 <- 100*votebuyer2$votes_0.8/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2014_0.9 <- 100*votebuyer2$votes_0.9/votebuyer2$total_ac_votes

## Number of vote-buying parties and vote-buyer vote-shares - Spec 3
votebuyer3 <- elec2014 %>% 
  group_by(state_name, pc_name, ac_name, ac_num) %>%
  summarize(votes_0 = sum(cand_votes[votebuyer_3_0==1], na.rm=T),
            votes_0.1 = sum(cand_votes[votebuyer_3_0.1==1], na.rm=T),
            votes_0.2 = sum(cand_votes[votebuyer_3_0.2==1], na.rm=T),
            votes_0.3 = sum(cand_votes[votebuyer_3_0.3==1], na.rm=T),
            votes_0.4 = sum(cand_votes[votebuyer_3_0.4==1], na.rm=T),
            votes_0.5 = sum(cand_votes[votebuyer_3_0.5==1], na.rm=T),
            votes_0.6 = sum(cand_votes[votebuyer_3_0.6==1], na.rm=T),
            votes_0.7 = sum(cand_votes[votebuyer_3_0.7==1], na.rm=T),
            votes_0.8 = sum(cand_votes[votebuyer_3_0.8==1], na.rm=T),
            votes_0.9 = sum(cand_votes[votebuyer_3_0.9==1], na.rm=T),
            num_spec3_2014 = sum(votebuyer_3_0, na.rm=T),
            total_ac_votes = unique(total_ac_votes))
votebuyer3$voteshare_spec3_2014 <- 100*votebuyer3$votes_0/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2014_0.1 <- 100*votebuyer3$votes_0.1/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2014_0.2 <- 100*votebuyer3$votes_0.2/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2014_0.3 <- 100*votebuyer3$votes_0.3/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2014_0.4 <- 100*votebuyer3$votes_0.4/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2014_0.5 <- 100*votebuyer3$votes_0.5/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2014_0.6 <- 100*votebuyer3$votes_0.6/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2014_0.7 <- 100*votebuyer3$votes_0.7/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2014_0.8 <- 100*votebuyer3$votes_0.8/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2014_0.9 <- 100*votebuyer3$votes_0.9/votebuyer3$total_ac_votes

## merge 2014 data together
votebuyer1 <- select(votebuyer1, state_name:ac_num, num_spec1_2014:voteshare_spec1_2014_0.9)
votebuyer2 <- select(votebuyer2, state_name:ac_num, num_spec2_2014:voteshare_spec2_2014_0.9, -total_ac_votes)
votebuyer3 <- select(votebuyer3, state_name:ac_num, num_spec3_2014:voteshare_spec3_2014_0.9, -total_ac_votes)

voteshare_data <- left_join(votebuyer1, votebuyer2, by = c('state_name', 'pc_name', 'ac_name', 'ac_num'))
voteshare_data <- left_join(voteshare_data, votebuyer3, by = c('state_name', 'pc_name', 'ac_name', 'ac_num'))


## prep election results - 2009
elec2009 <- results.10states2009.clean
elec2009 <- subset(elec2009, ac_num>0)
elec2009 <- left_join(elec2009,ECI.sched.clean, by=c("state_name", "pc_num", 'pc_name') )

## Standardizing party/alliance names
elec2009$party_name <- elec2009$cand_party
elec2009$party_name[elec2009$party_name %in% c("BJP","TDP","SHS","SWP","RPI(A)","RSP","LJP","AD")] <- "NDA"
elec2009$party_name[elec2009$party_name %in% c("INC","NCP","RJD","RLD","JMM")] <- "UPA"

## merge journalist data into election outcomes
elec2009$poll_date <- as.character(elec2009$poll_date)
elec2009 <- left_join(elec2009, votebuyers.spec1, by=c("state_name", "pc_num", "party_name"))
elec2009 <- left_join(elec2009, votebuyers.spec2, by=c("state_name", "poll_date", "party_name"))
elec2009 <- left_join(elec2009, votebuyers.spec3, by=c("state_name", "party_name"))

## Number of vote-buying parties and vote-buyer vote-shares - Spec 1
votebuyer1 <- elec2009 %>% 
  group_by(state_name, pc_name, ac_name, ac_num) %>%
  summarize(votes_0 = sum(cand_votes[votebuyer_1_0==1], na.rm=T),
            votes_0.1 = sum(cand_votes[votebuyer_1_0.1==1], na.rm=T),
            votes_0.2 = sum(cand_votes[votebuyer_1_0.2==1], na.rm=T),
            votes_0.3 = sum(cand_votes[votebuyer_1_0.3==1], na.rm=T),
            votes_0.4 = sum(cand_votes[votebuyer_1_0.4==1], na.rm=T),
            votes_0.5 = sum(cand_votes[votebuyer_1_0.5==1], na.rm=T),
            votes_0.6 = sum(cand_votes[votebuyer_1_0.6==1], na.rm=T),
            votes_0.7 = sum(cand_votes[votebuyer_1_0.7==1], na.rm=T),
            votes_0.8 = sum(cand_votes[votebuyer_1_0.8==1], na.rm=T),
            votes_0.9 = sum(cand_votes[votebuyer_1_0.9==1], na.rm=T),
            num_spec1_2009 = sum(votebuyer_1_0, na.rm=T),
            total_ac_votes = unique(total_ac_votes))
votebuyer1$voteshare_spec1_2009 <- 100*votebuyer1$votes_0/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2009_0.1 <- 100*votebuyer1$votes_0.1/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2009_0.2 <- 100*votebuyer1$votes_0.2/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2009_0.3 <- 100*votebuyer1$votes_0.3/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2009_0.4 <- 100*votebuyer1$votes_0.4/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2009_0.5 <- 100*votebuyer1$votes_0.5/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2009_0.6 <- 100*votebuyer1$votes_0.6/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2009_0.7 <- 100*votebuyer1$votes_0.7/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2009_0.8 <- 100*votebuyer1$votes_0.8/votebuyer1$total_ac_votes
votebuyer1$voteshare_spec1_2009_0.9 <- 100*votebuyer1$votes_0.9/votebuyer1$total_ac_votes

## Number of vote-buying parties and vote-buyer vote-shares - Spec 2
votebuyer2 <- elec2009 %>% 
  group_by(state_name, pc_name, ac_name, ac_num) %>%
  summarize(votes_0 = sum(cand_votes[votebuyer_2_0==1], na.rm=T),
            votes_0.1 = sum(cand_votes[votebuyer_2_0.1==1], na.rm=T),
            votes_0.2 = sum(cand_votes[votebuyer_2_0.2==1], na.rm=T),
            votes_0.3 = sum(cand_votes[votebuyer_2_0.3==1], na.rm=T),
            votes_0.4 = sum(cand_votes[votebuyer_2_0.4==1], na.rm=T),
            votes_0.5 = sum(cand_votes[votebuyer_2_0.5==1], na.rm=T),
            votes_0.6 = sum(cand_votes[votebuyer_2_0.6==1], na.rm=T),
            votes_0.7 = sum(cand_votes[votebuyer_2_0.7==1], na.rm=T),
            votes_0.8 = sum(cand_votes[votebuyer_2_0.8==1], na.rm=T),
            votes_0.9 = sum(cand_votes[votebuyer_2_0.9==1], na.rm=T),
            num_spec2_2009 = sum(votebuyer_2_0, na.rm=T),
            total_ac_votes = unique(total_ac_votes))
votebuyer2$voteshare_spec2_2009 <- 100*votebuyer2$votes_0/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2009_0.1 <- 100*votebuyer2$votes_0.1/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2009_0.2 <- 100*votebuyer2$votes_0.2/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2009_0.3 <- 100*votebuyer2$votes_0.3/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2009_0.4 <- 100*votebuyer2$votes_0.4/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2009_0.5 <- 100*votebuyer2$votes_0.5/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2009_0.6 <- 100*votebuyer2$votes_0.6/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2009_0.7 <- 100*votebuyer2$votes_0.7/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2009_0.8 <- 100*votebuyer2$votes_0.8/votebuyer2$total_ac_votes
votebuyer2$voteshare_spec2_2009_0.9 <- 100*votebuyer2$votes_0.9/votebuyer2$total_ac_votes

## Number of vote-buying parties and vote-buyer vote-shares - Spec 3
votebuyer3 <- elec2009 %>% 
  group_by(state_name, pc_name, ac_name, ac_num) %>%
  summarize(votes_0 = sum(cand_votes[votebuyer_3_0==1], na.rm=T),
            votes_0.1 = sum(cand_votes[votebuyer_3_0.1==1], na.rm=T),
            votes_0.2 = sum(cand_votes[votebuyer_3_0.2==1], na.rm=T),
            votes_0.3 = sum(cand_votes[votebuyer_3_0.3==1], na.rm=T),
            votes_0.4 = sum(cand_votes[votebuyer_3_0.4==1], na.rm=T),
            votes_0.5 = sum(cand_votes[votebuyer_3_0.5==1], na.rm=T),
            votes_0.6 = sum(cand_votes[votebuyer_3_0.6==1], na.rm=T),
            votes_0.7 = sum(cand_votes[votebuyer_3_0.7==1], na.rm=T),
            votes_0.8 = sum(cand_votes[votebuyer_3_0.8==1], na.rm=T),
            votes_0.9 = sum(cand_votes[votebuyer_3_0.9==1], na.rm=T),
            num_spec3_2009 = sum(votebuyer_3_0, na.rm=T),
            total_ac_votes = unique(total_ac_votes))
votebuyer3$voteshare_spec3_2009 <- 100*votebuyer3$votes_0/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2009_0.1 <- 100*votebuyer3$votes_0.1/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2009_0.2 <- 100*votebuyer3$votes_0.2/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2009_0.3 <- 100*votebuyer3$votes_0.3/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2009_0.4 <- 100*votebuyer3$votes_0.4/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2009_0.5 <- 100*votebuyer3$votes_0.5/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2009_0.6 <- 100*votebuyer3$votes_0.6/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2009_0.7 <- 100*votebuyer3$votes_0.7/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2009_0.8 <- 100*votebuyer3$votes_0.8/votebuyer3$total_ac_votes
votebuyer3$voteshare_spec3_2009_0.9 <- 100*votebuyer3$votes_0.9/votebuyer3$total_ac_votes

## merge everything together
votebuyer1 <- select(votebuyer1, state_name, ac_num, voteshare_spec1_2009:voteshare_spec1_2009_0.9)
votebuyer1$pc_name=NULL; votebuyer1$ac_name=NULL
votebuyer2 <- select(votebuyer2, state_name, ac_num, voteshare_spec2_2009:voteshare_spec2_2009_0.9)
votebuyer2$pc_name=NULL; votebuyer2$ac_name=NULL
votebuyer3 <- select(votebuyer3, state_name, ac_num, voteshare_spec3_2009:voteshare_spec3_2009_0.9)
votebuyer3$pc_name=NULL; votebuyer3$ac_name=NULL

voteshare_data <- left_join(voteshare_data, votebuyer1, by = c('state_name', 'ac_num'))
voteshare_data <- left_join(voteshare_data, votebuyer2, by = c('state_name', 'ac_num'))
voteshare_data <- left_join(voteshare_data, votebuyer3, by = c('state_name', 'ac_num'))

write.csv(voteshare_data, "Data/voteshare_data.csv")

## merge in experimental sample
AC.expt.sample <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/3. Sample Data/AC_expt_sample.dta")
voteshare_data <- inner_join(voteshare_data, 
                             select(AC.expt.sample, -pc_name, -ac_name),
                            by = c('state_name', 'ac_num'))

## write to csv for each specification
voteshare_data1 <- select(voteshare_data, state_name:ac_num, total_ac_votes, 
                          grep("_spec1_", colnames(voteshare_data)),
                          station_name1:wgt_treatany)
  ## subset to only ACs where we have voting data 
voteshare_data1 <- filter(voteshare_data1, num_spec1_2014>0)
write.csv(voteshare_data1, 'Data/voteshare1.csv')
voteshare_data2 <- select(voteshare_data, state_name:ac_num, total_ac_votes, 
                          grep("_spec2_", colnames(voteshare_data)),
                          station_name2:wgt_treatany)
write.csv(voteshare_data2, 'Data/voteshare2.csv')
voteshare_data3 <- select(voteshare_data, state_name:ac_num, total_ac_votes, 
                          grep("_spec3_", colnames(voteshare_data)),
                          station_name2:wgt_treatany)
write.csv(voteshare_data3, 'Data/voteshare3.csv')


##########
## build turnout vars
## adapted from turnout_data.do
## main output turnout_data.dta
##########

results.10states2014.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/results_10states2014_clean.dta")
results.10states2009.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/results_10states2009_clean.dta")
electors.2009 <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/electors_2009.dta")

## calculate turnout - 2014
elec2014 <- results.10states2014.clean
elec2014 <- subset(elec2014, ac_num>0)
elec2014$turnoutrate_2014 <- 100*elec2014$total_ac_votes/elec2014$ac_electors
elec2014 <- unique(select(elec2014, state_name, ac_num, ac_name, turnoutrate_2014))

## calculate turnout - 2009
elec2009 <- results.10states2009.clean
elec2009 <- subset(elec2009, ac_num>0)
elec2009 <- left_join(elec2009, electors.2009, by = c('state_name', 'ac_num', 'ac_name'))
elec2009$turnoutrate_2009 <- 100*elec2009$total_ac_votes/elec2009$electors_2009
elec2009 <- unique(select(elec2009, state_name, ac_num, ac_name, turnoutrate_2009))

## merge together
elec <- left_join(elec2014, elec2009, by = c('state_name', 'ac_num', 'ac_name'))

## merge in expeirmental sample
elec <- left_join(elec, 
                  select(AC.expt.sample, state_name, ac_num, ac_name,
                         treatany, wgt_treatany, num_eligible, illit_pc,
                         rural_pc, scst_pc, poll_date, station_id1, station_id2,
                         station_id3, prob_treatany),
                  by = c('state_name', 'ac_num', 'ac_name'))

## gen state election var
elec$state_election <- 0
elec$state_election[elec$state_name=="Andhra Pradesh" | elec$state_name=="Orissa"] <- 1

## output to csv
write.csv(elec, 'Data/turnout_data.csv')


##########
## create election variables 
## adapted from election2014_vars.do
## main output: election2014_data.dta
##########

voteshare.data <- read.csv("Data/voteshare_data.csv")
turnout.data <- read.csv("Data/turnout_data.csv")

elec <- results.10states2014.clean
elec <- subset(elec, ac_num>0) # dropping postal ballot

## Candidates
elec <- elec %>% 
  group_by(state_name, ac_num) %>% 
  mutate(num_cand_2014 = n())

## Independents
elec$ind[elec$cand_party=="IND"] <- elec$cand_votes[elec$cand_party=="IND"]
elec <- elec %>% 
  group_by(state_name, ac_num) %>% 
  mutate(indvoteshare_2014 = sum(ind, na.rm=T))
elec$indvoteshare_2014 <- 100*elec$indvoteshare_2014/elec$total_ac_votes
elec <- elec %>% group_by(state_name, ac_num) %>% mutate(num_ind_2014 = sum(!is.na(ind)))

## Margin
elec <- elec %>% group_by(state_name, ac_num) %>% mutate(rank = dense_rank(-cand_votes))
elec <- elec %>% group_by(state_name, ac_num) %>% mutate(winner_votes = cand_votes[rank==1])
elec <- elec %>% group_by(state_name, ac_num) %>% mutate(runnerup_votes = cand_votes[rank==2])
elec$margin_2014 <- 100*(elec$winner_votes-elec$runnerup_votes)/elec$total_ac_votes
elec$state_name <- as.character(elec$state_name)

## drop duplicates
elec <- elec %>% arrange(state_name, ac_num) %>%
  select(-rank, -winner_votes, -runnerup_votes)
elec <- filter(elec, cand_num < 2)

## merge in AC sample, voteshare data, and turnout data
merge <- inner_join(elec, select(AC.expt.sample, -state_code), by=c("state_name", "pc_num",
                                               'pc_name', "ac_num", "ac_name")) 
merge <- inner_join(merge, 
                    select(voteshare.data, -total_ac_votes, -X),
                    by=c("state_name", "pc_name", "ac_name", "ac_num"))
merge <- left_join(merge, 
                   select(turnout.data, state_name:turnoutrate_2009, state_election),
                   by=c("state_name", "ac_num", 'ac_name'))
merge$electors_2014 <- merge$ac_electors

## write to csv
write.csv(merge, "Data/election2014_data.csv")


