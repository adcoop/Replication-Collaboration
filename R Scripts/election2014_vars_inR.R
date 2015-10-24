rm(list=ls())
library(dplyr)
library(foreign)
library(haven)
library(readstata13)

# read Journalist Data
votebuyers.spec1 <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec1.dta")
votebuyers.spec2 <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec2.dta")
votebuyers.spec3 <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/votebuyers_spec3.dta")
journalist.data.nopii.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Output Data/journalist_data_nopii_clean.dta")
journalist.data.nopii <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Input Data/journalist_data_nopii.dta")

# Read Electoral Data
results.10states2014.clean <- read_dta("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/results_10states2014_clean.dta")
ECI.sched.clean <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/ECI_sched_clean.dta")
PC.results <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/2. Electoral Data/PC_results.dta")

# Read Sample Data
AC.expt.sample <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/3. Sample Data/AC_expt_sample.dta")

# Read Analysis Data
election2014.data <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Stata Data/election2014_data.dta")
voteshare.data <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Stata Data/voteshare_data.dta")
turnout.data <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Stata Data/turnout_data.dta")



# election2014_vars.do ----------------------------------------------------
#*******************************************2014 Variables************************************
#  *Number votebuyers, number parties, number candidates, votebuyer voteshare, independent voteshare, margin, turnoutrate and electors
elec <- results.10states2014.clean
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
elec$state_name <- as.character(elec$state_name)

merge <- inner_join(elec, AC.expt.sample, by=c("state_name", "ac_num")) 
# Stata code only retains treatany variable from the AC.expt.sample dataset. I kept all here.
# Stata code only does keep(matched) so I used inner_join
merge <- inner_join(merge,voteshare.data, by=c("state_name", "ac_num"))
# Stata code only retains voteshare_spec?_2014 num_spec?_2014 variables from the voteshare.data dataset. I kept all here.
# Stata code only does keep(matched) so I used inner_join
merge <- left_join(merge,turnout.data, by=c("state_name", "ac_num") )
# Stata code only retains turnoutrate_2014 variable from the turnoutshare.data dataset. I kept all here.
# Stata code only does keep(matched) so I used inner_join
merge$electors_2014 <- merge$ac_electors
# Stata code drops a lot of variables. I keep them in for now.
# Saves this dataset as "4. Analysis/Stata Data/election2014_data.dta"
save(merge, file="~/Desktop/Replication Collaboration Clone/R Scripts/election2014_data.Rdata")

# votebuyer_specs.do -------------------------------------------------------

# the journalist.data.nopii is the uncleaned results from the journalists - so keeps original parties
# The file cleans it and gets rid of original parties.
# *Standardizing party/alliance names
# foreach var of varlist spend_1-win_3 party_? actual_cand_party? {
#   replace `var' = "NDA" if inlist(`var',"BJP","SHS","TDP","SWP","RPI(A)","LJP","RSP","AD")
# replace `var' = "UPA" if inlist(`var',"INC","NCP","RJD","RLD","JMM")
# }
# Exports three different votebuying specifications by standardized party/alliance names
# Matches up parties with actual voteshare from PC.results

# voteshare_data.do -------------------------------------------------------

# Most of the merging in stata to combine journalist specifications with vote share from 2009 and 2014.
# Matched on journalist reports by state_name pc_num party_name
# Assume same parties are buying votes in 2009 and 2014, same party/alliance in 2009 and 2014 (is this true?)
# Small parties lumped into alliances
# rename  cand_party party_name
# replace party_name = "NDA" if inlist(party_name,"BJP","TDP","SHS","SWP","RPI(A)","RSP","LJP","AD")
# replace party_name = "UPA" if inlist(party_name,"INC","NCP","RJD","RLD","JMM") 

# To create csvs for analysis, the file merges voteshare_data.dta with AC_expt_sample


