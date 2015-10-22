rm(list=ls(all=T))
setwd("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)")

# Load dataset - Spec 1
dat1 <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare1.csv"))
names(dat1)

# IPW - Spec 1
IPW1 = (lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009, data=dat1, weights=wgt_treatany))
summary(IPW1)

# FE - Spec 1
FE1 <- lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009 + num_eligible1 + num_eligible2, dat1)
summary(FE1)

# Regressions - Spec 1
reg1a <- lm(voteshare_spec1_2014 ~ treatany, data=dat1, weights = wgt_treatany)
summary(reg1a)

dat1$lit_pc <- 100 - dat1$illit_pc
reg1b <- lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009 + lit_pc + rural_pc + scst_pc + poll_date1 + poll_date2 + poll_date3 + poll_date4 + poll_date5 + poll_date6 + state_election, dat1, weights=wgt_treatany)
summary(reg1b)

# Load dataset - Spec 2
dat2 <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare2.csv"))
names(dat2)

# IPW - Spec 2
IPW2 = (lm(voteshare_spec2_2014 ~ treatany + voteshare_spec2_2009, data=dat2, weights=wgt_treatany))
summary(IPW2)

# FE - Spec 2
FE2 <- lm(voteshare_spec2_2014 ~ treatany + voteshare_spec2_2009 + num_eligible1 + num_eligible2, dat2)
summary(FE2)

# Regressions - Spec 2
reg2a <- lm(voteshare_spec2_2014 ~ treatany, data=dat2, weights = wgt_treatany)
summary(reg2a)

dat2$lit_pc <- 100 - dat2$illit_pc
reg2b <- lm(voteshare_spec2_2014 ~ treatany + voteshare_spec2_2009 + lit_pc + rural_pc + scst_pc + poll_date1 + poll_date2 + poll_date3 + poll_date4 + poll_date5 + poll_date6 + state_election, dat2, weights=wgt_treatany)
summary(reg2b)

# Load dataset - Spec 3
dat3 <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare3.csv"))
names(dat3)

# IPW - Spec 3
IPW3 = (lm(voteshare_spec3_2014 ~ treatany + voteshare_spec3_2009, data=dat3, weights=wgt_treatany))
summary(IPW3)

# FE - Spec 3
FE3 <- lm(voteshare_spec3_2014 ~ treatany + voteshare_spec3_2009 + num_eligible1 + num_eligible2, dat3)
summary(FE3)

# Regressions - Spec 3
reg3a <- lm(voteshare_spec3_2014 ~ treatany, data=dat3, weights = wgt_treatany)
summary(reg3a)

dat3$lit_pc <- 100 - dat3$illit_pc
reg3b <- lm(voteshare_spec3_2014 ~ treatany + voteshare_spec3_2009 + lit_pc + rural_pc + scst_pc + poll_date1 + poll_date2 + poll_date3 + poll_date4 + poll_date5 + poll_date6 + state_election, dat3, weights=wgt_treatany)
summary(reg3b)