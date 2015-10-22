rm(list=ls(all=T))
setwd("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)")

# Load dataset
dat1 <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare1.csv"))
names(dat1)

# IPW - Spec 1
IPW1 = (lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009, data=dat1, weights=wgt_treatany))
summary(IPW1)
ATE.IPW1 <- coefficients(IPW1)[2]
print(ATE.IPW1)

reg1a <- lm(voteshare_spec1_2014 ~ treatany, data=dat1, weights = wgt_treatany)
summary(reg1a)

library(lfe)
dat1$lit_pc <- 100 - dat1$illit_pc
reg1b <- felm(voteshare_spec1_2014 ~ treatany + G(lit_pc) + G(rural_pc) + G(scst_pc), dat1, weights=dat1$wgt_treatany)
summary(reg1b)

# FE - Spec 1
#library(lfe)
#FE1 <- felm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009 | (illit_pc + scst_pc + rural_pc), data=dat1)
#summary(FE1)

# Load dataset
dat2 <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare2.csv"))
names(dat2)

# IPW - Spec 2
IPW2 = (lm(voteshare_spec2_2014 ~ treatany + voteshare_spec2_2009, data=dat2, weights=wgt_treatany))
summary(IPW2)
ATE.IPW2 <- coefficients(IPW2)[2]
print(ATE.IPW2)

# Load dataset
dat3 <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare3.csv"))
names(dat3)

# IPW - Spec 3
IPW3 = (lm(voteshare_spec3_2014 ~ treatany + voteshare_spec3_2009, data=dat3, weights=wgt_treatany))
summary(IPW3)
ATE.IPW3 <- coefficients(IPW3)[2]
print(ATE.IPW3)

