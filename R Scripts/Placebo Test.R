# Load placebo dataset
dat3p <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare3_placebo.csv"))
names(dat3p)

# Placebo test
placebo <- lm(voteshare_spec3_2014 ~ treatany + voteshare_spec3_2009, dat3p)
summary(placebo)