# Load placebo dataset (vote share)
dat3pvs <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare3_placebo.csv"))
names(dat3pvs)

# Placebo test (vote share)
placebovs <- lm(voteshare_spec3_2014 ~ treatany + voteshare_spec3_2009, dat3pvs)
summary(placebovs)

# Load placebo dataset (turnout)
dat3pto <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/turnout_placebo.csv"))
names(dat3pto)

# Placebo test (turnout)
placeboto <- lm(turnoutrate_2014 ~ treatany + turnoutrate_2009, dat3pto)
summary(placeboto)