rm(list=ls(all=T))
setwd("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)")

# Define randomization test function
randtest <- function(x,y,fun=mean,reps=10000) {
  n <- length(x)
  m <- length(y)
  data <- c(x,y)
  results <- numeric(reps)
  for (i in 1:reps) {
    simtemp <- sample(data)
    results[i] <- fun(simtemp[1:n])-fun(simtemp[(n+1):(n+m)])
  }
  greater.p <- sum(results >= (fun(x)-fun(y)))/reps
  less.p <- sum(results <= (fun(x)-fun(y)))/reps
  test.stat <- abs(fun(x)-fun(y))
  two.sided.p <- sum(abs(results)>=test.stat)/reps
  p.values <- c(greater.p, less.p, two.sided.p)
  names(simtemp) <- c("p.greater.than", "p.less.than",
                      "two.sided.p")
  return(list(results,p.values,test.stat))
}

# Load dataset - Voteshare Spec 1 
dat1 <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare1.csv"))
names(dat1)

# IPW - Voteshare Spec 1
IPW1 = (lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009, data=dat1, weights=wgt_treatany))
summary(IPW1)

# FE - Voteshare Spec 1
FE1 <- lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009 + num_eligible1 + num_eligible2, dat1)
summary(FE1)

# Regressions - Voteshare Spec 1
reg1a <- lm(voteshare_spec1_2014 ~ treatany, data=dat1, weights = wgt_treatany)
summary(reg1a)

dat1$lit_pc <- 100 - dat1$illit_pc
reg1b <- lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009 + lit_pc + rural_pc + scst_pc + poll_date1 + poll_date2 + poll_date3 + poll_date4 + poll_date5 + poll_date6 + state_election, dat1, weights=wgt_treatany)
summary(reg1b)

# Load dataset - Voteshare Spec 2
dat2 <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare2.csv"))
names(dat2)

# IPW - Voteshare Spec 2
IPW2 = (lm(voteshare_spec2_2014 ~ treatany + voteshare_spec2_2009, data=dat2, weights=wgt_treatany))
summary(IPW2)

# FE - Voteshare Spec 2
FE2 <- lm(voteshare_spec2_2014 ~ treatany + voteshare_spec2_2009 + num_eligible1 + num_eligible2, dat2)
summary(FE2)

# Regressions - Voteshare Spec 2
reg2a <- lm(voteshare_spec2_2014 ~ treatany, data=dat2, weights = wgt_treatany)
summary(reg2a)

dat2$lit_pc <- 100 - dat2$illit_pc
reg2b <- lm(voteshare_spec2_2014 ~ treatany + voteshare_spec2_2009 + lit_pc + rural_pc + scst_pc + poll_date1 + poll_date2 + poll_date3 + poll_date4 + poll_date5 + poll_date6 + state_election, dat2, weights=wgt_treatany)
summary(reg2b)

# Load dataset - Voteshare Spec 3
dat3 <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/voteshare3.csv"))
names(dat3)

# IPW - Voteshare Spec 3
IPW3 = (lm(voteshare_spec3_2014 ~ treatany + voteshare_spec3_2009, data=dat3, weights=wgt_treatany))
summary(IPW3)

# FE - Voteshare Spec 3
FE3 <- lm(voteshare_spec3_2014 ~ treatany + voteshare_spec3_2009 + num_eligible1 + num_eligible2, dat3)
summary(FE3)

# Regressions - Voteshare Spec 3
reg3a <- lm(voteshare_spec3_2014 ~ treatany, data=dat3, weights = wgt_treatany)
summary(reg3a)

dat3$lit_pc <- 100 - dat3$illit_pc
reg3b <- lm(voteshare_spec3_2014 ~ treatany + voteshare_spec3_2009 + lit_pc + rural_pc + scst_pc + poll_date1 + poll_date2 + poll_date3 + poll_date4 + poll_date5 + poll_date6 + state_election, dat3, weights=wgt_treatany)
summary(reg3b)

# Load dataset - Turnout
datTO <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/turnout_data.csv"))
names(datTO)

# IPW - Turnout
IPWTO = (lm(turnoutrate_2014 ~ treatany + turnoutrate_2009, data=datTO, weights=wgt_treatany))
summary(IPWTO)

# FE - Turnout
FETO <- lm(turnoutrate_2014 ~ treatany + turnoutrate_2009 + num_eligible1 + num_eligible2, datTO)
summary(FETO)

# Regressions - Turnout
regTOa <- lm(turnoutrate_2014 ~ treatany, data=datTO, weights = wgt_treatany)
summary(regTOa)

datTO$lit_pc <- 100 - datTO$illit_pc
regTOb <- lm(turnoutrate_2014 ~ treatany + turnoutrate_2009 + lit_pc + rural_pc + scst_pc + poll_date1 + poll_date2 + poll_date3 + poll_date4 + poll_date5 + poll_date6 + state_election, datTO, weights=wgt_treatany)
summary(regTOb)

# Load dataset - Incumbent
datInc <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/incumbent_data.csv"))
names(datInc)

# IPW - Incumbent
IPWInc = (lm(incvoteshare_2014 ~ treatany + incvoteshare_2009, data=datInc, weights=wgt_treatany))
summary(IPWInc)

# Load dataset - Independent
datInd <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/independent_data.csv"))
names(datInd)

# IPW - Independent
IPWInd = (lm(indvoteshare_2014 ~ treatany + indvoteshare_2009, data=datInd, weights=wgt_treatany))
summary(IPWInd)

# Load dataset - NOTA
datNOTA <- as.data.frame(read.csv("/Users/zeno/Dropbox/Green and Vasudevan (2015) Data (1)/4. Analysis/Matlab Data/nota_data.csv"))
names(datNOTA)

# IPW - NOTA
IPWNOTA = (lm(notavoteshare_2014 ~ treatany, data=datNOTA, weights=wgt_treatany))
summary(IPWNOTA)