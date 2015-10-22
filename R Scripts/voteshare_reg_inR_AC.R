library(foreign)
dat1 <- read.csv("~/Dropbox/Green and Vasudevan (2015) replication/4. Analysis/Matlab Data/voteshare1.csv")
N<- 628
station1 <- dat1[,11]
station2 <- dat1[,12]
station3 <- dat1[,13]
dep_matrix <- matrix(0,N,N) 
for (i in 1:N){
  for (j in 1:N){
    ifelse(station1[i] == station1[j],dep_matrix[i,j] <- 1,NA)
    ifelse(station1[i] == station2[j],dep_matrix[i,j] <- 1,NA)
    ifelse(station1[i] == station3[j],dep_matrix[i,j] <- 1,NA)
    ifelse(station2[i] == station1[j] & station2[i] == -9,dep_matrix[i,j] <- 1,NA)
    ifelse(station2[i] == station2[j] & station2[i] == -9,dep_matrix[i,j] <- 1,NA)
    ifelse(station2[i] == station3[j] & station2[i] == -9,dep_matrix[i,j] <- 1,NA)
    ifelse(station3[i] == station1[j] & station3[i] == -9,dep_matrix[i,j] <- 1,NA)
    ifelse(station3[i] == station2[j] & station3[i] == -9,dep_matrix[i,j] <- 1,NA)
    ifelse(station3[i] == station3[j] & station3[i] == -9,dep_matrix[i,j] <- 1,NA)
  }
}

num_terms <- sum(dep_matrix)

# Unweighted - Spec 1
UW1 = (lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009, data=dat1))
UW1.vcov <- vcov(UW1)
UW1.resid <- resid(UW1)
UW1.omega_hat <- 

# IPW - Spec 1
IPW1 = (lm(voteshare_spec1_2014 ~ treatany + voteshare_spec1_2009, data=dat1, weights=wgt_treatany))
summary(IPW1)
ATE.IPW1 <- coefficients(IPW1)[2]
print(ATE.IPW1)



# If using their matlab format:
d <- data.frame(dat1[,1])
colnames(d) <- "voteshare14"
d$voteshare09 <- dat1[,2]
d$treat <- dat1[,3]
d$rural <- dat1[,4]
d$illit <- dat1[,5]
d$scst <- dat1[,6]
d$weight0 <- dat1[,7]
d$weight <- sqrt(d$weight0)
num_stations <- dat1[,8:9]
station1 <- dat1[,11]
station2 <- dat1[,12]
station3 <- dat1[,13]
poll_date <- dat1[,14:18]
d$state_elec <- dat1[,20]