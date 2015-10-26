#####
## Randomization inference script
#####
# 
# rm(list=ls())
# 
# setwd("/Users/Lauren/Dropbox/Green and Vasudevan (2015) replication")
# 
#   # read in data
# dat <- read.csv('4. Analysis/Matlab Data/voteshare1.csv')
# 
#   # define binary treatmnet indicator Z and outcome
# Z <- dat$treatany
# Y <- dat$voteshare_spec1_2014
# 
#   # identify covariates
# cov <- c('voteshare_spec1_2009')
# cov <- c('voteshare_spec1_2009', 'rural_pc', 'illit_pc', 'scst_pc')
# 
#   # identify prob of treatment
# prob <- 'prob_treatany'
# 
#   # set iterations
# iter = 10000

ri <- function(dat, Z, Y, cov = NULL, prob = NULL, iter = 10000) {
  
  library(readstata13)
  library(foreign)
  library(ri)
  library(data.table)
  
  # read in blocking var
  block <- read.dta13('0. Randomization/Output Data/stations_sample_randomized.dta')
  block <- subset(block, select =c(station_id, block, treat))
  
  # set output structure 
  results <- data.frame('ate' = NA, 'lo95' = NA, 'up95' = NA, 'p' = NA)
  
  # define vars
  Z <- dat[,Z]
  Y <- dat[,Y]
  cov <- as.matrix(dat[,cov])
  prob <- dat[,prob]
  obs <- nrow(dat)
  
  # estimate ate
  ate <- estate(Y, Z, X=cov, prob=prob)
  results$ate <- ate
  
  # generate permutations of treatment by station
  perms <- genperms(block$treat, blockvar = block$block, maxiter=iter)
  perms <- as.data.frame(perms)
  perms <- cbind.data.frame('station_id' = block$station_id, perms)
    
  # merge station-level perms into table of stations by AC
    # define AC-level id to sort
  dat$ac_id <- seq(1, obs)
  perms <- as.data.table(perms)
  # merge station treatment into AC for each 1-3 stations
  for (i in 1:3){
    # pull out the identifiers from the ac data
    permsi <- as.data.table(subset(dat, select = c('ac_id', paste0('station_id', i))))
    setnames(permsi, paste0('station_id',i), 'station_id')
    # merge permutations of treatment into ac data by station
    permsi <- merge(permsi, perms, all.x = T, by = 'station_id')
    permsi <- as.matrix(permsi[order(permsi$ac_id),])
    permsi[is.na(permsi)] <- 0
    assign(paste0('perms',i), permsi)
  }
  
  # create "treatany" var by station combinations
  perms_final <- matrix(NA, obs, iter)
  for (i in 1:iter){
    t <- ifelse(perms1[,i+2]==1 | perms2[,i+2]==1 | perms3[,i+2]==1, 1, 0)
    perms_final[,i] <- t
  }
  hist(colMeans(perms_final))
  
  # get p-value for observed ATE under sharp null
  Ys <- genouts(Y, Z, ate=0)    # generate potential outcomes under sharp null
  distout <- gendist(Ys, perms_final, cov, prob = prob)
  results$p <- (sum(distout <= ate))/iter
  save <- dispdist(distout, ate, display.plot = T)
  
  # generate confidence intervals
  Ys <- genouts(Y, Z, ate=ate)  # generate potential outcomes UNDER THE ASSUMPTION THAT ATE=ESTIMATED ATE
  distout <- gendist(Ys, perms_final, cov, prob=prob)
  save <- dispdist(distout, ate, display.plot = F) 
  results$lo95 <- save$quantile[1]
  results$up95 <- save$quantile[2]
  
  return(results)
  
}
