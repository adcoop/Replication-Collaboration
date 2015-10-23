#####
## Randomization inference script
#####

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
#   # create covariate matrix
# cov2 <- as.matrix(dat[,c('voteshare_spec1_2009')])
# cov <- as.matrix(dat[,c('rural_pc', 'illit_pc', 'scst_pc', 'voteshare_spec1_2009')])
# 
#   # set iterations
# iter = 10000

ri <- function(dat, Z, Y, cov = NULL, iter = 10000) {
  
  library(readstata13)
  library(foreign)
  library(ri)
  library(data.table)
  
  # read in blocking var
  block <- read.dta13('0. Randomization/Output Data/stations_sample_randomized.dta')
  block <- subset(block, select =c(station_id, block, treat))
  
  # set output structure 
  results <- data.frame('ate' = NA, 'lo95' = NA, 'up95' = NA, 'p' = NA)
  
  # make sure cov is a matrix
  cov <- as.matrix(cov)
    
  # estimate ate
  ate <-  estate(Y, Z, X = cov, prob=dat$prob_treatany)
  results$ate <- ate
  
  # generate permutations of treatment by station
  perms <- genperms(block$treat, blockvar = block$block, maxiter=iter)
  perms <- as.data.frame(perms)
  perms <- cbind.data.frame('station_id' = block$station_id, perms)
  
  # merge station-level perms into table of stations by AC
    # define AC-level id to sort
  dat$ac_id <- seq(1, nrow(dat))
  perms <- as.data.table(perms)
    # create frame of ACs and stations and merge perms into AC list - station 1
  perms1 <- as.data.table(subset(dat, select = c('ac_id', 'station_id1')))
  setnames(perms1, 'station_id1', 'station_id')
  perms1 <- merge(perms1, perms, all.x = T, by = 'station_id')
    # create frame of ACs and stations and merge perms into AC list - station 2
  perms2 <- as.data.table(subset(dat, select = c('ac_id', 'station_id2')))
  setnames(perms2, 'station_id2', 'station_id')
  perms2 <- merge(perms2, perms, all.x = T, by = 'station_id')
    # sort perms for all stations by AC
  perms1 <- as.matrix(perms1[order(perms1$ac_id),])
  perms2 <- as.matrix(perms2[order(perms2$ac_id),])

  # create "treatany" var by station combinations
  perms_final <- matrix(NA, dim(perms1)[1], iter)
  perms2[is.na(perms2)] <- 0
  for (i in 1:iter){
    t <- ifelse(perms1[,i+2]==1 | perms2[,i+2]==1, 1, 0)
    perms_final[,i] <- t
  }
  hist(colMeans(perms_final))
  
  # get p-value for observed ATE under sharp null
  Ys <- genouts(Y, Z, ate=0)    # generate potential outcomes under sharp null
  distout <- gendist(Ys, perms_final, prob = dat$prob_treatany)
  results$p <- (sum(distout <= ate))/iter
  save <- dispdist(distout, ate, display.plot = T)
  Ys <- genouts(Y, Z, ate=ate)  # generate potential outcomes UNDER THE ASSUMPTION THAT ATE=ESTIMATED ATE
  distout <- gendist(Ys, perms_final, prob=dat$prob_treatany)
  save <- dispdist(distout, ate, display.plot = F) 
  results$lo95 <- save$quantile[1]
  results$up95 <- save$quantile[2]
  return(results)
  
}
