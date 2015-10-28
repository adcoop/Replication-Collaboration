
#####
## Visualizing vote buying data
#####

rm(list=ls())

library(dplyr)
library(foreign)
library(readstata13)
library(data.table)

# read in journalist data
datj <- read.dta13("~/Dropbox/Green and Vasudevan (2015) replication/1. Journalist Data/Input Data/journalist_data_nopii.dta")

# create affiliations
ally <- data.frame('party_ally' = c(rep('NDA', 8), rep('UPA', 5)),
                   'party_name' = c("BJP","TDP","SHS","SWP","RPI(A)","RSP","LJP","AD",
                                    "INC","NCP","RJD","RLD","JMM"),
                   stringsAsFactors=F)
setnames(ally, c('party_ally', 'party_name'), c('secret_1_ally', 'secret_1'))
datj <- left_join(datj, ally, by=c("secret_1"))
setnames(ally, c('secret_1_ally', 'secret_1'), c('secret_2_ally', 'secret_2'))
datj <- left_join(datj, ally, by=c("secret_2"))
setnames(ally, c('secret_2_ally', 'secret_2'), c('secret_3_ally', 'secret_3'))
datj <- left_join(datj, ally, by=c("secret_3"))
setnames(ally, c('secret_3_ally', 'secret_3'), c('secret_4_ally', 'secret_4'))
datj <- left_join(datj, ally, by=c("secret_4"))
datj$secret_1_ally[is.na(datj$secret_1_ally)] <- datj$secret_1[is.na(datj$secret_1_ally)]
datj$secret_2_ally[is.na(datj$secret_2_ally)] <- datj$secret_2[is.na(datj$secret_2_ally)]
datj$secret_3_ally[is.na(datj$secret_3_ally)] <- datj$secret_3[is.na(datj$secret_3_ally)]
datj$secret_4_ally[is.na(datj$secret_4_ally)] <- datj$secret_4[is.na(datj$secret_4_ally)]

## create dummies for vote buying parties
datj$vb <- paste(datj$secret_1_ally, datj$secret_2_ally, datj$secret_3_ally, datj$secret_4_ally, sep = ",")
datj$nda_vb <- ifelse(grepl("NDA", datj$vb), 1, 0)
datj$upa_vb <- ifelse(grepl("UPA", datj$vb), 1, 0)
datj$oth_vb <- ifelse(grepl("AAAP|AJSUP|BJD|BSP|BVA|JBSP|JD(S)|JKP|JVM|SP|TRS|YSRCP", datj$vb), 1, 0)

## create measure of pct of journos who name major parties by PC
datj_pc <- datj %>% group_by(state_name, pc_name) %>% summarize(pct_nda = mean(nda_vb, na.rm=T), pct_upa = mean(upa_vb, na.rm=T), pct_oth = mean(oth_vb, na.rm=T))

## plot histograms of vote buying claims by journos for each coalition by PC
pdf("~/Desktop/Replication Collaboration Clone/Figures/journalist_concurrence_nda.pdf")
hist(datj_pc$pct_nda, main = "Pct of Journalists Naming NDA 
     as Vote Buyer by PC", xlab = "Pct Journalists", cex.lab = 2, cex.main = 2, cex.axis = 2)
dev.off()
pdf("~/Desktop/Replication Collaboration Clone/Figures/journalist_concurrence_upa.pdf")
hist(datj_pc$pct_upa, main = "Pct of Journalists Naming UPA 
     as Vote Buyer by PC", xlab = "Pct Journalists", cex.lab = 2, cex.main = 2, cex.axis = 2)
dev.off()


## read in maps for states
m <- readShapePoly("~/Downloads/indiamap/andhrapradesh/andhrapradesh.parliamentary.shp")
m1 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))
m <- readShapePoly("~/Downloads/indiamap/bihar/bihar.parliamentary.shp")
m2 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))
m <- readShapePoly("~/Downloads/indiamap/chhattisgarh/chhattisgarh.parliamentary.shp")
m3 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))
m <- readShapePoly("~/Downloads/indiamap/jharkhand/jharkhand.parliamentary.shp")
m4 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))
m <- readShapePoly("~/Downloads/indiamap/madhyapradesh/madhyapradesh.parliamentary.shp")
m5 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))
m <- readShapePoly("~/Downloads/indiamap/maharashtra/maharashtra.parliamentary.shp")
m6 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))
m <- readShapePoly("~/Downloads/indiamap/orissa/orissa.parliamentary.shp")
m7 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))
m <- readShapePoly("~/Downloads/indiamap/rajasthan/rajasthan.parliamentary.shp")
m8 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))
m <- readShapePoly("~/Downloads/indiamap/uttarpradesh/uttarpradesh.parliamentary.shp")
m9 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))
m <- readShapePoly("~/Downloads/indiamap/karnataka/karnataka.parliamentary.shp")
m10 <- spChFIDs(m, paste(m$state, m$pc, sep = ""))

map <- spRbind(m1, spRbind(m2, spRbind(m3, spRbind(m4, spRbind(m5, spRbind(m6, spRbind(m7, spRbind(m8, spRbind(m9, m10)))))))))

## merge in the journalist info
dat <- map@data
dat$sort <- seq(1:dim(dat)[1])
setnames(datj_pc, c('state_name'), 'state')
  # make names character strings
dat$pc_name <- as.character(dat$pc_name); dat$state <- as.character(dat$state)
datj_pc$pc_name <- as.character(datj_pc$pc_name); datj_pc$state <- as.character(datj_pc$state)
  # check for concordance if necessary
# sort(datj_pc$pc_name[!(datj_pc$pc_name %in% dat$pc_name)])
# sort(dat$pc_name[!(dat$pc_name %in% datj_pc$pc_name)])
# sort(datj_pc$state[!(datj_pc$state %in% dat$state)])
# sort(dat$state[!(dat$state %in% datj_pc$state)])
  # rename a few PCs
dat$state[dat$state=="Chhattisgarh"] <- "Chattisgarh"
dat$pc_name[dat$pc_name=="Palamu"] <- "Palamau"
dat$pc_name[dat$pc_name=="Ratnagiri–Sindhudurg"] <- "Ratnagiri-Sindhudurg"
dat$pc_name[dat$pc_name=="Tirupati"] <- "Tirupathi"
dat$pc_name[dat$pc_name=="Tonk-Sawai"] <- "Tonk-Sawai Madhopur"
  # merge
dat <- left_join(dat, datj_pc, by=c("state", "pc_name"))

## put map data in original order nad put back into map
dat <- arrange(dat, sort)
map@data <- dat

## output the maps for NDA, UPA and other
pdf("~/Desktop/Replication Collaboration Clone/Figures/nda_vb_map.pdf")
spplot(map, 'pct_nda', at = seq(0,1,0.1), col.regions = rev(heat.colors(10)), 
       colorkey=list(space="bottom"),
       main = "Pct Journalists Reporting NDA Vote Buying by PC")
dev.off()
pdf("~/Desktop/Replication Collaboration Clone/Figures/upa_vb_map.pdf")
spplot(map, 'pct_upa', at = seq(0,1,0.1), col.regions = rev(terrain.colors(10)), 
       colorkey=list(space="bottom"),
       main = "Pct Journalists Reporting UPA Vote Buying by PC")
dev.off()
pdf("~/Desktop/Replication Collaboration Clone/Figures/oth_vb_map.pdf")
spplot(map, 'pct_oth', at = seq(0,1,0.1), col.regions = rev(topo.colors(10)), 
       colorkey=list(space="bottom"),
       main = "Pct Journalists Reporting Other Parties Vote Buying by PC")
dev.off()

