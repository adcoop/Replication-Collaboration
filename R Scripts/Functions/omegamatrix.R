omegamatrix <- function(data){
## set up omega matrix
N <- dim(data)[1]
station1 <- data[,'station_id1']
station2 <- data[,'station_id2']
station3 <- data[,'station_id3']
dep_matrix <- matrix(0,N,N) 
for (i in 1:N){
  for (j in 1:N){
    dep_matrix[i,j] <- ifelse(station1[i] == station1[j],1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station1[i] == station2[j],1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station1[i] == station3[j],1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station2[i] == station1[j] & station2[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station2[i] == station2[j] & station2[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station2[i] == station3[j] & station2[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station3[i] == station1[j] & station3[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station3[i] == station2[j] & station3[i] != -9,1,dep_matrix[i,j])
    dep_matrix[i,j] <- ifelse(station3[i] == station3[j] & station3[i] != -9,1,dep_matrix[i,j])
  }
}
return(dep_matrix)}