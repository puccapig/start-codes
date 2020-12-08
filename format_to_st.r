#Rachel Yuan Nong 2020-OCT-13
#mat to st by positions.
#2020-NOV-10 copied and modified to work with read in spot_number 

print("format.mat.to.st | format.mat, unfoud.st ")
format.mat.to.st <- function(format.mat, unfound.st){
print("spot number:")
spot_number <- readline()
spot_number <- as.numeric(spot_number)

format.st <- c(1:spot_number)
format.st <- as.data.frame(format.st)
colnames(format.st) <- c("format.st")

for(i in 1:spot_number){
	a <- as.character(tissue_position[i,4])
	if(match(a, colnames(cpm_mat), nomatch = 0) == 0){format.st[i,1] <- unfound.st}
	if(match(a, colnames(cpm_mat), nomatch = 0) != 0){format.st[i,1] <- format.mat[match(a, colnames(cpm_mat), nomatch = 0)]}
}
	return(format.st)
}
