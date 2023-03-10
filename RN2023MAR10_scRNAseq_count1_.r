#Rachel Yuan Nong Mikkelsen 2023MAR10

print(">>>>>>proportion count1 in sampled single cell")
print(dim(mat))
mat1_ <- mat1_[1:dim(mat1_)[1],]
mat1_ <- mat1_[rowSums(mat1_) != 0,]
print(dim(mat1_))

print("number of genes:")
number_of_genes <- readline()
number_of_genes <- as.integer(number_of_genes)

print("number of cells:")
number_of_cells <- readline()
number_of_cells <- as.integer(number_of_cells)

a <- c(1:number_of_genes)
a[1:length(a)] <- -1
a <- as.data.frame(a)
for(i in 1:dim(a)[1]){
	a[i,1] <- table(mat1_[i,] == 1)[[1]]
	if(a[i,1] == number_of_cells){
		a[i, 1] <- 0
		a[i, 2] <- number_of_cells
	}else{
		a[i,2] <- table(mat1_[i,] == 1)[[2]]
	}
}
a[,3] <- round(a[,1]/dim(mat1_)[2]*100)

for(i in 1:dim(a)[1]){
	if(a[i,3] > 0 & a[i,3] <= 10){
		a[i,4] <- 1
		}
	if(a[i,3] > 10 & a[i,3] <= 20){
                a[i,4] <- 2
                }
	if(a[i,3] > 20 & a[i,3] <= 30){
                a[i,4] <- 3
                }
	if(a[i,3] > 30 & a[i,3] <= 40){
                a[i,4] <- 4
                }
	if(a[i,3] > 40 & a[i,3] <= 50){
                a[i,4] <- 5
                }
	if(a[i,3] > 50 & a[i,3] <= 60){
                a[i,4] <- 6
                }
	if(a[i,3] > 60 & a[i,3] <= 70){
                a[i,4] <- 7
                }
	if(a[i,3] > 70 & a[i,3] <= 80){
                a[i,4] <- 8
                }
	if(a[i,3] > 80 & a[i,3] <= 90){
                a[i,4] <- 9
                }
	if(a[i,3] > 90 & a[i,3] <= 100){
                a[i,4] <- 10
                }
}

table_a <- as.data.frame(table(a[,4]))
table_a_index <- c("(0-10%)", "(11-20%)", "(21-30%)", "(31-40%)", "(41-50%)", "(51-60%)", "(61-70%)", "(71-80%)", "(81-90%)", "(91-100%)")
row_start <- as.integer(as.character(table_a[1, 1]))
row_stop <- as.integer(as.character(table_a[dim(table_a)[1], 1]))
rownames(table_a) <- table_a_index[row_start:row_stop]
table_a[,3] <- round(table_a[,2]/sum(table_a[,2])*100)
table_a[,4] <- paste0(rownames(table_a), ": ", table_a[, 3], "%")

par(mfrow = c(1,4), cex = 0.5)
hist(a[,1], main = "expression > 1", xlab = "number of single cell")
hist(a[,2], main = "expression == 1", xlab = "number of single cell")
hist(a[,3], main = paste0("prop. count1 in n=", dim(mat1_)[2]), xlab = "%")
legend("topleft", legend = table_a[,4], text.col = rainbow(dim(table_a)[1]), bty = "n")
barplot(table_a[,3], col = rainbow(4), names.arg = rownames(table_a))
print("plot title_:")
title_ <- readline()
title(sub = title_)

