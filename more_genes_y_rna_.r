#Rachel Yuan Nong Mikkelsen 2021-January-24 Uppsala Sweden
#

more_genes_y_rna <- read.csv("RN2021January22_more_genes_Y_RNA.csv")

more_genes_y_rna <- more_genes_y_rna[,3:dim(more_genes_y_rna)[2]]
more_genes_y_rna[,7] <- more_genes_y_rna[,5] - more_genes_y_rna[,4]

hist(more_genes_y_rna[,7], main = "more_genes_Y_RNA", xlab = "chromosome position:start - end")

b <- as.data.frame(table(more_genes_y_rna[,3]))
b <- b[order(b[,2]),]
par(cex = 0.5)
barplot(b[,2], names.arg = b[,1], col = "red", main = "more_genes_Y_RNA")
dev.new()
par(cex = 0.5)
plot(more_genes_y_rna[,4], more_genes_y_rna[,3], pch = 19, col = "blue")

