#Rachel Yuan Nong Mikkelsen 2022-JANUARY-25
#2022-JULY-08 copied and modified to work in this folder
#sample genes to orginal number
#2022-AUG09 copied and modified to work in this folder

LD_gene_from_genome1 <- function(){
	ref_human <- read.csv("~genes.csv", header = FALSE)
	
	more_genes <- genes_
	grab_ <- grab1_[grab0_]
	
	ld_gene_table <- matrix(0, ncol = dim(ref_human)[2], nrow = 1)
	ld_gene_table <- as.data.frame(ld_gene_table)
	for(i in 1:dim(more_genes)[1]){
		ki <- more_genes[i,]
		region_ki <- ref_human[ref_human[,4] == as.character(unlist(ki[4])), ]
		region_ki[,5] <- as.numeric(as.character(unlist(region_ki[,5])))
		region_ki <- region_ki[order(region_ki[,5]),]
		
		ii <- which(region_ki[,2] == as.character(unlist(ki[2])))
		ii_left <- ii - grab_
		ii_right <- ii + grab_
		if(ii_left < 0){ 
			ii_left <- 1
			ii_right <- ii_left + grab_ + grab_
			}
		if(ii_right > dim(region_ki)[1]){
			ii_right <- dim(region_ki)[1]
			ii_left <- ii_right - grab_ - grab_
			}
		ld_gene1 <- region_ki[ii_left:ii_right, ]
		ld_gene_table <- rbind(ld_gene_table, ld_gene1)
	}
		ld_gene_table <- ld_gene_table[2:dim(ld_gene_table)[1],]
		o_ <- dim(ld_gene_table)[1]
		o_ <- sample(c(1:o_), dim(more_genes)[1])
		ld_gene_table <- ld_gene_table[o_,]
		write.csv(ld_gene_table, file = "ld_gene_table.csv")
		print(dim(ld_gene_table))
		print("ld_gene_table.csv")
}
