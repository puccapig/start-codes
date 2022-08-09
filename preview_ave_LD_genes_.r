#Rachel Yuan Nong Mikkelsen 2022JUL07
#for plotting average expression from a few genes on tsne and their clusters
#2022JUL08 add LD genes
#2022AUG08 copied and modified to work in this file
#2022AUG09 copied and modified to work in this file

#print("pre_fix:")
#pre_fix <- readline()

library(tsne)
source("LD_gene_from_genome1_auto_.r")
read_genes_from_ <- read.csv("processing_files_.csv")
read_genes_from_ <- read_genes_from_[,2:dim(read_genes_from_)[2]]

for(read1_ in 1:dim(read_genes_from_)[1]){
	grab1_ <- c(0, 1, 10, 100)
	for(grab0_ in 1:4){
		genes_ <- as.character(read_genes_from_[read1_, 1])
		print(paste0("processing >>>> ", genes_))
		genes_ <- read.csv(genes_)
        	print("format example:")
        	print(genes_[1,])
	
                in_title_ <- as.character(read_genes_from_[read1_, 6])
		matrix_in <- cpm_data_

	#print("genes in column:")
	#column_ <- readline()
	#column_ <- as.numeric(column_)
	column_ <- 2

	genes_ <- as.character(genes_[,column_])
	source("~ref_human_table.r")
	
	gene1_ <- genes_[1]
	gene1i_ <- sapply(substr(gene1_, 1, 4), function(x){x[1]})
	if(gene1i_ != "ENSG"){
		genes_ <- ref_human[match(genes_, ref_human[,2], nomatch = 0),]
		}else{
		genes_ <- ref_human[match(genes_, ref_human[,1], nomatch = 0),]
	}

	LD_gene_from_genome1()

	genes_ <- read.csv("ld_gene_table.csv")
	genes_ <- genes_[,2:dim(genes_)[2]]

	gene1_ <- rownames(matrix_in)[1]
	gene1i_ <- sapply(substr(gene1_, 1, 4), function(x){x[1]})
	if(gene1i_ != "ENSG"){
		matrix_out <- matrix_in[match(genes_[,2], rownames(matrix_in), nomatch = 0),]
		}else{
		matrix_out <- matrix_in[match(genes_[,1], rownames(matrix_in), nomatch = 0),]
	}

	matrix_out_ave <- colSums(2^matrix_out - 1)/dim(matrix_out)[1]
	matrix_out_ave <- as.data.frame(matrix_out_ave)
	colnames(matrix_out_ave) <- c("ave_expression")
	print(">>>>>>>> average expression in matrix_out_ave")

	print("clustering...")
	pc_matrix_out <- prcomp(matrix_out)
	pc_matrix_out_a <- pc_matrix_out[[2]]

	if(dim(genes_)[1] > 1000){k0 <- 10}else{k0 <- 3}
	my_tsne_pc_matrix_out <- tsne(pc_matrix_out_a[,1:k0], k = 3)
	cutree_tsne_matrix_out <- cutree(hclust(dist(my_tsne_pc_matrix_out)), k = 24)
	factor_name <- paste0(cutree_tsne_matrix_out, "_")
	source("~assign_legend_text_and_color.r")
	cols.by.matrix_out <- cols.by.input
	legend.by.matrix_out <- legend.by.input
	print(">>>>>>> cols.by.matrix_out | legend.by.matrix_out")

	matrix_in <- as.data.frame(t(log2(matrix_out_ave[,1]+1)))
	print("ave_expression")
	source("~function_color_from_target~.r")

	file_name <- paste0(pre_fix, "x", in_title_, grab0_, "____.pdf")
	pdf(file = file_name, height = 4)
	par(mfrow = c(1,2), cex = 0.5)
	plot(my_tsne_pc_cpm_data1_[,2], my_tsne_pc_cpm_data1_[,1], pch = 19, col = cols.by.raw.counts)
	title(sub = paste0(dim(genes_)[1], " genes| ave_expression"))

	plot(my_tsne_pc_cpm_data1_[,2], my_tsne_pc_cpm_data1_[,1], pch = 19, col = cols.by.matrix_out)
	legend("bottomright", legend = legend.by.matrix_out[,3], text.col = legend.by.matrix_out[,2], bty = "n")
	title_ <- in_title_ 
	sub_title_ <- paste0(" extend_", grab1_[grab0_], "sub", dim(matrix_out)[1])
	title(title_)
	title(sub = sub_title_)
	dev.off()
	print(paste0("file as >>>>", file_name))
	}
}
