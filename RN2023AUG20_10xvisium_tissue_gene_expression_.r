#Rachel Yuan Nong Mikkelsen 2023AUG20
#plot through genes tissue expression -visium dataset

pre_fix <- c()
o <- sample(4992, 1000)
matrix_in <- cpm_mat[,o]
matrix_in <- matrix_in[which(rowSums(matrix_in) != 0),]
tissue_position_ <- read.csv("spatial/tissue_positions_list.csv", header = FALSE)
tissue_position_1k <- tissue_position_[match(colnames(matrix_in), tissue_position_[,1], nomatch = 0),]

source(".../ref_human_table.r")
genes_ <- ref_human[match(rownames(matrix_in), ref_human[,1], nomatch = 0),]
genes_[,5] <- as.numeric(as.character(genes_[,5]))
genes_[,6] <- as.numeric(as.character(genes_[,6]))

chromosome_ <- c(1:22)
chromosome_ <- c(chromosome_, "X", "Y")
chromosome_ <- paste0("chr", chromosome_)

for(key_chr_ in 1:24){
	chr_ <- chromosome_[key_chr_]
	expression_ <- genes_[which(genes_[,4] == chr_),]
	expression_ <- expression_[order(expression_[,5]),]
	
	part_ <- round(dim(expression_)[1]/63)
	if(part_*63 < dim(expression_)[1]){
		part_ <- part_ + 1
	}

	for(key_part_ in 1:part_){
		start_ <- (key_part_ - 1)*63
		shift_ <- start_ + 63
		if(shift_ > dim(expression_)[1]){
			shift_ <- dim(expression_)[1]
		}
		shift_ <- shift_ - start_
		
		file_name <- paste0(pre_fix, "__", chr_, "__part", key_part_, "___.pdf")
		pdf(file = file_name)
		par(mfrow = c(8,8), cex = 0.1)
		
		for(ii in 1:shift_){
			key_gene_ <- start_ + ii
			sub_ <- as.character(expression_[key_gene_,2])
			key_gene_ <- as.character(expression_[key_gene_,1])

			target <- my.pal[1:length(key_gene_)]
                	values <- matrix_in[match(key_gene_, rownames(matrix_in), nomatch = 0),]
                	cols.by.raw.counts <- col.from.target(target, values)
		
			par(cex = 0.1)
			plot(tissue_position_1k[,6], -tissue_position_1k[,5], pch = 19, col = cols.by.raw.counts)
			par(cex = 0.3)
			title(main = key_gene_)
			par(cex = 0.5)
			title(sub = sub_)
		}
		dev.off()
	}
}

