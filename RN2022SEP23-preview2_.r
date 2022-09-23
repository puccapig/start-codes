#Rachel Yuan Nong Mikkelsen 2022SEPTEMBER19

data1 <- read.csv("~Cell-2015-Cacchiarelli-PIIS009286741500700X-mmc2-All-Gene-expression-FPKM.csv", dec = ",", sep = ";", header = TRUE)
data1_expression <- data1[,4:13]
rownames(data1_expression) <- sapply(strsplit(as.character(data1[,1]), "[.]"), function(x){x[1]})
source("~ref_human_table.r")
#genes1 <- rownames(ave_table)
genes1 <- rownames(cpm_tumor1_1k)
genes1 <- ref_human[match(genes1, ref_human[,1], nomatch = 0),]
data1_expression_1 <- data1_expression[match(genes1[,1], rownames(data1_expression), nomatch = 0),]
genes1 <- genes1[match(rownames(data1_expression_1), genes1[,1], nomatch = 0),]

#my_tsne_pc_matrix_out_k3 <- tsne(pc_matrix_out_a[,1:10], k = 3)
#cutree_tsne_matrix_out <- cutree(hclust(dist(my_tsne_pc_matrix_out_k3)), k = 24)
#factor_name <- cutree_tsne_matrix_out
#factor_name <- paste0(factor_name, "_")
#source("~assign_legend_text_and_color.r")
matrix_in <- matrix_in0

repro_axies_ <- matrix(0, ncol = 2, nrow = 1)
repro_axies_ <- as.data.frame(repro_axies_)
colnames(repro_axies_) <- c("cluster_", "repro_axies_")
par(mfrow = c(1,3), cex = 0.5)
for(fat_ki in 1:24){
	matrix_out_feature0 <- matrix_in[,cutree_tsne_matrix_out == fat_ki]
	matrix_out_feature_1 <- 2^matrix_out_feature0 - 1

	data1_expression_1 <- data1_expression[match(genes1[,1], rownames(data1_expression), nomatch = 0),]
	ave_matrix_out <- matrix_out_feature_1
	ave_matrix_out <- rowSums(ave_matrix_out)/dim(matrix_out_feature_1)[2]
	ave_matrix_out <- as.data.frame(ave_matrix_out)
	rownames(ave_matrix_out) <- rownames(matrix_in)

	data_expression_combined <- cbind(data1_expression_1, ave_matrix_out[match(genes1[,1], rownames(ave_matrix_out), nomatch = 0),])
	colnames(data_expression_combined)[11] <- ki
	pc_data_expression_combined <- prcomp(log10(data_expression_combined + 1))
	pc_data_expression_combined_a <- pc_data_expression_combined[[2]]

	repro_axies_[fat_ki,1] <- fat_ki
        repro_axies_[fat_ki,2] <- pc_data_expression_combined_a[11,3]
	}

repro_axies_[,3] <- abs(repro_axies_[,2])
a <- repro_axies_[order(repro_axies_[,2]),]
plot(a[,2], col = "grey", ylab = paste0("PC3 ", ki))
text(a[,2], label = a[,1], col = c("red"))

a <- repro_axies_[order(repro_axies_[,3]),]
plot(a[,3], col = "grey", ylab = paste0("abs PC3 ", ki))
text(a[,3], label = a[,1], col = c("purple"))

title(sub = paste0("BioRxiv2014-SCRBseq | ", ki))

plot(my_tsne_pc_matrix_out[,2], my_tsne_pc_matrix_out[,1], col = "white")
text(my_tsne_pc_matrix_out[,2], my_tsne_pc_matrix_out[,1], col = cols.by.input, label = cutree_tsne_matrix_out)









