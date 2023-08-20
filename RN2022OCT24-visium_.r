#Rachel Yuan Nong Mikkelsen 2022OCT24

library(tsne)

#pre_fix <- c("RN2022OCT24-visium_-1")
pre_fix <- c("RN2022OCT24-visium_-2-1")

#rank_tissue_position <- tissue_position[order(tissue_position[,6]),]
rank_tissue_position <- tissue_position[order(tissue_position[,5]),]

left0_ <- c(1, 1001, 2001, 3001, 4001)
right0_ <- c(1000, 2000, 3000, 4000, 4992)

for(ki in 1:5){
	ii <- ki + 4
	cpm_mat_left1k <- cpm_mat[,match(rank_tissue_position[left0_[ii]:right0_[ii],1], colnames(cpm_mat), nomatch = 0)]
	to_print_ <- paste0(">>>>>>> pc_cpm_mat_left1k", "_", left0_[ii], "_", right0_[ii])
	print(to_print_)
	pc_cpm_mat_left1k <- prcomp(cpm_mat_left1k)
	pc_cpm_mat_left1k_a <- pc_cpm_mat_left1k[[2]]
	
	to_print_ <- paste0(">>>>>>> my_tsne_pc_cpm_mat_left1k", "_", left0_[ii], "_", right0_[ii])
	print(to_print_)
	my_tsne_pc_cpm_mat_left1k <- tsne(pc_cpm_mat_left1k_a[,1:10], k = 3)
	cutree_my_tsne_pc_cpm_mat_left1k <- cutree(hclust(dist(my_tsne_pc_cpm_mat_left1k)), k = 12)
	factor_name <- cutree_my_tsne_pc_cpm_mat_left1k
	factor_name <- paste0(factor_name, "_")
	source("~assign_legend_text_and_color.r")
	sub_tissue_analysis_left1k <- tissue_position[match(colnames(cpm_mat_left1k), tissue_position[,1], nomatch = 0),]
	
	file_name <- paste0(pre_fix, "-", ii, "____.pdf")
	rplotname <- paste0(pre_fix, "-", ii)
	pdf(file = file_name)
	par(cex = 0.5)
	plot(-tissue_position[,6], -tissue_position[,5], pch = 19, col = "grey", xlab = rplotname, cex = 0.7)
	par(cex = 1)
	points(-sub_tissue_analysis_left1k[,6], -sub_tissue_analysis_left1k[,5], pch = 19, col = cols.by.input)
	par(cex = 0.5)
	text(-sub_tissue_analysis_left1k[,6], -sub_tissue_analysis_left1k[,5], col = "white", labels = cutree_my_tsne_pc_cpm_mat_left1k)
	dev.off()
	}


