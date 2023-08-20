#Rachel Yuan Nong Mikkelsen 2022OCT25

library(tsne)
pre_fix <- c("RN2022OCT25-visium_-1")

left0_ <- c(1, 1001, 2001, 2001, 2001, 4001, 4001)
right0_ <- c(2000, 2000, 4000, 4000, 4000, 4992, 4992)
left1_ <- c(1, 2001, 1148, 2497, 3745, 1, 2497)
right1_ <- c(2000, 4992, 2496, 3744, 4992, 2496, 4992)

par(cex = 0.5)
plot(-tissue_position[,6], -tissue_position[,5], pch = 19, col = "grey", xlab = pre_fix, cex = 0.7)

for(ii in 1:7){
	iik <- ii
	rank_tissue_position0 <- tissue_position[order(tissue_position[,6]),]
	cpm_mat_left1k <- cpm_mat[,match(rank_tissue_position0[left0_[iik]:right0_[iik],1], colnames(cpm_mat), nomatch = 0)]
	rank_tissue_position1 <- rank_tissue_position0[order(rank_tissue_position0[,5]),]
	cpm_mat_left1k <- cpm_mat_left1k[,match(rank_tissue_position1[left1_[iik]:right1_[iik],1], colnames(cpm_mat_left1k), nomatch = 0)]
	
	sub_tissue_analysis_left1k <- tissue_position[match(colnames(cpm_mat_left1k), tissue_position[,1], nomatch = 0),]
	
	par(cex = 1)	
	points(-sub_tissue_analysis_left1k[,6], -sub_tissue_analysis_left1k[,5], pch = 19, col = rainbow(iik))
	}


