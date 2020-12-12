#Rachel Yuan Nong 2020-09-29
#to look at downloaded single cell cDNA bed file from 10xgenomics
#by chromosome number, start_position, end_position
#2020-OCT-05 modified to save plots in pdf files.

#fragment_bed <- read.delim("breast_tissue_aggr_10k_node_unmerged_cnv_calls.bed")

view_sc_cnv <- function(chr_number, region_key, start_position, end_position, hot_spot){

	region_to_look_at <- fragment_bed[fragment_bed[,1] == chr_number,]
	region_to_look_at1 <- region_to_look_at[region_to_look_at[,2] >= start_position & region_to_look_at[,3] <= end_position,]
	
        hg19_table <- read.csv("", header = TRUE)
	

        ref_human <- read.csv("", header = TRUE)
	ref_human <- ref_human[,2:dim(ref_human)[2]]
	
	hot_spot_name <- ref_human[ref_human[,2] == hot_spot,2]
        hot_spot_name <- as.character(hot_spot_name)
	hot_spot_old_ref <- hg19_table[hg19_table[,3] == hot_spot_name, 5]
        
        print("hot_spot_old_ref")
        print(hot_spot_old_ref)
        
        print("gene name2")
        print(hot_spot_name)
        
	dev.new(); par(mfrow = c(2,1), cex = 0.2)
        plot(region_to_look_at1[,2], region_to_look_at1[,5], pch = 19, cex = 0.5, col = c("blue"))
        title(sub = paste0("chr", as.character(unique(region_to_look_at[,1])), "_copy_number | 10xgenomics_breast_tissue_aggr"))
        abline(v = hot_spot_old_ref, col = "grey")
        text(hot_spot_old_ref, max(region_to_look_at1[,5])/2, hot_spot_name, col = "red")

        event_confidence_inverse = 1 -(255-region_to_look_at1[,6])/255

        plot(region_to_look_at1[,2], event_confidence_inverse, pch = 19, cex = 0.5, col = c("blue"))
        title(sub = paste0("chr", as.character(unique(region_to_look_at[,1])), "_copy_number | 10xgenomics_breast_tissue_aggr |event_confidence"), cex.sub = 0.5)


	filename <- paste0(pre_fix, "_", region_key, "-", chr_number, "-", hot_spot_name, "_.pdf")
        pdf(filename)
	par(mfrow = c(2,1), cex = 0.2)
        plot(region_to_look_at1[,2], region_to_look_at1[,5], pch = 19, cex = 0.5, col = c("blue"))
      title(sub = paste0("chr", as.character(unique(region_to_look_at[,1])), "_copy_number | 10xgenomics_breast_tissue_aggr"))
	abline(v = hot_spot_old_ref, col = "grey")
        text(hot_spot_old_ref, max(region_to_look_at1[,5])/2, hot_spot_name, col = "red")

        plot(region_to_look_at1[,2], event_confidence_inverse, pch = 19, cex = 0.5, col = c("blue"))
        title(sub = paste0("chr", as.character(unique(region_to_look_at[,1])), "_copy_number | 10xgenomics_breast_tissue_aggr |event_confidence"), cex.sub = 0.5)

	dev.off()
}


