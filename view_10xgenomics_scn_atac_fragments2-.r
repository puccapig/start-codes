#Rachel Yuan Nong 2020-OCT-01
#to view atac_fragments from mat
#2020-okt-05 modified to save plots as pdf file


view_atac_fragments <- function(chr_number, region_key, start_position, end_position, hot_spot){
	region_to_look_at <- atac_fragments[atac_fragments[,2] == chr_number,]
	region_to_look_at1 <- region_to_look_at[region_to_look_at[,4] >= start_position & region_to_look_at[,5] <= end_position,]
	region_to_look_at1 <- region_to_look_at1[order(region_to_look_at1[,4]),]
	par(mfrow = c(3,1), cex = 0.2)
	barplot(region_to_look_at1[,6], col = rainbow(30))
	title(sub = paste0(chr_number, "/total counts"), cex.sub = 1)
	barplot(region_to_look_at1[,7], col = rainbow(30))
	title(sub = "region length", cex.sub = 1)
	barplot(region_to_look_at1[,8], col = rainbow(30))
	title(sub = "counts/nucleotides", cex.sub = 1)

	dev.new(); par(mfrow = c(3,1), cex = 0.2)
	plot(region_to_look_at1[,4], region_to_look_at1[,6], col = "red")
	title(sub = "total counts", cex.sub = 2)
	plot(region_to_look_at1[,4], region_to_look_at1[,7], col = "red")
	title(sub = "region length", cex.sub = 2)
	plot(region_to_look_at1[,4], region_to_look_at1[,8], col = "red")
	title(sub = "counts/nucleotides", cex.sub = 2)
	abline(v = hot_spot, col = "grey")

	hgTable <- read.csv("", header = TRUE)
        ref_human <- read.csv("", header = FALSE)
        hot_spot_name <- ref_human[ref_human[,5] == hot_spot,2]
        hot_spot_name <- as.character(hot_spot_name)
        hot_spot_old_ref <- hgTable[hgTable$name2 == hot_spot_name, 5][1]
        
        print("hot_spot_old_ref")
        print(hot_spot_old_ref)
        
        print("gene name2")
        print(hot_spot_name)
        
        abline(v = hot_spot_old_ref, col = "grey")
        text(hot_spot_old_ref, max(region_to_look_at1[,8])/2, hot_spot_name, col = "blue")

        filename <- paste0(pre_fix, "_", region_key, "-", chr_number, "-", hot_spot_name, "_.pdf")
        pdf(filename)
	#dev.new()
	par(mfrow = c(3,1), cex = 0.2)
        barplot(region_to_look_at1[,6], col = rainbow(30))
        title(sub = paste0(chr_number, "/total counts"), cex.sub = 1)
        barplot(region_to_look_at1[,7], col = rainbow(30))
        title(sub = "region length", cex.sub = 1)
        barplot(region_to_look_at1[,8], col = rainbow(30))
        title(sub = "counts/nucleotides", cex.sub = 1)
	dev.off()

	filename <- paste0(pre_fix, "_", region_key, "-", chr_number, "-", hot_spot_name, "_2_.pdf")
        pdf(filename)
	par(mfrow = c(3,1), cex = 0.2)
        plot(region_to_look_at1[,4], region_to_look_at1[,6], col = "red")
        title(sub = "total counts", cex.sub = 2)
        plot(region_to_look_at1[,4], region_to_look_at1[,7], col = "red")
        title(sub = "region length", cex.sub = 2)
        plot(region_to_look_at1[,4], region_to_look_at1[,8], col = "red")
        title(sub = "counts/nucleotides", cex.sub = 2)
        abline(v = hot_spot, col = "grey")
	text(hot_spot_old_ref, max(region_to_look_at1[,8])/2, hot_spot_name, col = "blue")
	dev.off()

}

