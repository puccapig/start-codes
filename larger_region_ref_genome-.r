#Rachel Yuan Nong 2020-OCT-02 Uppsala Sweden
#set a chromosome region by a given gene name
#2020-10-22 copied and modified.

#length = 20MB

#larger_region <- function(more_genes){
	print("INPUT file [more_genes].csv")
	more_genes <- readline()
	more_genes <- read.csv(more_genes)

	ref_human <- read.csv("", header = TRUE)
	ref_human <- ref_human[, 2:dim(ref_human)[2]]
		
	more_genes <- ref_human[match(more_genes[,2], ref_human[,2], nomatch = 0),]
	write.csv(more_genes, file = "more_genes.csv")
	more_genes <- read.csv("more_genes.csv")
	more_genes <- more_genes[,2:dim(more_genes)[2]]
	
	more_genes[,7] <- (round(more_genes[,4]/1000000)-10)*1000000
	more_genes[,8] <- (round(more_genes[,4]/1000000)+10)*1000000
	region_larger <- matrix(0, ncol = 5, nrow = dim(more_genes)[1])
	rownames(region_larger) <- more_genes[,1]
	colnames(region_larger) <- c("gene_name2", "chr_number", "txStart", "start_position", "end_position")
	region_larger[,1] <- as.character(more_genes[,2])
	region_larger[,2] <- more_genes[,3]
	region_larger[,3] <- more_genes[,4]
	region_larger[,4] <- more_genes[,7]
	region_larger[,5] <- more_genes[,8]

	write.csv(region_larger, file = "larger_region.csv")
	
	more_genes <- read.csv("larger_region.csv")
	print("RETURN more_genes | larger_region.csv")
#}



