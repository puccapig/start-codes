#Rachel Yuan Nong 2020-SEPT-23 Uppsala Sweden
#This is to look at a part of chromosome's ATAC-seq expression from 10xgenomics
#2020-09-25 copied and modified to work in this file, using abbrevations. 
#require matrix_in, labels.experiments
#2020-10-16 modified to add option usage of ensg-XXXXXXXXXXX and abbreviation.

print("Otherwise assign new genes.lookup :  more_genes.csv")
print("Chromosome number: chr")
chr_number <- readline()
chr_number <- paste0("chr", chr_number)
print("Start position:")
start_position <- readline()
start_position <- as.numeric(start_position)
print("End position:")
end_position <- readline()
end_position <- as.numeric(end_position)

print("region to look at:")
region_to_look_at <- paste0(chr_number, ":", as.character(start_position), "-", as.character(end_position))
print(region_to_look_at)

ref_human <- read.csv("", header = FALSE)

region_to_look_at1 <- ref_human[ref_human[,4] == chr_number, ]
write.csv(region_to_look_at1, file = "region_to_look_at1.csv")
region_to_look_at1 <- read.csv("region_to_look_at1.csv")
region_to_look_at1 <- region_to_look_at1[,2:dim(region_to_look_at1)[2]]
region_to_look_at1 <- region_to_look_at1[region_to_look_at1[,5] >= start_position & region_to_look_at1[,5] <= end_position, ]

more_genes <- region_to_look_at1[match(rownames(matrix_in), region_to_look_at1[,1], nomatch = 0), ]
more_genes <- more_genes[order(more_genes[,5]),]
write.csv(more_genes, file = "more_genes_en.csv")

print("gene name format: ensg[1] abbrev[2]")
gene_name_format <- readline()
gene_name_format <- as.numeric(gene_name_format)
more_genes <- region_to_look_at1[match(rownames(matrix_in), region_to_look_at1[,gene_name_format], nomatch = 0),]
more_genes <- more_genes[order(more_genes[,5]),2]
more_genes <- as.character(more_genes)
write.csv(more_genes, file = "more_genes.csv")

#par(cex = 0.5)
#boxplot(split(matrix_in[more_genes,], as.factor(labels.experiments)), col = rainbow(30))
