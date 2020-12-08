#Rachel Yuan Nong 2019-July-19 Uppsala Sweden
#This is written for looking up interested genes' location in human genome...
#2020-April-01 copied and modified to work in this file.
#2020-April-09 copied and modified to work in this file.
#2020-April-21 copied and modified to work in this file.
#2020-SEPT-16 copied and modified to generatelist with +11 and -11 neighbor-genes
#2020-SEPT-23 copied and modified to work in this file.
#2020-SEPT-25 copied and modified to work in this file.
#2020-OCT-06 copied and modified to work on mm10 file.

print("Otherwise assign new genes.lookup :  more_genes.csv")
moregenes <- readline()
moregenes <- as.character(moregenes)
more_genes <- read.csv(moregenes)

print(dim(more_genes))
print(more_genes[1,2])
print("Option: ensemble [1] abbrevation [2]")
name_opt <- readline()
name_opt <- as.integer(name_opt)

ref_mouse <- read.csv("", header = TRUE)
ref_mouse <- ref_mouse[, 2:dim(ref_mouse)[2]]

Ensmusg_gene <- ref_mouse[match(more_genes[,2], ref_mouse[,name_opt], nomatch = 0), 1]
Ensmusg_gene <- as.character(Ensmusg_gene)

Abbrev_gene <- ref_mouse[match(Ensmusg_gene, ref_mouse[,1], nomatch = 0), 2]
Abbrev_gene <- as.character(Abbrev_gene)

mmTable <- ref_mouse
genes.lookup.1 <- mmTable[match(Abbrev_gene, mmTable[,2], nomatch = 0),]

mmTable_unique <- unique(as.character(mmTable[,2]))
mmTable_unique <- as.character(mmTable_unique)
mmTable_unique <- mmTable[match(mmTable_unique, mmTable[,2], nomatch = 0),]

for(i in 1:dim(genes.lookup.1)[1]){
	gene_name1 <- genes.lookup.1[i,2]
	gene_name1 <- as.character(gene_name1)
	#rownumber <- which(hgTable_unique[,2] == gene_name1)
	chr_number <- mmTable_unique[mmTable_unique[,2] == gene_name1, 3]
	
	region_to_look_at1 <- ref_mouse[ref_mouse[,3] == chr_number, ]
	write.csv(region_to_look_at1, file = "region_to_look_at1.csv")
	region_to_look_at1 <- read.csv("region_to_look_at1.csv")
	region_to_look_at1 <- region_to_look_at1[,2:dim(region_to_look_at1)[2]]
	region_to_look_at1 <- region_to_look_at1[order(region_to_look_at1[,4]),]
	
	rownumber <- which(region_to_look_at1[,2] == gene_name1)
	start_row <- rownumber - 11
	if(start_row <= 0){
		start_row <- 1
	}else{
		start_row <- start_row
	}
	end_row <- rownumber + 11
	if(end_row >= dim(region_to_look_at1)[1]){
		end_row <- dim(region_to_look_at1)[1]
	}else{
		end_row <- end_row
	}

	new_slot <- region_to_look_at1[start_row : end_row,2]
	filename <- paste0("new_slot_",i,".csv")
	write.csv(new_slot, file = filename)
}



print(dim(genes.lookup.1))

ref_mouse <- 0
mmTable_unique <- 0
mmTable <- 0


