#Rachel Yuan Nong 2019-July-19 Uppsala Sweden
#This is written for looking up interested genes' location in human genome. ..
#2020-April-01 copied and modified to work in this file.
#2020-April-09 copied and modified to work in this file.
#2020-April-21 copied and modified to work in this file.
#2020-SEPT-16 copied and modified to generatelist with +11 and -11 neighbor-genes
#2020-SEPT-23 copied and modified to work in this file.
#2020-SEPT-25 copied and modified to work in this file.
#2020-OCT-10 update file to assign to ref_human

print("Otherwise assign new genes.lookup :  more_genes.csv")
moregenes <- readline()
moregenes <- as.character(moregenes)
more_genes <- read.csv(moregenes)

print(dim(more_genes))
print(more_genes[1,2])
print("Option: ensemble [1] abbrevation [2]")
name_opt <- readline()
name_opt <- as.integer(name_opt)

ref_human <- read.csv("")
ref_human <- ref_human[,2:dim(ref_human)[2]]

ensg_gene <- ref_human[match(more_genes[,2], ref_human[,name_opt], nomatch = 0), 1]
ensg_gene <- as.character(ensg_gene)

Abbrev_gene <- ref_human[match(ensg_gene, ref_human[,1], nomatch = 0), 2]
Abbrev_gene <- as.character(Abbrev_gene)

hgTable <- ref_human
genes.lookup.1 <- hgTable[match(Abbrev_gene, hgTable[,2], nomatch = 0),]

hgTable_unique <- unique(as.character(hgTable[,2]))
hgTable_unique <- as.character(hgTable_unique)
hgTable_unique <- hgTable[match(hgTable_unique, hgTable[,2], nomatch = 0),]

for(i in 1:dim(genes.lookup.1)[1]){
	gene_name1 <- genes.lookup.1[i,2]
	gene_name1 <- as.character(gene_name1)
	#rownumber <- which(hgTable_unique[,2] == gene_name1)
	chr_number <- hgTable_unique[hgTable_unique[,2] == gene_name1, 3]
	
	region_to_look_at1 <- ref_human[ref_human[,3] == chr_number, ]
	write.csv(region_to_look_at1, file = "region_to_look_at1.csv")
	region_to_look_at1 <- read.csv("region_to_look_at1.csv")
	region_to_look_at1 <- region_to_look_at1[,2:dim(region_to_look_at1)[2]]
	region_to_look_at1 <- region_to_look_at1[order(region_to_look_at1[,5]),]
	
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


#return(user_input)

ref_human <- 0
hgTable <- 0
hgTable_unique <- 0
