#Rachel Yuan Nong Mikkelsen 2022-JAN-15

return_coordinate2 <- function(){
	print("search by first 6bases")
	piece1 <- read.delim("piece1.txt")
	piece1 <- as.character(piece1[1,1])
	print("to_find_coordinates_for:")
	find_coordinate <- readline()

	partition1 <- 6
	entire_ <- nchar(piece1)

	a_round1 <- round(entire_/partition1)
		if(a_round1*partition1 >= entire_){
         	a_round1 <- a_round1
         }
		if(a_round1*partition1 < entire_){
         	a_round1 <- around1 + 1
         }
 
	partition_table <- matrix(0, ncol = partition1, nrow = a_round1)
	for(i in 1:a_round1){
        	ki_0 <- (i-1)*partition1
        	ki_1 <- ki_0 + partition1
        	ki <- sapply(substr(piece1, ki_0 + 1, ki_1), function(x){x[1]})
        for(k in 1:dim(partition_table)[2]){
                partition_table[i, k] <- sapply(strsplit(as.character(ki), ""), function(x){x[k]}) 
         	}
         }
 
	find_co <- matrix(0, nrow = 1, ncol = partition1)
	for(i in 1:dim(find_co)[2]){
        	find_co[1,i] <- sapply(strsplit(as.character(find_coordinate), ""), function(x){x[i]})
         }
 
	compare_table <- matrix(0, ncol = partition1, nrow = a_round1)
	for(i in 1:a_round1){
        	for(k in 1:partition1){
			if(partition_table[i, k] == find_co[1,k]){
                        	compare_table[i, k] <- -1
                	}
        	}
	}

	#score_compare_table <- rowSums(compare_table)

	score_compare_table_0 <- matrix(0, ncol = partition1, nrow = a_round1)
	for(i in 1:a_round1){
		for(k in 1:(partition1-1)){
			if(compare_table[i, k] == -1 & compare_table[i, (k+1)]){
				score_compare_table_0[i,k] <- -10
			}
		}
	}

	score_compare_table <- rowSums(score_compare_table_0)

	found_start <- which(score_compare_table == min(score_compare_table[1:(a_round1-1)]))

	b <- sapply(substr(piece1, (found_start-1)*partition1+1, found_start*partition1), function(x){x[1]})
	a <- sapply(substr(find_coordinate, 1, partition1), function(x){x[1]})
	print(paste0("found start at:", (found_start-1)*partition1, ":", b))
	if(a == b){
		print(paste0("for first 6bases of [input]", find_coordinate))
	}
	if(a != b){
		print(paste0(b, " != [input]", a))
		print("use another scripts ]")
	}
}



