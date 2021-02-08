#Rachel Yuan Nong Mikkelsen 2021-February-08 Uppsala Sweden
#plots to look at 10x ..vdj_b_all_contig_annotations.bed file

print("read BED file:")
data_sc5p_v2_hs_b_bed <- readline()

data_sc5p_v2_hs_b_bed <- read.delim(data_sc5p_v2_hs_b_bed, header = FALSE)

dim(data_sc5p_v2_hs_b_bed)
data_sc5p_v2_hs_b_bed[,5] <- data_sc5p_v2_hs_b_bed[,3] - data_sc5p_v2_hs_b_bed[,2]
data_sc5p_v2_hs_b_bed[,6] <- sapply(strsplit(as.character(data_sc5p_v2_hs_b_bed[,4]), "_"), function(x){x[1]})
data_sc5p_v2_hs_b_bed[,7] <- sapply(substring(as.character(data_sc5p_v2_hs_b_bed[,6]), 3,4), function(x){x[1]})

par(mfrow = c(1,3), cex = 0.5)
hist(data_sc5p_v2_hs_b_bed[,5])
#abline(v = median(data_sc5p_v2_hs_b_bed[,5]), col = "red")
abline(v = mean(data_sc5p_v2_hs_b_bed[,5]), col = "blue")
boxplot(split(data_sc5p_v2_hs_b_bed[,5], as.factor(data_sc5p_v2_hs_b_bed[,7])), col = rainbow(12))

print("read CSV file:")
data_sc5p_v2_hs_b_csv <- readline()
data_sc5p_v2_hs_b_csv <- read.csv(data_sc5p_v2_hs_b_csv)
dim(data_sc5p_v2_hs_b_csv)
data_sc5p_v2_hs_b_csv[,19] <- nchar(as.character(data_sc5p_v2_hs_b_csv$cdr3))
boxplot(split(data_sc5p_v2_hs_b_csv[,19], as.factor(data_sc5p_v2_hs_b_csv$chain)))
title(main = "10x genomics | Single Cell Immune Profiling datasets")
