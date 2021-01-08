#Rachel Yuan Nong 2021-01-06
#ccle_expression <- read.csv("CCLE_Expression_Entrez_2012-09-29 copy.csv", sep = ";")

print("feature cell line:")
cell_line_keyword <- readline()
cell_line_keyword <- as.character(cell_line_keyword)


print(ccle_full[grep(cell_line_keyword, ccle_full)])
print("feature full name:")
feature_cell_line <- readline()
feature_cell_line <- as.character(feature_cell_line)

plot(my_tsne_pc_ccle_expression[,2], my_tsne_pc_ccle_expression[,1], col = cols.by.tissue_feature)
points(my_tsne_pc_ccle_expression[ccle_full == feature_cell_line, 2], my_tsne_pc_ccle_expression[ccle_full == feature_cell_line, 1], pch = 19, col = "grey")
text(my_tsne_pc_ccle_expression[a == feature_cell_line, 2], my_tsne_pc_ccle_expression[a == feature_cell_line, 1], label = feature_cell_line, col = "blue")
title(main = "Broad | CCLE_expression 1037 cell lines | 18900 genes") 
