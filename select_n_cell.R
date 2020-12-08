# Rachel Yuan Nong 2019-Aug-28 Uppsala Sweden
# This is written to select a set of single cell to run R/prcomp in macair... (originally from tumordotR scripts for pancreatic cancer cells.)

print("total number of cells:"); total_number_of_cells <- readline(); total_number_of_cells <- as.integer(total_number_of_cells)
print("select cells n.per.group: "); n.per.group <- readline(); n.per.group <- as.integer(n.per.group)

df <- matrix(0, nrow = 1, ncol = total_number_of_cells)
my.factor <- as.factor(df)
index.vec <- 1:length(df)
o <- unlist(lapply(split(index.vec, my.factor), function(x){sample(x, n.per.group)}))

print("dim(as.matrix(o))")
print(dim(as.matrix(o)))

#return (o)


