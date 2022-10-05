#Rachel Yuan Nong Mikkelsen 2022OCT04|happy cinnamonbunday!!!

a <- c(1:10000)
b <- c(1:10)
col.ab <- rainbow(10)

source("~select_n_cell.R")#500
a <- a[o]

dev.new()
par(cex = 0.5)
for(i in 1:10){
	par(new = TRUE)
	plot(a, log2(b[i]/a), xlab = "", ylab = "", ylim = c(-20, 0), xlim = c(1, 10000), col = col.ab[i])
	}
par(cex = 2)
title(sub = "y ~ b/x | b in 1:10")

legend.ab <- paste0("y ~ ", b, "/x")
legend("topright", legend = legend.ab, col.text = col.ab, bty = "n")

