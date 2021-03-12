#Rachel Yuan Nong Mikkelsen 2021-March-12, Uppsala
#this file is modified from find_two_cells_genes2.r for 1 cell.

#Rachel Yuan Nong 2020-OCT-28 | find_two_cells_genes2.r
#

point_1_and_2 <- matrix(0, ncol = 2, nrow = 2)
rownames(point_1_and_2) <- c("cell-1", "cell-2")
colnames(point_1_and_2) <- c("x-coordinate", "y-coordinate")

my_tsne_axies <- read.csv("my_tsne.csv")
my_tsne_axies <- my_tsne_axies[,2:dim(my_tsne_axies)[2]]

#locate one cell
print("start locate one cell:")
print(" use R locator ")
s <- locator(n = 1)
#print(s)

s <- as.data.frame(s)
s <- t(s)
s <- as.data.frame(s)
s[,2] <- round(s[,1])

print(s)
print("REQUIRE: x_min_value and x_max_value")
print("| x_min_value")
x_min_value <- readline() 
x_min_value <- as.numeric(x_min_value)
x_max_value <- x_min_value + 2
print(paste0("| x_min_value ", x_min_value))
print(paste0("| x_max_value ", x_max_value))

print("REQUIRE: y_min_value and y_max_value")
print("| y_min_value")
y_min_value <- readline()
y_min_value <- as.numeric(y_min_value)
y_max_value <- y_min_value + 2
print(paste0("| y_min_value ", y_min_value))
print(paste0("| y_max_value ", y_max_value))

point1_coordinates <- my_tsne_axies[my_tsne_axies[,2] > x_min_value & my_tsne_axies[,2] < x_max_value & my_tsne_axies[,1] > y_min_value & my_tsne_axies[,1] < y_max_value,]
point1_coordinates <- as.data.frame(point1_coordinates)

print(point1_coordinates)
print("assign a point to use (left y - right x)")
print("| y_coordinate")
point_1_and_2[1,2] <- readline()
print("| x_coordinate")
point_1_and_2[1,1] <- readline()

points(x = point_1_and_2[1,1], y = point_1_and_2[1,2], col = "black")

#repeat above and locate another cell
print("...now locate another cell")
print(" use R locator ")
s <- locator(n = 1)
s <- as.data.frame(s)
s <- t(s)
s <- as.data.frame(s)
s[,2] <- round(s[,1])

print(s)
print("REQUIRE: x_min_value and x_max_value")
print("| x_min_value")
x_min_value <- readline()
x_min_value <- as.numeric(x_min_value)
x_max_value <- x_min_value + 2
print(paste0("| x_min_value ", x_min_value))
print(paste0("| x_max_value ", x_max_value))

print("REQUIRE: y_min_value and y_max_value")
print("| y_min_value")
y_min_value <- readline()
y_min_value <- as.numeric(y_min_value)
y_max_value <- y_min_value + 2
print(paste0("| y_min_value ", y_min_value))
print(paste0("| y_max_value ", y_max_value))

point1_coordinates <- my_tsne_axies[my_tsne_axies[,2] > x_min_value & my_tsne_axies[,2] < x_max_value & my_tsne_axies[,1] > y_min_value & my_tsne_axies[,1] < y_max_value,]
point1_coordinates <- as.data.frame(point1_coordinates)

print(point1_coordinates)
print("assign a point to use (left y - right x)")
print("| y_coordinate")
point_1_and_2[2,2] <- readline()
print("| x_coordinate")
point_1_and_2[2,1] <- readline()

points(x = point_1_and_2[2,1], y = point_1_and_2[2,2], col = "black")

write.csv(point_1_and_2, file = "selected_cell_coordinates.csv")
point_1_and_2 <- read.csv("selected_cell_coordinates.csv", header = TRUE)
rownames(point_1_and_2) <- point_1_and_2[,1]
point_1_and_2 <- point_1_and_2[,2:dim(point_1_and_2)[2]]

print(point_1_and_2)

text(x = point_1_and_2[1,1], y = point_1_and_2[1,2], label = "cell-1", col = "pink")
text(x = point_1_and_2[2,1], y = point_1_and_2[2,2], label = "cell-2", col = "green")

print("assign round number for cell1's coordinates:")
round_number <- readline()
round_number <- as.numeric(round_number)
cell1 <- which(round(my_tsne_axies[,2], round_number) == point_1_and_2[1,1])
print("assign round number for cell2's coordinates:")
round_number <- readline()
round_number <- as.numeric(round_number)
cell2 <- which(round(my_tsne_axies[,2], round_number) == point_1_and_2[2,1])

selected_features <- cbind(principle_components_axies[,cell1], principle_components_axies[,cell2])
selected_features <- as.matrix(selected_features)

pre_fix_color <- matrix("lightblue", nrow = 1000, ncol = 1)
pca_axies <- principle_components_axies[order(principle_components_axies[,pc_column], decreasing = TRUE),]
cell1 <- rownames(principle_components_axies)[cell1]
pre_fix_color[which(rownames(pca_axies) == cell1),1] <- c("pink")
plot(pca_axies[, pc_column], pch = 19, col = pre_fix_color)
points(which(pre_fix_color == "pink") + 50, pca_axies[which(pre_fix_color == "pink"), pc_column], pch = 19, col = "pink")
text(which(pre_fix_color == "pink") + 50, pca_axies[which(pre_fix_color == "pink"), pc_column], pch = 19, col = "pink", label = "cell1")
