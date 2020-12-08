#Rachel Yuan Nong 2020-OCT-28
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

selected_features <- cbind(matrix_in[,cell1], matrix_in[,cell2])
selected_features <- as.matrix(selected_features)
colnames(selected_features) <- c(colnames(matrix_in)[cell1], colnames(matrix_in)[cell2])
selected_features <- as.data.frame(selected_features)
dim(selected_features)

selected_features[,3] <- selected_features[,2] - selected_features[,1]

a <- selected_features[order(selected_features[,3], decreasing = TRUE),]
a <- a[order(a[,3], decreasing = FALSE),]
c <- a[1:10,]
write.csv(c, file = "new_slot_region_specific_genes.csv")
c <- read.csv("new_slot_region_specific_genes.csv")

a <- a[order(a[,3], decreasing = TRUE),]
write.csv(a[1:10,], file = "new_slot_region_specific_genes.csv")
c <- cbind(c, read.csv("new_slot_region_specific_genes.csv"))

a <- selected_features[selected_features[,1] > 0 & selected_features[,2] > 0,]
a <- a[order(a[,3], decreasing = FALSE),]
write.csv(a[1:10,], file = "new_slot_region_specific_genes.csv")
c <- cbind(c, read.csv("new_slot_region_specific_genes.csv"))

a <- a[order(a[,3], decreasing = TRUE),]
write.csv(a[1:10,], file = "new_slot_region_specific_genes.csv")
c <- cbind(c, read.csv("new_slot_region_specific_genes.csv"))

write.csv(c, file = "new_slot_region_specific_genes.csv")

c <- 0
a <- read.csv("new_slot_region_specific_genes.csv") 

print("name file:")
file_name_1 <- readline()

k <- c(2,6,10,14)
for(i in 1:4){
file_name <- paste0(file_name_1, "_", i, ".csv")
n <- k[i]
write.csv(a[,n], file = file_name)
}

print(paste0(" a | new_slot_region_specific_genes.csv | ", file_name, "_[1-4].csv "))

