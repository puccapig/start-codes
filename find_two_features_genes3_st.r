#Rachel Yuan Nong 2020-OCT-28
#2020-nov-02 copied and modified to work on 10x_visium_file

point_1_and_2 <- matrix(0, ncol = 2, nrow = 2)
rownames(point_1_and_2) <- c("feature-1", "feature-2")
colnames(point_1_and_2) <- c("x-coordinate", "y-coordinate")

tissue_position <- read.csv("spatial/tissue_positions_list.csv", header = FALSE)

#locate one feature
print("start locate one feature:")
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
x_max_value <- x_min_value + 200
print(paste0("| x_min_value ", x_min_value))
print(paste0("| x_max_value ", x_max_value))

print("REQUIRE: (-y)_min_value and (-y)_max_value")
print("| (-y)_min_value")
y_min_value <- readline()
y_min_value <- as.numeric(y_min_value)
y_max_value <- y_min_value + 200
print(paste0("| y_min_value ", y_min_value))
print(paste0("| y_max_value ", y_max_value))

point1_coordinates <- tissue_position[tissue_position[,6] > x_min_value & tissue_position[,6] < x_max_value & tissue_position[,5] > y_min_value & tissue_position[,5] < y_max_value,]
point1_coordinates <- as.data.frame(point1_coordinates)

print(point1_coordinates)
print("assign a point to use (left y - right x)")
print("| y_coordinate")
point_1_and_2[1,2] <- readline()
print("| x_coordinate")
point_1_and_2[1,1] <- readline()

write.csv(point_1_and_2, file = "selected_cell_coordinates.csv")
point_1_and_2 <- read.csv("selected_cell_coordinates.csv", header = TRUE)
rownames(point_1_and_2) <- point_1_and_2[,1]
point_1_and_2 <- point_1_and_2[,2:dim(point_1_and_2)[2]]
points(x = point_1_and_2[1,1], y = (-1) * point_1_and_2[1,2], col = "black")

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
x_max_value <- x_min_value + 200
print(paste0("| x_min_value ", x_min_value))
print(paste0("| x_max_value ", x_max_value))

print("REQUIRE: (-y)_min_value and (-y)_max_value")
print("| (-y)_min_value")
y_min_value <- readline()
y_min_value <- as.numeric(y_min_value)
y_max_value <- y_min_value + 200
print(paste0("| y_min_value ", y_min_value))
print(paste0("| y_max_value ", y_max_value))

point1_coordinates <- tissue_position[tissue_position[,6] > x_min_value & tissue_position[,6] < x_max_value & tissue_position[,5] > y_min_value & tissue_position[,5] < y_max_value,]
point1_coordinates <- as.data.frame(point1_coordinates)

print(point1_coordinates)
print("assign a point to use (left y - right x)")
print("| y_coordinate")
point_1_and_2[2,2] <- readline()
print("| x_coordinate")
point_1_and_2[2,1] <- readline()

write.csv(point_1_and_2, file = "selected_cell_coordinates.csv")
point_1_and_2 <- read.csv("selected_cell_coordinates.csv", header = TRUE)
rownames(point_1_and_2) <- point_1_and_2[,1]
point_1_and_2 <- point_1_and_2[,2:dim(point_1_and_2)[2]]

points(x = point_1_and_2[2,1], y = (-1) * point_1_and_2[2,2], col = "black")

write.csv(point_1_and_2, file = "selected_cell_coordinates.csv")
point_1_and_2 <- read.csv("selected_cell_coordinates.csv", header = TRUE)
rownames(point_1_and_2) <- point_1_and_2[,1]
point_1_and_2 <- point_1_and_2[,2:dim(point_1_and_2)[2]]

print(point_1_and_2)

text(x = point_1_and_2[1,1], y = (-1) * point_1_and_2[1,2], label = "feature-1", col = "pink")
text(x = point_1_and_2[2,1], y = (-1) * point_1_and_2[2,2], label = "feature-2", col = "green")

print("extract tissue_position and expression profile of selected_features")
feature_1 <- which(tissue_position[,5] == point_1_and_2[1,2] & tissue_position[,6] == point_1_and_2[1,1])
feature_2 <- which(tissue_position[,5] == point_1_and_2[2,2] & tissue_position[,6] == point_1_and_2[2,1])

a <- tissue_position[feature_1,]
a[2,] <- tissue_position[feature_2,]
selected_features <- cpm_mat[,a[,1]]
selected_features <- as.data.frame(selected_features)
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

