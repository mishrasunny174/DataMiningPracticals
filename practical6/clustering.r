install.packages("dbscan")

library(dbscan)
View(iris)
#kmeans
cl <- kmeans(iris[,-5], 3)
plot(iris[,-5], col = cl$cluster)
points(cl$centers, col = 1:3, pch = 8)


#heirarchical
clusters <- hclust(dist(iris[, -5]))
plot(clusters)

#DBScan
cl <- dbscan(iris[,-5], eps = .5, minPts = 5)
plot(iris[,-5], col = cl$cluster)
