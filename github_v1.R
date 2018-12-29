rm(list = ls())
install.packages("xlsx")
# Read data
getwd()
setwd("/data")
all_data<-read.csv("e_com_data.csv")
View(all_data)
Custom_data<-all_data[, c(4,6)]
View(Custom_data)
# correlation matrix of custom data
cor_results<-cor(Custom_data)
# structure of data
str(cor_results)
capture.output(cor_results, file="Correlation.xls")
Custom_data_norm<-scale(Custom_data)
View(Custom_data_norm)
# K-means clustering
wss = (nrow(Custom_data_norm) - 1) * sum(apply(Custom_data_norm, 2, var))
for (i in 2:15) 
  wss[i] = sum(kmeans(Custom_data_norm, centers = i, iter.max = 1000)$withinss)
plot(wss)
bss = sum(kmeans(Custom_data_norm, centers = 1, iter.max = 1000)$betweenss)
for (i in 2:15) 
  bss[i] = sum(kmeans(Custom_data_norm, centers = i, iter.max = 1000)$betweenss)
plot(bss)
cluster_results<-kmeans(Custom_data_norm, centers = 8, iter.max = 1000)
str(cluster_results)
cluster_results$size
cluster_results$centers
cluster_results
all_data<-data.frame(all_data, cluster_results$cluster)
View(all_data)
write.table(all_data, file = "ClusteringResultsData.csv", sep = "%", row.names = FALSE)
capture.output(cluster_results, file = "ClusteringResultsFinal.xls")
 