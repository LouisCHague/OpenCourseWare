# MIT OpenCourseWare; The Analytics Edge
# Louis Hague, 05/11/2024

# Answers within the present script may differ from those on the MIT website
# as the entire dataset was not used due to its large size


# Clustering Assignment
setwd('C:\\Users\\louis\\opencourse\\analytics_edge\\clustering')
# Import data
# Column: Instance of word that has appeared in at least 50 different articles (1,545 words in total).
# Row: Instance of a document
# The set of  words has been trimmed 
# For each document, the variable values are the number of times that word appeared in the document. 

library(readr)
dailykos <- read_csv("dailykos.csv")
dim(dailykos)
# My PC will run out of RAM with that dataset so we will half the rowlen
# dailykos_samp <- head(dailykos,3450)

# Problem 1.1
# Compute the distances (using method=“euclidean”), hclust to build the model (using method=“ward.D2”)
# The reason behind the long computation is because each row must have its euclidean distance compared
# to each other point, there are 3430 documents in the dataset (3450*3450=5.8million comparisons)
dist1 <- dist(dailykos,method="euclidean")
hclust1 <- hclust(dist1,method="ward.D2")
plot(hclust1)

# Problem 1.2-3
# When categorising news articles into groups it might seem as if 7-8 groups would be more
# appropriate. However, to estimate the best number of clusters we could use the 
# 'Elbow method', 'Silhouette Score', we will go off of the dendrogram in this instance
heirGroups <- cutree(hclust1, k=7)
# 1 has the most articles clustered, 7 has the fewest articles clustered
table(heirGroups)

# Problem 1.4
# What is the most commmon word within each cluster
library(dplyr)
Cluster1 <- dailykos %>% filter(heirGroups == 1)
Cluster2 <- dailykos %>% filter(heirGroups == 2)
Cluster3 <- dailykos %>% filter(heirGroups == 3)
Cluster4 <- dailykos %>% filter(heirGroups == 4)
Cluster5 <- dailykos %>% filter(heirGroups == 5)
Cluster6 <- dailykos %>% filter(heirGroups == 6)
Cluster7 <- dailykos %>% filter(heirGroups == 7)
# Common words
Clust1_common_words <- tail(sort(colMeans(Cluster1)))
Clust2_common_words <- tail(sort(colMeans(Cluster2)))
Clust3_common_words <- tail(sort(colMeans(Cluster3)))
Clust4_common_words <- tail(sort(colMeans(Cluster4)))
Clust5_common_words <- tail(sort(colMeans(Cluster5)))
Clust6_common_words <- tail(sort(colMeans(Cluster6)))
Clust7_common_words <- tail(sort(colMeans(Cluster7)))

Clust1_common_words
Clust2_common_words
Clust3_common_words
Clust4_common_words
Clust5_common_words
Clust6_common_words
Clust7_common_words

# Which cluster could best be described as the cluster related to the Iraq war?
# Cluster 6
# Given this information, which cluster best corresponds to the democratic party?
# Cluster 7

# Problem 2.1 
set.seed(1000)
kmen1 <- kmeans(dailykos,centers = 7)
table(kmen1$cluster)
# How many articles are in cluster 3: 300
# Cluster 4 has the most observations
# Cluster 5 has the fewest observations

# Problem 2.2
kmeans1 <- dailykos %>% filter(kmen1$cluster == 1)
kmeans2 <- dailykos %>% filter(kmen1$cluster == 2)
kmeans3 <- dailykos %>% filter(kmen1$cluster == 3)
kmeans4 <- dailykos %>% filter(kmen1$cluster == 4)
kmeans5 <- dailykos %>% filter(kmen1$cluster == 5)
kmeans6 <- dailykos %>% filter(kmen1$cluster == 6)
kmeans7 <- dailykos %>% filter(kmen1$cluster == 7)

kword1 <- tail(sort(colMeans(kmeans1)))
kword2 <- tail(sort(colMeans(kmeans2)))
kword3 <- tail(sort(colMeans(kmeans3)))
kword4 <- tail(sort(colMeans(kmeans4)))
kword5 <- tail(sort(colMeans(kmeans5)))
kword6 <- tail(sort(colMeans(kmeans6)))
kword7 <- tail(sort(colMeans(kmeans7)))

kword1
kword2
kword3
kword4
kword5
kword6
kword7
# Which k-means cluster best corresponds to the Iraq War: Cluster 6
# Which k-means cluster best corresponds to the democratic party: Cluster 5

# Problem 2.3
table(kmen1$cluster, heirGroups)
# HClust 3 best represents kmeans cluster 2 
# HClust 2 best represents kmeans cluster 3
