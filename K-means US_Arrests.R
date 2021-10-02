#Importing data
data("USArrests")
str(USArrests)
#Looking at the data
head(USArrests)
tail(USArrests)
str(USArrests)
#Is there any missing value in the dataset? 
any(is.na(USArrests))
#Any errors in the data set
summary(USArrests)
#Scaling the data 
USArrests_scaled <- scale(USArrests)
head(USArrests_scaled)
#K-means clustering-k=2
set.seed(2)
USArrests_scaled_kmeans <- kmeans(USArrests_scaled, centers = 2, nstart = 25)
#Examining the return values of the kmeans algorithm
USArrests_scaled_kmeans$tot.withinss
#Creating a function that calculates number of total within sum of squares for a particular value of k  
wss <- function(k){
    USArrests_scaled_kmeans <- kmeans(USArrests_scaled, centers = k, nstart = 25)
     return(USArrests_scaled_kmeans$tot.withinss)
    }
# Calculating total withing sum_sum of squares for up to 10 clusters
k_wss <- 1:10
wss_10 <- sapply(k_wss, wss)
wss_10
#Preparing data for an elbow plot 
elbow_plot_data <- as.data.frame(cbind(k_wss,wss_10))
class(elbow_plot_data)
#Plotting an elbow plot using ggplot2
library(ggplot2)
ggplot(data = elbow_plot_data, aes(x = k_wss,y = wss_10)) +
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = sequence)
  labs(title = "Elbow plot", x = "No of clusters", y= "Total within sum of squares")

#k-means clustering using the optimal number of clusters k=2
set.seed(3)
USArrests_scaled_kmeans2 = kmeans(USArrests_scaled, centers = 2, nstart = 25) 
#Accessing the clusters
USArrests_scaled_kmeans2$cluster
#Adding clusters to the original data 
US_Arrests_with_clusters = cbind(USArrests, clusters = USArrests_scaled_kmeans2$cluster)
head(US_Arrests_with_clusters)
#Visualizing the clusters using a cluster plot 
library(factoextra)
fviz_cluster(USArrests_scaled_kmeans2, data = USArrests)


