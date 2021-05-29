data <- read.csv('D:/Bootcamp Data Science/Dibimbing/file/customer_movie_rating.csv')
head(data)

# summary data and check missing value
summary(data)

# b. Check outliers data with Multiple Boxplot
boxplot(data$Horror, data$Romcom, data$Action, data$Comedy, data$Fantasy,
        main = "Multiple boxplots for comparision",
        data = data,
        names = c("Horror", "Romcom", "Action", "Comedy", "Fantasy"),
        col = "yellow",
        border = "brown"
)

# outliers data in Romcom
a <- boxplot(data$Romcom)
a$out
length(a$out)
unique(a$out)

# outliers data in Action
b <- boxplot(data$Action)
b$out
length(b$out)
unique(b$out)

#### Handling outliers
data[data$Romcom %in% a$out, "Romcom"] = NA
data[data$Action %in% b$out, "Action"] = NA
sum(is.na(data$Romcom))
sum(is.na(data$Action))

data$Romcom[is.na(data$Romcom)]<-mean(data$Romcom,na.rm=TRUE)
data$Action[is.na(data$Action)]<-mean(data$Action,na.rm=TRUE)

sum(is.na(data$Romcom))
sum(is.na(data$Action))

library(tidyverse)  # data manipulation and visualization
library(gridExtra)  # plot arrangement
library(factoextra)

data_scale <- scale(data)
head(data_scale)

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data_scale, k, nstart = 10 )$tot.withinss}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:30
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Average Silhouette Method

fviz_nbclust(data_scale, kmeans, method = "silhouette")

#gap statistic

library(cluster)
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(data_scale, FUN = kmeans, nstart = 25,
                    K.max = 30, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

library(NbClust)
nbclust_out <- NbClust(
  data = data,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 15, # maximum number of clusters
  method = "kmeans", # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
  index = "alllong",
  alphaBeale = 0.1
)

# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 5 clusters
nbclust_plot <- subset(nbclust_plot, clusters >= 2 & clusters <= 15)

# create plot
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal()

# create plot
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal() +xlim(1,5)

# Choose 123 as our random seed
set.seed(123)

# Cluster the players using kmeans with 3 clusters
cluster_solution <- kmeans(data_scale, centers = 3)

fviz_cluster(cluster_solution, data = data)

# Store the cluster assignments back into the clustering data frame object
data$cluster <- factor(cluster_solution$cluster)
head(data)

# Look at the distribution of cluster assignments
table(data$cluster)
