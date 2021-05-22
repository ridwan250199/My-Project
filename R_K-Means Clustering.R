library("readxl")

# read the data and display top 6 data
data <- read_excel('D:/Bootcamp Data Science/Dibimbing/file/customer_movie_rating.xlsx')
head(data)

# summary the data
summary(data)

# convert all entries to integers
data[c(1:5)] <- lapply(data[c(1:5)], as.integer)
head(data)

summary(data)

#Check outliers data with Multiple Boxplot
boxplot(data$Horror, data$Romcom, data$Action, data$Comedy, data$Fantasy,
        main = "Multiple boxplots for comparision",
        data = data,
        names = c("Horror", "Romcom", "Action", "Comedy", "Fantasy"),
        col = "yellow",
        border = "brown"
)

# outliers data in Horror
a <- boxplot(data$Horror)
a$out
length(a$out)
unique(a$out)

# outliers data in Action
b <- boxplot(data$Action)
b$out
length(b$out)
unique(b$out)

# outliers data in Comedy
c <- boxplot(data$Comedy)
c$out
length(c$out)
unique(c$out)

#### Handling outliers
data[data$Horror %in% a$out, "Horror"] = median(data$Horror)
data[data$Action %in% b$out, "Action"] = median(data$Action)
data[data$Comedy %in% c$out, "Comedy"] = median(data$Comedy)

library(tidyverse)  # data manipulation and visualization
library(gridExtra)  # plot arrangement
library(factoextra)

# scale the data
data_scale <- scale(data)
head(data_scale)

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data_scale, k, nstart = 10 )$tot.withinss}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
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
                    K.max = 15, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

library(NbClust)
nbclust_out <- NbClust(
  data = data,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 15, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
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
  theme_minimal() +xlim(2,7)

# Choose 123 as our random seed
set.seed(123)

# Cluster the players using kmeans with four clusters
cluster_solution <- kmeans(data_scale, centers = 4)

fviz_cluster(cluster_solution, data = data)

# Store the cluster assignments back into the clustering data frame object
data$cluster <- factor(cluster_solution$cluster)
head(data)

# Look at the distribution of cluster assignments
table(data$cluster)


