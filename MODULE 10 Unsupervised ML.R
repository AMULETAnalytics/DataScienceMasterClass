# MODULE 10 - Unsupervised Machine Learning
#
# (c) Copyright 2015 - AMULET Analytics
# ---------------------------------------------------------------


# EDA on simulated data set 

set.seed(1234)    # Seed, for reproducible results

par(mar=c(0,0,0,0))   # No margin, c(bottom,left,top,right)

# Create a simulated data set
# Actually two vectors x, y for coordinates of plot
# Using mean and sd args we have created 3 clusters

# NOTE: experiment with different mean and sd to see effect!
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)

# Scatterplot of data
plot(x,y,col="blue",pch=19,cex=2)

# Display integer labels (12) to the upper-right of the dot
text(x+0.05,y+0.05,labels=as.character(1:12))


# -------------------------------------------------------

# Hierarchical clustering with simulated data set

df <- data.frame(x=x, y=y)   # 12x2
# Calculate distance between points. dist() is just for clustering
dist(df)    # Calculate and display distance between variables

# Calculate distance between 12 points observed (distance between columns)
distxy <- dist(df)   # Default distance method = euclidean metric
#distxy <- dist(df, method="minkowski")   # Class=dist!!


# Produce cluster object
hClustering <- hclust(distxy, method="complete")  # hclust requires a dist object, returns hclust object
# Plot dendrogram showing 3 clusters
plot(hClustering)   

# Cut the tree high yields fewer clusters
cutree(hClustering,h=1.5)   # Will yield fewer clusters
# [1] 1 1 1 1 2 2 2 2 3 3 3 3

# Cut the tree low
cutree(hClustering,h=0.5)   # Will yield more clusters
# [1] 1 2 2 1 3 3 3 4 5 5 5 5


# -------------------------------------------------------

# Visualizing hierarchical clustering using a Heatmap

dataFrame <- data.frame(x=x, y=y)
set.seed(143)

# Take a small sample of the rows. Each sample() returns different random seq
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]  # 12x2

# Clusters together the rows and columns
heatmap(dataMatrix)   # heatmap() requires matrix argument


# ---------------------------------------------------------------

# Use kmeans() algorithm

# K-means clustering algorithm:
# Specify starting centroids
# Assign to closest centroid
# Recalculate centroids
# Reassign values
# Update centroids

dataFrame <- data.frame(x,y)

# You choose 3 centroids
# YOu can also define the max # of iterations the algorithm is to 
# perform just in case it doesn't converge. Defaults for iter.max is 10
kmeansObj <- kmeans(dataFrame,centers=3)

names(kmeansObj)   # List components of the kmeans object
#[1] "cluster"      "centers"      "totss"        "withinss"    
#[5] "tot.withinss" "betweenss"    "size"         "iter"        
#[9] "ifault" 

# Which cluster each data point has been assigned to. 
kmeansObj$cluster    # Grouping numbers of data points
#[1] 3 3 3 3 1 1 1 1 2 2 2 2

# K-means is not a deterministic algorithm. If you use the exact same
# data set with different starting centroids, it may converge to a 
# different set of clusters. So you can use the nstart arg to take
# an average. 

# Datavis for clusters

par(mar=rep(0.2,4))    # Set margins

# Use col arg for coloring clusters
# Use pch for the plot symbol to use (use ? pch)
# Use cex for symbol magnification (default 1)
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)

# Use centers component of kmeans object - matrix of cluster centers
kmeansObj$centers    # 3x2 matrix: x column, y column

# pch=3 is a "+" symbol, and cex=3 magnifies it. 
# lwd arg is for line width for drawing symbols
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)




