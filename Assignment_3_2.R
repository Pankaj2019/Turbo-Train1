cerealData<- read.csv("cereal3.csv")
cerealData
cerealData <- data.matrix(cerealData)
cerealData

# NOTE : the 1st column ' CEREAL ' is Categorical data
# KMEANS needs only Numerical data. So, convert the Categorical data to Numeric first
cerealData[,1] <- as.numeric(cerealData[,1])
class(cerealData[,1])


# ````````````````````````````Using Elbow method to find optimal no. of clusters``````````````````````````
set.seed(6)
wcss <- vector()
for(i in 1:10)wcss[i]<-sum(kmeans(cerealData,i)$withinss)
plot(1:10,wcss,type="b",main=paste('Clusters of Cereals'),xlab="no. of clusters",ylab="wcss")

# So, 5 is the optimal Cluster no. size
cerealData.5MC.Elbow <- kmeans(cerealData,5,iter.max= 300)
cerealData.5MC.Elbow

# Vizualizing the Clusters
clusplot(cerealData,cerealData.5MC.Elbow$cluster,lines=0
         ,shade=TRUE,color=TRUE,labels=2,plotchar=FALSE,span=TRUE,main=paste("Cluster of Cereals"))


# `````````````````````````` Using Silhouette method to find optimal no. of clusters ````````````````````````

#2MC
set.seed(123)
cerealData.2MC.Silhouette <-kmeans(cerealData,2) 
cerealData.2MC.Silhouette
class(cerealData.2MC.Silhouette)
cerealData.2MC.Silhouette$cluster

D2 <- daisy(cerealData)
plot(silhouette(cerealData.2MC.Silhouette$cluster, D2))  # Average Silhouette value : 0.41

# 3MC
set.seed(123)
cerealData.3MC.Silhouette <-kmeans(cerealData,3) 
cerealData.3MC.Silhouette
class(cerealData.3MC.Silhouette)
cerealData.3MC.Silhouette$cluster

D3 <- daisy(cerealData)
plot(silhouette(cerealData.3MC.Silhouette$cluster, D3)) # Average Silhouette value : 0.44

# 4MC
set.seed(12)
cerealData.4MC.Silhouette <-kmeans(cerealData,4) 
cerealData.4MC.Silhouette
class(cerealData.4MC.Silhouette)
cerealData.4MC.Silhouette$cluster

D4 <- daisy(cerealData)
plot(silhouette(cerealData.4MC.Silhouette$cluster, D4)) # Average Silhouette value : 0.38


# 5MC
set.seed(12)
cerealData.5MC.Silhouette <-kmeans(cerealData,5) 
cerealData.5MC.Silhouette
class(cerealData.5MC.Silhouette)
cerealData.5MC.Silhouette$cluster

D5 <- daisy(cerealData)
plot(silhouette(cerealData.5MC.Silhouette$cluster, D5)) # Average Silhouette value : 0.38

# 6MC
set.seed(12)
cerealData.6MC.Silhouette <-kmeans(cerealData,6) 
cerealData.6MC.Silhouette
class(cerealData.6MC.Silhouette)
cerealData.6MC.Silhouette$cluster

D6 <- daisy(cerealData)
plot(silhouette(cerealData.6MC.Silhouette$cluster, D6)) # Average Silhouette value : 0.4

# 7MC
set.seed(12)
cerealData.7MC.Silhouette <-kmeans(cerealData,7) 
cerealData.7MC.Silhouette
class(cerealData.7MC.Silhouette)
cerealData.7MC.Silhouette$cluster

D7 <- daisy(cerealData)
plot(silhouette(cerealData.7MC.Silhouette$cluster, D7))   # Average Silhouette value : 0.39


# Since 3MC gives max silhouette value, chosing 3MC Clustering
# Vizualizing the Clusters

clusplot(cerealData,cerealData.3MC.Silhouette$cluster,lines=0
         ,shade=TRUE,color=TRUE,labels=2,plotchar=FALSE,span=TRUE,main=paste("Cluster of Cereals"))
