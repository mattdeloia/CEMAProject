library (dbscan)
library(cluster)
library(factoextra)
library(tidyverse)
######K Means Clustering
df_CyberPro_Interest<- df_Interest_Score %>% filter(Group=="Cyber Pro") %>%
  dplyr::select(id,Information, Electronics, Mathematics, Physical, Office, Protection, Combat, Leadership, Medical)
#df_CyberPro_Interest<- df_Interest_Score %>% filter(Group=="Cyber Pro") %>%
  dplyr::select(id,ISCTtotraw, GTSCORE, CRT_total, STTrials, Information, Electronics, Mathematics, Physical, Office, Protection, Combat, Leadership, Medical)
#for (i in 2:ncol(df_CyberPro_Interest)){df_CyberPro_Interest[is.na(df_CyberPro_Interest[,i]), i] <- mean(df_CyberPro_Interest[,i], na.rm=TRUE)}
  
df_CyberPro_Interest$id <- as.character(df_CyberPro_Interest$id)
dfclusterx<- df_CyberPro_Interest 
dfcluster1x<-data.frame(dfclusterx, row.names=1)
dfcluster2x <- as.matrix(dfcluster1x)
dfcluster3x <- scale(dfcluster2x)
#dfcluster3a[is.na(dfcluster3a)] <- 0
k <- kmeans(dfcluster3x, centers = 3, nstart = 25)
fviz_cluster(k, data = dfcluster3x)
print(k)

###### Density Based Clustering
dfcluster3x
kNNdistplot(dfcluster3x, k=4)
abline(h=2.5, col="red")

set.seed(1234)
db <- dbscan(dfcluster3x, 2.5, 4)
db$cluster
hullplot(dfcluster3x, db$cluster)
table(df_CyberPro_Interest$id, db$cluster)


#### Hiercarchical clustering 
clusters<- hclust((dist(dfcluster3x)))
plot(clusters)
##################
df_RASP_Interest<- df_Interest_Score %>% filter(Group=="RASP") %>%
  dplyr::select(id,GTSCORE, Information, Electronics, Mathematics, Physical, Office, Protection, Combat, Leadership, Medical) 
for (i in 2:ncol(df_RASP_Interest)){df_RASP_Interest[is.na(df_RASP_Interest[,i]), i] <- mean(df_RASP_Interest[,i], na.rm=TRUE)}

dfclusterz<- df_RASP_Interest 
dfcluster1z<-data.frame(dfclusterz, row.names=1)
dfcluster2z <- as.matrix(dfcluster1z)
dfcluster3z <- scale(dfcluster2z)
#dfcluster3a[is.na(dfcluster3a)] <- 0
k <- kmeans(dfcluster3z, centers = 3, nstart = 25)
fviz_cluster(k, data = dfcluster3z)
print(k)

###### Density Based Clustering
dfcluster3z
kNNdistplot(dfcluster3z, k=4)
abline(h=3.0, col="red")

set.seed(12345)
db <- dbscan(dfcluster3z, 3.0, 4)
db
hullplot(dfcluster3z, db$cluster)

set.seed(124)

# function to compute total within-cluster sum of square 
wssz <- function(k) {
  kmeans(dfcluster3z, k, nstart = 25 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# extract wss for 2-15 clusters
wss_valuesz <- map_dbl(k.values, wssz)
plot(k.values, wss_valuesz,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

