                                              ### FIRST PART : RAW DATA ANALYSIS ### 

# Read the csv file
file <- read.csv2("C:/Users/.../fichier.csv", header=TRUE,sep=";",dec=",");

# Quick view of features'distribution
ggplot(data, aes(x = var)) + geom_density()
summary(data$var)


# Select outliers values
liste<-vector("list", ncol(data))
value.toSuppress<-vector(mode="logical", length= nrow(data))

for (i in 1:ncol(data)) {
  liste[[i]]<- vector(mode = "logical", length = nrow(data));
}

for (i in 1:length(liste)){
  mad <- mad(data[,i]);
  median <- median(data[,i]);
  infBound <- median - 3*mad;
  supBound <- median + 3*mad;
  liste[[i]] <- liste[[i]] | (data[,i] < infBound);
  liste[[i]] <- liste[[i]] | (data[,i] > supBound);
}

# % delete necessery on each features
rowSuppr<-vector("list", ncol(data))
for (i in 1:length(liste)){
  joueurSuppr[i]<- 100 * sum(liste[[i]]) / length(liste[[i]]);
}

# Total data to delete
rowSupprAll<-vector("logical", nrow(data))
for (i in 1:length(rowSuppr)){
  rowSupprAll<-liste[[i]] | rowSupprAll
}
100*sum(rowSupprAll/length(rowSupprAll))

# Delete outliers values
data.filtered <- data[!row.toSuppress,]; 
summary(data.filtered);

# Display new density
ggplot(data.filtered, aes(x=var)) + geom_density();



                                            ### SECOND PART : CLUSTERING ### 

# Select numeric columns for clustering
data.filtered.typo<-data[c("Var1","Var2","Var3","Var4")]


## Analyse optimal cluster number ##
 
# Methode 1: K Means
wss <- (nrow(data.filtered.typo)-1)*sum(apply(data.filtered.typo,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data.filtered.typo,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Methode 2: Partitioning Around Medoids
library(fpc)
library("cluster", lib.loc="~/R/R-3.2.1/library")
pamk.best <- pamk(data.filtered.typo)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(data.filtered.typo, pamk.best$nc))

asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- pam(data.filtered.typo, k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")

# Methode 3: Calinsky criterion
require(vegan)
fit <- cascadeKM(scale(data, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

## Clustering ##

# K Medoids with clara
library("cluster", lib.loc="~/R/R-3.2.1/library")
kMedoids<-clara(data.filtered.typo,5, pamLike = TRUE)
kMedoids$clusinfo
kMedoids$medoids

# K Means 
kMeans<-kmeans(data.filtered.typo,5)
kMeans$size
kMeans$centers


## Graphics visualization ##

plotcluster(data1015v2.typo,kMeans$cluster)
plot(data.filtered.typo[,c(1:4)],col=data.filtered.typo[,5])
plot(kMedoids)

ggpairs(data.filtered.typo, colour=data.filtered.typo$ClusterMeans)

makePairs <- function(data.filtered.typo) 
{
  grid <- expand.grid(x = 1:ncol(data.filtered.typo), y = 1:ncol(data.filtered.typo))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data.filtered.typo)[ycol], yvar = names(data.filtered.typo)[xcol], 
               x = data.filtered.typo[, xcol], y = data.filtered.typo[, ycol], data.filtered.typo)
  }))
  all$xvar <- factor(all$xvar, levels = names(data.filtered.typo))
  all$yvar <- factor(all$yvar, levels = names(data.filtered.typo))
  densities <- do.call("rbind", lapply(1:ncol(data.filtered.typo), function(i) {
    data.frame(xvar = names(data.filtered.typo)[i], yvar = names(data.filtered.typo)[i], x = data.filtered.typo[, i])
  }))
  list(all=all, densities=densities)
}

# expand iris data frame for pairs plot
gg1 = makePairs(data.filtered.typo[,c(1:4)])

# new data frame mega iris
mega_iris = data.frame(gg1$all, typo=rep(data.filtered.typo$typo, length=nrow(gg1$all)));

# pairs plot
ggplot(mega_iris, aes_string(x = "x", y = "y")) + 
  facet_grid(xvar ~ yvar, scales = "free") + 
  geom_point(aes(colour=typo), na.rm = TRUE, alpha=0.8) + 
  stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
               data = gg1$densities, position = "identity", 
               colour = "grey20", geom = "line");




                                          ### THIRD PART: TESTING CLUSTERING QUALITY ###

## Linear descriminant analysis ##
library("MASS", lib.loc="~/R/R-3.2.1/library")

# Add column clustering
data.filtered.typo$ClusterMedoids<-kMedoids$clustering
data.filtered.typo$ClusterMeans<-kMeans$cluster

# Linear descriminant analysis for K Medoids clustering
clusterMedoid.lda<-lda(ClusterMedoids~Var1+Var2+Var3+Var4, data=data.filtered.typo)
clusterMedoid.lda.p<-predict(clusterMedoid.lda,newdata=data.filtered.typo)$class

tableMedoids<-table(clusterMedoid.lda.p, data.filtered.typo[,Var5])
prop.table(tableMedoids,2)

# % of good affectation between initial clustering and classification made by LDA
totalMedoid<-(sum(diag(tableMedoids))/sum(tableMedoids))*100
totalMedoid


## Clustering Criteria ##
library("clusterCrit", lib.loc="~/R/R-3.2.1/library")
mx<-data.matrix(data.filtered.typo[,1:4])
medoidsIC<-intCriteria(mx, kMedoids$cluster,c("Silhouette"))
medoidsIC$silhouette