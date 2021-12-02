# SETUP ----
#getwd()
#setwd("C:/Users/anton/OneDrive/Dokument/R/dsss/Socialism")
data.soc <- read.csv("Socialism.csv")
library(FactoMineR)
library(explor)
library(ggplot2)
library(dendextend)
library(factoextra)
library(corrplot)

# WRANGLING ----
data.soc <- data.soc[!is.na(data.soc$country),]
data.soc1 <- lapply(data.soc[,3:32],as.numeric) # Sub-setting data
data.soc1 <- as.data.frame(data.soc1)
for(i in 1:ncol(data.soc1)){
  data.soc1[is.na(data.soc1[,i]), i] <- mean(data.soc1[,i], na.rm = TRUE)
}

data.soc2 <- scale(data.soc1)
data.soc2 <- as.data.frame(data.soc2)

# PCA ----
res.pca1 <- prcomp(data.soc2, center = T, scale = T)

plot(res.pca1,type = "l") # Scree-plot
summary(res.pca1) 
biplot(res.pca1, scale = 0, expand = 1) 

print(res.pca1$rotation) # Start to investigate results

diag(var(res.pca1$x[,]))

eig.val <- get_eigenvalue(res.pca1) 
eig.val

var <- get_pca_var(res.pca1)
var

corrplot(var$cos2, is.corr=FALSE) # cos2 plot

corrplot(var$contrib, is.corr=FALSE) 

#explor(res.pca1)

data.soc1 <- scale(data.soc1)
data.soc1 <- as.data.frame(data.soc1)

data.soc2 <- cbind(data.soc2, res.pca1$x[,1:2]) 
head(data.soc2)

cor(data.soc1, data.soc2[, 31:32], use = "complete.obs")

data.soc2$scode <- data.soc$scode 
data.soc2$country <- data.soc$country
data.soc2$ccode <- data.soc$ccode
data.soc2$polity2 <- data.soc$polity2

plone <- ggplot(data.soc2, aes(PC1, PC2)) +
  geom_text(aes(label = scode, vjust = -1)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

plone 


#K-MEANS ----

fitK <- kmeans(data.soc2[,31:32], 4)
fitK

plot(data.soc2[,31:32], col = fitK$cluster)

k <- list()
for (i in 1:10){
  k[[i]] <- kmeans((data.soc2[,31:32]), i)
}

betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

plot(1:10, betweenss_totss, type= "b",
     ylab = "Between SS / Total SS", xlab = "clusters(k)")


K3 <- k[[3]]$cluster
K4 <- k[[4]]$cluster

data.soc2.3k <- cbind(data.soc2, K3)
data.soc2.4k <- cbind(data.soc2, K4)

pltwo <- ggplot(data.soc2.3k, aes(PC1, PC2, col = as.factor(K3), fill = as.factor(K3))) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_text(aes(label = scode, vjust = -1)) +
  geom_point(shape = 21, col = "black")

pltwo

plthree <- ggplot(data.soc2.4k, aes(PC1, PC2, col = as.factor(K4), fill = as.factor(K4))) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_text(aes(label = scode, vjust = -1)) +
  geom_point(shape = 21, col = "black")

plthree


#HIERARCHICAL CLUSTERING ----
d <- dist(data.soc2[,31:32])
fitH <- hclust(d, method = "complete")
fitH$labels <- data.soc2$scode
plot(fitH)
rect.hclust(fitH, k = 4, border = "red") 
clusters <- cutree(fitH, 4) 
plot(data.soc2[,31:32], col = clusters, asp=T)

fitH2 <- as.dendrogram(fitH)
plot(colour_branches(fitH2, k = 4, groupLabels = T))

fviz_cluster(object = list(data = data.soc2[,31:32],
                           cluster = cutree(fitH2, 4),
                           geom = "point",
                           show.clust.cent = FALSE,
                           main = "Agglomerative HC - Complete Linkage"
                           ))
