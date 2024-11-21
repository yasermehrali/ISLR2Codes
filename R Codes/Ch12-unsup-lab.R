
# Lab: Unsupervised Learning ------------------


## Principal Components Analysis --------------

###
states <- row.names(USArrests)
states
###
names(USArrests)
###
apply(USArrests, 2, mean)
###
apply(USArrests, 2, var)
###
pr.out <- prcomp(USArrests, scale = TRUE)
###
names(pr.out)
###
pr.out$center
pr.out$scale
###
pr.out$rotation
###
dim(pr.out$x)
###
biplot(pr.out, scale = 0)
###
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)
###
pr.out$sdev
###
pr.var <- pr.out$sdev^2
pr.var
###
pve <- pr.var / sum(pr.var)
pve
###
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
    ylab = "Proportion of Variance Explained", ylim = c(0, 1),
    type = "b")
plot(cumsum(pve), xlab = "Principal Component",
    ylab = "Cumulative Proportion of Variance Explained",
    ylim = c(0, 1), type = "b")
###
a <- c(1, 2, 8, -3)
cumsum(a)

### Methods for Principal Components Analysis ----

fit_pca=prcomp(iris[,-5], scale = TRUE)
fit_pca
summary(fit_pca)
fit_pca2=princomp(iris[,-5], cor = TRUE)
fit_pca2
summary(fit_pca2)

library(broom)
# ?tidy.prcomp
tidy(fit_pca)
tidy(fit_pca,matrix="u")
tidy(fit_pca,matrix="v")
tidy(fit_pca,matrix="d")
augment(fit_pca,data=iris)

biplot(fit_pca)
biplot(fit_pca2)
biplot(fit_pca,col=c(2,3))
screeplot(fit_pca)
screeplot(fit_pca2)
screeplot(fit_pca,type="lines")

library(ggfortify)
library(tibble)
fortify(fit_pca) |> as_tibble()
# ?autoplot.pca_common
# ?ggfortify::ggbiplot
autoplot(fit_pca)
autoplot(fit_pca2)
autoplot(fit_pca,x=2,y=3)
autoplot(fit_pca,variance_percentage = FALSE)
autoplot(fit_pca,data=iris,color="Species")
autoplot(fit_pca,data=iris,shape="Species")
autoplot(fit_pca,label=T)
autoplot(fit_pca,loadings=T)
autoplot(fit_pca,loadings=T, loadings.label = TRUE)
# ?chull
autoplot(fit_pca,frame=T)
autoplot(fit_pca,frame=T,data=iris,frame.colour="Species")
autoplot(fit_pca,frame=T,data=iris,colour="Species")
autoplot(fit_pca,frame=T,frame.type="convex")
autoplot(fit_pca,frame=T,frame.type="t")
autoplot(fit_pca,frame=T,frame.type="t",frame.level=0.1)
autoplot(fit_pca,frame=T,frame.type="norm")
autoplot(fit_pca,frame=T,frame.type="euclid")
autoplot(fit_pca,frame=T,frame.type="euclid",asp=1)

# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot::ggbiplot(fit_pca)
ggbiplot::ggbiplot(fit_pca2)
ggscreeplot(fit_pca)
ggscreeplot(fit_pca2)
ggscreeplot(fit_pca,type="pev")
ggscreeplot(fit_pca,type="cev")

library(factoextra)
fviz_pca(fit_pca)
fviz_pca(fit_pca2)
fviz_pca_ind(fit_pca)
fviz_pca_var(fit_pca)
fviz_eig(fit_pca)

library(qgraph)
qgraph(fit_pca2$loadings)
qgraph(fit_pca2$loadings,layout="spring")

## Matrix Completion --------------------------

###
X <- data.matrix(scale(USArrests))
pcob <- prcomp(X)
summary(pcob)
###
sX <- svd(X)
names(sX)
round(sX$v, 3)
###
pcob$rotation
###
t(sX$d * t(sX$u))
pcob$x
###
nomit <- 20
set.seed(15)
ina <- sample(seq(50), nomit)
inb <- sample(1:4, nomit, replace = TRUE)
Xna <- X
index.na <- cbind(ina, inb)
Xna[index.na] <- NA
###
fit.svd <- function(X, M = 1) {
   svdob <- svd(X)
   with(svdob,
       u[, 1:M, drop = FALSE] %*%
       (d[1:M] * t(v[, 1:M, drop = FALSE]))
     )
}
###
###
Xhat <- Xna
xbar <- colMeans(Xna, na.rm = TRUE)
Xhat[index.na] <- xbar[inb]
###
thresh <- 1e-7
rel_err <- 1
iter <- 0
ismiss <- is.na(Xna)
mssold <- mean((scale(Xna, xbar, FALSE)[!ismiss])^2)
mss0 <- mean(Xna[!ismiss]^2)
###
while(rel_err > thresh) {
    iter <- iter + 1

    # Step 2(a)

    Xapp <- fit.svd(Xhat, M = 1)

    # Step 2(b)

    Xhat[ismiss] <- Xapp[ismiss]

    # Step 2(c)

    mss <- mean(((Xna - Xapp)[!ismiss])^2)
    rel_err <- (mssold - mss) / mss0
    mssold <- mss
    cat("Iter:", iter, "MSS:", mss,
      "Rel. Err:", rel_err, "\n")
    }
###
cor(Xapp[ismiss], X[ismiss])

## Clustering ---------------------------------

### $K$-Means Clustering ----------------------

###
set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
###
km.out <- kmeans(x, 2, nstart = 20)
###
km.out$cluster
###
par(mfrow = c(1, 2))
plot(x, col = (km.out$cluster + 1),
    main = "K-Means Clustering Results with K = 2",
    xlab = "", ylab = "", pch = 20, cex = 2)
###
set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1),
    main = "K-Means Clustering Results with K = 3",
    xlab = "", ylab = "", pch = 20, cex = 2)
###
set.seed(4)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss

#### Methods for $K$-Means Clustering ---------

library(NbClust)
nc=NbClust(iris[,-5],method = "kmeans")
table(nc$Best.nc[1,])
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]))
library(factoextra)
# fviz_nbclust(nc) # Error
fviz_nbclust(iris[,-5],FUNcluster=kmeans,method = "silhouette")
fviz_nbclust(iris[,-5],FUNcluster=kmeans,method = "wss")
fviz_nbclust(iris[,-5],FUNcluster=kmeans,method = "gap_stat")

k=3
set.seed(123)
fit_km=kmeans(iris[,-5], centers = k)
fit_km

ct_km=table(iris$Species,fit_km$cluster)
ct_km
library(flexclust)
randIndex(ct_km)
sum(diag(ct_km))/sum(ct_km)
randIndex(ct_km,correct = F)
comPart(iris$Species,fit_km$cluster)

library(broom)
# ?tidy.kmeans
tidy(fit_km)
glance(fit_km)
augment(fit_km,data=iris)

library(ggfortify)
# ?fortify.kmeans
head(fortify(fit_km,data=iris))
# ?autoplot.kmeans
# ?ggfortify::ggbiplot
autoplot(fit_km,data=iris)
autoplot(fit_km,data=iris,x=2,y=3)
autoplot(fit_km,data=iris,variance_percentage = FALSE)
autoplot(fit_km,data=iris,colour="Species")
autoplot(fit_km,data=iris,shape="Species")
autoplot(fit_km,data=iris,label=T)
autoplot(fit_km,data=iris,loadings=T)
autoplot(fit_km,data=iris,loadings=T, loadings.label = TRUE)
# ?chull
autoplot(fit_km,data=iris,frame=T)
autoplot(fit_km,data=iris,frame=T,frame.colour="Species")
autoplot(fit_km,data=iris,frame=T,colour="Species")
autoplot(fit_km,data=iris,frame=T,shape="Species")
autoplot(fit_km,data=iris,frame=T,frame.type="convex")
autoplot(fit_km,data=iris,frame=T,frame.type="t")
autoplot(fit_km,data=iris,frame=T,frame.type="t",frame.level=0.1)
autoplot(fit_km,data=iris,frame=T,frame.type="norm")
autoplot(fit_km,data=iris,frame=T,frame.type="euclid")
autoplot(fit_km,data=iris,frame=T,frame.type="euclid",frame.level=0.1,asp=1)

library(factoextra)
fviz_cluster(fit_km,data=iris[,-5])
fviz_cluster(fit_km,data=iris[,-5],ellipse.type = "t")

library(useful)
# ?plot.kmeans
plot(fit_km)
plot(fit_km,data=iris)
plot(fit_km,data=iris,class="Species")

library(ggChernoff)
ggplot(iris, aes(Sepal.Width, Sepal.Length, smile = Petal.Length, fill = Species)) +
  geom_chernoff()

tidy_km=augment(fit_km,data=iris)
tidy_km
ggplot(tidy_km, aes(Sepal.Width, Sepal.Length, smile = Petal.Length, fill = .cluster)) +
  geom_chernoff()

fit_pca=prcomp(iris[,-5], scale = TRUE)
tidy_pca=fortify(fit_pca,data=iris)
head(tidy_pca)
library(dplyr)
tidy_data=tidy_pca |> 
  select(PC1:PC4) |> 
  bind_cols(tidy_km)
head(tidy_data)
ggplot(
  tidy_data, 
  aes(PC1, PC2, fill = Species , size = .cluster)
) +
  geom_chernoff()
ggplot(
  tidy_data, 
  aes(
    PC1, PC2, fill = Species , size = .cluster,
    smile=Sepal.Length, brow=Sepal.Width, nose=Petal.Length, eyes=Petal.Width
  )
) +
  geom_chernoff()+
  theme(legend.direction = "vertical", legend.box = "horizontal", legend.position = "bottom")
ggplot(
  tidy_data, 
  aes(
    PC1, PC2, fill = Species , size = .cluster,
    smile=Sepal.Length, brow=Sepal.Width, nose=Petal.Length, eyes=Petal.Width
  )
) +
  geom_chernoff()+
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.position = "bottom")

### Hierarchical Clustering -------------------

###
hc.complete <- hclust(dist(x), method = "complete")
###
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")
###
par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage",
    xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage",
    xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage",
    xlab = "", sub = "", cex = .9)
###
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
###
cutree(hc.single, 4)
###
xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"),
    main = "Hierarchical Clustering with Scaled Features")
###
x <- matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"),
    main = "Complete Linkage with Correlation-Based Distance",
    xlab = "", sub = "")

#### Methods for Hierarchical Clustering ------

set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
colnames(x)=c("X1","X2")
plot(x,col=rep(1:2,each=25),pch=19)

library(NbClust)
nc=NbClust(x,method = "complete")
table(nc$Best.nc[1,])
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]))
# library(factoextra)
# fviz_nbclust(nc)

# Distances:
library(flexclust)
library(factoextra)
library(philentropy)

# ?stats::dist
# ?flexclust::dist2
# ?factoextra::get_dist
# ?philentropy::distance

d=dist(x,method = "euclidean")
d=get_dist(x,method = "euclidean")
d=distance(x,method = "euclidean") |> as.dist()

library(factoextra)
fviz_dist(d)

# hclust
# ?hclust
hc=hclust(d,method="complete")
plot(hc)
cutree(hc,4)
# ?rect.hclust
rect.hclust(hc, k = 4, border = "red")
x <- rect.hclust(hc, h = 3, which = c(2,7), border = 3:4)
x

library(factoextra)
fviz_dend(hc)
fviz_dend(hc,horiz = T)
fviz_dend(
  hc, k = 4, # Cut in four groups
  cex = 0.5, # label size
  k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
  color_labels_by_k = TRUE, # color labels by groups
  rect = TRUE, # Add rectangle around groups
  # rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
  rect_fill = TRUE
)
fviz_dend(hc, cex = 0.5, k = 4, k_colors = "jco")
fviz_dend(hc, k = 4, cex = 0.4, horiz = TRUE, k_colors = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
fviz_dend(hc, cex = 0.5, k = 4, k_colors = "jco", type = "circular")
require("igraph")
fviz_dend(hc, k = 4, k_colors = "jco", type = "phylogenic", repel = TRUE)
require("igraph")
fviz_dend(hc, k = 4, k_colors = "jco", type = "phylogenic", 
          repel = TRUE, phylo_layout = "layout.gem")

library(ggdendro)
# Demonstrate plotting directly from object class hclust
ggdendrogram(hc, rotate = TRUE)

# demonstrate converting hclust to dendro using dendro_data first
hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate = TRUE, size = 2) + 
  labs(title = "Dendrogram in ggplot2")

## NCI60 Data Example -------------------------

###
library(ISLR2)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
###
dim(nci.data)
###
nci.labs[1:4]
table(nci.labs)

### PCA on the NCI60 Data ---------------------

###
pr.out <- prcomp(nci.data, scale = TRUE)
###
Cols <- function(vec) {
   cols <- rainbow(length(unique(vec)))
   return(cols[as.numeric(as.factor(vec))])
 }
###
par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
    xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19,
    xlab = "Z1", ylab = "Z3")
###
summary(pr.out)
###
plot(pr.out)
###
pve <- 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
par(mfrow = c(1, 2))
plot(pve,  type = "o", ylab = "PVE",
    xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE",
    xlab = "Principal Component", col = "brown3")

### Clustering the Observations of the NCI60 Data ----

###
sd.data <- scale(nci.data)
###
par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
    labels = nci.labs, main = "Complete Linkage")
plot(hclust(data.dist, method = "average"),
    labels = nci.labs, main = "Average Linkage",
    xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"),
    labels = nci.labs,  main = "Single Linkage",
    xlab = "", sub = "", ylab = "")
###
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)
###
par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")
###
hc.out
###
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
###
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs,
    main = "Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
###
