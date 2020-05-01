#Clustering Project

#READ DATA

Adult = read.csv(file="E:/CHPC cluster/test/Adult/Rec/Adult-loged.csv", header=TRUE)

length(Adult[1,]) #22185
x=Adult[,1]
Adult=Adult[,-1]
Adult=(data.matrix(data.frame(Adult)))
rownames(Adult)=x

UMAP = read.csv(file="E:/CHPC cluster/test/ImputedData/Adult/UMAP/AdultData-Umap-dimension.csv", header=TRUE)

tSNE = read.csv(file="E:/CHPC cluster/test/ImputedData/Adult/tSNE/Adult-tSNE2.csv", header=TRUE)

UMAP = UMAP[,-1]
rownames(UMAP) = colnames(Adult)

tSNE = tSNE[,-1]
rownames(tSNE) = colnames(Adult)

################################
library(ggplot2)
require(cowplot)
library(jcolors)
################################
#plot(tSNE,col=factor(substr(rownames(tSNE),1,4)))
#tSNE
C=factor(substr(rownames(tSNE),1,4))
tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  C)
P1=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE")
P1

#Kmeans 5 on tSNE 
C=as.numeric(kmeans(tSNE,5)$cluster)
tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  C)
P2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-5")+ scale_color_gradientn(colours = rainbow(5))
P2


#Kmeans 8 on tSNE 
C=as.numeric(kmeans(tSNE,8)$cluster)
tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  C)
P2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-8")+ scale_color_gradientn(colours = rainbow(8))
P2


#Kmeans 10 on tSNE 
C=as.numeric(kmeans(tSNE,10)$cluster)
tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  C)
P2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-10")+ scale_color_gradientn(colours = rainbow(10))
P2

#Kmeans 27 on tSNE 
C=as.numeric(kmeans(tSNE,27)$cluster)
tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  C)
P2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-27")+ scale_color_gradientn(colours = rainbow(27))
P2

#Kmeans 37 on tSNE 
C=as.numeric(kmeans(tSNE,37)$cluster)
tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  C)
P2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-37")+ scale_color_gradientn(colours = rainbow(37))
P2

#Kmeans 59 on tSNE 
C=as.numeric(kmeans(tSNE,59)$cluster)
tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  C)
P2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-59")+ scale_color_gradientn(colours = rainbow(59))
P2

################################
#UMAP
C=factor(substr(rownames(UMAP),1,4))
UMAP_plot <- data.frame(x = UMAP[,1], y = UMAP[,2], col =  C)
T1=ggplot(UMAP_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("UMAP")
T1

#Kmeans 59 on UMAP 
C=as.numeric(kmeans(UMAP,59)$cluster)
UMAP_plot <- data.frame(x = UMAP[,1], y = UMAP[,2], col =  C)
T2=ggplot(UMAP_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("UMAP-59")+ scale_color_gradientn(colours = rainbow(59))
T2

##########################################
#Clustering based on correlation matrix

Adult.cor = cor(Adult)
K5 = as.numeric(kmeans(Adult.cor,5)$cluster)
UMAP_plot <- data.frame(x = UMAP[,1], y = UMAP[,2], col =  K5)
T2=ggplot(UMAP_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("UMAP-Correlation-5")+ scale_color_gradientn(colours = rainbow(5))
T2

tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  K5)
T2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-Correlation-5")+ scale_color_gradientn(colours = rainbow(5))
T2

K8 = as.numeric(kmeans(Adult.cor,8)$cluster)
UMAP_plot <- data.frame(x = UMAP[,1], y = UMAP[,2], col =  K8)
T2=ggplot(UMAP_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("UMAP-Correlation-8")+ scale_color_gradientn(colours = rainbow(8))
T2

tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  K8)
T2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-Correlation-8")+ scale_color_gradientn(colours = rainbow(8))
T2

K10 = as.numeric(kmeans(Adult.cor,10)$cluster)
UMAP_plot <- data.frame(x = UMAP[,1], y = UMAP[,2], col =  K10)
T2=ggplot(UMAP_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("UMAP-Correlation-10")+ scale_color_gradientn(colours = rainbow(10))
T2

tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  K10)
T2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-Correlation-10")+ scale_color_gradientn(colours = rainbow(10))
T2
#############################################
#tSNE on Correlation matrix
Cor_tSNE = Rtsne(Adult.cor)
Cor_tSNE$Y

Cor_tsne_plot <- data.frame(x = Cor_tSNE$Y[,1], y = Cor_tSNE$Y[,2], col =  K5)
T2=ggplot(Cor_tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("Correlation-5")+ scale_color_gradientn(colours = rainbow(5))
T2

###############################################
#kmeans 5
AK5 = as.numeric(kmeans(t(Adult),5)$cluster)
UMAP_plot <- data.frame(x = UMAP[,1], y = UMAP[,2], col =  AK5)
T2=ggplot(UMAP_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("UMAP-KmeansOnOriginalData-5")+ scale_color_gradientn(colours = rainbow(5))
T2

tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  AK5)
T2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-KmeansOnOriginalData-5")+ scale_color_gradientn(colours = rainbow(5))
T2

#kmeans 8
AK5 = as.numeric(kmeans(t(Adult),10)$cluster)
UMAP_plot <- data.frame(x = UMAP[,1], y = UMAP[,2], col =  AK5)
T2=ggplot(UMAP_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("UMAP-KmeansOnOriginalData-10")+ scale_color_gradientn(colours = rainbow(10))
T2

tsne_plot <- data.frame(x = tSNE[,1], y = tSNE[,2], col =  AK5)
T2=ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col),alpha = 0.3) + ggtitle("tSNE-KmeansOnOriginalData-10")+ scale_color_gradientn(colours = rainbow(10))
T2
