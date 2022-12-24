install.packages("ggplot2")
install.packages("devtools")
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
options(max.print = 9999999)

mydata_whole = read_tsv("master_ON_ratio_var_whole_genes.tsv")

pca_res_whole <- prcomp(mydata_whole[4:ncol(mydata_whole)], scale. = TRUE)

# 主成分寄与率を求める
v_1 <- pca_res_whole$sdev ^ 2
Contribution_rate_whole <- head(v_1 / sum(v_1), 10)
names(Contribution_rate_whole)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")

x_1 <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
y_1 <- head(v / sum(v), 10)
data_bar_whole <- data.frame(x_1, y_1)

ggplot(data_bar_whole, aes(x=factor(x, levels=unique(x_1)), y = y_1))+
  geom_bar(stat = "identity")

# 主成分負荷量を求める
pcl_whole<-(t(t(pca_res_whole$rotation)*pca_res_whole$sdev) )
pcl_x_whole <- as.data.frame(pcl_whole)

# PC1を主成分負荷量の大きい順にソート
pcl_PC1_whole<-head(pcl_x_whole[order(pcl_x_whole$PC1, decreasing=T),],20)
pcl_PC2_whole<-head(pcl_x_whole[order(pcl_x_whole$PC2, decreasing=T),],20)
rownames(pcl_PC1_whole)
rownames(pcl_PC2_whole)

PC1_whole <- pca_res_whole$x[, 1]
PC2_whole <- pca_res_whole$x[, 2]
PC3_whole <- pca_res_whole$x[, 3]

ggplot(pcl_PC1_whole, aes(x = rownames(pcl_PC1_whole), y = PC1_whole)) +
  geom_bar(stat = "identity")

color_palette <- c("grey77",
  "royalblue",
  "salmon",
  "salmon",
  "salmon",
  "salmon",
  "salmon",
  "salmon",
  "salmon",
  "salmon",
  "salmon",
  "grey77",
  "salmon",
  "grey77")

ggplot(data=mydata_whole, aes(x=PC1_whole, y=PC2_whole,shape=Cell,color=Drug))+
  geom_point(size=3) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC2")
# + scale_color_manual(values = color_palette)

ggplot(data=mydata_whole, aes(x=PC1_whole, y=PC3_whole,shape=Cell,color=Drug))+
  geom_point(size=3) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC3")
