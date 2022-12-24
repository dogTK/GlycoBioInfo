install.packages("ggplot2")
install.packages("devtools")
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)
install.packages("ggforce")
library(ggforce)

mydata_glyco = as.data.frame(read_tsv('master_ON_ratio_var_glycogenes.tsv'))

pca_res_glyco <- prcomp(mydata_glyco[4:408], scale. = TRUE)

PC1_glyco <- pca_res_glyco$x[, 1]
PC2_glyco <- pca_res_glyco$x[, 2]
PC3_glyco <- pca_res_glyco$x[, 3]

# 主成分寄与率を求める
v <- pca_res_glyco$sdev ^ 2
Contribution_rate_glyco <- head(v / sum(v), 10)
names(Contribution_rate_glyco)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
barplot(Contribution_rate_glyco)

# 主成分負荷量を求める
pcl<-(t(t(pca_res_glyco$rotation)*pca_res_glyco$sdev) )
pcl_x <- as.data.frame(pcl)
pcl_x

# 主成分負荷量の大きい遺伝子を求める
pcl_PC1_glyco<-head(pcl_x[order(pcl_x$PC1, decreasing=T),],20)
pcl_PC2_glyco<-head(pcl_x[order(pcl_x$PC2, decreasing=T),],20)
rownames(pcl_PC1_glyco)
rownames(pcl_PC2_glyco)

# 主成分分析（PC1-PC2）
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC2_glyco,shape=Cell,color=Drug)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC2") 

# 主成分分析（PC1-PC2）elipseあり
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC2_glyco,color=Drug)) +
  geom_mark_ellipse(aes(fill=Drug)) +
  geom_text(aes(label=Drug), hjust=0, vjust=0, nudge_x=0.5) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  theme(legend.position="none") +
  xlab("PC1") +
  ylab("PC2") 


autoplot(pca_res_glyco, data = mydata_glyco, colour = 'Drug',Label = 'gene',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

autoplot(pca_res_glyco, data = mydata_glyco, colour = 'gene', frame = TRUE, frame.type = 'norm')


# 主成分分析（PC1-PC3）
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC3_glyco,shape=Cell,color=Drug))+
  geom_point(size=3) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC3") 
