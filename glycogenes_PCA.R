install.packages("ggplot2")
install.packages("devtools")
install.packages('tidyverse')
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

mydata_glyco_colnames = as.data.frame(read_tsv('master_ON_ratio_var_glycogenes_colnames.tsv'))
colnames(mydata_glyco)<-mydata_glyco_colnames[1,]

pca_res_glyco <- prcomp(mydata_glyco[4:408], scale. = TRUE)

PC1_glyco <- pca_res_glyco$x[, 1]
PC2_glyco <- pca_res_glyco$x[, 2]
PC3_glyco <- pca_res_glyco$x[, 3]
PC4_glyco <- pca_res_glyco$x[, 4]
PC5_glyco <- pca_res_glyco$x[, 5]
PC6_glyco <- pca_res_glyco$x[, 6]
PC7_glyco <- pca_res_glyco$x[, 7]
PC8_glyco <- pca_res_glyco$x[, 8]
PC9_glyco <- pca_res_glyco$x[, 9]
PC10_glyco <- pca_res_glyco$x[, 10]

# 主成分寄与率を求める
v <- pca_res_glyco$sdev ^ 2
Contribution_rate_glyco <- head((v / sum(v))*100, 10)

# 主成分寄与率を描画する
names(Contribution_rate_glyco)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
barplot(Contribution_rate_glyco, ylim=c(0,35.0))

png("Contribution_rate_glyco.png")
barplot(Contribution_rate_glyco, ylab = "Contribution Rate (%)", ylim=c(0,30))
dev.off() 

# 主成分負荷量の大きい遺伝子を求める
pcl<-(t(t(pca_res_glyco$rotation)*pca_res_glyco$sdev*100) )
pcl_x <- as.data.frame(pcl)

pcl_PC1_desc_glyco<-head(pcl_x[order(pcl_x$PC1, decreasing=T),],50)
pcl_PC2_desc_glyco<-head(pcl_x[order(pcl_x$PC2, decreasing=T),],50)
pcl_PC1_glyco<-pcl_PC1_desc_glyco$PC1
names(pcl_PC1_glyco)<-rownames(pcl_PC1_desc_glyco)

barplot(pcl_PC1_glyco, ylab = "Principal component loading (%)", ylim=c(0,100), las = 2)

write_tsv(as.data.frame(pcl_PC1_glyco), "test.tsv")

# 主成分分析（PC1-PC2）
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC2_glyco,shape=Cell,color=Drug)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC2") 

# 主成分分析（PC1-PC2）elipseあり
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC2_glyco,color=Drug)) +
  geom_mark_ellipse(aes(fill=Drug)) +
  geom_text(aes(label=Drug), hjust=0, size=2.5, vjust=0, nudge_x=0.5) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  # theme(legend.position="none") +
  xlab("PC1") +
  ylab("PC2") 

ggsave("glycogenes_PC1-PC2.png", dpi = 300)

# 主成分分析（PC1-PC3）elipseあり
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC3_glyco,color=Drug)) +
  geom_mark_ellipse(aes(fill=Drug)) +
  geom_text(aes(label=Drug), hjust=0, size=2.5, vjust=0, nudge_x=0.5) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC3") 

ggsave("glycogenes_PC1-PC3.png", dpi = 300)

# 主成分分析（PC2-PC3）elipseあり
ggplot(data=mydata_glyco, aes(x=PC2_glyco, y=PC3_glyco,color=Drug)) +
  geom_mark_ellipse(aes(fill=Drug)) +
  geom_text(aes(label=Drug), hjust=0, size=2.5, vjust=0, nudge_x=0.5) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC2") +
  ylab("PC3") 

ggsave("glycogenes_PC2-PC3.png", dpi = 300)

# 主成分分析（PC1-PC4）elipseあり
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC4_glyco,color=Drug)) +
  geom_mark_ellipse(aes(fill=Drug)) +
  geom_text(aes(label=Drug), hjust=0, size=2.5, vjust=0, nudge_x=0.5) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC4") 

ggsave("glycogenes_PC1-PC4.png", dpi = 300)

# 主成分分析（PC1-PC5）elipseあり
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC5_glyco,color=Drug)) +
  geom_mark_ellipse(aes(fill=Drug)) +
  geom_text(aes(label=Drug), hjust=0, size=2.5, vjust=0, nudge_x=0.5) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC5") 

ggsave("glycogenes_PC1-PC5.png", dpi = 300)

# 主成分分析（PC1-PC6）elipseあり
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC6_glyco,color=Drug)) +
  geom_mark_ellipse(aes(fill=Drug)) +
  geom_text(aes(label=Drug), hjust=0, size=2.5, vjust=0, nudge_x=0.5) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC6") 

ggsave("glycogenes_PC1-PC6.png", dpi = 300)

# 主成分分析（PCX-PCY）elipseあり
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC10_glyco,color=Drug)) +
  geom_mark_ellipse(aes(fill=Drug)) +
  geom_text(aes(label=Drug), hjust=0, size=2.5, vjust=0, nudge_x=0.5) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC10") 

ggsave("glycogenes_PC1-PC10.png", dpi = 300)

# autoplotモード
autoplot(pca_res_glyco, data = mydata_glyco, colour = 'Drug',Label = 'gene',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

autoplot(pca_res_glyco, data = mydata_glyco, colour = 'gene', frame = TRUE, frame.type = 'norm')


# 一番シンプルな主成分分析（PC1-PC3）
ggplot(data=mydata_glyco, aes(x=PC1_glyco, y=PC2_glyco,shape=Cell,color=Drug))+
  geom_point(size=3) +
  scale_shape_manual(values=c(15,17,18,19,21,22,7,9,10,11,12,13,14)) +
  xlab("PC1") +
  ylab("PC2") 
