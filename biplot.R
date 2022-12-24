library(devtools)
install_github("vqv/ggbiplot",force = TRUE) 
library(ggbiplot)

help(state.x77)
head(state.x77)

state.x77_data<-state.x77[,c("Population", "Life Exp","Frost", "Area")] 
head(state.x77_data)
head(state.region)

state.x77_data_region<-transform(state.x77_data, Region=state.region)
head(state.x77_data_region)

state.x77_data_region.pca <- prcomp(state.x77_data_region[,c(1:4)], center = TRUE,scale. = TRUE)

ggbiplot(state.x77_data_region.pca)　#①biplotで視覚化
ggbiplot(state.x77_data_region.pca, labels=rownames(state.x77_data_region))+theme_bw()　#②①＋各プロットに州の名前を入れて、背景白にする
ggbiplot(state.x77_data_region.pca,ellipse=TRUE,  labels=rownames(state.x77_data_region), groups=state.x77_data_region$Region)+theme_bw()+xlim(-2.5,3)　#③②＋グループによる色分け＋図の軸目盛設定（x軸）
ggbiplot(state.x77_data_region.pca,ellipse=TRUE,choices=c(3,4), labels=rownames(state.x77_data_region), groups=state.x77_data_region$Region)+theme_bw()　#④③をPC3とPC4の2軸で表す


# こっからglycogenesでやる
ggbiplot(pca_res_glyco,choices=c(1,2), groups=mydata_glyco$Drug)+ #完成
xlab("PC1") +
ylab("PC2") 

ggbiplot(pca_res_glyco,choices=c(1,2), groups=mydata_glyco$Drug, labels=mydata_glyco$Drug)+theme_bw()
ggbiplot(pca_res_glyco,ellipse=TRUE, labels=mydata_glyco$Drug, varname.adjust = 2,groups=mydata_glyco$Drug)+theme_bw()
ggbiplot(pca_res_glyco,ellipse=TRUE,choices=c(1,2), labels=mydata_glyco$Drug, groups=mydata_glyco$Drug)+theme_bw()
