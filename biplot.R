library(devtools)
install_github("vqv/ggbiplot",force = TRUE) 
library(ggbiplot)


# #サンプルファイル
# help(state.x77)
# head(state.x77)
# 
# state.x77_data<-state.x77[,c("Population", "Life Exp","Frost", "Area")] 
# head(state.x77_data)
# head(state.region)
# 
# state.x77_data_region<-transform(state.x77_data, Region=state.region)
# head(state.x77_data_region)
# 
# state.x77_data_region.pca <- prcomp(state.x77_data_region[,c(1:4)], center = TRUE,scale. = TRUE)
# 
# ggbiplot(state.x77_data_region.pca)　#①biplotで視覚化
# ggbiplot(state.x77_data_region.pca, labels=rownames(state.x77_data_region))+theme_bw()　#②①＋各プロットに州の名前を入れて、背景白にする
# ggbiplot(state.x77_data_region.pca,ellipse=TRUE,  labels=rownames(state.x77_data_region), groups=state.x77_data_region$Region)+theme_bw()+xlim(-2.5,3)　#③②＋グループによる色分け＋図の軸目盛設定（x軸）
# ggbiplot(state.x77_data_region.pca,ellipse=TRUE,choices=c(3,4), labels=rownames(state.x77_data_region), groups=state.x77_data_region$Region)+theme_bw()　#④③をPC3とPC4の2軸で表す


# glycogenesでbiplot
mydata_glyco = as.data.frame(read_tsv('master_ON_ratio_var_glycogenes.tsv'))
mydata_glyco_colnames = as.data.frame(read_tsv('master_ON_ratio_var_glycogenes_colnames.tsv'))
colnames(mydata_glyco)<-mydata_glyco_colnames[1,]

pca_res_glyco <- prcomp(mydata_glyco[4:408], scale. = TRUE)

ggbiplot(pca_res_glyco,choices=c(1,2), groups=mydata_glyco$Drug, varname.size = 1.5, varname.adjust = 6)+ #完成
xlab("PC1") +
ylab("PC2") +
scale_color_discrete(name = "Drug")

ggbiplot(pca_res_glyco,choices=c(1,2), groups=mydata_glyco$Drug, labels=mydata_glyco$Drug)+theme_bw()
ggbiplot(pca_res_glyco,ellipse=TRUE, labels=mydata_glyco$Drug, varname.adjust = 2,groups=mydata_glyco$Drug)+theme_bw()
ggbiplot(pca_res_glyco,ellipse=TRUE,choices=c(1,2), labels=mydata_glyco$Drug, groups=mydata_glyco$Drug)+theme_bw()

# wholegenesでbiplot
mydata_whole = as.data.frame(read_tsv('master_ON_ratio_var_whole_genes.tsv'))
mydata_whole_colnames = as.data.frame(read_tsv('master_ON_ratio_var_whole_genes_colnames.tsv'))
colnames(mydata_whole)<-mydata_whole_colnames[1,]

pca_res_whole <- prcomp(mydata_whole[4:19459], scale. = TRUE)

ggbiplot(pca_res_whole,choices=c(1,2), groups=mydata_whole$Drug, varname.size = 1, varname.adjust = 12)+ #完成
  xlab("PC1") +
  ylab("PC2") +
  scale_color_discrete(name = "Drug")

ggbiplot(pca_res_whole,choices=c(1,2), groups=mydata_whole$Drug, labels=mydata_whole$Drug)+theme_bw()
ggbiplot(pca_res_whole,ellipse=TRUE, labels=mydata_whole$Drug, varname.adjust = 2,groups=mydata_whole$Drug)+theme_bw()
ggbiplot(pca_res_whole,ellipse=TRUE,choices=c(1,2), labels=mydata_whole$Drug, groups=mydata_whole$Drug)+theme_bw()

