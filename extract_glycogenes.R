library(fgsea)
library(ggplot2)
library(ggfortify)

# glycogenesリストを読み出す
glycoenzonto_gmt_path<-'GlycoEnzOnto.gmt'
GlycoEnzOnto_pathways <- gmtPathways(glycoenzonto_gmt_path)

# 指定したlistの名前に入った要素を取り出す
GlycoEnzOnto_pathways$`"uridine-5'-diphosphate n-acetylgalactosamine transport"`

# listのnames属性を取得
names(GlycoEnzOnto_pathways)

# glycogenesリストのパース
for(i in GlycoEnzOnto_pathways){
  pre_glycogenes <- unique(c(glycogene, i))
}
glycogenes <- gsub("[(\")]","", pre_glycogenes)

# wholegene遺伝子マトリクスを読み出す
mydata = as.data.frame(read_tsv('master_ON_ratio_whole_genes.tsv', col_names = FALSE))
sample_names <- mydata[1,]
t_mydata=as.data.frame(t(mydata))

# wholegene遺伝子マトリクスからglycogenesだけを抽出
glycogene_list <- c()
gene_names2 <- c()
for(glycogene in glycogenes){
  for(i in t_mydata){
    if(glycogene == i[1]){
      glycogene_list <- cbind(glycogene_list, i[2:length(i)])
      gene_names2 <- c(gene_names2, i[1])
    }
  }
}
glycogene_list=as.data.frame(glycogene_list)

sample_names <- c(sample_names, as.character)
colnames(glycogene_list) <- gene_names2
rownames(glycogene_list) <- sample_names[2:140]

# glycogene_list[] <- apply(glycogene_list, 2, function(x) as.numeric(x))
# write_tsv(as.data.frame(glycogene_list), "sample_3.tsv")

glycogene_list2 <- c()
for(x in glycogene_list) {
  glycogene_list2 <- cbind(glycogene_list2, as.numeric(x))
}