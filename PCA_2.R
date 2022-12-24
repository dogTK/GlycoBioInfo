install.packages("ggplot2")
library(ggplot2)
install.packages("devtools")
library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)
library(tidyverse)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

mydata = read_tsv('test.tsv')
t_mydata=as.data.frame(t(mydata))

gene_names=t_mydata[1,]
colnames(t_mydata) <- gene_names

delete_data <- c()
for(x in t_mydata) {
  data5 <- sapply(x[2:length(x)], as.numeric)
  data5_var <- var(data5)
  if (data5_var == 0) {
    delete_data <- c(delete_data, x[1])
  } 
}

for(x in delete_data){
  t_mydata <- select(t_mydata, -x)
}

t_mydata_as_num <- sapply(t_mydata, as.numeric)
data2 <- na.omit(t_mydata_as_num)

pca_res <- prcomp(data2, scale. = TRUE)
autoplot(pca_res, data=data2)

mydata = read_tsv('sample_2.tsv')

pca_res <- prcomp(mydata[2:ncol(mydata)], scale. = TRUE)
autoplot(pca_res, data=mydata, colour="Sample")
