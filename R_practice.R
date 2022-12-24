library('tidyverse')
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

mydata = read_csv('ON_ratio_whole_drug.csv')

class(mydata)
view(mydata)
mydata$PLC_TC...2

t_mydata=t(mydata)


head(t_mydata[1:5])


pca_res <- prcomp(t_mydata, scale. = TRUE)

a <- as.numeric("a")
a

# data.frameを作る（表みたいなもの）
b=data.frame(number=c(1,2,3,4),name=c("Aさん","Bさん","Eさん","Dさん"))

# 実際のdata.frameの中身を見れる
View(b)

# data.flameの中から特定のカラムを取り出すのは、変数名$カラム名
b$name

# 各ベクトルを定義
vec_id <- c(1:7)
vec_namae <- c("A","B","C","D","E","F","G")
vec_by <- c(1987, 1930, 1940, 1972, 1954, 1987, 1930)
vec_gender <- c("男","女","男","男","男","女","男")
vec_admission <- c("2018-1-23", "2018-1-27", "2018-2-4",
                   "2018-3-2","2018-3-10","2018-3-12",
                   "2018-3-15")
vec_discharge <- c("2018-1-30", "2018-2-1", "2018-2-9",
                   "2018-3-3","2018-3-13","2018-3-13",
                   "2018-4-1")
vec_is_dead <- c(0,0,0,0,0,1,0)

# 定義したベクトルをdata.frameにして変数に入れる
hyou <- data.frame(
  id = vec_id,
  name = vec_namae,
  seinen = vec_by,
  seibetu = vec_gender,
  admission_date = vec_admission,
  discharge_date= vec_discharge,
  is_dead = vec_is_dead
)

# 平均や中央値や型など、カラムの情報を見れる
summary(hyou)

# NA があると関数が実行できないので、引数にna.rm(remove)をつけてTRUEやFALSEをつける
sum(1,2,3,4,5,6,NA, na.rm =FALSE)

# 作業ディレクトリを明らかにする
getwd()

# 絶対パスのファイルを読み込む
library(readr)
read_file(
  "/Users/koreedatatsuya/Desktop/研究/PCA_v2/ON_ratio_whole_sh.tsv"
)

# エクセルファイルを読み込む
install.packages("readxl")
library(readxl)
read_excel("/Users/koreedatatsuya/Desktop/研究/PCA_v2/Book1.xlsx")


factor_table <- data.frame(
  seibetu = c("男","女","男","男","男","男"),
  ketueki = c("A","B","O","O","AB","A"),
  umare = c("大阪","京都","兵庫","兵庫","京都","大阪"),
  alcohol = c("週1日以内","週3-6日","週3-6日","毎日","のまない","週1-2日")
)
summary(c("男","女","男","男","男","男"))
summary(factor_table$seibetu)

string_table <- data.frame(
  seibetu = c("男","女","男","男","男","男"),
  ketueki = c("A","B","O","O","AB","A"),
  umare = c("大阪","京都","兵庫","兵庫","京都","大阪"),
  alcohol = c("週1日以内","週3-6日","週3-6日","毎日","のまない","週1-2日"),
  stringsAsFactors = FALSE
)

summary(string_table)

factor_table$umare