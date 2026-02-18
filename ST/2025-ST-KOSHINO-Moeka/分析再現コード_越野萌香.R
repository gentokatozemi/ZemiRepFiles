## なぜ国内において外国人観光客の受け入れ意向に差があるのか ##

## 更新履歴：
## 2026/01/31 越野萌香（提出版）
## 2026/02/06 加藤言人（最終調整）

## 環境をクリーンにする
rm(list = ls())

## The directory of this file
dirhere <- dirname(rstudioapi::getActiveDocumentContext()$path) 
## ワーキングディレクトリをセット
setwd(dirhere)

## 図表出力用のフォルダ（out）を作成
if (!"./out" %in% list.dirs()) dir.create("./out")

## ワーキングディレクトリを確認
getwd() 
options(scipen = 999)

##############################
## データセットのインポート ##
##############################

## 都民の観光に対する意識調査データ
## 令和5年度（2023年）データ
##（以下から入手：ダウンロード日２０２５年６月１９日）
## https://data.tourism.metro.tokyo.lg.jp/data/ishiki/ 

library(readxl)
dt <- read_xlsx("【R5】都民の観光に対する意識調査.xlsx") 

## データの中身をチェック
names(dt)

##########
## 変数 ##
##########

## 分析用の新しいデータの作成
dn <- data.frame(id = 1:nrow(dt))

## 従属変数（観光客の受け入れ意向)
# 1    積極的
# 2    どちらかといえば積極的
# 3    どちらともいえない
# 4    どちらかといえば消極的
# 5    消極的
table(dt$`観光客の受入（外国人）`) # 分布をチェック

### 新しいデータに値を逆転させて挿入
dn$ukeire <- 5 - dt$`観光客の受入（外国人）`
table(dn$ukeire)

###======================================================================

##独立変数

##（回答者の職業）
table(dt$`雇用形態・職種`)
dn$shokugyo <- NA
dn$shokugyo[which(dt$`雇用形態・職種`==1)] <- 1
dn$shokugyo[which(dt$`雇用形態・職種`%in%c(2,3,4))] <- 2
dn$shokugyo[which(dt$`雇用形態・職種`==5)] <- 3
dn$shokugyo[which(dt$`雇用形態・職種`%in%c(6,7,8))] <- 4
dn$shokugyo <- 
  factor(dn$shokugyo, 
         levels = c(1,2,3,4),
         labels = c("正規","非正規","自由業","無職"))
table(dn$shokugyo, exclude = F)

### 旅行意向
### ※過去の旅行頻度についてはR3、R4調査でしか聞いていない
# 1	これまで以上に旅行したい
# 2	これまでと同様程度の頻度で旅行したい
# 3	これまでのようには旅行に行きたくない（旅行頻度、回数を減らしたい）
# 4	全く旅行に行きたくない
# 5	わからない
table(dt$`旅行意向（国内旅行）`)
table(dt$`旅行意向（海外観光）`)
## とりあえず、旅行したい人を１、それ以外を0とする
dn$kokunai_iko <- NA
dn$kokunai_iko[which(dt$`旅行意向（国内旅行）`%in%c(1,2))] <- 1
dn$kokunai_iko[which(dt$`旅行意向（国内旅行）`%in%c(3,4,5))] <- 0
dn$kaigai_iko <- NA
dn$kaigai_iko[which(dt$`旅行意向（海外観光）`%in%c(1,2))] <- 1
dn$kaigai_iko[which(dt$`旅行意向（海外観光）`%in%c(3,4,5))] <- 0

dn$kaigai_iko_factor <- 
  factor(dn$kaigai_iko, 
         levels = c(0,1),
         labels = c("それ以外","旅行したい"))
table(dn$kaigai_iko, exclude = F)

## 図６の出力
library(ggplot2)
ggplot(dn, aes(x=kaigai_iko_factor, y=after_stat(count/sum(count)))) + 
  geom_bar(color = "white") + 
  labs(x = "回答者の旅行意向", 
       y="回答割合") +
  theme_minimal()

ggsave("./out/kmfig6.png", width=5, height=3) ## 保存

### ※職種はR4、R3調査でしか聞いていない

### 居住地
table(dt$居住地)
dn$kyozyuti <- NA
dn$kyozyuti[which(dt$`居住地`%in%c(1,2,3,4,5))] <- 1
dn$kyozyuti[which(dt$`居住地`%in%c(16,17,18,19,21))] <- 2
dn$kyozyuti[which(dt$`居住地`%in%c(6,7,8,22,23))] <- 3
dn$kyozyuti[which(dt$`居住地`%in%c(12,13,14,15,20))] <- 4
dn$kyozyuti[which(dt$`居住地`%in%c(9,10,11))] <- 5
dn$kyozyuti[which(dt$`居住地`%in%c(24~27,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,49,50))] <- 6
dn$kyozyuti[which(dt$`居住地`%in%c(28,48,51,52,53))] <- 7
dn$kyozyuti[which(dt$`居住地`%in%c(54,55,56,57,58,59,60,61,62))] <- 8

dn$kyozyuti <- 
  factor(dn$kyozyuti, 
         levels = c(1,2,3,4,5,6,7,8),
         labels = c("都心部","23区北部","23区東部","23区西部","23区南部","多摩東部","多摩西部","島しょ部"))
table(dn$kyozyuti, exclude = F)

## 図４の出力
library(ggplot2)
ggplot(dn, aes(x=kyozyuti, y=after_stat(count/sum(count)))) + 
  geom_bar(color = "white") + 
  labs(x = "回答者の居住地", 
       y="回答割合") +
  theme_minimal()

ggsave("./out/kmfig4.png", width=6, height=4) ## 保存

## 在留外国人数データ
## 2023年12月時点での在留外国人統計（都道府県市町村別）
##（以下から入手：ダウンロード日２０２５年７月２６日）
## https://www.moj.go.jp/isa/policies/statistics/toukei_ichiran_touroku.html
gt <- read_xlsx("23-12-a.xlsx",skip=1)
head(gt)
colnames(gt) <- c("mcode","mname","zairyu") #列名をシンプルにする。

library(readxl)
library(ggplot2)
library(dplyr)

# 1. Excelの読み込み（1行目が列名）
df <- read_excel("23-12-a.xlsx", sheet = 1, col_names = TRUE,skip = 1)

# 2. 列名を確認（必要に応じて調整）
names(df)

# 3. 東京都の市区町村部分だけ抽出（千代田区〜小笠原村）
# 列名は仮に以下のようになっていると仮定します（実際の名前で置き換えてください）
# 例： "市区町村名" と "在留外国人数"
tokyo_cities <- c("千代田区", "中央区", "港区", "新宿区", "文京区", "台東区", "墨田区", "江東区",
                  "品川区","目黒区","大田区","世田谷区","渋谷区", "中野区", "杉並区", "豊島区",
                  "北区", "荒川区", "板橋区", "練馬区", "足立区", "葛飾区", "江戸川区",
                  "八王子市", "立川市", "武蔵野市", "三鷹市", "青梅市", "府中市", "昭島市", "調布市",
                  "町田市", "小金井市", "小平市", "日野市", "東村山市", "国分寺市", "国立市",
                  "福生市", "狛江市", "東大和市", "清瀬市", "東久留米市", "武蔵村山市",
                  "多摩市", "稲城市", "羽村市", "あきる野市", "西東京市",
                  "瑞穂町", "日の出町", "檜原村", "奥多摩町",
                  "大島町", "利島村", "新島村", "神津島村", "三宅村", "御蔵島村", "八丈町", "青ヶ島村",
                  "小笠原村")


filtered <- df %>% 
  filter(都道府県市区町村 %in% tokyo_cities)

## 図２の出力（X=市区町村、Y=在留外国人数）
ggplot(filtered, aes(x = reorder(都道府県市区町村, -在留外国人), y = 在留外国人)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(
    title = "東京都 市区町村別 在留外国人数",
    x = "市区町村",
    y = "在留外国人数"
  ) +
  theme_minimal(base_size = 9) +
  coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("./out/kmfig2.png", width=7, height=8) ## 保存

colnames(filtered)
nrow(filtered)
head(filtered)

## まずは市区町村コードをdnに作成
dn$mcode <- NA
# 1	千代田区
dn$mcode[which(dt$居住地==1)] <- gt$mcode[which(gt$mname=="千代田区")]
# 2	中央区
dn$mcode[which(dt$居住地==2)] <- gt$mcode[which(gt$mname=="中央区")]
# 3	港区
dn$mcode[which(dt$居住地==3)] <- gt$mcode[which(gt$mname=="港区")]
# 4	新宿区
dn$mcode[which(dt$居住地==4)] <- gt$mcode[which(gt$mname=="新宿区")]
# 5	文京区
dn$mcode[which(dt$居住地==5)] <- gt$mcode[which(gt$mname=="中央区")]
# 6	台東区
dn$mcode[which(dt$居住地==6)] <- gt$mcode[which(gt$mname=="台東区")]
# 7	墨田区
dn$mcode[which(dt$居住地==7)] <- gt$mcode[which(gt$mname=="墨田区")]
# 8	江東区
dn$mcode[which(dt$居住地==8)] <- gt$mcode[which(gt$mname=="江東区")]
# 9	品川区
dn$mcode[which(dt$居住地==9)] <- gt$mcode[which(gt$mname=="品川区")]
# 10	目黒区
dn$mcode[which(dt$居住地==10)] <- gt$mcode[which(gt$mname=="目黒区")]
# 11	大田区
dn$mcode[which(dt$居住地==11)] <- gt$mcode[which(gt$mname=="大田区")]
# 12	世田谷区
dn$mcode[which(dt$居住地==12)] <- gt$mcode[which(gt$mname=="世田谷区")]
# 13	渋谷区
dn$mcode[which(dt$居住地==13)] <- gt$mcode[which(gt$mname=="渋谷区")]
# 14	中野区
dn$mcode[which(dt$居住地==14)] <- gt$mcode[which(gt$mname=="中野区")]
# 15	杉並区
dn$mcode[which(dt$居住地==15)] <- gt$mcode[which(gt$mname=="杉並区")]
# 16	豊島区
dn$mcode[which(dt$居住地==16)] <- gt$mcode[which(gt$mname=="中央区")]
# 17	北区
dn$mcode[which(dt$居住地==17)] <- gt$mcode[which(gt$mname=="中央区")]
# 18	荒川区
dn$mcode[which(dt$居住地==18)] <- gt$mcode[which(gt$mname=="荒川区")]
# 19	板橋区
dn$mcode[which(dt$居住地==19)] <- gt$mcode[which(gt$mname=="板橋区")]
# 20	練馬区
dn$mcode[which(dt$居住地==20)] <- gt$mcode[which(gt$mname=="練馬区")]
# 21	足立区
dn$mcode[which(dt$居住地==21)] <- gt$mcode[which(gt$mname=="足立区")]
# 22	葛飾区
dn$mcode[which(dt$居住地==22)] <- gt$mcode[which(gt$mname=="葛飾区")]
# 23	江戸川区
dn$mcode[which(dt$居住地==23)] <- gt$mcode[which(gt$mname=="江戸川区")]
# 24	八王子市
dn$mcode[which(dt$居住地==24)] <- gt$mcode[which(gt$mname=="八王子市")]
# 25	立川市
dn$mcode[which(dt$居住地==25)] <- gt$mcode[which(gt$mname=="立川市")]
# 26	武蔵野市
dn$mcode[which(dt$居住地==26)] <- gt$mcode[which(gt$mname=="武蔵野市")]
# 27	三鷹市
dn$mcode[which(dt$居住地==27)] <- gt$mcode[which(gt$mname=="三鷹市")]
# 28	青梅市
dn$mcode[which(dt$居住地==28)] <- gt$mcode[which(gt$mname=="青梅市")]
# 29	府中市
dn$mcode[which(dt$居住地==29)] <- gt$mcode[which(gt$mname=="府中市")]
# 30	昭島市
dn$mcode[which(dt$居住地==30)] <- gt$mcode[which(gt$mname=="昭島市")]
# 31	調布市
dn$mcode[which(dt$居住地==31)] <- gt$mcode[which(gt$mname=="調布市")]
# 32	町田市
dn$mcode[which(dt$居住地==32)] <- gt$mcode[which(gt$mname=="町田市")]
# 33	小金井市
dn$mcode[which(dt$居住地==33)] <- gt$mcode[which(gt$mname=="小金井市")]
# 34	小平市
dn$mcode[which(dt$居住地==34)] <- gt$mcode[which(gt$mname=="小平市")]
# 35	日野市
dn$mcode[which(dt$居住地==35)] <- gt$mcode[which(gt$mname=="日野市")]
# 36	東村山市
dn$mcode[which(dt$居住地==36)] <- gt$mcode[which(gt$mname=="東村山市")]
# 37	国分寺市
dn$mcode[which(dt$居住地==37)] <- gt$mcode[which(gt$mname=="国分寺市")]
# 38	国立市
dn$mcode[which(dt$居住地==38)] <- gt$mcode[which(gt$mname=="国立市")]
# 39	福生市
dn$mcode[which(dt$居住地==39)] <- gt$mcode[which(gt$mname=="福生市")]
# 40	狛江市
dn$mcode[which(dt$居住地==40)] <- gt$mcode[which(gt$mname=="狛江市")]
# 41	東大和市
dn$mcode[which(dt$居住地==41)] <- gt$mcode[which(gt$mname=="東大和市")]
# 42	清瀬市
dn$mcode[which(dt$居住地==42)] <- gt$mcode[which(gt$mname=="清瀬市")]
# 43	東久留米市
dn$mcode[which(dt$居住地==43)] <- gt$mcode[which(gt$mname=="東久留米市")]
# 44	武蔵村山市
dn$mcode[which(dt$居住地==44)] <- gt$mcode[which(gt$mname=="武蔵村山市")]
# 45	多摩市
dn$mcode[which(dt$居住地==45)] <- gt$mcode[which(gt$mname=="多摩市")]
# 46	稲城市
dn$mcode[which(dt$居住地==46)] <- gt$mcode[which(gt$mname=="稲城市")]
# 47	羽村市
dn$mcode[which(dt$居住地==47)] <- gt$mcode[which(gt$mname=="羽村市")]
# 48	あきる野市
dn$mcode[which(dt$居住地==48)] <- gt$mcode[which(gt$mname=="あきる野市")]
# 49	西東京市
dn$mcode[which(dt$居住地==49)] <- gt$mcode[which(gt$mname=="西東京市")]
# 50	瑞穂町
dn$mcode[which(dt$居住地==50)] <- gt$mcode[which(gt$mname=="瑞穂町")]
# 51	日の出町
dn$mcode[which(dt$居住地==51)] <- gt$mcode[which(gt$mname=="日の出町")]
# 52	檜原村
dn$mcode[which(dt$居住地==52)] <- gt$mcode[which(gt$mname=="檜原村")]
# 53	奥多摩町
dn$mcode[which(dt$居住地==53)] <- gt$mcode[which(gt$mname=="奥多摩町")]
# 54	大島町
dn$mcode[which(dt$居住地==54)] <- gt$mcode[which(gt$mname=="大島町")]
# 55	利島村
dn$mcode[which(dt$居住地==55)] <- gt$mcode[which(gt$mname=="利島村")]
# 56	新島村
dn$mcode[which(dt$居住地==56)] <- gt$mcode[which(gt$mname=="新島村")]
# 57	神津島村
dn$mcode[which(dt$居住地==57)] <- gt$mcode[which(gt$mname=="神津島村")]
# 58	三宅村
dn$mcode[which(dt$居住地==58)] <- gt$mcode[which(gt$mname=="中央区")]
# 59	御蔵島村
dn$mcode[which(dt$居住地==59)] <- gt$mcode[which(gt$mname=="中央区")]
# 60	八丈町
dn$mcode[which(dt$居住地==60)] <- gt$mcode[which(gt$mname=="八丈町")]
# 61	青ヶ島村
dn$mcode[which(dt$居住地==61)] <- gt$mcode[which(gt$mname=="青ヶ島村")]
# 62	小笠原村
dn$mcode[which(dt$居住地==62)] <- gt$mcode[which(gt$mname=="小笠原村")]
# 63	東京以外の道府県（は欠損）

## 市区町村名（市区町村コードを利用）
dn$mname <- gt$mname[match(dn$mcode,gt$mcode)]

## 在留外国人数（市区町村コードを利用）
dn$zairyu <- gt$zairyu[match(dn$mcode,gt$mcode)]
hist(dn$zairyu)

## 2020年度時点での各市町村の総人口
##（以下から入手：ダウンロード日２０２５年７月２６日）
## https://www.e-stat.go.jp/regional-statistics/ssdsview/municipality
mpopdt <- read_xlsx("FEI_CITY_250726230426.xlsx", skip = 5)
head(mpopdt) 
colnames(mpopdt) <- c("mcode","mname","cat","pop","popx","jpop")
mpopdt$pop <- as.numeric(gsub(",","",mpopdt$pop))
mpopdt$jpop <- as.numeric(gsub(",","",mpopdt$jpop))

## 市区町村人口（市区町村コードを利用）
dn$jinko <- mpopdt$pop[match(dn$mcode,mpopdt$mcode)]
hist(dn$jinko, main = "人口のヒストグラム", xlab = "市区町村の人口規模（人）", ylab = "対象者の人数")

## 図５の出力　居住市区町村の在留外国人比率（在留外国人数／人口＊100）
dn$zairyu_hiritsu <- dn$zairyu/dn$jinko*100
# hist(dn$zairyu_hiritsu)
# hist(dn$zairyu_hiritsu, main = "在留外国人比率", xlab = "頻度(%)", ylab = "人口規模（人）")
ggplot(dn, aes(x=zairyu_hiritsu)) + 
  geom_histogram(color="white", bins = 15) +
  labs(title="在留外国人比率のヒストグラム",
       x="市区町村人口あたりの外国人の比率（％）",
       y="対象者の人数") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))
ggsave("./out/kmfig5.png", width=5, height=3)


###===================================================================

## 統制変数

### 性別
table(dt$性別, exclude = F)
dn$josei <- NA
dn$josei[which(dt$性別==1)] <- 0
dn$josei[which(dt$性別==2)] <- 1
table(dn$josei)

## 図８
library(ggplot2)
ggplot(dn, aes(x = factor(josei))) +
  geom_bar() +
  scale_x_discrete(
    labels = c("0" = "男性", "1" = "女性", "NA" = "不明")
  ) +
  labs(
    x = "性別",
    y = "人数"
  ) +
  theme_minimal()
ggsave("./out/kmfig8.png", width=5, height=3)


### 年齢
table(dt$年齢, exclude = F)
dn$nenrei10 <- dt$年齢/10
table(dn$nenrei10)
dn$nenrei10sq <- dn$nenrei10^2 ## 年齢の二乗
table(dn$nenrei10sq)

## 図９
library(ggplot2)
ggplot(dn, aes(x = nenrei10)) +
  geom_histogram( binwidth = 0.5, boundary = 0, closed = "left", color = "white", linewidth = 0.3) +
  scale_x_continuous(
    breaks = seq(2, 8, by = 1),
    labels = function(x) paste0(x * 10, "")
  ) +
  labs(
    x = "年齢",
    y = "人数"  ) +
  theme_minimal()
ggsave("./out/kmfig9.png", width=5, height=3)

###国内観光客の受け入れ
table(dt$`観光客の受入（国内）`)
# 1    積極的
# 2    どちらかといえば積極的
# 3    どちらともいえない
# 4    どちらかといえば消極的
# 5    消極的
dn$ukeire2 <- 5 - dt$`観光客の受入（国内）`
table(dn$ukeire2)

## 図１０
library(ggplot2)
ggplot(dn, aes(x = factor(
  ukeire2,
  levels = c(0, 1, 2, 3, 4),
  labels = c(
    "消極的",
    "どちらか\nといえば\n消極的",
    "どちらとも\nいえない",
    "どちらか\nといえば\n積極的",
    "積極的"
  )
))) +
  geom_bar() +
  labs(
    x = "国内観光客の受け入れ意識",
    y = "人数"
  ) +
  theme_minimal() #+
  # theme(
  #   axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
  # )
ggsave("./out/kmfig10.png", width=5, height=3)

## 記述統計（本文では使われていない）
# summary_stats <- c(
#   Mean     = mean(dt$性別, na.rm = TRUE),
#   SD       = sd(dt$性別, na.rm = TRUE),
#   Min      = min(dt$性別, na.rm = TRUE),
#   Max      = max(dt$性別, na.rm = TRUE),
#   NA_Count = sum(is.na(dt$性別))
# )
# 
# truncate_2dp <- function(value) {
#   trunc(value * 100) / 100
# }
# 
# summary_stats
# 
# summary_stats2 <- c(
#   Mean     = mean(dt$年齢, na.rm = TRUE),
#   SD       = sd(dt$年齢, na.rm = TRUE),
#   Min      = min(dt$年齢, na.rm = TRUE),
#   Max      = max(dt$年齢, na.rm = TRUE),
#   NA_Count = sum(is.na(dt$年齢)))
# 
# summary_stats2
# 
# summary_stats3 <- c(
#   Mean     = mean(dt$`観光客の受入（国内）`, na.rm = TRUE),
#   SD       = sd(dt$`観光客の受入（国内）`, na.rm = TRUE),
#   Min      = min(dt$`観光客の受入（国内）`, na.rm = TRUE),
#   Max      = max(dt$`観光客の受入（国内）`, na.rm = TRUE),
#   NA_Count = sum(is.na(dt$`観光客の受入（国内）`)))
# 
# summary_stats3

##################
## 変数のグラフ ##
##################

## 従属変数の分布図（図７の出力）
library(ggplot2)
ggplot(dn, aes(x=ukeire, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 5, color = "white") + 
  labs(x = "外国人観光客受け入れ態度\n（0＝消極的～４＝積極的）", 
       y="回答割合") +
  theme_minimal()
ggsave("./out/kmfig7.png", width=5, height=3)

## 独立変数の分布図（図３の出力）
library(ggplot2)
ggplot(dn, aes(x=shokugyo, y=after_stat(count/sum(count)))) + 
  geom_bar(color = "white") + 
  labs(x = "回答者の雇用形態", 
       y="回答割合") +
  theme_minimal()
ggsave("./out/kmfig3.png", width=5, height=3)

##########
## 分析 ##
##########

## 交差項なし（基本モデル）
m1b <- lm(ukeire ~ kokunai_iko +　kaigai_iko + zairyu_hiritsu + 
            shokugyo, 
          data = na.omit(dn))

## 交差項なし（交絡変数）
m1 <- lm(ukeire ~  kokunai_iko + kaigai_iko +  zairyu_hiritsu + 
           shokugyo + 
           josei + nenrei10 + ukeire2, 
         data = na.omit(dn))

## 交差項なし（基本モデル）
m2b <- lm(ukeire ~ kokunai_iko + kaigai_iko + zairyu_hiritsu + 
            kokunai_iko*shokugyo + 
            kaigai_iko*shokugyo + 
            zairyu_hiritsu*shokugyo, 
          data = na.omit(dn))

## 交差項なし（交絡変数）
m2 <- lm(ukeire ~  kokunai_iko + kaigai_iko +  zairyu_hiritsu + 
           kokunai_iko*shokugyo + 
           kaigai_iko*shokugyo + 
           zairyu_hiritsu*shokugyo + 
           josei + nenrei10 + ukeire2, 
         data = na.omit(dn))

## 簡易的な結果
library(texreg)
screenreg(list(m1b, m1, m2b, m2),
          digits=3, stars = c(0.001,0.01,0.05,0.1), symbol="+")

## もっときれいな結果
### 変数ラベル
vnmap <- list("(Intercept)" = "（定数項）",
              "kokunai_iko" = "国内旅行意向",
              "kokunai_iko:shokugyo非正規" = "国内旅行意向＊非正規",
              "kokunai_iko:shokugyo自由業" = "国内旅行意向＊自由業",
              "kokunai_iko:shokugyo無職" = "国内旅行意向＊無職",
              "kaigai_iko" = "海外旅行意向",
              "kaigai_iko:shokugyo非正規" = "海外旅行意向＊非正規",
              "kaigai_iko:shokugyo自由業" = "海外旅行意向＊自由業",
              "kaigai_iko:shokugyo無職" = "海外旅行意向＊無職",
              "kaigai_iko旅行したい" = "海外旅行意向",
              "kaigai_iko旅行したい:shokugyo非正規" = "海外旅行意向＊非正規",
              "kaigai_iko旅行したい:shokugyo自由業" = "海外旅行意向＊自由業",
              "kaigai_iko旅行したい:shokugyo無職" = "海外旅行意向＊無職",
              "zairyu_hiritsu" = "在留外国人％",
              "zairyu_hiritsu:shokugyo非正規" = "外国人％＊非正規",
              "zairyu_hiritsu:shokugyo自由業" = "外国人％＊自由業",
              "zairyu_hiritsu:shokugyo無職" = "外国人％＊無職",
              "shokugyo非正規" = "職業（非正規）",
              "shokugyo自由業" = "職業（自由業）",
              "shokugyo無職" = "職業（無職）",
              "josei" = "性別（女性）",
              "nenrei10" = "年齢（10歳刻み）",
              "ukeire2" = "国内観光客受け入れ意向",
              "jinko")

### HTMLで表を出力（表１の出力）
library(htmltools)
html <- htmlreg(list(m1b, m1, m2b, m2), include.ci = FALSE,
  digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
  symbol = "+", star.symbol = "*", inline.css = FALSE,
  html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
  single.row = TRUE, caption = "",
  custom.coef.map = vnmap,
  custom.header = list("仮説1・2・3" = 1:2, "条件付き仮説1・2" = 3:4),
  custom.model.names = c("基本", "拡張", "基本", "拡張"),
  custom.note = "%stars。OLS回帰分析結果。括弧内は標準誤差。"
)

browsable(tagList(tags$head(tags$style(HTML("table {line-height: 1.1;}th, td {padding-top: 2px;padding-bottom: 2px;}"))),HTML(html)))

## 表をoutに保存する
save_html(tagList(tags$head(tags$style(HTML("table {line-height: 1.1;}th, td {padding-top: 2px;padding-bottom: 2px;}"))),HTML(html)),
           "./out/kmtab1.html")
library(webshot2)
webshot(url="./out/kmtab1.html", file="./out/kmtab1.png", zoom=3, selector="table")

## 予測値 ##

###########################################################
## 予測値の算出（必要なければ内容を変更しないこと）########
##（ここから）#############################################
genpr <- function(dpr, # 予測値算出用データ（元データ）
                  mpr, # 予測値算出用分析結果
                  setx, # 独立変数の名前
                  setxvals, # 独立変数の値設定（numeric/character）
                  setxlabs=NULL, # 独立変数ラベル（カテゴリ変数の場合）
                  setm=NULL, # 条件付け変数の名前
                  setmvals=NULL, # 条件付変数のシミュレーション用値（list）
                  datalab=NULL) { # データにラベル
  
  ## Generate simulation data
  simdt <- na.omit(dpr[,all.vars(mpr$terms)])
  ## Extract all x values (if more than 30, limit to 30)
  simx <- setxvals
  
  ## Summarize simulation values (if m not available)
  if (is.null(setm)) {
    
    simv <- data.frame(simx = simx)
    
    ## Setting m values (if available)
  } else {
    
    ## Extract all m values
    simm <- sapply(setmvals, function(x) x[1])
    names(simm) <- NULL
    ## Summarize simulation values
    simv <- data.frame(simx = rep(simx,each=length(simm)), 
                       simm = simm)
    
  }
  
  ## Export predictions
  prout <- as.data.frame(t(apply(simv, 1, function(k) {
    tmpdt <- simdt
    tmpdt[,setx] <- k[1]
    if (!is.null(setm)) tmpdt[,setm] <- k[2]
    if (is.character(tmpdt[,setm]) & is.numeric(setxvals)) {
      tmpdt[,setx] <- as.numeric(tmpdt[,setx])
    }
    tmp <- colMeans(as.data.frame(predict(mpr, newdata=tmpdt, se.fit=TRUE)))
    tmp <- c(k, tmp[1:2], 
             tmp[1]-tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]-tmp[2]*qt(0.95,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.95,df=mpr$df[1]))
    if (is.null(setm)) {
      names(tmp) <- c("x","pr","se",
                      "lo95","up95","lo90","up90")
    } else {
      names(tmp) <- c("x","m","pr","se",
                      "lo95","up95","lo90","up90")
    }
    return(tmp)
  })))
  
  ## Convert character to numeric (if necessary)
  if (class(prout$pr)=="character") {
    prout$pr <- as.numeric(prout$pr)
    prout$se <- as.numeric(prout$se)
    prout$lo95 <- as.numeric(prout$lo95)
    prout$up95 <- as.numeric(prout$up95)
    prout$lo90 <- as.numeric(prout$lo90)
    prout$up90 <- as.numeric(prout$up90)
    if (class(simdt[,setx])=="factor") {
      prout$x <- factor(prout$x, levels=levels(simdt[,setx]))
    }
    if (class(simdt[,setm])=="factor") {
      prout$m <- factor(prout$m, levels=levels(simdt[,setm]))
    }
  }
  
  ## x values to numeric if necessary
  if (is.character(prout$x) & is.numeric(setxvals)) {
    prout$x <- as.numeric(prout$x)
  }
  
  ### Assign labels to x
  if(!is.null(setxlabs)) {
    prout$labelledx <- 
      factor(prout$x,levels=setxvals,labels=setxlabs)
  }
  ### Assign labels to m
  if (!is.null(setm)) {
    prout$labelledm <- 
      factor(names(setmvals)[match(prout$m,unlist(setmvals))],
             levels = names(setmvals))
  }
  ### Assign common label to data
  if (!is.null(datalab)) {
    prout$datalab <- datalab
  }
  
  return(prout)
}
##（ここまで）#############################################

## 条件なし ##
## 予算値の算出
## 図１１
yosokuout <- genpr(dpr = na.omit(dn),
                   mpr = m1b,
                   setx = "shokugyo", 
                   setxvals = levels(dn$shokugyo))
yosokuout$x <- factor(yosokuout$x, unique(yosokuout$x))
yosokuout$pr <- as.numeric(yosokuout$pr)
yosokuout$lo90 <- as.numeric(yosokuout$lo90)
yosokuout$up90 <- as.numeric(yosokuout$up90)
yosokuout$lo95 <- as.numeric(yosokuout$lo95)
yosokuout$up95 <- as.numeric(yosokuout$up95)

## 予測値をプロットする
ggplot(yosokuout, aes(x=x, y=as.numeric(pr))) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95), 
                linewidth=1, width = 0.2) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90), 
                linewidth=1.5, width = 0) + 
  geom_point(size = 2.5) + 
  labs(x="回答者の職業", 
       y="外国人観光客受け入れ意向\nの予測値平均",
       caption = "注：基本モデルを使用。縦線は、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig11.png", width=6, height=4)

## 予測値の算出
## 図１２
yosokuout <- genpr(dpr = na.omit(dn),
                   mpr = m1,
                   setx = "shokugyo", 
                   setxvals = levels(dn$shokugyo))
yosokuout$x <- factor(yosokuout$x, unique(yosokuout$x))
yosokuout$pr <- as.numeric(yosokuout$pr)
yosokuout$lo90 <- as.numeric(yosokuout$lo90)
yosokuout$up90 <- as.numeric(yosokuout$up90)
yosokuout$lo95 <- as.numeric(yosokuout$lo95)
yosokuout$up95 <- as.numeric(yosokuout$up95)

## 予測値をプロットする
ggplot(yosokuout, aes(x=x, y=as.numeric(pr))) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95), 
                linewidth=1, width = 0.2) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90), 
                linewidth=1.5, width = 0) + 
  geom_point(size = 2.5) + 
  labs(x="回答者の職業", 
       y="外国人観光客受け入れ意向\nの予測値平均",
       caption = "注：拡張モデルを使用。縦線は、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig12.png", width=6, height=4)

## 独立変数：在留外国人比率 ##
## 図１３
summary(dn$zairyu_hiritsu)
yosokuout1D <- genpr(dpr = na.omit(dn),
                     mpr = m1b,
                     setx = "zairyu_hiritsu", 
                     setxvals = seq(0.3,12.83,by=0.1))

## 予測値をプロットする
ggplot(yosokuout1D) + 
  # geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
  #               width=0.1, linewidth=0.5) +
  # geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
  #               width=0, linewidth=2) +
  # geom_point(aes(x=as.factor(x), y=pr),color="white") +
  # scale_x_discrete() + ## labelsを設定する
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) +
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) +
  geom_line(aes(x=x, y=pr)) +
  labs(x="在留外国人比率", 
       y="外国人観光客受け入れ意向（０〜４）\nの予測値平均",
       caption = "注：基本モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig13.png", width=6, height=4)

## 独立変数：在留外国人比率2
## 図１４
summary(dn$zairyu_hiritsu)
yosokuout1D <- genpr(dpr = na.omit(dn),
                     mpr = m1,
                     setx = "zairyu_hiritsu", 
                     setxvals = seq(0.3,12.83,by=0.1))

## 予測値をプロットする
ggplot(yosokuout1D) + 
  # geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
  #               width=0.1, linewidth=0.5) +
  # geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
  #               width=0, linewidth=2) +
  # geom_point(aes(x=as.factor(x), y=pr),color="white") +
  # scale_x_discrete() + ## labelsを設定する
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) +
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) +
  geom_line(aes(x=x, y=pr)) +
  labs(x="在留外国人比率", 
       y="外国人観光客受け入れ意向（０〜４）\nの予測値平均",
       caption = "注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig14.png", width=6, height=4)

## 独立変数：国内旅行意向 ##
## 図１５
yosokuout1A <- genpr(dpr = na.omit(dn),
                     mpr = m1b,
                     setx = "kokunai_iko", 
                     setxvals = c(0,1))

## 予測値をプロットする
ggplot(yosokuout1A) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr),color="white") +
  scale_x_discrete(labels = c("行きたくない（0）","行きたい（1）")) + ## labelsを設定する
  # geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) +
  # geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) +
  # geom_line(aes(x=x, y=pr)) +
  labs(x="国内旅行意向", 
       y="外国人観光客受け入れ意向（０〜４）\nの予測値平均",
       caption = "注：基本モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig15.png", width=6, height=4)

## 独立変数：国内旅行意向2
## 図１７
yosokuout1A <- genpr(dpr = na.omit(dn),
                     mpr = m1,
                     setx = "kokunai_iko", 
                     setxvals = c(0,1))

## 予測値をプロットする
ggplot(yosokuout1A) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr),color="white") +
  scale_x_discrete(labels = c("行きたくない（0）","行きたい（1）")) + ## labelsを設定する
  # geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) +
  # geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) +
  # geom_line(aes(x=x, y=pr)) +
  labs(x="国内旅行意向", 
       y="外国人観光客受け入れ意向（０〜４）\nの予測値平均",
       caption = "注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig17.png", width=6, height=4)

## 独立変数：海外旅行意向 ##
## 図１６
yosokuout1B <- genpr(dpr = na.omit(dn),
                     mpr = m1b,
                     setx = "kaigai_iko", 
                     setxvals = c(0,1))

## 予測値をプロットする
ggplot(yosokuout1B) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr),color="white") +
  scale_x_discrete(labels = c("行きたくない（0）","行きたい（1）")) + ## labelsを設定する
  # geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) +
  # geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) +
  # geom_line(aes(x=x, y=pr)) +
  labs(x="海外旅行意向", 
       y="外国人観光客受け入れ意向（０〜４）\nの予測値平均",
       caption = "注：基本モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig16.png", width=6, height=4)

## 独立変数：海外旅行意向2
## 図１８
yosokuout1B <- genpr(dpr = na.omit(dn),
                     mpr = m1,
                     setx = "kaigai_iko", 
                     setxvals = c(0,1))

## 予測値をプロットする
ggplot(yosokuout1B) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr),color="white") +
  scale_x_discrete(labels = c("行きたくない（0）","行きたい（1）")) + ## labelsを設定する
  # geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) +
  # geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) +
  # geom_line(aes(x=x, y=pr)) +
  labs(x="海外旅行意向", 
       y="外国人観光客受け入れ意向（０〜４）\nの予測値平均",
       caption = "注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig18.png", width=6, height=4)


## 職業による条件付け ##

## 独立変数：在留外国人比率 ##

## 予測値の算出
## 図１９
yosokuout2D <- genpr(dpr = dn,
                     mpr = m2b,
                     setx = "zairyu_hiritsu", 
                     setxvals = seq(0.3,12.83,by=0.1),
                     setm = "shokugyo",
                     setmvals = list("正規" = "正規",
                                     "非正規" = "非正規",
                                     "自由業" = "自由業",
                                     "無職" = "無職"))

## 予測値をプロットする
ggplot(yosokuout2D, aes(x=x, y=pr)) + 
  # geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95,
  #                   color = labelledm),
  #               width=0.1, linewidth=0.5,
  #               position = position_dodge(width=0.3)) +
  # geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90,
  #                   color = labelledm),
  #               width=0, linewidth=2,
  #               position = position_dodge(width=0.3)) +
  # geom_point(aes(x=as.factor(x), y=pr,
  #                shape = labelledm),color="white",
  #            position = position_dodge(width=0.3)) +
# scale_x_discrete(labels=c("なし（0）","あり（1）")) + ## labelsを設定する
geom_ribbon(aes(ymin=lo95, ymax=up95), alpha=0.3) +
  geom_ribbon(aes(ymin=lo90, ymax=up90), alpha=0.5) +
  geom_line() + 
  facet_grid(. ~ labelledm) + 
  scale_shape_discrete(name = "回答者の職業") + 
  scale_linetype_discrete(name = "回答者の職業") + 
  scale_color_brewer(name = "回答者の職業", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "回答者の職業", 
                    type = "qual", palette = 2) + 
  labs(x="在留外国人％",
       y="外国人観光客受け入れ意向（０〜４）", 
       caption = "注：基本モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig19.png", width=6, height=4)

## 予測値の算出
## 図２０
yosokuout2D <- genpr(dpr = dn,
                     mpr = m2,
                     setx = "zairyu_hiritsu", 
                     setxvals = seq(0.3,12.83,by=0.1),
                     setm = "shokugyo",
                     setmvals = list("正規" = "正規",
                                     "非正規" = "非正規",
                                     "自由業" = "自由業",
                                     "無職" = "無職"))

## 予測値をプロットする
ggplot(yosokuout2D, aes(x=x, y=pr)) + 
  # geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95,
  #                   color = labelledm),
  #               width=0.1, linewidth=0.5,
  #               position = position_dodge(width=0.3)) +
  # geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90,
  #                   color = labelledm),
  #               width=0, linewidth=2,
  #               position = position_dodge(width=0.3)) +
  # geom_point(aes(x=as.factor(x), y=pr,
  #                shape = labelledm),color="white",
  #            position = position_dodge(width=0.3)) +
# scale_x_discrete(labels=c("なし（0）","あり（1）")) + ## labelsを設定する
geom_ribbon(aes(ymin=lo95, ymax=up95), alpha=0.3) +
  geom_ribbon(aes(ymin=lo90, ymax=up90), alpha=0.5) +
  geom_line() + 
  facet_grid(. ~ labelledm) + 
  scale_shape_discrete(name = "回答者の職業") + 
  scale_linetype_discrete(name = "回答者の職業") + 
  scale_color_brewer(name = "回答者の職業", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "回答者の職業", 
                    type = "qual", palette = 2) + 
  labs(x="在留外国人％",
       y="外国人観光客受け入れ意向（０〜４）", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig20.png", width=6, height=4)

## 独立変数：国内旅行意向 ##

## 予測値の算出
## 図２１
yosokuout2A <- genpr(dpr = dn,
                     mpr = m2b,
                     setx = "kokunai_iko", 
                     setxvals = c(0,1),
                     setm = "shokugyo",
                     setmvals = list("正規" = "正規",
                                     "非正規" = "非正規",
                                     "自由業" = "自由業",
                                     "無職" = "無職"))

## 予測値をプロットする
ggplot(yosokuout2A, aes(x=x, y=pr)) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95,
                    color = labelledm),
                width=0.1, linewidth=0.5,
                position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90,
                    color = labelledm),
                width=0, linewidth=2,
                position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr,
                 shape = labelledm),color="white",
             position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("行きたくない（0）","行きたい（1）")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95), alpha=0.3) +
  # geom_ribbon(aes(ymin=lo90, ymax=up90), alpha=0.5) +
  # geom_line() + 
  # facet_grid(. ~ labelledm) + 
  scale_shape_discrete(name = "回答者の職業") + 
  scale_linetype_discrete(name = "回答者の職業") + 
  scale_color_brewer(name = "回答者の職業", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "回答者の職業", 
                    type = "qual", palette = 2) + 
  labs(x="国内旅行意向",
       y="外国人観光客受け入れ意向（０〜４）", 
       caption = "注：基本モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig21.png", width=6, height=4)

## 予測値の算出
##　図２２
yosokuout2A <- genpr(dpr = dn,
                     mpr = m2,
                     setx = "kokunai_iko", 
                     setxvals = c(0,1),
                     setm = "shokugyo",
                     setmvals = list("正規" = "正規",
                                     "非正規" = "非正規",
                                     "自由業" = "自由業",
                                     "無職" = "無職"))

## 予測値をプロットする
ggplot(yosokuout2A, aes(x=x, y=pr)) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95,
                    color = labelledm),
                width=0.1, linewidth=0.5,
                position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90,
                    color = labelledm),
                width=0, linewidth=2,
                position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr,
                 shape = labelledm),color="white",
             position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("行きたくない（0）","行きたい（1）")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95), alpha=0.3) +
  # geom_ribbon(aes(ymin=lo90, ymax=up90), alpha=0.5) +
  # geom_line() + 
  # facet_grid(. ~ labelledm) + 
  scale_shape_discrete(name = "回答者の職業") + 
  scale_linetype_discrete(name = "回答者の職業") + 
  scale_color_brewer(name = "回答者の職業", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "回答者の職業", 
                    type = "qual", palette = 2) + 
  labs(x="国内旅行意向",
       y="外国人観光客受け入れ意向（０〜４）", 
       caption = "注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig22.png", width=6, height=4)

## 独立変数：海外旅行意向 ##

## 予測値の算出
## 図２３
yosokuout2B <- genpr(dpr = dn,
                     mpr = m2b,
                     setx = "kaigai_iko", 
                     setxvals = c(0,1),
                     setm = "shokugyo",
                     setmvals = list("正規" = "正規",
                                     "非正規" = "非正規",
                                     "自由業" = "自由業",
                                     "無職" = "無職"))

## 予測値をプロットする
ggplot(yosokuout2B, aes(x=x, y=pr)) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95,
                    color = labelledm),
                width=0.1, linewidth=0.5,
                position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90,
                    color = labelledm),
                width=0, linewidth=2,
                position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr,
                 shape = labelledm),color="white",
             position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("行きたくない（0）","行きたい（1）")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95), alpha=0.3) +
  # geom_ribbon(aes(ymin=lo90, ymax=up90), alpha=0.5) +
  # geom_line() + 
  # facet_grid(. ~ labelledm) + 
  scale_shape_discrete(name = "回答者の職業") + 
  scale_linetype_discrete(name = "回答者の職業") + 
  scale_color_brewer(name = "回答者の職業", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "回答者の職業", 
                    type = "qual", palette = 2) + 
  labs(x="海外旅行意向",
       y="外国人観光客受け入れ意向（０〜４）", 
       caption = "注：基本モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig23.png", width=6, height=4)

## 予測値の算出
## 図２４
yosokuout2B <- genpr(dpr = dn,
                     mpr = m2,
                     setx = "kaigai_iko", 
                     setxvals = c(0,1),
                     setm = "shokugyo",
                     setmvals = list("正規" = "正規",
                                     "非正規" = "非正規",
                                     "自由業" = "自由業",
                                     "無職" = "無職"))

## 予測値をプロットする
ggplot(yosokuout2B, aes(x=x, y=pr)) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95,
                    color = labelledm),
                width=0.1, linewidth=0.5,
                position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90,
                    color = labelledm),
                width=0, linewidth=2,
                position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr,
                 shape = labelledm),color="white",
             position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("行きたくない（0）","行きたい（1）")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95), alpha=0.3) +
  # geom_ribbon(aes(ymin=lo90, ymax=up90), alpha=0.5) +
  # geom_line() + 
  # facet_grid(. ~ labelledm) + 
  scale_shape_discrete(name = "回答者の職業") + 
  scale_linetype_discrete(name = "回答者の職業") + 
  scale_color_brewer(name = "回答者の職業", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "回答者の職業", 
                    type = "qual", palette = 2) + 
  labs(x="海外旅行意向",
       y="外国人観光客受け入れ意向（０〜４）", 
       caption = "注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/kmfig24.png", width=6, height=4)

## ※以下は本文では使われていないので参考用

#############################################################################
## 限界効果（条件付変数が名義変数）の算出関数(本論文では図を使用していない)##
##（ここから）###############################################################
intereff_modfactor <- 
  function(m, # 回帰モデルオブジェクト
           main, # 独立変数
           mod){ # 条件付け変数（factorクラス）
    
    modval = levels(m$model[,mod])
    
    ## Initiate out value
    out = data.frame(mod = modval[1],
                     est = coef(m)[which(names(coef(m))==main)],
                     se = sqrt(vcov(m)[which(rownames(vcov(m))==main),
                                       which(colnames(vcov(m))==main)]))
    
    for (i in 2:length(modval)) {
      mainmod = paste0(main,":",mod,modval[i])
      if (!mainmod%in%rownames(vcov(m))) {
        mainmod = paste0(mod,modval[i],":",main)
      }
      cfset <- c(coef(m)[which(names(coef(m))==main)],
                 coef(m)[which(names(coef(m))==mainmod)])
      vcset <- c(vcov(m)[which(rownames(vcov(m))==main),
                         which(colnames(vcov(m))==main)],
                 vcov(m)[which(rownames(vcov(m))==mainmod),
                         which(colnames(vcov(m))==mainmod)],
                 vcov(m)[which(rownames(vcov(m))==main),
                         which(colnames(vcov(m))==mainmod)])
      out = 
        rbind(out,
              data.frame(mod = modval[i],
                         est = cfset[1]+cfset[2],
                         se = sqrt(vcset[1]+1^2*vcset[2]+2*1*vcset[3])))
    }

    if ("df"%in%names(m)) {
      dfset <- m$df[1]
      # Assuming that df is the same across all.
      # CAUTION: The above is not true if lm_robust's se_type="CR2".
    } else {
      dfset <- df.residual(m)
    }
    out$qt90 = qt(0.95,dfset)
    out$qt95 = qt(0.975,dfset)
    out$lo90 = out$est-out$se*out$qt90
    out$up90 = out$est+out$se*out$qt90
    out$lo95 = out$est-out$se*out$qt95
    out$up95 = out$est+out$se*out$qt95
    out$pval = (1 - pt(abs(out$est/out$se),dfset))*2
    out$mod <- factor(out$mod,
                      levels = levels(m$model[,mod]))

    return(out)
  }
##（ここまで）#############################################

## 国内旅行意向×職業

## 限界効果の出力
genkaioutA <- intereff_modfactor(m2, "kokunai_iko", "shokugyo")

## 限界効果プロット
ggplot(genkaioutA, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=mod),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=mod),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：外国人旅行客受け入れ意向（０〜４）",
       y="国内旅行意向の限界効果",
       x="回答者の職業",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))

# ## グラフを保存
# ggsave("h2a_genkaikoka_plot.png", width = 6, height = 4)

## 海外旅行意向×職業

## 限界効果の出力
genkaioutB <- intereff_modfactor(m2, "kaigai_iko", "shokugyo")

## 限界効果プロット
ggplot(genkaioutB, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=mod),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=mod),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：外国人旅行客受け入れ意向（０〜４）",
       y="海外旅行意向の限界効果",
       x="回答者の職業",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))

# ## グラフを保存
# ggsave("h2b_genkaikoka_plot.png", width = 6, height = 4)

## 在留外国人比率×職業

## 限界効果の出力
genkaioutD <- intereff_modfactor(m2, "zairyu_hiritsu", "shokugyo")

## 限界効果プロット
ggplot(genkaioutD, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=mod),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=mod),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：外国人旅行客受け入れ意向（０〜４）",
       y="在留外国人％（居住市区町村）の限界効果",
       x="回答者の職業",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))

# ## グラフを保存
# ggsave("h2d_genkaikoka_plot.png", width = 6, height = 4)