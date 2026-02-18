##分析再現ファイル（調査1、調査2接合後・本文登場順）
##タイトル　：国内における原子力発電所の再稼働に対する態度の規定要因：発電所と回答者の地理的関係に注目して

## 更新履歴：
## 2026-01-30 菅野尊瑠（提出版）
## 2026-02-15 加藤言人（最終調整）

## このスクリプトファイルがあるディレクトリ
dirhere <- dirname(rstudioapi::getActiveDocumentContext()$path) 

## ワーキングディレクトリをセット
setwd(dirhere)

## 図表出力用のフォルダ（out）を作成
if (!"./out" %in% list.dirs()) dir.create("./out")

## 使うパッケージ
library(readr) # CSVデータの読み込み
library(readxl) # エクセルデータの読み込み

######################
## データの読み込み ##
######################

### 調査1（UTAS）
dt <- read_csv("2017UTASV20200326.csv", 
               locale = locale(encoding="SHIFT_JIS"))
## データごとの表記の揺れを修正
dt$CITY <- gsub("^九戸郡$","九戸郡洋野町",dt$CITY) #なぜかここだけ町名がない
dt$CITY <- gsub("^.*郡","",dt$CITY) # 郡はdidデータにないので削除
dt$CITY <- gsub("^山市$","郡山市",dt$CITY) #郡を消すと郡山市が山市になってしまうので元に戻す 
dt$CITY <- gsub("浜松市中区","浜松市中央区",dt$CITY) #浜松の中・西・南区は2024年から中央区
dt$CITY <- gsub("浜松市西区","浜松市中央区",dt$CITY) #浜松の中・西・南区は2024年から中央区
dt$CITY <- gsub("浜松市南区","浜松市中央区",dt$CITY) #浜松の中・西・南区は2024年から中央区
head(dt)

### DIDデータの結合
dt_did <- read_xlsx("全国DID居住比率_人口密度リスト.xlsx", skip=8, na = "-")
dt_did <- dt_did[,-grep("注釈|/項目",colnames(dt_did))] # 空白列を削除
colnames(dt_did)
colnames(dt_did) <- c("nen_code","nen","chiiki_code","chiiki",
                      "jinko","jinko_did","menseki_did","mitsudo")
dt_did$PREFNAME <- gsub(" .*$","",dt_did$chiiki)
dt_did$CITY <- gsub("^.* ","",gsub("市 ","市",dt_did$chiiki))

## 必要な変数だけにする
dt_did <- dt_did[,c("PREFNAME","CITY","jinko","jinko_did","menseki_did","mitsudo")]

## データ内の表記の揺れを修正
dt_did$CITY <- gsub("茅ヶ崎市","茅ケ崎市",dt_did$CITY) 
dt_did$CITY <- gsub("那珂川市","那珂川町",dt_did$CITY)
dt_did$CITY <- gsub("吉野ヶ里町","吉野ケ里町",dt_did$CITY)
## 都道府県と市町村名の一致を確認
all(unique(paste(dt$PREFNAME,dt$CITY))%in%paste(dt_did$PREFNAME,dt_did$CITY))

### UPZデータの結合
dt_upz <- read_xlsx("全国原発UPZまとめ.xlsx")
dt_upz <- dt_upz[,c(1:16)] # 沿岸内陸までを読み込み
## 英語で列名を再設定
colnames(dt_upz) <- 
  c("ID","PREFNAME","CITY","UPZ", "RICCH",
    "HOKKAIDO","TOHOKU","HOKURIKU","TOKYO",
    "CHUBU","KANSAI","CHUGOKU","KYUSHU","SHIKOKU",
    "OKINAWA","ENGAN")

## データ内の表記の揺れを修正
dt_upz$CITY <- gsub("^九戸郡$","九戸郡洋野町",dt_upz$CITY)
dt_upz$CITY <- gsub("^.*郡","",dt_upz$CITY)
dt_upz$CITY <- gsub("^山市$","郡山市",dt$CITY)
dt_upz$CITY <- gsub("浜松市中区","浜松市中央区",dt_upz$CITY)
dt_upz$CITY <- gsub("浜松市西区","浜松市中央区",dt_upz$CITY)
dt_upz$CITY <- gsub("浜松市南区","浜松市中央区",dt_upz$CITY)
head(dt_upz)
## 都道府県と市町村名の一致を確認
all(unique(paste(dt$PREFNAME,dt$CITY))%in%paste(dt_upz$PREFNAME,dt_upz$CITY))

### データを結合する
dt <- merge(dt, dt_did, by = c("PREFNAME","CITY"))
dt <- merge(dt, dt_upz, by = c("ID","PREFNAME","CITY"))

##############
## 変数準備 ##
##############

## UPZかどうか
table(dt$UPZ)

##立地県かどうか
table(dt$Ricch)

## 沿岸かどうか
table(dt$ENGAN) 

##################
## 交絡変数作成 ##
##################

##年齢（除く無回答・10歳刻みで分類）
table(dt$F2)
dt$nenrei10 <- dt$F2
dt$nenrei10[which(!dt$F2 %in% c(1, 2, 3, 4, 5, 6, 7))] <- NA
table(dt$nenrei10, useNA = "always")
hist(dt$nenrei10)

##性別（除く無回答・女性1, 男性0）
table(dt$F1)
dt$sex <- NA
dt$sex[which(dt$F1==1)] <- 0
dt$sex[which(dt$F1==2)] <- 1
table(dt$sex, useNA="always")
hist(dt$sex)

##職業(安定収入1, 不安定収入0)
##1→会社員、公務員
##0→自営業、農林漁業、派遣・パート・アルバイト、主婦・主夫、学生、無職
##NA→その他、無回答
table(dt$F4)
dt$syokugyou <- NA
dt$syokugyou[which(dt$F4 %in% c(1:8))] <- 0
dt$syokugyou[which(dt$F4 %in% c(1,2))] <- 1
table(dt$syokugyou, useNA = "always")
hist(dt$syokugyou)

##居住地域が沿岸か否か（沿岸1, 内陸0)
table(dt$ENGAN)
hist(dt$ENGAN)

##非核三原則を堅持するか（堅持する1, 堅持しない0）
table(dt$Q23_4)
dt$Hikaku <- NA
dt$Hikaku[which(dt$Q23_4 %in% c(1, 2, 3, 4, 5))] <- 0
dt$Hikaku[which(dt$Q23_4 %in% c(1, 2))] <- 1
table(dt$Hikaku, useNA = "always")
hist(dt$Hikaku)


#######################
## UPZの条件別で抽出 ##
#######################

#UPZに該当するかどうかで分別する
UPZ0 <- which(dt$UPZ == 0)
UPZ1 <- which(dt$UPZ == 1)
RicchUPZ <- which(dt$UPZ == 1 & dt$RICCH == 1) ##立地県内かつUPZ範囲内
Ricch <- which(dt$UPZ == 0 & dt$RICCH == 1) ##立地県内
NotricchUPZ <- which(dt$UPZ == 1 & dt$RICCH == 0) ##立地県外かつUPZ範囲内
Notricch <- which(dt$UPZ == 0 & dt$RICCH == 0) ##立地県外

####################
## 従属変数の作成 ##
####################

##Q23_13 原子力規制委員会の審査に合格した原子力発電所は運転を再開すべきだ
##1賛成, 2どちらかと言えば賛成
##3どちらとも言えない, 4どちらかと言えば反対, 5反対, 99無回答

## 賛成している方が大きな値になるようにする。
dt$saikadou2 <- 
  ifelse(dt$Q23_13 == 99, NA, 5-dt$Q23_13)
table(dt$saikadou2, useNA="always")

#【Figure1】グラフを描画
library(ggplot2)
ggplot(dt, aes(x = factor(saikadou2, exclude = NULL))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  scale_x_discrete(labels = c(
    "0" = "0:反対",
    "1" = "1:どちらかと\n言えば反対",
    "2" = "2:どちらとも\n言えない",
    "3" = "3:どちらかと\n言えば賛成",
    "4" = "4:賛成",
    "NA" = "NA"
  )) +
  labs(
    x = "再稼働への賛成度（Q23_13）",
    y = "回答者数"
  )
## グラフを保存
ggsave("./out/ktfig1.png", width = 6, height = 4)

## （再稼働賛成１、その他・無回答0）
dt$saikadou <- NA
dt$saikadou[which(dt$Q23_13 %in% c(3, 4, 5))] <- 0
dt$saikadou[which(dt$Q23_13 %in% c(1, 2))] <- 1
### 平均を取る。全体での再稼働賛成率は24.6%
table(dt$saikadou, useNA="always")

#【Figure2】グラフを描画
library(ggplot2)
ggplot(dt, aes(x = factor(saikadou, exclude = NULL))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  scale_x_discrete(labels = c(
    "0" = "0:反対",
    "1" = "1:賛成",
    "NA" = "NA"
  )) +
  labs(
    x = "再稼働への賛成度（Q23_13）",
    y = "回答者数"
  )
## グラフを保存
ggsave("./out/ktfig2.png", width = 6, height = 4)

############
## データ ##
############

##  データの読み込み（掛川市市民意識調査）
dt_kakegawa <- read_xlsx("掛川市_R6ローデータ.xlsx",
                         col_names = FALSE, skip = 3)

## 回答回収数の確認
nrow(dt_kakegawa)

dtcol <- read_xlsx("掛川市_R6ローデータ.xlsx",n_max=2)

setcol <- colnames(dtcol)
setcol <- gsub("問","Q", setcol)
setcol[1] <- "id"
setcol[which(setcol%in%c("Q15",paste0("...",17:27)))] <-
  paste0("Q15-",1:12)
setcol[which(setcol%in%c("Q16",paste0("...",29:39)))] <-
  paste0("Q16-",1:12)
setcol[which(setcol%in%c("Q29　満足度",paste0("...",60:99)))] <-
  paste0("Q29A-",1:40)
setcol[which(setcol%in%c("Q29　優先度",paste0("...",101:139)))] <-
  paste0("Q29B-",1:40)
setcol[which(setcol%in%c("Q30",paste0("...",141:175)))] <-
  paste0("Q30-",1:36)

## 元のデータの列名を置き換える
colnames(dt_kakegawa) <- setcol
saveRDS(dt_kakegawa, "kakegawa_data.rds")

## 上で作って保存したデータを読み込む
dt_kakegawa <- readRDS("kakegawa_data.rds")

##########
## 変数 ##
##########

## 分析用の新しいデータを作る。
dn <- data.frame(id = dt_kakegawa$id) #回答者ID

## 独立変数（距離）##

##中学校区別の人口を見る
##1東中学校,2西中学校, 3栄川中学校, 4北中学校, 5原野谷中学校, 6桜が丘中学校
##7大浜中学校, 8城東中学校, 9大須賀中学校, 10NA 

## 小学校区のラベル付け
dn$chugaku <- NA
dn$chugaku[which(dt_kakegawa$Q3==1)] <- "東"
dn$chugaku[which(dt_kakegawa$Q3==2)] <- "西"
dn$chugaku[which(dt_kakegawa$Q3==3)] <- "栄川"
dn$chugaku[which(dt_kakegawa$Q3==4)] <- "北"
dn$chugaku[which(dt_kakegawa$Q3==5)] <- "原野谷"
dn$chugaku[which(dt_kakegawa$Q3==6)] <- "桜が丘"
dn$chugaku[which(dt_kakegawa$Q3==7)] <- "大浜"
dn$chugaku[which(dt_kakegawa$Q3==8)] <- "城東"
dn$chugaku[which(dt_kakegawa$Q3==9)] <- "大須賀"

### 原発からの距離を代入
dn$kyori <- NA
dn$kyori[which(dt_kakegawa$Q3==1)] <- 21.66 
dn$kyori[which(dt_kakegawa$Q3==2)] <- 22.26 
dn$kyori[which(dt_kakegawa$Q3==3)] <- 20.27 
dn$kyori[which(dt_kakegawa$Q3==4)] <- 23.13 
dn$kyori[which(dt_kakegawa$Q3==5)] <- 29.08 
dn$kyori[which(dt_kakegawa$Q3==6)] <- 24.58 
dn$kyori[which(dt_kakegawa$Q3==7)] <- 11.30 
dn$kyori[which(dt_kakegawa$Q3==8)] <- 14.29 
dn$kyori[which(dt_kakegawa$Q3==9)] <- 17.11 
summary(dn$kyori)


#【Figure3】グラフを描画
library(ggplot2)
ggplot(dn, aes(x = factor(kyori, exclude = NULL))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  scale_x_discrete(labels = c(
    "11.3" = "11.3\n(大浜)",
    "14.29" = "14.29\n(城東)",
    "17.11" = "17.11\n(大須賀)",
    "20.27" = "20.27\n(栄川)",
    "21.66" = "21.66\n(東)",
    "22.26" = "22.26\n(西)",
    "23.13" = "23.13\n(北)",
    "24.58" = "24.58\n(桜が丘)",
    "29.08" = "29.08\n(原野谷)",
    "NA" = "NA"
  )) +
  labs(
    x = "浜岡原発からの距離(km)",
    y = "回答者数"
  )
## グラフを保存
ggsave("./out/ktfig3.png", width = 6, height = 4)


####################
## 従属変数の作成 ##
####################

##Q18浜岡原発の廃炉に対する意見
##1廃炉にした方がよい, 2停止しておいた方がよい
##3安全が確認できれば稼働した方がよい, 4どちらともいえない, 5わからない, NA
table(dt_kakegawa$Q18, useNA="always") 

## 連続変数版
##0→廃炉にした方がよい、1→停止しておいた方がよい
##2→どちらともいえない、3→安全が確認できれば再稼働した方がよい
##欠損→無回答、わからない
library(dplyr)
dn$genpatsuC <- case_when(
  dt_kakegawa$Q18 == 3 ~ 3,
  dt_kakegawa$Q18 == 4 ~ 2,
  dt_kakegawa$Q18 == 2 ~ 1,
  dt_kakegawa$Q18 == 1 ~ 0,
  TRUE ~ as.numeric(NA))

summary(dn$genpatsuC)
table(dn$genpatsuC, useNA="always")

#【Figure4】グラフを描画
library(ggplot2)
ggplot(dn, aes(x = factor(genpatsuC, exclude = NULL))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  scale_x_discrete(labels = c(
    "0" = "0:廃炉",
    "1" = "1:停止",
    "2" = "2:どちらとも\n言えない",
    "3" = "3:安全が確認\nできれば再稼働",
    "NA" = "NA"
  )) +
  labs(
    x = "再稼働への賛成度(Q18)",
    y = "回答者数"
  )
## グラフを保存
ggsave("./out/ktfig4.png", width = 6, height = 4)

## 二値変数（ダミー）版
## 1→再稼働賛成、0→廃炉／停止／どちらともいえない
## 欠損→無回答、分からない
dn$genpatsuD <- 0
dn$genpatsuD[which(dt_kakegawa$Q18==3)] <- 1
dn$genpatsuD[which(dt_kakegawa$Q18==5)] <- NA
dn$genpatsuD[which(is.na(dt_kakegawa$Q18))] <- NA
summary(dn$genpatsuD)
table(dn$genpatsuD, useNA="always")


#【Figure5】グラフを描画
library(ggplot2)
ggplot(dn, aes(x = factor(genpatsuD, exclude = NULL))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  scale_x_discrete(labels = c(
    "0" = "0:廃炉/停止/\nどちらともいえない",
    "1" = "1:安全が確認\nできれば再稼働",
    "NA" = "NA/わからない"
  )) +
  labs(
    x = "再稼働への賛成度・純粋な再稼働賛成率(Q18)",
    y = "回答者数"
  )
## グラフを保存
ggsave("./out/ktfig5.png", width = 6, height = 4)

##調査2・交絡変数
##年齢（10歳刻みで分類）
table(dt_kakegawa$Q2)
dn$nenrei10 <- dt_kakegawa$Q2/10

##性別（女性1, 男性0）
table(dt_kakegawa$Q1)
dn$sexA <- NA
dn$sexA[which(dt_kakegawa$Q1 == 1)] <- 0
dn$sexA[which(dt_kakegawa$Q1 == 2)] <- 1

##職業(安定収入1, 不安定収入0)
##NA→その他、無回答
table(dt_kakegawa$Q4)
dn$syokugyou <- NA
dn$syokugyou[which(dt_kakegawa$Q4 %in% c(1:7))] <- 0
dn$syokugyou[which(dt_kakegawa$Q4 %in% c(1))] <- 1

##居住歴（掛川市にルーツがある1, 掛川市外にルーツがある0）
table(dt_kakegawa$Q12)
dn$kyojyuureki <- NA
dn$kyojyuureki[which(dt_kakegawa$Q12 %in% c(1:3))] <- 0
dn$kyojyuureki[which(dt_kakegawa$Q12 %in% c(1,2))] <- 1

#利用メディア（市の公式媒体を1, テレビ・新聞を0）
table(dt_kakegawa$`Q30-9`)
dn$official <- NA
dn$official[which(dt_kakegawa$`Q30-9` %in% c(1))] <- 0
dn$official[which(dt_kakegawa$`Q30-9` %in% c(2,3))] <- 1 
table(dn$official, useNA="always")

#群別再稼働賛成率##
#UPZ域内に居住する人
saikadou_UPZ1 <- dt$saikadou[UPZ1]
#UPZ域外に居住する人
saikadou_UPZ0 <- dt$saikadou[UPZ0]

#立地県かつUPZ域内に居住する人
saikadou_RicchUPZ <- dt$saikadou[RicchUPZ]
#立地県かつUPZ域外に居住する人
saikadou_Ricch <- dt$saikadou[Ricch]
#非立地県のUPZ内に居住する人
saikadou_NotRicchUPZ <- dt$saikadou[NotricchUPZ]
#非立地県に居住する人
saikadou_NotRicch <- dt$saikadou[Notricch]


#【Figure6】グラフを描画
rate_all <- mean(dt$saikadou, na.rm = TRUE)
rate_upz1 <- mean(saikadou_UPZ1, na.rm = TRUE)
rate_upz0 <- mean(saikadou_UPZ0, na.rm = TRUE)
rate_RicchUPZ <- mean(saikadou_RicchUPZ, na.rm = TRUE)
rate_Ricch <- mean(saikadou_Ricch, na.rm = TRUE)
rate_NotRicchUPZ <- mean(saikadou_NotRicchUPZ, na.rm = TRUE)
rate_NotRicch <- mean(saikadou_NotRicch, na.rm = TRUE)

comparison_data <- data.frame(
  Group = factor(
    c("全体", "UPZ域内", "UPZ域外", "UPZ域内＆\n立地県", "UPZ域外＆\n立地県"
      , "UPZ域内＆\n非立地県", "UPZ域外＆\n非立地県"),
    # グラフのX軸の表示順を指定
    levels = c("全体", "UPZ域内", "UPZ域外", "UPZ域内＆\n立地県", "UPZ域外＆\n立地県"
               , "UPZ域内＆\n非立地県", "UPZ域外＆\n非立地県") ),
  # 賛成率を格納（0-1の値をパーセントに変換して見やすくする）
  Rate = c(rate_all, rate_upz1, rate_upz0, rate_RicchUPZ,
           rate_Ricch, rate_NotRicchUPZ, rate_NotRicch) * 100 )

library(ggplot2)

# 棒グラフの作成
ggplot(comparison_data, aes(x = Group, y = Rate, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  
  # ラベルを表示する
  geom_text(
    aes(label = paste0(round(Rate, 1), "%")), # 小数点第1位まで表示
    vjust = -0.5, # 棒の上端より少し上に配置
    size = 5) +
  
  #Y軸のスケールを設定
  scale_y_continuous(
    limits = c(0, max(comparison_data$Rate) * 1.2), 
    name = "再稼働賛成率 (%)") +
  
  #"見た目を設定する
  labs(title = "居住地域別の原子力発電所再稼働賛成率",
       x = "居住地域の所属",
       caption = paste0("全地域: ", round(rate_all*100, 1), "% | UPZ域内: ", round(rate_upz1*100, 1), "%
                   | UPZ域外: ", round(rate_upz0*100, 1), "% | UPZ域内＆立地県: ", round(rate_RicchUPZ*100, 1), "%
                　 | UPZ域外＆立地県: ", round(rate_Ricch*100, 1), "%　| 立地県外: ", round(rate_NotRicch*100, 1), "%" )) +
  
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none" )
## グラフを保存
ggsave("./out/ktfig6.png", width = 6, height = 4)

#############################
## 独立変数UPZでの回帰分析 ##
#############################

##仮説１: UPZ内の方が、UPZ外よりも、原発再稼働に賛成する。
### （ロジック：原発近隣は地元利権がからむから）
## 仮説２：立地都道府県では、立地していない都道府県より、仮説1の関係が強い。
### （ロジック：原発に関する報道は、立地都道府県の方が量が多い？）

library(texreg)
library(htmltools)

## 賛成している方が大きな値になるようにする。
dt$saikadou2 <- 
  ifelse(dt$Q23_13 == 99, NA, 5-dt$Q23_13)
table(dt$saikadou2, useNA="always")

## 仮説１基本モデル（単回帰）
m1b <- lm(saikadou2 ~ UPZ, data = dt)
## 仮説１拡張モデル
m1 <- lm(saikadou2 ~ UPZ + 
           RICCH + ENGAN + Hikaku + 
           sex + nenrei10 + syokugyou, data = dt)
## 仮説２基本モデル（単回帰）
m2b <- lm(saikadou2 ~ UPZ * RICCH, data = dt)
## 仮説２拡張モデル
m2 <- lm(saikadou2 ~ UPZ * RICCH + 
           ENGAN + Hikaku + 
           sex + nenrei10 + syokugyou, data = dt)

## 結果
### 変数ラベル
vnmap <- list("(Intercept)" = "（定数項）",
              "UPZ" = "UPZ圏内自治体ダミー",
              "RICCH" = "原発立地道県ダミー",
              "UPZ:RICCH" = "UPZ * 立地道県",
              "ENGAN" = "沿岸自治体ダミー",
              "Hikaku" = "非核三原則支持ダミー",
              "sex" = "性別（女性）",
              "nenrei10" = "年齢（7段階）",
              "syokugyou" = "正規雇用ダミー")
### 【Table1】HTMLで表を出力
library(htmltools)
tab1 <- HTML(htmlreg(list(m1b, m1, m2b, m2), include.ci = FALSE,
                       digits=3, stars = c(0.001,0.01,0.05,0.1), 
                       symbol="+",  star.symbol = "*", inline.css = FALSE,
                       html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                       single.row = FALSE, caption = "",
                       custom.coef.map = vnmap, 
                       custom.header = list("仮説１" = 1:2, "仮説２" = 3:4), 
                       custom.model.names =  c("基本","拡張","基本","拡張"),
                       custom.note = "%stars。OLS回帰分析結果。括弧内は標準誤差。すべてのモデルに国・年固定効果を含む。"))
## 表を仮表示
browsable(tab1)
## 表をoutに保存する
save_html(tab1, "./out/kttab1.html")
webshot(url="./out/kttab1.html", file="./out/kttab1.png", zoom=3, selector="table")


#################
## 予測値の算出##
#################
genpr <- function(dpr, # 予測値算出用データ（元データ）
                  mpr, # 予測値算出用分析結果
                  setx = NULL, # 独立変数の名前  
                  setxvals = NULL, # 独立変数の値設定（numeric/character）
                  setxlabs=NULL, # 独立変数ラベル（カテゴリ変数の場合）
                  setm=NULL, # 条件付け変数の名前
                  setmvals=NULL, # 条件付変数のシミュレーション用値（list）
                  datalab=NULL) { # データにラベル
  
  ## 予測値算出用の元データの作成
  simdt <- na.omit(dpr[,all.vars(mpr$terms)])
  
  ## Xがセットされていない場合
  if (is.null(setx)) {
    simdt$NOXSET <- 1
    setx <- "NOXSET"
    setxvals <- 1
    if (is.null(setm)) stop("If setx is NULL, needs setm!")
  }
  
  ## 独立変数の値設定
  simx <- setxvals
  
  ## 予測用プロファイルの作成（条件付け変数なし）
  if (is.null(setm)) {
    
    ## プロファイル作成
    simv <- data.frame(simx = simx)
    
    ## 予測用プロファイルの作成（条件付け変数あり）
  } else {
    
    ## 条件付け変数の値設定
    simm <- sapply(setmvals, function(x) x[1])
    names(simm) <- NULL
    ## プロファイル作成
    simv <- data.frame(simx = rep(simx,each=length(simm)), 
                       simm = simm)
    
    ## 条件付け変数が2つ以上ある場合
    if (length(setm)>1) {
      for(i in 2:length(setm)) {
        ## 条件付け変数の値設定
        simmx <- sapply(setmvals, function(x) x[i])
        names(simmx) <- NULL
        simv[,paste0("simm",i)] <- simmx
      }
    }
    
  }
  
  ## 予測値の出力
  prout <- as.data.frame(t(sapply(1:nrow(simv), function(iii) {
    
    k <- simv[iii,]
    
    ## 予測値算出用仮データ
    tmpdt <- simdt
    tmpdt[,setx] <- k[1] # 独立変数の値割り当て
    if (!is.null(setm)) {
      # 条件付け変数の値割り当て
      tmpdt[,setm[1]] <- k[2] 
      if (length(setm)>1) {
        for (i in 2:length(setm)) {
          tmpdt[,setm[i]] <- k[1+i]
        }
      }
    }
    ## 予測値算出
    tmp <- colMeans(as.data.frame(predict(mpr, newdata=tmpdt, se.fit=TRUE)))
    tmp <- c(k, tmp[1:2], 
             tmp[1]-tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]-tmp[2]*qt(0.95,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.95,df=mpr$df[1]))
    ## データの列名割り当て
    if (is.null(setm)) {
      names(tmp) <- c("x","pr","se",
                      "lo95","up95","lo90","up90")
    } else {
      names(tmp) <- c("x",paste0("m",1:length(setm)),"pr","se",
                      "lo95","up95","lo90","up90")
    }
    return(tmp)
  })))
  ## 数値化する
  prout$x <- unlist(prout$x)
  prout$pr <- as.numeric(prout$pr)
  prout$pr <- as.numeric(prout$pr)
  prout$se <- as.numeric(prout$se)
  prout$lo95 <- as.numeric(prout$lo95)
  prout$up95 <- as.numeric(prout$up95)
  prout$lo90 <- as.numeric(prout$lo90)
  prout$up90 <- as.numeric(prout$up90)
  ### 独立変数にラベルを割り当て（ある場合）
  if(!is.null(setxlabs)) {
    prout$x <- 
      factor(prout$x,levels=setxvals,labels=setxlabs)
  } 
  ### 条件付け変数にラベルを割り当て（ある場合）
  if (!is.null(setm)) {
    if (length(setm)==1) {
      prout$labelledm <- 
        factor(names(setmvals)[match(prout[,grep("^m",colnames(prout))],
                                     unlist(setmvals))],
               levels = names(setmvals))
    } else {
      prout$labelledm <- factor(names(setmvals), levels = names(setmvals))
    }
  }
  ### データ自体にラベルを割り当て（ある場合）
  if (!is.null(datalab)) {
    prout$datalab <- datalab
  }
  
  ## 結果を出力
  return(prout)
}
##（ここまで）#############################################

## 仮説１ ##

## 予測値の算出
yosokuout1 <- genpr(dpr = dt,
                    mpr = m1,
                    setx = "UPZ", 
                    setxvals = seq(0,1,length=2))
yosokuout1

## 【Figure7】予測値をプロットする
ggplot(yosokuout1) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr),color="white") +
  scale_x_discrete(labels = c("UPZ圏外(0)","UPZ圏内(1)")) + ## labelsを設定する
  # geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=x, y=pr)) + 
  labs(x="UPZ圏内の自治体かどうか",
       y="原発再稼働支持態度の予測値\n(0=反対,4=賛成)", 
       caption = "注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_minimal() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5)) 
## グラフを保存
ggsave("./out/ktfig7.png", width = 6, height = 4)


## 仮説２ ##

## 予測値の算出
yosokuout2 <- genpr(dpr = dt,
                    mpr = m2,
                    setx = "UPZ", 
                    setxvals = seq(0,1,length=2),
                    setm = "RICCH",
                    setmvals = list("原発不立地道県(0)" = 0,
                                    "原発立地道県(1)" = 1))
yosokuout2$x

## 【Figure8】予測値をプロットする
ggplot(yosokuout2, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels = c("UPZ圏外(0)","UPZ圏内(1)")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95,
  #                 fill = labelledm), alpha=0.3) + 
  # geom_ribbon(aes(ymin=lo90, ymax=up90,
  #                 fill = labelledm), alpha=0.5) + 
  # geom_line(aes(linetype=labelledm)) + 
  scale_shape_discrete(name = "対象国") + 
  scale_linetype_discrete(name = "対象国") + 
  scale_color_brewer(name = "対象国", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "対象国", 
                    type = "qual", palette = 2) + 
  labs(x="UPZ圏内の自治体かどうか",
       y="原発再稼働支持態度の予測値\n(0=反対,4=賛成)", 
       caption = "注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_minimal() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ktfig8.png", width = 6, height = 4)


#######################
## 限界効果の算出関数##
##（ここから）#########
intereff <- function(m, # 回帰モデルオブジェクト
                     main, # 独立変数
                     mod,　# 条件付け変数
                     modrange, # 条件付変数を動かす範囲
                     nsim = 30) { # 条件付変数で結果を出力する値の数
  modval = seq(modrange[1],modrange[2],length=nsim)
  mainmod = paste(main,mod,sep=":")
  if (!mainmod%in%rownames(vcov(m))) mainmod = paste(mod,main,sep=":")
  if ("df"%in%names(m)) {
    dfset <- m$df[1]
    # Assuming that df is the same across all.
    # CAUTION: The above is not true if lm_robust's se_type="CR2".
  } else {
    dfset <- df.residual(m)
  }
  cfset <- c(coef(m)[which(names(coef(m))==main)],
             coef(m)[which(names(coef(m))==mainmod)])
  vcset <- c(vcov(m)[which(rownames(vcov(m))==main),
                     which(colnames(vcov(m))==main)],
             vcov(m)[which(rownames(vcov(m))==mainmod),
                     which(colnames(vcov(m))==mainmod)],
             vcov(m)[which(rownames(vcov(m))==main),
                     which(colnames(vcov(m))==mainmod)])
  
  out = data.frame(mod = modval,
                   est = cfset[1]+cfset[2]*modval,
                   se = sqrt(vcset[1]+modval^2*vcset[2]+2*modval*vcset[3]),
                   qt90 = qt(0.95,dfset),
                   qt95 = qt(0.975,dfset))
  out$lo90 = out$est-out$se*out$qt90
  out$up90 = out$est+out$se*out$qt90
  out$lo95 = out$est-out$se*out$qt95
  out$up95 = out$est+out$se*out$qt95
  out$pval = (1 - pt(abs(out$est/out$se),dfset))*2
  
  return(out)
}
##（ここまで）#############################################

## 限界効果の出力
genkaiout <- intereff(m2, "UPZ", 
                      "RICCH", c(0,1), nsim=2)

## 【Figure9】限界効果プロット
ggplot(genkaiout, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("原発不立地道県(0)",
                            "原発立地道県(1)")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：原発再稼働支持態度(0-4)",
       y="UPZ圏内ダミーの限界効果",
       x="対象国",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ktfig9.png", width = 6, height = 4)



##↓掛川↓##


##########
## 準備 ##
##########

## 使うパッケージ
## パッケージがない場合は install.packages("パッケージ名")でインストール
library(haven) # install.packages("haven") # SPSS/Stataファイルの読み込み
library(labelled) # install.packages("labelled") # 変数ラベルの管理
library(ggplot2) # install.packages("ggplot2") # グラフの作成・出力
library(gt) # install.packages("gt") # 変数の記述統計表の出力
library(texreg) # install.packages("texreg") # 回帰表の出力

#################
## 予測値の算出##
##（ここから）###
genpr <- function(dpr, # 予測値算出用データ（元データ）
                  mpr, # 予測値算出用分析結果
                  setx = NULL, # 独立変数の名前  
                  setxvals = NULL, # 独立変数の値設定（numeric/character）
                  setxlabs=NULL, # 独立変数ラベル（カテゴリ変数の場合）
                  setm=NULL, # 条件付け変数の名前
                  setmvals=NULL, # 条件付変数のシミュレーション用値（list）
                  datalab=NULL) { # データにラベル
  
  ## 予測値算出用の元データの作成
  simdt <- na.omit(dpr[,all.vars(mpr$terms)])
  
  ## Xがセットされていない場合
  if (is.null(setx)) {
    simdt$NOXSET <- 1
    setx <- "NOXSET"
    setxvals <- 1
    if (is.null(setm)) stop("If setx is NULL, needs setm!")
  }
  
  ## 独立変数の値設定
  simx <- setxvals
  
  ## 予測用プロファイルの作成（条件付け変数なし）
  if (is.null(setm)) {
    
    ## プロファイル作成
    simv <- data.frame(simx = simx)
    
    ## 予測用プロファイルの作成（条件付け変数あり）
  } else {
    
    ## 条件付け変数の値設定
    simm <- sapply(setmvals, function(x) x[1])
    names(simm) <- NULL
    ## プロファイル作成
    simv <- data.frame(simx = rep(simx,each=length(simm)), 
                       simm = simm)
    
    ## 条件付け変数が2つ以上ある場合
    if (length(setm)>1) {
      for(i in 2:length(setm)) {
        ## 条件付け変数の値設定
        simmx <- sapply(setmvals, function(x) x[i])
        names(simmx) <- NULL
        simv[,paste0("simm",i)] <- simmx
      }
    }
    
  }
  
  ## 予測値の出力
  prout <- as.data.frame(t(sapply(1:nrow(simv), function(iii) {
    
    k <- simv[iii,]
    
    ## 予測値算出用仮データ
    tmpdt <- simdt
    tmpdt[,setx] <- k[1] # 独立変数の値割り当て
    if (!is.null(setm)) {
      # 条件付け変数の値割り当て
      tmpdt[,setm[1]] <- k[2] 
      if (length(setm)>1) {
        for (i in 2:length(setm)) {
          tmpdt[,setm[i]] <- k[1+i]
        }
      }
    }
    ## 予測値算出
    tmp <- colMeans(as.data.frame(predict(mpr, newdata=tmpdt, se.fit=TRUE)))
    tmp <- c(k, tmp[1:2], 
             tmp[1]-tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]-tmp[2]*qt(0.95,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.95,df=mpr$df[1]))
    ## データの列名割り当て
    if (is.null(setm)) {
      names(tmp) <- c("x","pr","se",
                      "lo95","up95","lo90","up90")
    } else {
      names(tmp) <- c("x",paste0("m",1:length(setm)),"pr","se",
                      "lo95","up95","lo90","up90")
    }
    return(tmp)
  })))
  ## 数値化する
  prout$x <- unlist(prout$x)
  prout$pr <- as.numeric(prout$pr)
  prout$pr <- as.numeric(prout$pr)
  prout$se <- as.numeric(prout$se)
  prout$lo95 <- as.numeric(prout$lo95)
  prout$up95 <- as.numeric(prout$up95)
  prout$lo90 <- as.numeric(prout$lo90)
  prout$up90 <- as.numeric(prout$up90)
  ### 独立変数にラベルを割り当て（ある場合）
  if(!is.null(setxlabs)) {
    prout$x <- 
      factor(prout$x,levels=setxvals,labels=setxlabs)
  } 
  ### 条件付け変数にラベルを割り当て（ある場合）
  if (!is.null(setm)) {
    if (length(setm)==1) {
      prout$labelledm <- 
        factor(names(setmvals)[match(prout[,grep("^m",colnames(prout))],
                                     unlist(setmvals))],
               levels = names(setmvals))
    } else {
      prout$labelledm <- factor(names(setmvals), levels = names(setmvals))
    }
  }
  ### データ自体にラベルを割り当て（ある場合）
  if (!is.null(datalab)) {
    prout$datalab <- datalab
  }
  
  ## 結果を出力
  return(prout)
}
##（ここまで）#############################################

#######################
## 限界効果の算出関数##
##（ここから）#########
intereff <- function(m, # 回帰モデルオブジェクト
                     main, # 独立変数
                     mod,　# 条件付け変数
                     modrange, # 条件付変数を動かす範囲
                     nsim = 30) { # 条件付変数で結果を出力する値の数
  modval = seq(modrange[1],modrange[2],length=nsim)
  mainmod = paste(main,mod,sep=":")
  if (!mainmod%in%rownames(vcov(m))) mainmod = paste(mod,main,sep=":")
  if ("df"%in%names(m)) {
    dfset <- m$df[1]
    # Assuming that df is the same across all.
    # CAUTION: The above is not true if lm_robust's se_type="CR2".
  } else {
    dfset <- df.residual(m)
  }
  cfset <- c(coef(m)[which(names(coef(m))==main)],
             coef(m)[which(names(coef(m))==mainmod)])
  vcset <- c(vcov(m)[which(rownames(vcov(m))==main),
                     which(colnames(vcov(m))==main)],
             vcov(m)[which(rownames(vcov(m))==mainmod),
                     which(colnames(vcov(m))==mainmod)],
             vcov(m)[which(rownames(vcov(m))==main),
                     which(colnames(vcov(m))==mainmod)])
  
  out = data.frame(mod = modval,
                   est = cfset[1]+cfset[2]*modval,
                   se = sqrt(vcset[1]+modval^2*vcset[2]+2*modval*vcset[3]),
                   qt90 = qt(0.95,dfset),
                   qt95 = qt(0.975,dfset))
  out$lo90 = out$est-out$se*out$qt90
  out$up90 = out$est+out$se*out$qt90
  out$lo95 = out$est-out$se*out$qt95
  out$up95 = out$est+out$se*out$qt95
  out$pval = (1 - pt(abs(out$est/out$se),dfset))*2
  
  return(out)
}
##（ここまで）#############################################

##################
## 仮説１の分析 ##
##################
## 仮説１：居住地から浜岡原発への距離が近くなると、再稼働支持が上昇する

## 折れ線グラフ （genpatsuC）##
plotdataC <- as.data.frame(
  tapply(dn$genpatsuC, dn$kyori, mean, na.rm=TRUE))
colnames(plotdataC) <- "genpatsuC_mean"
plotdataC$kyori <- as.numeric(rownames(plotdataC))

## プロットする
library(ggplot2) # ない場合はinstall.packages("ggplot2")

## 中学校区ラベルを付ける
library(ggrepel) #ない場合はinstall.packages("ggrepel")
### ラベルデータをプロットデータに挿入
plotdataC$chugaku <- NA
for(i in 1:nrow(plotdataC)) {
  plotdataC$chugaku[i] <- 
    dn$chugaku[which(dn$kyori==plotdataC$kyori[i])][1]
}
plotdataC
### プロットする
p1 <- ggplot(plotdataC, aes(x=kyori, y=genpatsuC_mean)) + 
  geom_line() + geom_point(size = 2) + 
  geom_text_repel(aes(label=chugaku)) + 
  labs(x = "浜岡原発からの距離（中学校区、km）", 
       y="原発再稼働態度の平均値\n（0=廃炉,1=停止,2=どちらともいえない,3=再稼働）",
       title="連続変数") +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5))


## 折れ線グラフ （genpatsuD）##
plotdataD <- as.data.frame(
  tapply(dn$genpatsuD, dn$kyori, mean, na.rm=TRUE))
colnames(plotdataD) <- "genpatsuD_mean"
plotdataD$kyori <- as.numeric(rownames(plotdataD))

## 中学校区ラベルを付ける
library(ggrepel) #ない場合はinstall.packages("ggrepel")
### ラベルデータをプロットデータに挿入
plotdataD$chugaku <- NA
for(i in 1:nrow(plotdataD)) {
  plotdataD$chugaku[i] <- 
    dn$chugaku[which(dn$kyori==plotdataD$kyori[i])][1]
}
plotdataD
### プロットする
p2 <- ggplot(plotdataD, aes(x=kyori, y=genpatsuD_mean)) + 
  geom_line() + geom_point(size = 2) + 
  geom_text_repel(aes(label=chugaku)) + 
  labs(x = "浜岡原発からの距離（中学校区、km）", 
       y="原発再稼働態度の平均値\n（0=再稼働以外,1=再稼働）",
       title="二値変数") +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5))

## 【Figure10】２つのプロットを１つにまとめる
##install.packages("cowplot")
library(cowplot) 
plot_grid(p1,p2, ncol=2)
## グラフを保存
ggsave("./out/ktfig10.png", width=7, height=4)
# ggsave("./out/ktfig10.jpg", width=7, height=4)

## 回帰分析を導入する ##

## 基本モデル（単回帰）with 連続従属変数
m1d <- lm(genpatsuC ~ kyori, data = dn)
##拡張モデル with 連続従属変数
m1dd <- lm(genpatsuC ~ kyori + nenrei10 + sexA + syokugyou + kyojyuureki, data = dn)
## 基本モデル（単回帰）with 二値従属変数
md1d <- lm(genpatsuD ~ kyori, data = dn)
##拡張モデル with 二値従属変数
md1dd <- lm(genpatsuD ~ kyori + nenrei10 + sexA + syokugyou + kyojyuureki, data = dn)

### 変数ラベル
vnmap <- list("(Intercept)" = "（定数項）",
              "kyori" = "浜岡原発までの距離",
              "nenrei10" = "年齢（10歳刻み）",
              "sexA" = "性別",
              "syokugyou" = "安定収入ありダミー",
              "kyojyuureki" = "長い居住歴ダミー")
### 【Table2】HTMLで表を出力
library(htmltools)
tab2 <- HTML(htmlreg(list(m1d, m1dd, md1d, md1dd), include.ci = FALSE,
                       digits=3, stars = c(0.001,0.01,0.05,0.1), 
                       symbol="+",  star.symbol = "*", inline.css = FALSE,
                       html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                       single.row = FALSE, caption = "",
                       custom.coef.map = vnmap, 
                       custom.header = list("連続従属変数" = 1:2, 
                                            "二値従属変数" = 3:4), 
                       custom.model.names =  c("基本","拡張","基本","拡張"),
                       custom.note = "%stars。OLS回帰分析結果。括弧内は標準誤差。すべてのモデルに国・年固定効果を含む。"))
## 表を仮表示
browsable(tab2)
## 表をoutに保存する
save_html(tab2, "./out/kttab2.html")
webshot(url="./out/kttab2.html", file="./out/kttab2.png", zoom=3, selector="table")

## 回帰分析結果を予測値で解釈する ##

## 予測値の算出(基本モデル、連続従属変数)
yosokuout1 <- genpr(dpr = dn,
                    mpr = m1d,
                    setx = "kyori", 
                    setxvals = plotdataC$kyori)

## 予測値の算出(拡張モデル、連続従属変数)
yosokuout2 <- genpr(dpr = dn,
                    mpr = m1dd,
                    setx = "kyori", 
                    setxvals = plotdataC$kyori)


# 読み込み
library(ggplot2) # ない場合はinstall.packages("ggplot2")
library(patchwork) #ない場合は install.packages("patchwork")

# 1つ目のグラフを変数に代入
p1 <- ggplot(yosokuout1) + 
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.1) + 
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.3) + 
  geom_line(aes(x=x, y=pr)) + 
  labs(x = "浜岡原発からの距離（中学校区、km）", 
       y="原発再稼働態度の予測平均値\n（0=廃炉,1=停止,2=どちらともいえない,3=再稼働）", 
       caption = "注：単回帰を使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw(base_size = 9) + # 全体の文字サイズを小さく設定
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 8),
        plot.caption = element_text(size = 7),
        legend.position = "bottom")

# 2つ目のグラフを変数に代入
p2 <- ggplot(yosokuout2) + 
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.1) + 
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.3) + 
  geom_line(aes(x=x, y=pr)) + 
  labs(x = "浜岡原発からの距離（中学校区、km）", 
       y="原発再稼働態度の予測平均値\n（0=廃炉,1=停止,2=どちらともいえない,3=再稼働）", 
       caption = "注：多変量回帰を使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw(base_size = 9) + # 全体の文字サイズを小さく設定
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 8),
        plot.caption = element_text(size = 7),
        legend.position = "bottom")


# 【Figure11】グラフを横に並べて描画
p1 | p2
## グラフを保存
ggsave("./out/ktfig11.png", width=7, height=4)


## 予測値の算出(基本モデル、二値従属変数)
yosokuout3 <- genpr(dpr = dn,
                    mpr = md1d,
                    setx = "kyori", 
                    setxvals = plotdataD$kyori)

## 予測値の算出(拡張モデル、連続従属変数)
yosokuout4 <- genpr(dpr = dn,
                    mpr = md1dd,
                    setx = "kyori", 
                    setxvals = plotdataD$kyori)

#二値変数版の予測値を描画する 
p3 <- ggplot(yosokuout3) + 
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.1) + 
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.3) + 
  geom_line(aes(x=x, y=pr)) + 
  labs(x = "浜岡原発からの距離（中学校区、km）", 
       y="原発再稼働態度の予測平均値\n（0=再稼働以外,1=再稼働）", 
       caption = "注：単回帰を使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5)) +
  theme_bw(base_size = 9) + 
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 8),
        plot.caption = element_text(size = 7),
        legend.position = "bottom")

p4 <- ggplot(yosokuout4) + 
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.1) + 
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.3) + 
  geom_line(aes(x=x, y=pr)) + 
  labs(x = "浜岡原発からの距離（中学校区、km）", 
       y="原発再稼働態度の予測値平均\n（0=再稼働以外,1=再稼働）", 
       caption = "注：多変量回帰を使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw(base_size = 9) + 
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 8),
        plot.caption = element_text(size = 7),
        legend.position = "bottom")

# 【Figure12】グラフを横に並べて表示
p3 | p4
## グラフを保存
ggsave("./out/ktfig12.png", width=7, height=4)

#################
## 仮説4の分析 ##
#################

##仮説2の候補②【市の広報媒体を情報源にしている人は、テレビ、新聞、SNSを情報源にする人よりも再稼働に賛成である】
##背景→東大朝日の分析の際、「地元に根付いたメディア」の存在を立地県のUPZ外で反対が増える理由にしている。その理論が成り立つかの実証になる。
##理由→「広報かけがわ」のバックナンバーを分析すると、浜岡原発に関係するワードに対してネガティブな意味で触れていることが少ない。
##　　　一方で、静岡新聞社では中部電力と地元のいざこざに関して連載記事にするなど、原発に対して批判的な内容が多い。


##原発再稼働に対する意見（0.廃炉にするべき～4.再稼働するべき）
##情報源が市の情報媒体である人
genpatsuofficial <- dn$genpatsuC[which(dn$official == 1)]


##情報源がテレビ・新聞である人（SNSはその人の関心に合わせた内容が表示されるため除外）
genpatsunotofficial <- dn$genpatsuC[which(dn$official == 0)]
#summary(genpatsunotofficial$Q18, useNA = "always")

##グラフで分布を見る
##情報源が市の広報媒体の人々
plot_official <- data.frame(
  opinion = dn$genpatsuC[which(dn$official == 1)],
  source = "市の広報媒体"
)

##情報源がテレビ・新聞である人々
plot_notofficial <- data.frame(
  opinion = dn$genpatsuC[which(dn$official == 0)],
  source = "テレビ・新聞"
)

##結合
plot_data_combined <- bind_rows(plot_official, plot_notofficial)
plot_data_combined <- bind_rows(plot_official, plot_notofficial) %>% 
  filter(!is.na(opinion))

# 3. 意見のカテゴリをファクター型（順序）に変換
plot_data_combined$opinion <- factor(
  plot_data_combined$opinion,
  levels = c(0, 1, 2, 3), 
  labels = c("0", "1", "2", "3") 
)

# 【Figure13】グラフの描画
ggplot(plot_data_combined, aes(x = opinion, fill = source)) +
  
  # 棒グラフを作成 (position="dodge"で並列表示)
  geom_bar(position = "dodge", color = "black") + 
  
  # ラベルとタイトルの設定
  labs(
    title = "情報源別：浜岡原発再稼働に対する意見",
    x = "再稼働に対する意見 (0=廃炉, 1=停止, 2=わからない, 3=再稼働)",
    y = "回答者数 (人数)",
    fill = "市に関する情報の情報源" # 凡例のタイトル
  ) +
  
  theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = "bottom")

## グラフを保存
ggsave("./out/ktfig13.png", width=6, height=4)


##原発再稼働に対する意見（0.廃炉にするべき, 1.再稼働するべき）
##情報源が市の情報媒体である人
genpatsuofficial2 <- dn$genpatsuD[which(dn$official == 1)]

##情報源がテレビ・新聞である人（SNSはその人の関心に合わせた内容が表示されるため除外）
genpatsunotofficial2 <- dn$genpatsuD[which(dn$official == 0)]
#summary(genpatsunotofficial$Q18, useNA = "always")

##グラフで分布を見る
##情報源が市の広報媒体の人々
plot_official2 <- data.frame(
  opinion = dn$genpatsuD[which(dn$official == 1)],
  source = "市の広報媒体"
)

##情報源がテレビ・新聞である人々
plot_notofficial2 <- data.frame(
  opinion = dn$genpatsuD[which(dn$official == 0)],
  source = "テレビ・新聞"
)

##結合
plot_data_combined2 <- bind_rows(plot_official2, plot_notofficial2)
plot_data_combined2 <- bind_rows(plot_official2, plot_notofficial2) %>% 
  filter(!is.na(opinion))

# 3. 意見のカテゴリをファクター型（順序）に変換
plot_data_combined2$opinion <- factor(
  plot_data_combined2$opinion,
  levels = c(0, 1), 
  labels = c("0", "1") 
)

# 【Figure14】グラフの描画
ggplot(plot_data_combined2, aes(x = opinion, fill = source)) +
  
  # 棒グラフを作成 (position="dodge"で並列表示)
  geom_bar(position = "dodge", color = "black") + 
  
  # ラベルとタイトルの設定
  labs(
    title = "情報源別：浜岡原発再稼働に対する意見",
    x = "再稼働に対する意見 (0=廃炉, 1=再稼働)",
    y = "回答者数 (人数)",
    fill = "市に関する情報の情報源" # 凡例のタイトル
  ) +
  
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = "bottom")
## グラフを保存
ggsave("./out/ktfig14.png", width=6, height=4)

#各群の平均値・中央値を確認する
#連続変数×情報源が市の公式媒体
summary(genpatsuofficial)
#連続変数×情報源がテレビ・新聞
summary(genpatsunotofficial)
#二値変数×情報源が市の公式媒体
summary(genpatsuofficial2)
#二値変数×情報源がテレビ・新聞
summary(genpatsunotofficial2)

## 掛川市に関する情報源
table(dt_kakegawa$`Q30-9`)
dn$infosource <- "SNS/その他/無回答"
dn$infosource[which(dt_kakegawa$`Q30-9` %in% c(1))] <- "新聞・テレビ"
dn$infosource[which(dt_kakegawa$`Q30-9` %in% c(2,3))] <- "市の広報" 
dn$infosource <- 
  factor(dn$infosource, 
         levels = c("新聞・テレビ","市の広報",
                    "SNS/その他/無回答"))
table(dn$infosource, useNA="always")

## 基本モデル（単回帰）with 連続従属変数
m2d <- lm(genpatsuC ~ kyori * infosource, data = dn)
##拡張モデル with 連続従属変数
m2dd <- lm(genpatsuC ~ kyori * infosource + nenrei10 + sexA + syokugyou + kyojyuureki, data = dn)
## 基本モデル（単回帰）with 二値従属変数
md2d <- lm(genpatsuD ~ kyori * infosource, data = dn)
##拡張モデル with 二値従属変数
md2dd <- lm(genpatsuD ~ kyori * infosource + nenrei10 + sexA + syokugyou + kyojyuureki, data = dn)


## もっときれいな結果
### 変数ラベル
vnmap <- list("(Intercept)" = "（定数項）",
              "kyori" = "浜岡原発までの距離",
              "infosource市の広報" = "情報源：市の広報",
              "infosourceSNS/その他/無回答" = "情報源：SNS/その他/無回答",
              "kyori:infosource市の広報" = "距離*市広報",
              "kyori:infosourceSNS/その他/無回答" = "距離*SNS他",
              "nenrei10" = "年齢（10歳刻み）",
              "sexA" = "性別",
              "syokugyou" = "安定収入ありダミー",
              "kyojyuureki" = "長い居住歴ダミー")
###【Table3】 HTMLで表を出力
library(htmltools)
tab3 <- HTML(htmlreg(list(m2d, m2dd, md2d, md2dd), include.ci = FALSE,
                       digits=3, stars = c(0.001,0.01,0.05,0.1), 
                       symbol="+",  star.symbol = "*", inline.css = FALSE,
                       html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                       single.row = FALSE, caption = "",
                       custom.coef.map = vnmap, 
                       custom.header = list("連続従属変数" = 1:2, 
                                            "二値従属変数" = 3:4), 
                       custom.model.names =  c("基本","拡張","基本","拡張"),
                       custom.note = "%stars。OLS回帰分析結果。括弧内は標準誤差。すべてのモデルに国・年固定効果を含む。"))
## 表を仮表示
browsable(tab3)
## 表をoutに保存する
save_html(tab3, "./out/kttab3.html")
webshot(url="./out/kttab3.html", file="./out/kttab3.png", zoom=3, selector="table")

## 予測値の算出(基本vs拡張モデル、連続従属変数)
yosokuout7a <- genpr(dpr = dn,
                     mpr = m2d,
                     setx = "infosource", 
                     setxvals = c("新聞・テレビ",
                                  "市の広報",
                                  "SNS/その他/無回答"),
                     datalab = "基本モデル")
yosokuout7b <- genpr(dpr = dn,
                     mpr = m2dd,
                     setx = "infosource", 
                     setxvals = c("新聞・テレビ",
                                  "市の広報",
                                  "SNS/その他/無回答"),
                     datalab = "拡張モデル")
yosokuout7 <- rbind(yosokuout7a, yosokuout7b)
yosokuout7$x <- factor(yosokuout7$x, unique(yosokuout7$x))
yosokuout7$datalab <- factor(yosokuout7$datalab,
                             levels = unique(yosokuout7$datalab))

## 【Figure15】予測値をプロットする
library(ggplot2) # ない場合はinstall.packages("ggplot2")
ggplot(yosokuout7) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr),color="white") +
  scale_x_discrete(labels = c("新聞/TV","市広報","SNS他")) + ## labelsを設定する
  facet_grid(.~datalab) +
  labs(x = "情報源", 
       y="原発再稼働態度の予測平均値\n（0=廃炉,1=停止,2=どちらともいえない,3=再稼働）", 
       caption = "注：多変量回帰を使用。エラーバーは、90％および95％信頼区間を示している。",
       subtitle = "掛川市に関する情報源") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ktfig15.png", width=6, height=4)

## 予測値の算出(拡張モデル、二値従属変数)
yosokuout8a <- genpr(dpr = dn,
                     mpr = md2d,
                     setx = "infosource", 
                     setxvals = c("新聞・テレビ",
                                  "市の広報",
                                  "SNS/その他/無回答"),
                     datalab = "基本モデル")
yosokuout8b <- genpr(dpr = dn,
                     mpr = md2dd,
                     setx = "infosource", 
                     setxvals = c("新聞・テレビ",
                                  "市の広報",
                                  "SNS/その他/無回答"),
                     datalab = "拡張モデル")
yosokuout8 <- rbind(yosokuout8a, yosokuout8b)
yosokuout8$x <- factor(yosokuout8$x, unique(yosokuout8$x))
yosokuout8$datalab <- factor(yosokuout8$datalab,
                             levels = unique(yosokuout8$datalab))

## 【Figure16】予測値をプロットする
library(ggplot2) # ない場合はinstall.packages("ggplot2")
ggplot(yosokuout8) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr),color="white") +
  scale_x_discrete(labels = c("新聞/TV","市広報","SNS他")) + ## labelsを設定する
  facet_grid(.~datalab) +
  labs(x = "情報源", 
       y="原発再稼働態度の予測平均値\n（0=再稼働以外,1=再稼働）", 
       caption = "注：多変量回帰を使用。塗りつぶしは、90％および95％信頼区間を示している。",
       subtitle = "掛川市に関する情報源") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ktfig16.png", width=6, height=4)

## 予測値の算出(拡張モデル、連続従属変数)
yosokuout5 <- genpr(dpr = dn,
                    mpr = m2dd,
                    setx = "kyori", 
                    setxvals = plotdataC$kyori,
                    setm = "infosource",
                    setmvals = list("新聞/TV" = "新聞・テレビ",
                                    "市広報" = "市の広報",
                                    "SNS他" = "SNS/その他/無回答"))

## 【Figure17】予測値をプロットする
library(ggplot2) # ない場合はinstall.packages("ggplot2")
ggplot(yosokuout5) + 
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.1) + 
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.3) + 
  geom_line(aes(x=x, y=pr)) + 
  facet_grid(.~labelledm) + 
  labs(x = "浜岡原発からの距離（中学校区、km）", 
       y="原発再稼働態度の予測平均値\n（0=廃炉,1=停止,2=どちらともいえない,3=再稼働）", 
       caption = "注：多変量回帰を使用。塗りつぶしは、90％および95％信頼区間を示している。",
       subtitle = "掛川市に関する情報源") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ktfig17.png", width=6, height=4)

## 予測値の算出(拡張モデル、二値従属変数)
yosokuout6 <- genpr(dpr = dn,
                    mpr = md2dd,
                    setx = "kyori", 
                    setxvals = plotdataD$kyori,
                    setm = "infosource",
                    setmvals = list("新聞/TV" = "新聞・テレビ",
                                    "市広報" = "市の広報",
                                    "SNS他" = "SNS/その他/無回答"))

## 【Figure18】予測値をプロットする
library(ggplot2) # ない場合はinstall.packages("ggplot2")
ggplot(yosokuout6) + 
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.1) + 
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.3) + 
  geom_line(aes(x=x, y=pr)) + 
  facet_grid(.~labelledm) + 
  labs(x = "浜岡原発からの距離（中学校区、km）", 
       y="原発再稼働態度の予測平均値\n（0=再稼働以外,1=再稼働）", 
       caption = "注：多変量回帰を使用。塗りつぶしは、90％および95％信頼区間を示している。",
       subtitle = "掛川市に関する情報源") + 
  theme_bw() + 
  theme(
    legend.position="bottom",
    plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ktfig18.png", width=6, height=4)
