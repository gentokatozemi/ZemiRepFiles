## 選挙が近いことによって、政治家は選挙争点について述べるようになるのか

## 更新履歴：
## 2026/2/1 稲垣和由（提出版）
## 2026/02/16 加藤言人（最終調整）

##########
## 準備 ## 
##########

## このコードファイルがあるディレクトリを
## ワーキングディレクトリにする
dirhere <- dirname(rstudioapi::getActiveDocumentContext()$path) 
## ワーキングディレクトリをセット
setwd(dirhere)
getwd() # 

## 図表出力用のフォルダ（out）を作成
if (!"./out" %in% list.dirs()) dir.create("./out")

## 必要なパッケージ（なければinstall.packages("パッケージ名")）を実行。
library(readxl) #エクセルデータの読み込み
library(stringr) # 文字データの取り扱い
library(ggplot2) ## グラフ生成用
library(texreg) ## 回帰表の出力
library(htmltools) ## HTMLで表をプレビュー

######################
## データの読み込み ##
######################

## 首長アンケートデータの読み込み
dt <- read_xlsx("稲垣和由_卒論コーディングデータ.xlsx")
names(dt) ## 列名の確認

## 市町村レベルデータの読み込み
library(readxl)
### 「第１面事項_2020年」シートの読み込み
#### まずは、列名データを作成。5,7,9行目に必要なデータがあるように思われる。
tmp <- read_xlsx("major_results_2020.xlsx", 
          sheet="第１面事項_2020年", ## シートの指定
          skip = 4, ## 最初の4行は無視
          n_max = 5, ## 5,6,7,8,9の5行だけ読み込み
          col_names = FALSE ## 列名は設定しない
          ) 
tmp <- as.data.frame(t(as.matrix(tmp[c(1,3,5),]))) # 5,7,9行目だけ選択して転置
tmp$V1 <- ifelse(is.na(tmp$V1),"",tmp$V1) ## 欠損値を空白と置き換え
tmp$V2 <- ifelse(is.na(tmp$V2),"",tmp$V2) ## 欠損値を空白と置き換え
tmp$V3 <- ifelse(is.na(tmp$V3),"",tmp$V3) ## 欠損値を空白と置き換え
#### V1については空白セルを1つ前のデータで埋める
for(i in 2:nrow(tmp)) if(tmp$V1[i]=="") tmp$V1[i] <- tmp$V1[i-1]
tmp <- str_squish(paste0(tmp$V1,tmp$V2,tmp$V3)) ## 5,7,9行目の文字を結合して不要な空白を削除
tmp　## チェック
#### データを読み込む
munidt1 <- read_xlsx("major_results_2020.xlsx", 
                     sheet="第１面事項_2020年", ## シートの指定
                     skip = 9, ## 最初の9行はデータじゃないので無視
                     na = c("","-"), ## 空白セルと-のセルを欠損とみなす
                     col_names = tmp) ## さっき作った列名を挿入
names(munidt1)
### 「第２面事項_2020年」シートの読み込み
#### まずは、列名データを作成。5,7,9行目に必要なデータがあるように思われる。
tmp <- read_xlsx("major_results_2020.xlsx", 
                 sheet="第２面事項_2020年", ## シートの指定
                 skip = 4, ## 最初の4行は無視
                 n_max = 5, ## 5,6,7,8,9の5行だけ読み込み
                 col_names = FALSE ## 列名は設定しない
) 
tmp <- as.data.frame(t(as.matrix(tmp[c(1,3,5),]))) # 5,7,9行目だけ選択して転置
tmp$V1 <- ifelse(is.na(tmp$V1),"",tmp$V1) ## 欠損値を空白と置き換え
tmp$V2 <- ifelse(is.na(tmp$V2),"",tmp$V2) ## 欠損値を空白と置き換え
tmp$V3 <- ifelse(is.na(tmp$V3),"",tmp$V3) ## 欠損値を空白と置き換え
#### V1については空白セルを1つ前のデータで埋める
for(i in 2:nrow(tmp)) if(tmp$V1[i]=="") tmp$V1[i] <- tmp$V1[i-1]
tmp <- str_squish(paste0(tmp$V1,tmp$V2,tmp$V3)) ## 5,7,9行目の文字を結合して不要な空白を削除
tmp　## チェック
#### データを読み込む
munidt2 <- read_xlsx("major_results_2020.xlsx", 
                     sheet="第２面事項_2020年", ## シートの指定
                     skip = 9, ## 最初の9行はデータじゃないので無視
                     na = c("","-"), ## 空白セルと-のセルを欠損とみなす
                     col_names = tmp) ## さっき作った列名を挿入

### アンケートデータと市町村データの行を一致させる
#### munidt1とmunidt2の行は一緒なのでmunidt1だけを見ればいい
all(munidt1$`地域都道府県・市区町村名`==munidt2$`地域都道府県・市区町村名`)
#### アンケートの各行が、市町村データのどの行に対応するのかを検出
locmatch <- match(dt$municode,
                  ### munidtのコードはアンダーバーより後を削除して数値化
                  as.numeric(gsub("_.*$","",munidt1$`地域都道府県・市区町村名`)))
table(is.na(locmatch)) ## 欠損値はなしなので、すべて対応できている


###############
## 変数作成　##
###############

## 仮説群
## 仮説1: 選挙が近くなると、調査回答をしやすくなる
## 仮説2: 選挙が近くなると、イチ押し政策回答が長くなる
## 仮説3: 選挙が近くなると、イチ押し政策で子育てに言及しやすくなる
## 仮説4: 仮説3の効果は、市区に比べて町村では弱くなる

## 分析用の新しいデータの作成 ##
dn <- data.frame(prefcode = dt$prefcode,
                  pref = dt$pref,
                  municode = dt$municode,
                  muni = dt$muni)

## 従属変数Y の作成 ##

## 調査回答の有無（0が無回答、1が回答）
dn$answered <- dt$answered
summary(dn$answered)

## イチオシ政策回答の文字数
dn$ichioshi_length <- nchar(dt$Q08)
dn$ichioshi_length[which(is.na(dt$Q08))] <- 0 ## 質問未回答は0文字
dn$ichioshi_length[which(dt$answered==0)] <- NA ## 調査未回答は欠損
summary(dn$ichioshi_length) ## 異常に長い文字数の回答がある
table(dn$ichioshi_length>200) ## 制限字数超えは182件 
table(dn$ichioshi_length>300) ## 300字超えは11件だけ
sort(dn$ichioshi_length[which(nchar(dt$Q08)>300)])
## 300字以上は300字として扱うことにする（人為的措置）
dn$ichioshi_length[which(nchar(dt$Q08)>300)] <- 300 
## 記述統計をプロットする（連続変数として扱う）(図８)
ggplot(dn, aes(x=ichioshi_length, 
                y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 10, color = "white") + 
  labs(x = "イチオシ政策の回答文字数", 
       y="割合") + 
  theme_minimal()
ggsave("./out/ikfig8.png", width=6, heigh=4)

## 子育て政策への言及（1=言及有、0=言及無し、NA=質問／調査未回答）
dn$kosodate_genkyu <- dt$子育て
table(dn$kosodate_genkyu, useNA="always")

## 子育て政策の具体的なものへの言及
##（1=言及有、0=言及無し、NA=質問／調査未回答もしくは子育て政策言及無し）
dn$kosodate_gutai <- dt$具体
table(dn$kosodate_gutai, useNA="always")
table(dn$kosodate_genkyu,dn$kosodate_gutai, useNA="always") 

## 子育て政策の具体的なものの成果への言及
##（1=言及有、0=言及無し、NA=質問／調査未回答もしくは具体的な子育て政策言及無し）
dn$kosodate_seika <- dt$成果
table(dn$kosodate_seika, useNA="always")
table(dn$kosodate_gutai,dn$kosodate_seika, useNA="always") 

## 子育て政策の言及度スケール
dn$kosodate_level <- NA ## とりあえず初めは欠損
dn$kosodate_level[which(is.na(dn$kosodate_genkyu))] <- NA #質問に回答せず
dn$kosodate_level[which(dn$kosodate_genkyu==0)] <- 0 #質問は回答したが子育ては言及せず
dn$kosodate_level[which(dn$kosodate_genkyu==1)] <- 1 #質問に回答し子育て政策に言及
dn$kosodate_level[which(dn$kosodate_gutai==1)] <- 2 #質問に回答し具体的な子育て政策に言及
dn$kosodate_level[which(dn$kosodate_seika==1)] <- 3 #質問に回答し具体的な子育て政策成果に言及
table(dn$kosodate_level)

## 記述統計をプロットする（連続変数として扱う）（図１０）
ggplot(dn, aes(x=kosodate_level, 
               y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 4, color = "white") + 
  scale_x_continuous(breaks = c(0,1,2,3),
                     labels = c("言及無し(0)",
                                "言及有り\n具体性なし(1)",
                                "具体性あり\n成果なし(2)",
                                "具体性あり\n成果あり(3)")) +
  labs(x = "イチ押し政策の子育て政策言及度", 
       y="割合") + 
  theme_minimal()
## グラフを保存
ggsave("./out/ikfig10.png", width = 6, height = 4)

## 独立変数Xの作成 ##

## 次回選挙までの年数（便宜的に2023年3月1日を起点とし、1年は365日とする）
### 任期は4年と想定し、前選挙からの経過日数を365*4日から引いて365で割る
range(as.Date(dt$前回選挙))
dn$yrsnext <- as.numeric(4*365 - (as.Date("2023-03-01") - as.Date(dt$前回選挙)))/365
summary(dn$yrsnext)

## 記述統計をプロットする（図１）
ggplot(dn, aes(x=yrsnext, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 8, color = "white") + 
  scale_x_continuous(breaks=c(0,1,2,3,4)) + 
  labs(x = "次回選挙までの想定年数", 
       y="割合") +
  theme_minimal()
ggsave("./out/ikfig1.png", width=6, heigh=4)

## 条件付け変数Mの作成 ##

## 町村 vs. 市区
dn$choson <- NA
dn$choson[grep("市$",dt$muni)] <- 0
dn$choson[grep("区$",dt$muni)] <- 0
dn$choson[grep("町$",dt$muni)] <- 1
dn$choson[grep("村$",dt$muni)] <- 1
table(dn$choson, useNA="always")

## 交絡変数Zの作成 ##

## 前回選挙無投票
dn$mutohyo <- ifelse(dt$前回選挙投票の有無==0,1,0)
table(dn$mutohyo, useNA="always")

## 年齢（10歳刻み）
dn$nenrei10 <- dt$年齢/10 
table(is.na(dn$nenrei10)) ## 欠損値はなし
hist(dn$nenrei10) ## 分布を簡易的にチェック
## 分布図を整える（図３）
ggplot(dn, aes(x=nenrei10, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 14, color = "white") + 
  # scale_x_continuous(breaks=c(,1,2,3,4)) + 
  labs(x = "年齢＊1/10", 
       y="割合") +
  theme_minimal()
ggsave("./out/ikfig3.png", width=6, heigh=4)

## 当選回数
dn$tosenN <- dt$当選回数
table(dn$tosenN, useNA="always") ## 分布を簡易的にチェック
## 分布図を整える（図２）
ggplot(dn, aes(x=tosenN, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 12, color = "white") + 
  # scale_x_continuous(breaks=c(,1,2,3,4)) + 
  labs(x = "当選回数", 
       y="割合") +
  theme_minimal()
ggsave("./out/ikfig2.png", width=6, heigh=4)

## 人口（市町村データの挿入例です。同じように他の変数もできます）
summary(munidt1$`総人口（男女別）総数（人）`) ## 1件欠損あり　
dn$jinko <- munidt1$`総人口（男女別）総数（人）`[locmatch]
hist(dn$jinko) ## そのままだと偏りが大きすぎる
hist(log(dn$jinko)) ## 対数化した方が正規分布に近くなる
## 分布図を整える（図４）
ggplot(dn, aes(x=log(jinko), y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 12, color = "white") + 
  # scale_x_continuous(breaks=c(,1,2,3,4)) + 
  labs(x = "人口（対数化）", 
       y="割合") +
  theme_minimal()
ggsave("./out/ikfig4.png", width=6, heigh=4)

## 人口密度（市町村データの挿入例です。同じように他の変数もできます）
summary(munidt1$`総人口（男女別）人口密度（人/km2）`) ## 1件欠損あり　
dn$jinkomitsudo <- munidt1$`総人口（男女別）人口密度（人/km2）`[locmatch]
hist(dn$jinkomitsudo) ## そのままだと偏りが大きすぎる
hist(log(dn$jinkomitsudo)) ## 対数化した方が正規分布に近くなる
## 分布図を整える（図５）
ggplot(dn, aes(x=log(jinkomitsudo), y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 12, color = "white") + 
  # scale_x_continuous(breaks=c(,1,2,3,4)) + 
  labs(x = "人口密度（対数化）", 
       y="割合") +
  theme_minimal()
ggsave("./out/ikfig5.png", width=6, heigh=4)

## 15歳未満人口比（市町村データの挿入例です。同じように他の変数もできます）
summary(munidt1$`人口構成比［年齢別］(男女「総数」）15歳未満（％）`) 
dn$propunder15 <- munidt1$`人口構成比［年齢別］(男女「総数」）15歳未満（％）`[locmatch]
hist(dn$propunder15) ## おおむね正規分布
## 分布図を整える（図６）
ggplot(dn, aes(x=propunder15, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 12, color = "white") + 
  # scale_x_continuous(breaks=c(,1,2,3,4)) + 
  labs(x = "15歳未満人口比", 
       y="割合") +
  theme_minimal()
ggsave("./out/ikfig6.png", width=6, heigh=4)

## 地域ダミー

## 北海道・東北（参照カテゴリなので分析には含めない）  
dn$hokkaido_tohoku <- 0
dn$hokkaido_tohoku[which(dt$pref%in%c("北海道","青森県",
                                       "岩手県","宮城県",
                                       "秋田県","山形県",
                                       "福島県"))] <- 1
table(dn$hokkaido_tohoku)
## 関東
dn$kanto <- 0
dn$kanto[which(dt$pref%in%c("茨城県","栃木県","群馬県",
                             "埼玉県","千葉県","東京都",
                             "神奈川県"))] <- 1
table(dn$kanto)
## 中部・北陸
dn$chubu_hokuriku <- 0
dn$chubu_hokuriku[which(dt$pref%in%c("新潟県","長野県",
                                      "山梨県","富山県",
                                      "石川県","福井県",
                                      "岐阜県","静岡県",
                                      "愛知県"))] <- 1
table(dn$chubu_hokuriku)
## 関西
dn$kansai <- 0
dn$kansai[which(dt$pref%in%c("三重県","滋賀県","京都府",
                              "大阪府","兵庫県","奈良県",
                              "和歌山県"))] <- 1
table(dn$kansai)
## 中国・四国
dn$chugoku_shikoku <- 0
dn$chugoku_shikoku[which(dt$pref%in%c("鳥取県","島根県",
                                       "岡山県","広島県",
                                       "山口県","徳島県",
                                       "香川県","愛媛県",
                                       "高知県"))] <- 1
table(dn$chugoku_shikoku)
## 九州・沖縄
dn$kyushu_okinawa <- 0
dn$kyushu_okinawa[which(dt$pref%in%c("福岡県","佐賀県",
                                      "長崎県","熊本県",
                                      "大分県","宮崎県",
                                      "鹿児島県","沖縄県"))] <- 1
table(dn$kyushu_okinawa)

###########
## 分析　##
###########

## 仮説群（もう1回）
## 仮説1: 選挙が近くなると、調査回答をしやすくなる
## 仮説2: 選挙が近くなると、イチ押し政策回答が長くなる
## 仮説3: 選挙が近くなると、イチ押し政策で子育てに言及しやすくなる
## 仮説4: 仮説3の効果は、市区に比べて町村では弱くなる

## 仮説１：基本モデル
m1b <- lm(answered ~ yrsnext, data = dn)
## 仮説１：交絡変数あり
m1 <- lm(answered ~  yrsnext + 
           mutohyo + nenrei10 + tosenN + 
           choson + log(jinko) + 
           log(jinkomitsudo) + propunder15, dn)

## 仮説２：基本モデル
m2b <- lm(ichioshi_length ~ yrsnext, data = dn)
## 仮説２：交絡変数あり
m2 <- lm(ichioshi_length ~  yrsnext + 
           mutohyo + nenrei10 + tosenN + 
           choson + log(jinko) + 
           log(jinkomitsudo) + propunder15, dn)

## 仮説３：基本モデル
m3b <- lm(kosodate_level ~ yrsnext, data = dn)
## 仮説３：交絡変数あり
m3 <- lm(kosodate_level ~  yrsnext + 
            mutohyo + nenrei10 + tosenN + 
            choson + log(jinko) + 
            log(jinkomitsudo) + propunder15, dn)

## 仮説３＆４：基本モデル
m34b <- lm(kosodate_level ~ yrsnext*choson, data = dn)
## 仮説３＆４：交絡変数あり
m34 <- lm(kosodate_level ~  yrsnext*choson + 
           mutohyo + nenrei10 + tosenN + 
           choson + log(jinko) + 
            log(jinkomitsudo) + propunder15, dn)

## 簡易的な結果（仮説１・２）
screenreg(list(m1b, m1, m2b, m2),
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

## 簡易的な結果（仮説３＆４）
screenreg(list(m3b, m3, m34b, m34),
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

## もっときれいな結果
### 変数ラベル
vnmap <- list("(Intercept)" = "（定数項）",
              "yrsnext" = "次回選挙までの年数",
              "yrsnext:choson" = "次回選挙までの年数×町村",
              "mutohyo" = "前回無投票ダミー",
              "nenrei10" = "年齢（10歳刻み）",
              "tosenN" = "当選回数",
              "choson" = "町村=1, 市区=0",
              "log(jinko)" = "人口（対数）",
              "log(jinkomitsudo)" = "人口密度（対数）",
              "propunder15" = "15歳未満人口比（％）")

### HTMLで表を出力（表１）
tab1 <- HTML(htmlreg(list(m1b, m1, m2b, m2),
          digits=3, stars = c(0.001,0.01,0.05,0.1), 
          symbol="+",  star.symbol = "*", inline.css = FALSE,
          html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
          single.row = FALSE, caption = "",
          custom.coef.map = vnmap, 
          custom.header = list("仮説１" = 1:2, "仮説２" = 3:4), 
          custom.model.names =  c("基本","拡張","基本","拡張"),
          custom.note = "%stars。OLS回帰分析による結果。括弧内は標準誤差。"))
## 表を仮表示
browsable(tab1)
## 表をoutに保存する
save_html(tab1, "./out/iktab1.html")
webshot(url="./out/iktab1.html", file="./out/iktab1.png", zoom=3, selector="table")

### HTMLで表を出力（表２）
tab2 <- HTML(htmlreg(list(m3b, m3, m34b, m34),
          digits=3, stars = c(0.001,0.01,0.05,0.1), 
          symbol="+",  star.symbol = "*", inline.css = FALSE,
          html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
          single.row = FALSE, caption = "",
          custom.coef.map = vnmap, 
          custom.header = list("仮説３" = 1:2, "仮説３＆４" = 3:4), 
          custom.model.names =  c("基本","拡張","基本","拡張"),
          custom.note = "%stars。OLS回帰分析による結果。括弧内は標準誤差。"))
## 表を仮表示
browsable(tab2)
## 表をoutに保存する
save_html(tab2, "./out/iktab2.html")
webshot(url="./out/iktab2.html", file="./out/iktab2.png", zoom=3, selector="table")

###########################################################
## 予測値の算出########
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

## 仮説１ ##

## 予測値の算出
yosokuout1 <- genpr(dpr = dn,
                   mpr = m1,
                   setx = "yrsnext", 
                   setxvals = seq(0,4,length=11))

## 予測値をプロットする（図７）
ggplot(yosokuout1, aes(y=pr)) + 
  # geom_errorbar(aes(x=as.factor(x),ymin=lo95, ymax=up95), 
  #               width=0.1, linewidth=0.5) + 
  # geom_errorbar(aes(x=as.factor(x),ymin=lo90, ymax=up90), 
  #               width=0, linewidth=2) + 
  # geom_point(aes(x=as.factor(x)), color="white") + 
  # scale_x_discrete() +
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) +
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) +
  geom_line(aes(x=x, y=pr), linewidth=1) +
  labs(x="次回選挙までの年数",
       y="予測調査回答率", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ikfig7.png", width = 6, height = 4)

## 仮説２ ##

## 予測値の算出
yosokuout2 <- genpr(dpr = dn,
                    mpr = m2,
                    setx = "yrsnext", 
                    setxvals = seq(0,4,length=11))

## 予測値をプロットする（図９）
ggplot(yosokuout2, aes(y=pr)) + 
  # geom_errorbar(aes(x=as.factor(x),ymin=lo95, ymax=up95), 
  #               width=0.1, linewidth=0.5) + 
  # geom_errorbar(aes(x=as.factor(x),ymin=lo90, ymax=up90), 
  #               width=0, linewidth=2) + 
  # geom_point(aes(x=as.factor(x)), color="white") + 
  # scale_x_discrete() +
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) +
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) +
  geom_line(aes(x=x, y=pr), linewidth=1) +
  labs(x="次回選挙までの年数",
       y="予測イチ押し政策文字数", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ikfig9.png", width = 6, height = 4)

## 仮説３ ##

## 予測値の算出
yosokuout3 <- genpr(dpr = dn,
                    mpr = m3,
                    setx = "yrsnext", 
                    setxvals = seq(0,4,length=11))

## 予測値をプロットする（図１１）
ggplot(yosokuout3, aes(y=pr)) + 
  # geom_errorbar(aes(x=as.factor(x),ymin=lo95, ymax=up95), 
  #               width=0.1, linewidth=0.5) + 
  # geom_errorbar(aes(x=as.factor(x),ymin=lo90, ymax=up90), 
  #               width=0, linewidth=2) + 
  # geom_point(aes(x=as.factor(x)), color="white") + 
  # scale_x_discrete() +
  geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) +
  geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) +
  geom_line(aes(x=x, y=pr), linewidth=1) +
  labs(x="次回選挙までの年数",
       y="予測子育て政策言及度", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ikfig11.png", width = 6, height = 4)

## 仮説３＆４ ##

## 予測値の算出
yosokuout4 <- genpr(dpr = dn,
                    mpr = m34,
                    setx = "yrsnext", 
                    setxvals = seq(0,4,length=11),
                    setm = "choson",
                    setmvals = list("市区（0）" = 0,
                                    "町村（1）" = 1))

## 予測値をプロットする（図12）
ggplot(yosokuout4, aes(x=x, y=pr)) + 
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
  # scale_x_discrete() + ## labelsを設定する
  geom_ribbon(aes(ymin=lo95, ymax=up95,
                  fill = labelledm), alpha=0.3) +
  geom_ribbon(aes(ymin=lo90, ymax=up90,
                  fill = labelledm), alpha=0.5) +
  geom_line(aes(linetype=labelledm)) +
  scale_shape_discrete(name = "自治体種別") + 
  scale_linetype_discrete(name = "自治体種別") + 
  scale_color_brewer(name = "自治体種別", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "自治体種別", 
                    type = "qual", palette = 2) + 
  labs(x="次回選挙までの年数",
       y="予測子育て政策言及度", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ikfig12.png", width = 6, height = 4)

###########################################################
## 限界効果の算出関数（必要なければ内容を変更しないこと）##
##（ここから）#############################################
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
genkaiout <- intereff(m34, "yrsnext", "choson", 
                      c(0,1), nsim=2)

## 限界効果プロット(図13)
library(ggplot2)
ggplot(genkaiout, aes(y=est, x=mod, ymin=lo95, ymax=up95)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("市区（0）","町村（1）")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：子育て政策への言及度",
       y="次回選挙までの年数の限界効果",
       x="自治体種別",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/ikfig13.png", width = 6, height = 4)
