#論文タイトル：暗号資産の法定通貨化がもたらした影響

## 更新履歴：
## 2026/01/28 柴田浩嘉（提出版）
## 2026/02/11 加藤言人（最終調整）

##############
## 予備設定 ##
##############

# ワークスペースを掃除する
rm(list=ls())

# ワーキングディレクトリ
## setwd("~/GoogleDrive/Lectures/Zemi_Meiji/ZemiPrivateData/analysis_codes/04_regression")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # 自動設定
getwd() 

## 図表出力用のフォルダ（out）を作成
if (!"./out" %in% list.dirs()) dir.create("./out")

# パッケージ（追加機能）の追加
## エラーが出たら、install.packages("パッケージ名")でインストールすること。
library(readxl) # エクセルデータの読み込み
library(ggplot2) # グラフ出力に使う。
theme_set(theme_bw()) # Windowsの場合
# theme_set(theme_bw(base_family = "HiraKakuProN-W3")) # Macの場合
library(texreg) ## 回帰表の出力
library(htmltools) ## HTMLで表をプレビュー

##############################
## データセットのインポート ##
##############################

## データを読み込む
d <- read_xlsx("ゼミ仮想通貨データ.xlsx", 
               skip = 1, sheet = "Sheet2")

# View(d) ## データ全体を概観する。
names(d)

################
## 仮説（例） ##
################

# 仮説１：2021-2024の期間は、2014-2020の期間よりも外国からの送金額の増加率が高くなる。

# 仮説２：仮説1の効果は、エルサルバドルの方が他の中南米国家より大きい

##########
## 変数 ##
##########

## 新しいデータセットを作成
## idで個人を特定
dn <- data.frame(id = 1:nrow(d), #回答者ID
                 year = d$year, #調査年
                 country = as.factor(d$country)) #国名

## 従属変数 ##

## 外国からの送金額の増加率
dn$remittance <- d$remittance
# hist(dn$remittance)

### GDP growth
summary(d$economy)
dn$growth <- d$economy

## インターネットの利用率
table(d$internet)
dn$internet <- d$internet

## 観光客数
table(d$tourist)
dn$tourist <- d$tourist

## 独立変数 ##

## 2021-2024 = 1, 2014-2020 = 0
dn$after21 <- NA
dn$after21[which(dn$year%in%c(2014:2020))] <- 0
dn$after21[which(dn$year%in%c(2021:2024))] <- 1
table(dn$after21)

## 条件付け変数 ##
##国名を計算する##

table(dn$country)
##その列のすべての要素に**欠損値（NA）**を代入する操作##
dn$elsalvador <- NA
##、条件を満たす行にのみ値「1」を代入する操作を行っている##
dn$elsalvador[which(dn$country=="エルサルバドル")] <- 1
##件を満たさない残りのすべての行に値「0」を代入する##
dn$elsalvador[which(dn$country!="エルサルバドル")] <- 0
##エルサルバドルのデータが何件あり、それ以外の国名が何件あるか##
table(dn$elsalvador)
## 同じように他の国の変数を作る
dn$c1 <- ifelse(dn$country=="アルゼンチン",1,0)
dn$c2 <- ifelse(dn$country=="ウルグアイ",1,0)
dn$c3 <- ifelse(dn$country=="エクアドル",1,0)
dn$c4 <- ifelse(dn$country=="ガイアナ",1,0)
dn$c5 <- ifelse(dn$country=="ギニア",1,0)
dn$c6 <- ifelse(dn$country=="グアテマラ",1,0)
dn$c7 <- ifelse(dn$country=="コスタリカ",1,0)
dn$c8 <- ifelse(dn$country=="コロンビア",1,0)
dn$c9 <- ifelse(dn$country=="スリナム",1,0)
dn$c10 <- ifelse(dn$country=="チリ",1,0)
dn$c11 <- ifelse(dn$country=="ニカラグア",1,0)
dn$c12 <- ifelse(dn$country=="パナマ",1,0)
dn$c13 <- ifelse(dn$country=="パラグアイ",1,0)
dn$c14 <- ifelse(dn$country=="ブラジル",1,0)
dn$c15 <- ifelse(dn$country=="ベネズエラ",1,0)
dn$c16 <- ifelse(dn$country=="べリーズ",1,0)
dn$c17 <- ifelse(dn$country=="ペルー",1,0)
dn$c18 <- ifelse(dn$country=="ボリビア",1,0)
dn$c19 <- ifelse(dn$country=="メキシコ",1,0)

## 交絡変数（例） ##

## CPI
dn$CPI <- d$CPI
summary(dn$CPI)

### 人口 ヒストグラムをプリント（図１）
hist(d$populataion_million)
ggplot(d, aes(x=populataion_million)) + 
  geom_histogram(color="white",bins=9) + 
  labs(x="国の総人口（単位：100万人）", y="頻度")
ggsave("./out/shfig1.png", width=6, height=4)
### 人口を対数化
dn$logpop <- log(d$populataion_million)
hist(dn$logpop)

### 初等教育就学率
summary(d$elementary)
dn$elementary <- d$elementary

### 中等教育就学率
summary(d$tyuutou)
dn$tyuutou <- d$tyuutou

##########
## 分析 ##
##########

# パネルデータであるためyearとcountryは必ず投入する

## 仮説１：基本モデル
m1b <- lm(remittance ~ after21 * elsalvador + 
            as.factor(year)  + 
            elsalvador + 
            c1 + c2 + c3 + c4 + c5 + 
            c6 + c7 + c8 + c9 + c10 + 
            c11 + c12 + c13 + c14 + c15 + 
            c16 + c17 + c18, data = dn)

## 仮説１：交絡変数あり
m1 <- lm(remittance ~ after21 * elsalvador + 
           growth + CPI + logpop + 
           as.factor(year) + 
           elsalvador + 
           c1 + c2 + c3 + c4 + c5 + 
           c6 + c7 + c8 + c9 + c10 + 
           c11 + c12 + c13 + c14 + c15 + 
           c16 + c17 + c18, data = dn)

## 仮説２：基本モデル
m2b <- lm(growth ~ after21 * elsalvador + 
            as.factor(year)  + 
            elsalvador + 
            c1 + c2 + c3 + c4 + c5 + 
            c6 + c7 + c8 + c9 + c10 + 
            c11 + c12 + c13 + c14 + c15 + 
            c16 + c17 + c18, data = dn)

## 仮説２：交絡変数あり
m2 <- lm(growth ~ after21 * elsalvador + 
           remittance + logpop + 
           as.factor(year) + 
           elsalvador + 
           c1 + c2 + c3 + c4 + c5 + 
           c6 + c7 + c8 + c9 + c10 + 
           c11 + c12 + c13 + c14 + c15 + 
           c16 + c17 + c18, data = dn)

## 仮説３：基本モデル
m3b <- lm(internet ~ after21 * elsalvador + 
            as.factor(year)  + 
            elsalvador + 
            c1 + c2 + c3 + c4 + c5 + 
            c6 + c7 + c8 + c9 + c10 + 
            c11 + c12 + c13 + c14 + c15 + 
            c16 + c17 + c18, data = dn)

## 仮説３：交絡変数あり
m3 <- lm(internet ~ after21 * elsalvador + 
           growth + elementary + tyuutou +  
           as.factor(year) + 
           elsalvador + 
           c1 + c2 + c3 + c4 + c5 + 
           c6 + c7 + c8 + c9 + c10 + 
           c11 + c12 + c13 + c14 + c15 + 
           c16 + c17 + c18, data = dn)

## 仮説４：基本モデル
m4b <- lm(tourist ~ after21 * elsalvador + 
            as.factor(year)  + 
            elsalvador + 
            c1 + c2 + c3 + c4 + c5 + 
            c6 + c7 + c8 + c9 + c10 + 
            c11 + c12 + c13 + c14 + c15 + 
            c16 + c17 + c18, data = dn)

## 仮説４：交絡変数あり
m4 <- lm(tourist ~ after21 * elsalvador + 
           growth + logpop +  
           as.factor(year) + 
           elsalvador + 
           c1 + c2 + c3 + c4 + c5 + 
           c6 + c7 + c8 + c9 + c10 + 
           c11 + c12 + c13 + c14 + c15 + 
           c16 + c17 + c18, data = dn)

## 簡易的な結果
screenreg(list(m1b, m1, m2b, m2), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")
screenreg(list(m3b, m3, m4b, m4), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

## もっときれいな結果
### 変数ラベル
vnmap <- list("(Intercept)" = "（定数項）",
              "after21" = "2021年以降ダミー",
              "elsalvador" = "エルサルバドルダミー",
              "after21:elsalvador" = "2021年以降 x エルサルバドル",
              "remittance" = "外国投資増加率",
              "growth" = "GDP成長率",
              "CPI" = "CPI",
              "elementary" = "初等教育就学率",
              "tyuutou" = "中等教育就学率",
              "logpop" = "人口（対数）")

### HTMLで表を出力 仮説１及び２の基本モデル及び拡張モデルを表示（表１）
tab1 <- HTML(htmlreg(list(m1b, m1, m2b, m2), include.ci = FALSE,
                     digits=3, stars = c(0.001,0.01,0.05,0.1), 
                     symbol="+",  star.symbol = "*", inline.css = FALSE,
                     html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                     single.row = FALSE, caption = "",
                     custom.coef.map = vnmap, 
                     custom.header = list("H1:外国送金増加率" = 1:2, 
                                          "H2:GDP増加率 " = 3:4), 
                     custom.model.names =  c("基本","拡張","基本","拡張"),
                     custom.gof.rows = list("年固定効果" = c("有","有","有","有"),
                                            "国固定効果" = c("有","有","有","有")),
                     custom.note = "%stars。OLS回帰分析結果。括弧内は標準誤差。すべてのモデルに国・年固定効果を含む。"))
## 仮表示
browsable(tab1)
## 表をoutに保存する
save_html(tab1, "./out/shtab1.html")
library(webshot2)
webshot(url="./out/shtab1.html", file="./out/shtab1.png", zoom=3, selector="table")

### HTMLで表を出力 仮説３及び４の基本モデル及び拡張モデルを表示（表２）
tab2 <- HTML(htmlreg(list(m3b, m3, m4b, m4), include.ci = FALSE,
                     digits=3, stars = c(0.001,0.01,0.05,0.1), 
                     symbol="+",  star.symbol = "*", inline.css = FALSE,
                     html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                     single.row = FALSE, caption = "",
                     custom.coef.map = vnmap, 
                     custom.header = list("H3:ネット利用率" = 1:2, 
                                          "H4:観光客数" = 3:4), 
                     custom.model.names =  c("基本","拡張","基本","拡張"),
                     custom.gof.rows = list("年固定効果" = c("有","有","有","有"),
                                            "国固定効果" = c("有","有","有","有")),
                     custom.note = "%stars。OLS回帰分析結果。括弧内は標準誤差。すべてのモデルに国・年固定効果を含む。"))
## 仮表示
browsable(tab2)
## 表をoutに保存する
save_html(tab2, "./out/shtab2.html")
library(webshot2)
webshot(url="./out/shtab2.html", file="./out/shtab2.png", zoom=3, selector="table")

###########################################################
## 予測値の算出（必要なければ内容を変更しないこと）########
##（ここから）#############################################
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
  prout <- as.data.frame(t(apply(simv, 1, function(k) {
    
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
yosokuout1a <- genpr(dpr = subset(dn, elsalvador==1),
                     mpr = m1,
                     setx = "after21", 
                     setxvals = seq(0,1,length=2),
                     datalab = "エルサルバドル")
yosokuout1b <- genpr(dpr = subset(dn, elsalvador==0),
                     mpr = m1,
                     setx = "after21", 
                     setxvals = seq(0,1,length=2),
                     datalab = "エルサルバドル以外")
yosokuout1 <- rbind(yosokuout1a, yosokuout1b)
yosokuout1$labelledm <- factor(yosokuout1$datalab,
                               levels = rev(unique(yosokuout1$datalab)))
yosokuout1

## 予測値をプロットする（図２）
ggplot(yosokuout1, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels = c("2020年以前(0)","2021年以降(1)")) + ## labelsを設定する
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
  labs(x="調査年",
       y="外国からの送金額増加率の予測値", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))

## グラフを保存 仮説１の予測値
ggsave("./out/shfig2.png", width = 6, height = 4)

## 仮説２ ##

## 予測値の算出
yosokuout2a <- genpr(dpr = subset(dn, elsalvador==1),
                     mpr = m2,
                     setx = "after21", 
                     setxvals = seq(0,1,length=2),
                     datalab = "エルサルバドル")
yosokuout2b <- genpr(dpr = subset(dn, elsalvador==0),
                     mpr = m2,
                     setx = "after21", 
                     setxvals = seq(0,1,length=2),
                     datalab = "エルサルバドル以外")
yosokuout2 <- rbind(yosokuout2a, yosokuout2b)
yosokuout2$labelledm <- factor(yosokuout2$datalab,
                               levels = rev(unique(yosokuout2$datalab)))
yosokuout2

## 予測値をプロットする（図４）
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
  scale_x_discrete(labels = c("2020年以前(0)","2021年以降(1)")) + ## labelsを設定する
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
  labs(x="調査年",
       y="GDP増加率の予測値", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))

## グラフを保存　仮説２の予測値
ggsave("./out/shfig4.png", width = 6, height = 4)

## 仮説３ ##

## 予測値の算出
yosokuout3a <- genpr(dpr = subset(dn, elsalvador==1),
                    mpr = m3,
                    setx = "after21", 
                    setxvals = seq(0,1,length=2),
                    datalab = "エルサルバドル")
yosokuout3b <- genpr(dpr = subset(dn, elsalvador==0),
                     mpr = m3,
                     setx = "after21", 
                     setxvals = seq(0,1,length=2),
                     datalab = "エルサルバドル以外")
yosokuout3 <- rbind(yosokuout3a, yosokuout3b)
yosokuout3$labelledm <- factor(yosokuout3$datalab,
                             levels = rev(unique(yosokuout3$datalab)))

## 予測値をプロットする（図６）
ggplot(yosokuout3, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels = c("2020年以前(0)","2021年以降(1)")) + ## labelsを設定する
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
  labs(x="調査年",
       y="インターネット利用率の予測値", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))

## グラフを保存　仮説３の予測値
ggsave("./out/shfig6.png", width = 6, height = 4)

## 仮説４ ##

## 予測値の算出
yosokuout4a <- genpr(dpr = subset(dn, elsalvador==1),
                     mpr = m4,
                     setx = "after21", 
                     setxvals = seq(0,1,length=2),
                     datalab = "エルサルバドル")
yosokuout4b <- genpr(dpr = subset(dn, elsalvador==0),
                     mpr = m4,
                     setx = "after21", 
                     setxvals = seq(0,1,length=2),
                     datalab = "エルサルバドル以外")
yosokuout4 <- rbind(yosokuout4a, yosokuout4b)
yosokuout4$labelledm <- factor(yosokuout4$datalab,
                               levels = rev(unique(yosokuout4$datalab)))
yosokuout4

## 予測値をプロットする（図８）
ggplot(yosokuout4, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels = c("2020年以前(0)","2021年以降(1)")) + ## labelsを設定する
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
  labs(x="調査年",
       y="観光客数（単位：100万）の予測値", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))

## グラフを保存　仮説４の予測値
ggsave("./out/shfig8.png", width = 6, height = 4)

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

## 仮説１ ##

## 限界効果の出力
genkaiout <- intereff(m1, "after21", 
                      "elsalvador", c(0,1), nsim=2)

## 限界効果プロット（図３）
ggplot(genkaiout, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("エルサルバドル以外(0)",
                            "エルサルバドル(1)")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：外国からの送金額の増加率",
       y="2021年以降ダミーの限界効果",
       x="対象国",
       caption="注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))

## グラフを保存　仮説１ダミー
ggsave("./out/shfig3.png", width = 6, height = 4)

## 仮説２ ##

## 限界効果の出力（図５）
genkaiout <- intereff(m2, "after21", 
                      "elsalvador", c(0,1), nsim=2)

## 限界効果プロット
ggplot(genkaiout, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("エルサルバドル以外(0)",
                            "エルサルバドル(1)")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：GDP増加率",
       y="2021年以降ダミーの限界効果",
       x="対象国",
       caption="注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))

## グラフを保存　仮説２ダミー
ggsave("./out/shfig5.png", width = 6, height = 4)

## 仮説３ ##

## 限界効果の出力
genkaiout <- intereff(m3, "after21", 
                      "elsalvador", c(0,1), nsim=2)

## 限界効果プロット（図７）
ggplot(genkaiout, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("エルサルバドル以外(0)",
                            "エルサルバドル(1)")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：インターネット利用率",
       y="2021年以降ダミーの限界効果",
       x="対象国",
       caption="注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))

## グラフを保存　仮説３ダミー
ggsave("./out/shfig7.png", width = 6, height = 4)

## 仮説４ ##

## 限界効果の出力
genkaiout <- intereff(m4, "after21", 
                      "elsalvador", c(0,1), nsim=2)

## 限界効果プロット（図９）
ggplot(genkaiout, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("エルサルバドル以外(0)",
                            "エルサルバドル(1)")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：観光客数",
       y="2021年以降ダミーの限界効果",
       x="対象国",
       caption="注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))

## グラフを保存　仮説４ダミー
ggsave("./out/shfig9.png", width = 6, height = 4)

