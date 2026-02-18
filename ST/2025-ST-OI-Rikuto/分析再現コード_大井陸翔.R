# 近年のカビル人のカビル運動への真の動機

## 更新履歴：
## 2026/1/31 大井陸翔（提出版）
## 2026/02/14 加藤言人（最終調整）

# cleaning environment　
rm(list = ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # 自動設定
# setwd("C:/Users/大井陸翔/Desktop/ゼミ")
getwd()

## 図表出力用のフォルダ（out）を作成
if (!"./out" %in% list.dirs()) dir.create("./out")

# required packages
library(haven)
library(labelled)
library(gridExtra)
library(grid)
library(ggplot2)
library(texreg)
library(htmltools)

## Data Import
d <- read_sav("alg_r6_data.sav")

## New Data
dn <- data.frame(id = d$RESPNO) #回答者ID
nrow(dn) # 回答者数



###従属変数


## デモへの参加意欲
dn$protest <- NA
dn$protest[which(d$Q27E %in% c(0, 1))] <- 0
dn$protest[which(d$Q27E %in% c(2, 3, 4))] <- 1
table(dn$protest, useNA = "always")

# 以下のプロット作成コードは、従属変数であるデモへの参加意欲の分布を表す図1に対応。
# 欠損値を除いてプロット
g_protest <- ggplot(subset(dn, !is.na(protest)), aes(x = as.factor(protest))) +
  # 棒グラフ（y軸を割合に変換）
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count))), 
           fill = "grey40", width = 0.6) +
  
  # テーマ設定（レポート例に合わせた白黒テーマ）
  theme_bw() +
  
  # 軸ラベルとタイトル
  labs(x = "デモへの参加意欲 (0=なし, 1=あり)", 
       y = "回答割合") +
  
  # Y軸の目盛り調整（0.0 から始まり、見やすくする）
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), # %表示にする場合
                     expand = expansion(mult = c(0, 0.05))) +     # 下の隙間をなくす
  
  # X軸のラベル調整
  scale_x_discrete(labels = c("0" = "意欲なし(0)", "1" = "意欲あり(1)")) +
  
  # 文字サイズの調整（レポート用に見やすく）
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5)) # タイトル中央揃え

# プロットを表示
print(g_protest)

# 画像として保存
ggsave("./out/orfig1.png", plot = g_protest, width = 6, height = 4)


## 自分の民族が政府から不当な扱いを受けるか
table(d$Q88A, useNA="always")
dn$ethnic_unfair <- NA
dn$ethnic_unfair[which(d$Q88A == 0)] <- 0
dn$ethnic_unfair[which(d$Q88A %in% c(1, 2, 3))] <- 1
table(dn$ethnic_unfair, useNA = "always")

# 以下のプロット作成コードは、第二の独立変数である民族的不公平感の分布を表す図3に対応。
# 欠損値を除いてプロット
g_unfair <- ggplot(subset(dn, !is.na(ethnic_unfair)), aes(x = as.factor(ethnic_unfair))) +
  # 棒グラフ（y軸を割合に変換）
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count))), 
           fill = "grey40", width = 0.6) +
  
  # テーマ設定
  theme_bw() +
  
  # 軸ラベルとタイトル
  labs(x = "自民族への不当な扱いの認識 (0=なし, 1=あり)", 
       y = "回答割合") +
  
  # Y軸の設定（%表示）
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  
  # X軸ラベルの設定
  scale_x_discrete(labels = c("0" = "不当な扱いなし(0)", 
                              "1" = "不当な扱いあり(1)")) +
  
  # 文字サイズの調整
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5))

# プロットを表示
print(g_unfair)

# 画像として保存
ggsave("./out/orfig3.png", plot = g_unfair, width = 6, height = 4)



###独立変数


## 民族分け
table(d$Q87)
dn$group <- NA
dn$group[which(d$Q87==1420)] <- "Arab"
dn$group[which(d$Q87==1422)] <- "Berber (Kabyle)"
dn$group[which(d$Q87==1421)] <- "Berber (Non-Kabyle)"
dn$group[which(d$Q87==1423)] <- "Berber (Non-Kabyle)"
dn$group[which(d$Q87==1424)] <- "Berber (Non-Kabyle)"
dn$group[which(d$Q87==9990)] <- "Algerian"
dn$group <- factor(dn$group,
                   levels = c("Arab","Berber (Kabyle)",
                              "Berber (Non-Kabyle)",
                              "Algerian"))
table(dn$group)

# 以下のプロット作成コードは、第一の独立変数である民族的属性の分布を表す図2に対応。
# 欠損値（NA）を除いてプロット
g_group <- ggplot(subset(dn, !is.na(group)), aes(x = group)) +
  # 棒グラフ（y軸を割合に変換）
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count))), 
           fill = "grey40", width = 0.6) +
  
  # テーマ設定
  theme_bw() +
  
  # 軸ラベルとタイトル
  labs(x = "民族的属性（自己認識）", 
       y = "回答割合") +
  
  # Y軸の設定（%表示）
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  
  # X軸ラベルの日本語化（\n で改行して見やすく）
  scale_x_discrete(labels = c(
    "Arab" = "アラブ人",
    "Berber (Kabyle)" = "ベルベル人\n（カビル）",
    "Berber (Non-Kabyle)" = "ベルベル人\n（非カビル）",
    "Algerian" = "アルジェリア人\n（民族意識なし）"
  )) +
  
  # 文字サイズの調整
  theme(axis.text.x = element_text(size = 11), # X軸ラベルの文字サイズ
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5))

# プロットを表示
print(g_group)

# 画像として保存（ラベルが重ならないよう横幅を少し広めに設定）
ggsave("./out/orfig2.png", plot = g_group, width = 7, height = 5)


## 信仰心
dn$relig_practice <- NA
# それ以外 (0=全く, 1=年数回, 2=月1回, 3=週1回, 4=週数回, 5=日1回, 7=無宗教)
dn$relig_practice[which(d$Q98B %in% c(0, 1, 2, 3, 4, 5, 7))] <- 0
# 敬虔 (6=日複数回)
dn$relig_practice[which(d$Q98B == 6)] <- 1
table(dn$relig_practice, useNA = "always")

# 以下のプロット作成コードは、第三の独立変数である信仰心の分布を表す図4に対応。
# 欠損値を除いてプロット
g_relig <- ggplot(subset(dn, !is.na(relig_practice)), aes(x = as.factor(relig_practice))) +
  # 棒グラフ（y軸を割合に変換）
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count))), 
           fill = "grey40", width = 0.6) +
  
  # テーマ設定
  theme_bw() +
  
  # 軸ラベルとタイトル
  labs(x = "宗教的実践の頻度 (0=日1回以下, 1=日複数回)", 
       y = "回答割合") +
  
  # Y軸の設定（%表示）
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  
  # X軸ラベルの設定
  scale_x_discrete(labels = c("0" = "それ以外(0)", 
                              "1" = "敬虔(1)")) +
  
  # 文字サイズの調整
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5))

# プロットを表示
print(g_relig)

# 画像として保存
ggsave("./out/orfig4.png", plot = g_relig, width = 6, height = 4)



### 交絡変数


## 所得
dn$ses_group <- NA
#  "Worse" (1=はるかに悪い, 2=悪い)
dn$ses_group[which(d$Q5 %in% c(1, 2))] <- "Worse"
#  "Same" (3=同じ)
dn$ses_group[which(d$Q5 == 3)] <- "Same"
#  "Better" (4=良い, 5=はるかに良い)
dn$ses_group[which(d$Q5 %in% c(4, 5))] <- "Better"
dn$ses_group <- factor(dn$ses_group,
                       levels=c("Worse","Same","Better"))
table(dn$ses_group, useNA = "always")


## 年齢
dn$age_group <- NA
# 若年層 (Youth): 18歳 ～ 29歳
dn$age_group[which(d$Q1 >= 18 & d$Q1 <= 29)] <- "Youth"
# 中年層 (Middle): 30歳 ～ 59歳
dn$age_group[which(d$Q1 >= 30 & d$Q1 <= 59)] <- "Middle"
# 高齢層 (Elderly): 60歳以上
dn$age_group[which(d$Q1 >= 60 & d$Q1 < 998)] <- "Elderly" 
dn$age_group <- factor(dn$age_group,
                       levels = c("Youth", "Middle", "Elderly"))
# 確認
table(dn$age_group, useNA = "always")


## 法の下の不平等
dn$law_unfair <- NA
# 不平等は「ない・少ない」 (0=全くない, 1=ほとんどない) -> 0
dn$law_unfair[which(d$Q51B %in% c(0, 1))] <- 0
# 不平等は「ある・多い」 (2=よくある, 3=常に) -> 1
dn$law_unfair[which(d$Q51B %in% c(2, 3))] <- 1
# 確認
table(dn$law_unfair, useNA = "always")


# 以下の表作成コードは、交絡変数である所得、年齢、法の下での不平等感の分布を表す表1に対応。

table_data <- data.frame(
  "所得" = as.numeric(dn$ses_group),
  "年齢層" = as.numeric(dn$age_group),
  "法の不平等" = dn$law_unfair
)

calc_descriptive <- function(x) {
  c(
    平均     = mean(x, na.rm = TRUE),
    標準偏差 = sd(x, na.rm = TRUE),
    最小値   = min(x, na.rm = TRUE),
    最大値   = max(x, na.rm = TRUE),
    欠損数   = sum(is.na(x))
  )
}

table1_mat <- t(sapply(table_data, calc_descriptive))
table1_df <- as.data.frame(table1_mat)
table1_df$平均     <- round(table1_df$平均, 2)
table1_df$標準偏差 <- round(table1_df$標準偏差, 2)
table1_df$最小値   <- round(table1_df$最小値, 0)
table1_df$最大値   <- round(table1_df$最大値, 0)
table1_df$欠損数   <- round(table1_df$欠損数, 0)

table1_df <- cbind(変数名 = rownames(table1_df), table1_df)
rownames(table1_df) <- NULL

# 表のデザイン
my_theme <- ttheme_default(
  core = list(fg_params = list(fontsize = 12)),
  colhead = list(fg_params = list(fontsize = 12, fontface = "bold"))
)

# 表作成
p_table <- tableGrob(table1_df, rows = NULL, theme = my_theme)

# タイトル作成
title_text <- textGrob("表1：交絡変数の分布表", gp = gpar(fontsize = 16, fontface = "bold"))

p_combined <- arrangeGrob(p_table, top = title_text)

grid.newpage()
grid.draw(p_combined)

### 保存
ggsave("./out/ortab1.png", plot = p_combined, width = 10, height = 4, bg = "white")



##########
## 分析 ##
##########


### 最終仮説

## 仮説1：カビル人はアラブ人と比べて、デモへの参加意欲が高い
m1 <- lm(protest ~ group, data = dn)
# 交絡変数（信仰心・年齢・法の下での不平等・所得）
m1_c <- lm(protest ~ group + relig_practice + age_group + ses_group + law_unfair, data = dn)

## 仮説2：カビル人はアラブ人と比べて、不当な扱いを受けると感じている
m2 <- lm(ethnic_unfair ~ group, data = dn)
# 交絡変数（信仰心・年齢・法の下での不平等・所得）
m2_c <- lm(ethnic_unfair ~ group + relig_practice + age_group + ses_group + law_unfair, data = dn)

## 仮説3：不当な扱いを受けると感じている人は、デモへの参加意欲が高い
m3 <- lm(protest ~ ethnic_unfair , data = dn)
# 交絡変数（信仰心・年齢・民族・法の下での不平等・所得）
m3_c <- lm(protest ~ ethnic_unfair + relig_practice + age_group + group + ses_group + law_unfair, data = dn)

## 仮説4：信仰心が低い人は、デモへの参加意欲が高い
m4 <- lm(protest ~ relig_practice, data = dn)
# 交絡変数（年齢・民族・法の下での不平等・所得）
m4_c <- lm(protest ~ relig_practice + age_group + group + ses_group + law_unfair, data = dn)

## 結果 (単純モデルと統制モデル)
screenreg(list(m1, m1_c, m2, m2_c, m3, m3_c, m4, m4_c), 
          custom.model.names = c("M1:P ~ G(基)", "M1:P ~ G(統)", 
                                 "M2:Uf ~ G(基)", "M2:Uf ~ G(統)",
                                 "M3:P ~ Uf(基)", "M3:P ~ Uf(統)",
                                 "M4:P ~ R(基)", "M4:P ~ R(統)"),
          include.ci = FALSE, 
          digits = 3, 
          single.row = FALSE, 
          stars = c(0.001, 0.01, 0.05, 0.1), symbol = "+")

## もっときれいな結果
### 変数ラベル
vnmap <- list("(Intercept)" = "（定数項）",
  "groupBerber (Kabyle)" = "ベルベル人（カビル）",
  "groupBerber (Non-Kabyle)" = "ベルベル人（非カビル）",
  "groupAlgerian" = "汎アルジェリア人",
  "ethnic_unfair" = "自民族の不当な扱い",
  "relig_practice" = "信仰心の強さ",
  "age_groupMiddle" = "30-50代",
  "age_groupElderly" = "60代以上",
  "ses_groupSame" = "所得の変化（同じ）",
  "ses_groupBetter" = "所得の変化（良くなった）",
  "law_unfair" = "法の下の不平等（あり）")

### すべての仮説（m1～m4_c）を1つの表に出力するコード. 表2に対応.
tab_html <- HTML(htmlreg(
  list(m1, m1_c, m2, m2_c, m3, m3_c, m4, m4_c), 
  include.ci = FALSE,
  digits = 3, 
  stars = c(0.001, 0.01, 0.05, 0.1), 
  symbol = "+",  
  star.symbol = "*", 
  inline.css = FALSE,
  html.tag = TRUE, 
  head.tag = TRUE, 
  body.tag = TRUE,
  single.row = FALSE, 
  caption = "",             
  custom.coef.map = vnmap, 
  custom.header = list("仮説1" = 1:2, "仮説2" = 3:4, "仮説3" = 5:6, "仮説4" = 7:8), 
  custom.model.names = c("基本","拡張","基本","拡張","基本","拡張","基本","拡張"),
  custom.note = "%stars。OLS回帰分析結果。括弧内は標準誤差。"
))
## 表を仮表示
browsable(tab_html)
## 表をoutに保存する
save_html(tab_html, "./out/ortab2.html")
webshot(url="./out/ortab2.html", file="./out/ortab2.png", zoom=4, selector="table")



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
  ### 予測値を数値に変換
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

#### 統制モデルの予測値の算出

# 以下のプロットは、仮説1の民族とデモ参加意欲のプロット（図５）に対応。
## 予測値の算出（仮説１の基本モデルと拡張モデルを比べる）
### 基本モデル
yosokuout1a <- genpr(dpr = dn,
                     mpr = m1,
                     setx = "group", 
                     setxvals = c("Arab",
                                  "Berber (Kabyle)",
                                  "Berber (Non-Kabyle)",
                                  "Algerian"), 
                     datalab = "基本モデル（統制前）")
yosokuout1a$x <- factor(yosokuout1a$x,
                        levels = unique(yosokuout1a$x),
                        labels = c("Arab",
                                   "Berber\n (Kabyle)",
                                   "Berber\n (Non-Kabyle)",
                                   "Algerian"))
### 拡張モデル
yosokuout1b <- genpr(dpr = dn,
                     mpr = m1_c,
                     setx = "group", 
                     setxvals = c("Arab",
                                  "Berber (Kabyle)",
                                  "Berber (Non-Kabyle)",
                                  "Algerian"), 
                     datalab = "拡張モデル（統制後）")
yosokuout1b$x <- factor(yosokuout1b$x,
                        levels = unique(yosokuout1b$x),
                        labels = c("Arab",
                                   "Berber\n (Kabyle)",
                                   "Berber\n (Non-Kabyle)",
                                   "Algerian"))
### ２つの予測を結合
yosokuout1 <- rbind(yosokuout1a,yosokuout1b)
yosokuout1$datalab <- factor(yosokuout1$datalab,
                             levels = c("基本モデル（統制前）",
                                        "拡張モデル（統制後）"))

## プロット
ggplot(yosokuout1) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white", size=2) + # 点を見やすく調整
  geom_point(aes(x=as.factor(x), y=pr), shape=1, size=2) +       # 白抜きの枠線を追加
  facet_grid(.~datalab) + 
  theme_bw() + 
  labs(title="仮説1：民族とデモ参加意欲（統制前vs統制後）",
       x="民族（自己認識）",
       y="デモへの参加意欲（平均予測確率）", 
       caption = "注：エラーバーは90％および95％信頼区間。\n他の変数（信仰心・年齢・所得）は観察値に固定。") + 
  theme(plot.title = element_text(hjust = 0.5))

## 保存
ggsave("./out/orfig5.png", width = 6, height = 4)


# 以下のプロットは、仮説2の民族と不当な扱いのプロット（表6）に対応。
## 予測値の算出（仮説２の基本モデルと拡張モデルを比べる）
### 基本モデル
yosokuout2a <- genpr(dpr = dn,
                     mpr = m2,
                     setx = "group", 
                     setxvals = c("Arab",
                                  "Berber (Kabyle)",
                                  "Berber (Non-Kabyle)"),
                     datalab = "基本モデル（統制前）")
yosokuout2a$x <- factor(yosokuout2a$x,
                        levels = unique(yosokuout2a$x),
                        labels = c("Arab",
                                   "Berber\n (Kabyle)",
                                   "Berber\n (Non-Kabyle)"))
### 拡張モデル
yosokuout2b <- genpr(dpr = dn,
                     mpr = m2_c,
                     setx = "group", 
                     setxvals = c("Arab",
                                  "Berber (Kabyle)",
                                  "Berber (Non-Kabyle)"),
                     datalab = "拡張モデル（統制後）")
yosokuout2b$x <- factor(yosokuout2b$x,
                        levels = unique(yosokuout2b$x),
                        labels = c("Arab",
                                   "Berber\n (Kabyle)",
                                   "Berber\n (Non-Kabyle)"))
### ２つの予測を結合
yosokuout2 <- rbind(yosokuout2a,yosokuout2b)
yosokuout2$datalab <- factor(yosokuout2$datalab,
                             levels = c("基本モデル（統制前）",
                                        "拡張モデル（統制後）"))

## プロット
ggplot(yosokuout2) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white", size=2) +
  geom_point(aes(x=as.factor(x), y=pr), shape=1, size=2) +
  theme_bw() +
  facet_grid(.~datalab) +
  labs(title="仮説2：民族と不当な扱い（統制前vs統制後）",
       x="民族（自己認識）",
       y="自民族が不当な扱いを受けている確率（予測平均）", 
       caption = "注：エラーバーは90％および95％信頼区間。\n他の変数は観察値に固定。") + 
  theme(plot.title = element_text(hjust = 0.5))

## 保存
ggsave("./out/orfig6.png", width = 6, height = 4)


# 以下のプロットは、仮説3の不当な扱いとデモ参加意欲のプロット（図7）に対応。
## 予測値の算出（仮説３の基本モデルと拡張モデルを比べる）
### 基本モデル
yosokuout3a <- genpr(dpr = dn,
                     mpr = m3,
                     setx = "ethnic_unfair", 
                     setxvals = c(0,1), # 0=Fair, 1=Unfair
                     datalab = "基本モデル（統制前）")
yosokuout3a$x <- factor(yosokuout3a$x,
                        levels = unique(yosokuout3a$x))
### 拡張モデル
yosokuout3b <- genpr(dpr = dn,
                     mpr = m3_c,
                     setx = "ethnic_unfair", 
                     setxvals = c(0,1), # 0=Fair, 1=Unfair 
                     datalab = "拡張モデル（統制後）")
yosokuout3b$x <- factor(yosokuout3b$x,
                        levels = unique(yosokuout3b$x))
### ２つの予測を結合
yosokuout3 <- rbind(yosokuout3a,yosokuout3b)
yosokuout3$datalab <- factor(yosokuout3$datalab,
                             levels = c("基本モデル（統制前）",
                                        "拡張モデル（統制後）"))

## プロット
ggplot(yosokuout3) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white", size=2) +
  geom_point(aes(x=as.factor(x), y=pr), shape=1, size=2) +
  scale_x_discrete(labels=c("不当な扱いは\n受けていない(0)", 
                            "不当な扱いを\n受けている(1)")) + 
  facet_grid(.~datalab) +
  theme_bw() +
  labs(title="仮説3：不当な扱いとデモ参加意欲（統制前vs統制後）",
       x="自民族への扱いの認識",
       y="デモへの参加意欲（平均予測確率）",
       caption = "注：エラーバーは90％および95％信頼区間。\n他の変数（信仰心・年齢・民族）は観察値に固定した。") + 
  theme(plot.title = element_text(hjust = 0.5))

## 保存
ggsave("./out/orfig7.png", width = 6, height = 4)


# 以下のプロットは、仮説4の信仰心とデモ参加意欲のプロット（図8）に対応。
## 予測値の算出（仮説４の基本モデルと拡張モデルを比べる）
### 基本モデル
yosokuout4a <- genpr(dpr = dn,
                     mpr = m4,
                     setx = "relig_practice", 
                     setxvals = c(0,1), # 0=Fair, 1=Unfair
                     datalab = "基本モデル（統制前）")
yosokuout4a$x <- factor(yosokuout4a$x,
                        levels = unique(yosokuout4a$x))
### 拡張モデル
yosokuout4b <- genpr(dpr = dn,
                     mpr = m4_c,
                     setx = "relig_practice", 
                     setxvals = c(0,1), # 0=Fair, 1=Unfair 
                     datalab = "拡張モデル（統制後）")
yosokuout4b$x <- factor(yosokuout4b$x,
                        levels = unique(yosokuout4b$x))
### ２つの予測を結合
yosokuout4 <- rbind(yosokuout4a,yosokuout4b)
yosokuout4$datalab <- factor(yosokuout4$datalab,
                             levels = c("基本モデル（統制前）",
                                        "拡張モデル（統制後）"))

## プロット
ggplot(yosokuout4) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white", size=2) +
  geom_point(aes(x=as.factor(x), y=pr), shape=1, size=2) +
  scale_x_discrete(labels=c("信仰心：低～中(0)", "信仰心：高い(1)")) + 
  facet_grid(.~datalab) +
  theme_bw() +
  labs(title="仮説4：信仰心とデモ参加意欲（統制前vs統制後）",
       x="宗教的実践（信仰心）",
       y="デモへの参加意欲（平均予測確率）", 
       caption = "注：エラーバーは90％および95％信頼区間。\n他の変数（年齢・民族）は観察値に固定。") + 
  theme(plot.title = element_text(hjust = 0.5))

## 保存
ggsave("./out/orfig8.png", width = 6, height = 4)
