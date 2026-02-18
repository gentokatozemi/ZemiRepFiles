# オンライン投票の投票参加促進効果と政府・技術への信頼

## 更新履歴：
## 2026/01/31 軽部将伍（提出版）
## 2026/02/19 加藤言人（最終調整）

# ワークスペースを掃除する
rm(list=ls())

## このコードファイルがあるディレクトリを
## ワーキングディレクトリにする
dirhere <- dirname(rstudioapi::getActiveDocumentContext()$path) 
## ワーキングディレクトリをセット
setwd(dirhere)
getwd() # 

## 図表出力用のフォルダ（out）を作成
if (!"./out" %in% list.dirs()) dir.create("./out")

# パッケージ（追加機能）の追加
library(ggplot2) # グラフ出力に使う。
theme_set(theme_bw())
library(texreg) ## 回帰表の出力
library(htmltools) ## HTMLで表をプレビュー
library(estimatr) ## ロバスト標準誤差を使った回帰分析（lm_robust）の実行
## データ読み込み用のライブラリ
library(haven)
library(labelled)

## データセットのインポート ##
### ※レプリケーション用に実験に関連する変数だけのデータを作成
# d <- read_sav("experiment_origin_data.sav")
# d <- d[,c("ResponseId","Progress",
#           "StartDate","EndDate",
#           "exp2_govt","exp2_tech",
#           "exp2_govtH","exp2_techH",
#           "exp2_onlinevote","exp2_q1a_txt",
#           "exp2_q1a_1","exp2_q1b_1","exp2_q1b_2",
#           "exp2_q1b_3","exp2_q4","exp2_q5",
#           "exp2_mcheck1","exp2_mcheck2",
#           "polint","useinternet",
#           "gvtresp_1","gvtresp_2",
#           "gender","age","edu","income",
#           "employment","marrykids","pref",
#           "urban","tohyojo_1","tohyojo_2")]
# write_sav(d, "experiment_origin_subset.sav")
### データを読み込み
d <- read_sav("experiment_origin_subset.sav") 

## 実験実施日
range(as.Date(d$EndDate, tz="Japan"))

## 最後まで回答完了している人だけをデータに残す
d <- subset(d, Progress == 100)
nrow(d) ## 1191人の有効回答あり

##########
## 仮説 ##
##########

# H1. （利便性向上仮説）オンライン投票の導⼊は、対面のみの投票と⽐較して、投票参加意向を⾼める。
# H2A. （政府信頼度仮説）政府への信頼度が客観的に高い場合、高くない場合と⽐較して、投票参加意向が高まる。
# H2B. （政府信頼度仮説）政府への信頼度が客観的に高い場合、高くない場合と⽐較して、オンライン投票の利⽤意向が高まる。
# H3A. （技術信頼度仮説）オンライン投票システムに対する技術的信頼度が客観的に高い場合、高くない場合と⽐較して、投票参加意向が高まる。
# H3B. （技術信頼度仮説）オンライン投票システムに対する技術的信頼度が客観的に高い場合、高くない場合と⽐較して、オンライン投票の利⽤意向が高まる。
# H4. 政府の信頼度が客観的に低い場合より高い場合に、H3の効果量がより大きくなる。
# H5A. 政治関心が低いほど、H1の効果量が大きくなる。
# H5B. SNSの使用頻度が多いほど、H1の効果量が大きくなる。
# H5C. 景気やくらし向きに対する政府の責任認識が強いほど、H2の効果量が大きくなる。


#変数###########################################################################

## 新しいデータセットを作成
## idで個人を特定
dn <- data.frame(id = d$ResponseId) #回答者ID

## 従属変数 ##
# 投票参加意向
dn$sankaiko <- d$exp2_q1a_1

# オンライン投票利用意向
dn$online_sankaiko <- d$exp2_q1b_3

# 独立変数
# オンライン投票導入ダミー(1=導入, 0=非導入)
dn$onlinevote <- as.numeric(d$exp2_onlinevote)

# 政府信頼高ダミー（1=信頼高、0=信頼低）
dn$govtH <- as.numeric(d$exp2_govtH)

# 技術信頼高ダミー（1=信頼高、0=信頼低/非導入）
dn$techH <- as.numeric(d$exp2_techH)


## 条件付け変数 ##

# 政治関心
dn$kanshin <- NA
dn$kanshin[which(d$polint%in%c(3,4))] <- 0
dn$kanshin[which(d$polint%in%c(2))] <- 0.5
dn$kanshin[which(d$polint%in%c(1))] <- 1
table(dn$kanshin, exclude=F)

# インターネット使用量
dn$useinternet <- d$useinternet

# 社会への暮らし向きに対する政府の責任
dn$gvtresp_1 <- 5 - d$gvtresp_1 #GK 1が責任があるなので、逆転させる

# 私生活への暮らし向きに対する政府の責任
dn$gvtresp_2 <- 5 - d$gvtresp_2 #GK 1が責任があるなので、逆転させる

# （表１上）記述統計: 投票参加意向・オンライン投票利用意向・政治関心・SNS利用頻度の記述統計（平均・標準偏差・最小値・最大値・有効ケース数）
desc_stats <- function(x) {
  v <- x[!is.na(x)]
  c(平均 = round(mean(v), 2), 標準偏差 = round(sd(v), 2), 最小値 = min(v), 最大値 = max(v), 有効ケース数 = length(v))
}
rbind(
  "投票参加意向" = desc_stats(dn$sankaiko),
  "オンライン投票利用意向" = desc_stats(dn$online_sankaiko),
  "政治関心" = desc_stats(dn$kanshin),
  "SNS利用頻度" = desc_stats(dn$useinternet),
  "社会への政府の責任認識" = desc_stats(dn$gvtresp_1),
  "自身の暮らし向きへの政府の責任認識" = desc_stats(dn$gvtresp_2)
)

## 実験条件（X1, X2, X3）の度数・割合表（LaTeX用） ####
vars_dummy <- list(
  "X1: オンライン投票導入" = dn$onlinevote,
  "X2: 政府への信頼"       = dn$govtH,
  "X3: 技術への信頼"       = dn$techH
)
# 0, 1 の順で集計（念のため factor で水準を固定）
fmt_cell <- function(x) {
  t <- table(factor(x, levels = 0:1))
  n <- as.vector(t)
  pct <- round(100 * prop.table(t), 1)
  sprintf("%d (%.1f\\%%)", n, pct)
}
tab_rows <- lapply(vars_dummy, fmt_cell)

#（表１下）記述統計
header <- "変数 & 0（低/非導入） & 1（高/導入） \\\\ \\hline"
body   <- sapply(seq_along(tab_rows), function(i) {
  paste(names(tab_rows)[i], " & ", tab_rows[[i]][1], " & ", tab_rows[[i]][2], " \\\\")
})
footer <- "\\hline"
cat(c(header, body, footer), sep = "\n")

##オンライン投票群のみの分析####################################################

# オンライン投票=1の群に限定
dn_online <- subset(dn, onlinevote == 1)

# 分析（仮説１）
print("\n=====仮説1====\n")
# H1. Y=投票参加意向、X=オンライン投票導入、Z=政府信頼高、技術信頼高
mh1 <- lm_robust(sankaiko ~ onlinevote + govtH + techH, data = dn)
## 回帰表（表２）
screenreg(list(mh1), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

# 分析（仮説２）
print("\n=====仮説2====\n")
# H2A: Y=投票参加意向、X=政府信頼高、Z=オンライン投票導入、技術信頼高
mh2a <- lm_robust(sankaiko ~ govtH + onlinevote + techH, data = dn)
# H2b: （オンライン投票導入=1にサンプルを限定）Y=オンライン投票利用意向、X=政府信頼高、Z=技術信頼高
mh2b <- lm_robust(online_sankaiko ~ govtH + techH, data = dn_online)
## 回帰表（表３）
screenreg(list(mh2a, mh2b), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

# 分析（仮説３）
print("\n=====仮説3====\n")
# H3a. Y=投票参加意向、X=技術信頼高、Z=オンライン投票導入、政府信頼高
mh3a <- lm_robust(sankaiko ~ techH + onlinevote + govtH, data = dn)
# H3b. （オンライン投票導入=1にサンプルを限定）Y=オンライン投票利用意向、X=技術信頼高、Z=政府信頼高
mh3b <- lm_robust(online_sankaiko ~ techH + govtH, data = dn_online)
## 回帰表（表４）
screenreg(list(mh3a, mh3b), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

# 分析（仮説４）
print("\n=====仮説4====\n")
# H4a. Y=投票参加意向、X=技術信頼高、M=政府信頼高、Z=オンライン投票導入
mh4a <- lm_robust(sankaiko ~ techH*govtH + onlinevote, data = dn)
# H4b. （オンライン投票導入=1にサンプルを限定）Y=オンライン投票利用意向、X=技術信頼高, M=政府信頼高
mh4b <- lm_robust(online_sankaiko ~ techH*govtH, data = dn_online)
## 回帰表（表５）
screenreg(list(mh4a, mh4b), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

# 分析（仮説５）
print("\n=====仮説5====\n")
# H5a. Y=投票参加意向、X=オンライン投票導入、M=政治関心高、Z=政府信頼高、技術信頼高
mh5a <- lm_robust(sankaiko ~ onlinevote*kanshin + govtH + techH, 
                  data = dn)
# H5b. Y=投票参加意向、X=オンライン投票導入、M=SNSの使用頻度、Z=政府信頼高、技術信頼高
mh5b <- lm_robust(sankaiko ~ onlinevote*useinternet + govtH + techH, 
                  data = dn)
# H5c-a. Y=投票参加意向、X=政府信頼高、M=景気や暮らし向きに対する政府の責任認識、Z=オンライン投票導入、技術信頼高
mh5c_a_1 <- lm_robust(sankaiko ~ govtH*gvtresp_1 + onlinevote + techH, 
                      data = dn)
mh5c_a_2 <- lm_robust(sankaiko ~ govtH*gvtresp_2 + onlinevote + techH,
                      data = dn)
# H5c-b. （オンライン投票導入=1にサンプルを限定）Y=オンライン投票利用意向、X=政府信頼高、M=景気や暮らし向きに対する政府の責任認識、Z=技術信頼高
mh5c_b_1 <- lm_robust(online_sankaiko ~ govtH * gvtresp_1 + 
                      techH, data = dn_online)
mh5c_b_2 <- lm_robust(online_sankaiko ~ govtH * gvtresp_2 +
                      techH, data = dn_online)
# H5A回帰表（表６）
print("\n=====仮説5A====\n")
screenreg(list(mh5a), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")
# H5B回帰表（表７）
print("\n=====仮説5B====\n")
screenreg(list(mh5b), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")
# H5C_A、H5C_B回帰表（表８）
print("\n=====仮説5C_A,仮説5C_B====\n")
screenreg(list(mh5c_a_1, mh5c_b_1), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")
# H5C_A、H5C_B回帰表（表９、私生活への暮らし向きに対する政府の責任）
print("\n=====仮説5C_A,仮説5C_B====\n")
screenreg(list(mh5c_a_2, mh5c_b_2), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")


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


## H1 ##

## 予測値の算出
yosokuout <- genpr(dpr = dn, ## 実験データ
                   mpr = mh1, ## 回帰分析結果
                   setx = "onlinevote",  ## 独立変数名
                   setxvals = c(0,1)) ## 独立変数の任意の値
yosokuout　## pr列がxの値に対応するyの予測値平均

## プロットのモード設定
# "discrete"はXの値が少ないもしくはダミーの場合
# "continuous"はXの値が多くて連続している場合
setmode <- "discrete" 

## ラベル設定（共通）
setxlab <- "オンライン投票導入（実験条件）" ## Xの変数ラベル
setylab <- "投票参加意向" ## Yの変数ラベル

## ラベル設定（setmode=="discrete"のときのみ適用）
setlabels <- c("導入なし(0)","導入あり(1)") ## setxvalsと対応させる
# setlabels <- waiver() ## わからないときはこれでsetxvalsをそのまま表示

## 予測値平均をプロットする（半自動）####################
ggplot(yosokuout) + 
  {if (setmode=="discrete") { list(
    geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                  width=0.1, linewidth=0.5),
    geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                  width=0, linewidth=2),
    geom_point(aes(x=as.factor(x), y=pr),color="white"),
    scale_x_discrete(labels=setlabels)) } } + 
  {if (setmode=="continuous") { list(
    geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3),
    geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5),
    geom_line(aes(x=x, y=pr))) } } +
  labs(x=setxlab, y=paste0(setylab, "（予測値平均）"), 
       caption = paste0("注：",ifelse(setmode=="discrete","エラーバー","塗りつぶし"),
                        "は、90％および95％信頼区間を示している。")) + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## (ここまで) ############################################

## h1_yosokuchi_plot（図１）
ggsave("./out/h1_yosokuchi_plot.png", width = 6, height = 4)

## H2-H5のグラフ作成と保存 ##############################

# プロット用設定（共通）
setmode <- "discrete"

# --- H2A: 政府信頼 -> 投票参加意向 ---
yosoku_h2a <- genpr(dpr = dn, mpr = mh2a, setx = "govtH", setxvals = c(0, 1))

ggplot(yosoku_h2a) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white") +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  labs(x="政府への信頼度", y="投票参加意向（予測値）", subtitle="H2Aの検証")
# h2a_plot（図２a）
ggsave("./out/h2a_plot.png", width = 4, height = 4)

# --- H2B: 政府信頼 -> オンライン利用意向 ---
yosoku_h2b <- genpr(dpr = dn_online, mpr = mh2b, setx = "govtH", setxvals = c(0, 1))

ggplot(yosoku_h2b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white") +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  labs(x="政府への信頼度", y="オンライン投票利用意向（予測値）", subtitle="H2Bの検証")
# h2b_plot（図２b）
ggsave("./out/h2b_plot.png", width = 4, height = 4)

# --- H3A: 技術信頼 -> 投票参加意向 ---
yosoku_h3a <- genpr(dpr = dn, mpr = mh3a, setx = "techH", setxvals = c(0, 1))

ggplot(yosoku_h3a) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white") +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  labs(x="システムへの技術的信頼", y="投票参加意向（予測値）", subtitle="H3Aの検証")
# h3a_plot（図３a）
ggsave("./out/h3a_plot.png", width = 4, height = 4)

# --- H3B: 技術信頼 -> オンライン利用意向 ---
yosoku_h3b <- genpr(dpr = dn_online, mpr = mh3b, setx = "techH", setxvals = c(0, 1))

ggplot(yosoku_h3b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white") +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  labs(x="システムへの技術的信頼", y="オンライン投票利用意向（予測値）", subtitle="H3Bの検証")
# h3b_plot（図３b）
ggsave("./out/h3b_plot.png", width = 4, height = 4)

# --- H4A: 技術信頼*政府信頼 -> 投票参加意向 ---
yosoku_h4a <- genpr(dpr = dn, mpr = mh4a, setx = "techH", setxvals = c(0, 1),
                    setm = "govtH", setmvals = list("低（0）" = 0, "高（1）" = 1))

ggplot(yosoku_h4a) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府信頼", type="qual", palette=2) + 
  scale_shape_discrete(name="政府信頼") +
  labs(x="システムへの技術的信頼", y="投票参加意向（予測値）", subtitle="H4Aの検証")
# h4a_plot（図4a）
ggsave("./out/h4a_plot.png", width = 4, height = 4)

# --- H4B: 技術信頼*政府信頼 -> オンライン利用意向 ---
yosoku_h4b <- genpr(dpr = dn_online, mpr = mh4b, setx = "techH", setxvals = c(0, 1),
                    setm = "govtH", setmvals = list("低（0）" = 0, "高（1）" = 1))

ggplot(yosoku_h4b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府信頼", type="qual", palette=2) + 
  scale_shape_discrete(name="政府信頼") +
  labs(x="システムへの技術的信頼", y="オンライン投票利用意向（予測値）", subtitle="H4Bの検証")
# h4b_plot（図4b）
ggsave("./out/h4b_plot.png", width = 4, height = 4)

## H5A ##

## 予測値の算出
yosokuout <- genpr(dpr = dn, ## 実験データ
                   mpr = mh5a, ## 回帰分析結果
                   setx = "onlinevote", ## 独立変数名
                   setxvals = c(0,1), ## 独立変数の任意の値
                   setm = "kanshin", ## 条件付け変数名
                   setmvals = list("関心低（0）" = 0, # 条件付けのラベルと値
                                   "関心中（0.5）" = 0.5, 
                                   "関心高（1）" = 1))
yosokuout ## pr列がxとmの値に対応するyの予測値平均

## プロットのモード設定
# "discrete"はXの値が少ないもしくはダミーの場合
# "continuous"はXの値が多くて連続している場合
setmode <- "discrete" 

## ラベル設定（共通）
setxlab <- "オンライン投票導入（実験条件）" ## Xの変数ラベル
setylab <- "投票参加意向" ## Yの変数ラベル
setmlab <- "政治関心" ## M（条件付け変数）の変数ラベル

## ラベル設定（setmode=="discrete"のときのみ適用）
setlabels <- c("導入なし(0)","導入あり(1)") ## setxvalsと対応させる
# setlabels <- waiver() ## わからないときはこれでsetxvalsをそのまま表示
setdodgewidth <- 0.3 ## 同じXの値を取るエラーバー同士の隙間の大きさ

## 予測値平均をプロットする（半自動）####################
ggplot(yosokuout) +
  {if (setmode=="discrete") { list(
    geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95,
                      color = labelledm), width=0.1, linewidth=0.5,
                  position = position_dodge(width=setdodgewidth)),
    geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90,
                      color = labelledm),
                  width=0, linewidth=2,
                  position = position_dodge(width=setdodgewidth)),
    geom_point(aes(x=as.factor(x), y=pr,
                   shape = labelledm),color="white",
               position = position_dodge(width=setdodgewidth)),
    scale_x_discrete(labels = setlabels) ) } } +   
  {if (setmode=="continuous") { list(
    geom_ribbon(aes(x=x, ymin=lo95, ymax=up95,
                    fill = labelledm), alpha=0.3),
    geom_ribbon(aes(x=x, ymin=lo90, ymax=up90,
                    fill = labelledm), alpha=0.5),
    geom_line(aes(x=x, y=pr, linetype=labelledm))) } } +
  scale_shape_discrete(name = setmlab) + 
  scale_linetype_discrete(name = setmlab) + 
  scale_color_brewer(name = setmlab, type = "qual", palette = 2) + 
  scale_fill_brewer(name = setmlab, type = "qual", palette = 2) + 
  labs(x=setxlab, y=paste0(setylab, "（予測値平均）"),
       caption = paste0("注：",ifelse(setmode=="discrete","エラーバー","塗りつぶし"),
                        "は、90％および95％信頼区間を示している。")) +
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## (ここまで) ############################################

## h5a_yosokuchi_plot（図６）
ggsave("./out/h5a_yosokuchi_plot.png", width = 6, height = 4)

# --- H5B: OL投票導入*ネット利用度 -> 投票参加意向 ---
quantile(dn$useinternet, probs=c(0.1,0.9)) #10%点と、90%点をとる
yosoku_h5b <- genpr(dpr = dn, mpr = mh5b, setx = "onlinevote", setxvals = c(0, 1),
                    setm = "useinternet", setmvals = list("２時間\n（4; 10%点）" = 4, "５時間以上\n（7; 90%点）" = 7))

ggplot(yosoku_h5b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("非導入(0)", "導入あり(1)")) +
  scale_color_brewer(name="ネット利用時間/日", type="qual", palette=2) + 
  scale_shape_discrete(name="ネット利用時間/日") +
  labs(x="オンライン投票導入の有無", y="投票参加意向（予測値）", subtitle="H5Bの検証")
# h5b_plot（図８）
ggsave("./out/h5b_plot.png", width = 6, height = 4)

# --- H5C_A: 政府信頼*政府責任 -> 投票参加意向 ---
quantile(dn$gvtresp_1, probs=c(0.1,0.9)) #10%点と、90%点をとる
yosoku_h5c_a <- genpr(dpr = dn, mpr = mh5c_a_1, setx = "govtH", setxvals = c(0, 1),
                      setm = "gvtresp_1", setmvals = list("ある程度の責任\n（3; 10%点）" = 3, "大きな責任\n（4; 90%点）" = 4))

ggplot(yosoku_h5c_a) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府責任認識(社会)", type="qual", palette=2) + 
  scale_shape_discrete(name="政府責任認識(社会)") +
  labs(x="政府信頼", y="投票参加意向（予測値）", subtitle="H5C_Aの検証")
# h5c_a_plot（図１０a）
ggsave("./out/h5c_a_plot.png", width = 4, height = 4)

# --- H5C_A_2: 政府信頼*政府責任(自身の暮らし) -> 投票参加意向 ---
quantile(dn$gvtresp_2, probs=c(0.1,0.9)) #10%点と、90%点をとる
yosoku_h5c_a_2 <- genpr(dpr = dn, mpr = mh5c_a_2, setx = "govtH", setxvals = c(0, 1),
                        setm = "gvtresp_2", setmvals = list("ある程度の責任\n（3; 10%点）" = 3, "大きな責任\n（4; 90%点）" = 4))

ggplot(yosoku_h5c_a_2) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府責任認識(自身)", type="qual", palette=2) + 
  scale_shape_discrete(name="政府責任認識(自身)") +
  labs(x="政府信頼", y="投票参加意向（予測値）", subtitle="H5C_A_2の検証")
# h5c_a_2_plot（図１１a）
ggsave("./out/h5c_a_2_plot.png", width = 4, height = 4)

# --- H5C_B: 政府信頼*政府責任 -> オンライン利用意向 ---
yosoku_h5c_b <- genpr(dpr = dn_online, mpr = mh5c_b_1, setx = "govtH", setxvals = c(0, 1),
                      setm = "gvtresp_2", setmvals = list("ある程度の責任\n（3; 10%点）" = 3, "大きな責任\n（4; 90%点）" = 4))

ggplot(yosoku_h5c_b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府責任認識(社会)", type="qual", palette=2) + 
  scale_shape_discrete(name="政府責任認識(社会)") +
  labs(x="政府信頼", y="オンライン投票利用意向（予測値）", subtitle="H5C_Bの検証")
# h5c_b_plot（図１０b）
ggsave("./out/h5c_b_plot.png", width = 4, height = 4)

# --- H5C_B_2: 政府信頼*政府責任 -> オンライン利用意向 ---
yosoku_h5c_b <- genpr(dpr = dn_online, mpr = mh5c_b_2, setx = "govtH", setxvals = c(0, 1),
                      setm = "gvtresp_2", setmvals = list("ある程度の責任\n（3; 10%点）" = 3, "大きな責任\n（4; 90%点）" = 4))

ggplot(yosoku_h5c_b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府責任認識(自身)", type="qual", palette=2) + 
  scale_shape_discrete(name="政府責任認識(自身)") +
  labs(x="政府信頼", y="オンライン投票利用意向（予測値）", subtitle="H5C_B_2の検証")
# h5c_b_2_plot（図１１b）
ggsave("./out/h5c_b_2_plot.png", width = 4, height = 4)


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


###########################################################
## H4: 限界効果のプロット（技術信頼 × 政府信頼） #########
###########################################################

## H4a: 政府信頼の高さによって、技術信頼の効果がどう変わるか
genkai_h4a <- intereff(m = mh4a, 
                       main = "techH",   # 興味のある主効果
                       mod = "govtH",    # 条件付け変数
                       modrange = c(0, 1), 
                       nsim = 2)         # ダミー変数なので2つでOK

# ラベル設定
setxlab <- "技術信頼の効果量"
setmlab <- "政府信頼度"
setmlabels <- c("低信頼（0）", "高信頼（1）")

## 限界効果プロット（H4a）
ggplot(genkai_h4a, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", y="技術信頼の限界効果", x=setmlab)
# h4a_genkaikoka_plot（図５a）
ggsave("./out/h4a_genkaikoka_plot.png", width = 4, height = 4)

## H4b: 政府信頼の高さによって、技術信頼の効果がどう変わるか
genkai_h4b <- intereff(m = mh4b, 
                       main = "techH",   # 興味のある主効果
                       mod = "govtH",    # 条件付け変数
                       modrange = c(0, 1), 
                       nsim = 2)         # ダミー変数なので2つでOK

# ラベル設定
setxlab <- "技術信頼の効果量"
setmlab <- "政府信頼度"
setmlabels <- c("低信頼（0）", "高信頼（1）")

## 限界効果プロット（H4b）
ggplot(genkai_h4b, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：オンライン投票利用意向", y="技術信頼の限界効果", x=setmlab)
# h4b_genkaikoka_plot（図５b）
ggsave("./out/h4b_genkaikoka_plot.png", width = 4, height = 4)

## H5A ##

## 限界効果の出力
genkaiout <- intereff(m = mh5a, # 回帰モデルオブジェクト
                      main = "onlinevote", # 独立変数
                      mod = "kanshin", # 条件付け変数名
                      modrange = c(0,1), # 条件付け変数を動かす範囲
                      nsim=3) # 条件付け変数で結果を出力する値の数
genkaiout # estがmodに条件付けされたmainの係数

## プロットのモード設定
# "discrete"はMの値が少ないもしくはダミーの場合
# "continuous"はMの値が多くて連続している場合
setmode <- "discrete" 

## ラベル設定（共通）
setxlab <- "オンライン投票導入（実験条件）" ## Xの変数ラベル
setylab <- "投票参加意向" ## Yの変数ラベル
setmlab <- "政治関心" ## M（条件付け変数）の変数ラベル

## 条件付け変数のラベル設定（setmode=="discrete"のときのみ適用）
setmlabels <- c("関心低（0）","関心中（0.5）","関心高（1）") ## nsimの数と対応させる
# setmlabels <- waiver() ## わからないときはこれでsetxvalsをそのまま表示

## 限界効果プロット（半自動）####################
ggplot(genkaiout, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  {if (setmode=="discrete") { list(
    geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                  width=0.1, linewidth=0.5),
    geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                  width=0, linewidth=2),
    geom_point(aes(x=as.factor(mod)), color="white"),
    scale_x_discrete(labels=setmlabels)) } } +
  {if (setmode=="continuous") { list(
    geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3),
    geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5),
    geom_line(aes(x=mod, y=est))) } } +
  labs(subtitle=paste0("従属変数：",setylab),
       y=paste0(setxlab,"の限界効果"), x=setmlab,
       caption=paste0("注：",ifelse(setmode=="discrete","エラーバー","塗りつぶし"),
                      "は、90％および95％信頼区間を示している。")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
##（ここまで）###################################

## h5a_genkaikoka_plot（図７）
ggsave("./out/h5a_genkaikoka_plot.png", width = 6, height = 4)


###########################################################
## H5B & H5C: 限界効果のプロット（インターネット・責任）##
###########################################################

## H5B: インターネット使用頻度によるオンライン投票導入の効果差
genkai_h5b <- intereff(m = mh5b, 
                       main = "onlinevote", 
                       mod = "useinternet", 
                       modrange = range(dn$useinternet, na.rm=TRUE), 
                       nsim = 6) #GK 実際には6つ存在するのでデータに合わせる # 低・中・高の3点抽出

#GK グラフを作成
# ラベル設定 　
setmlab <- "ネット利用時間／日"
setmlabels <- c("30m\n(2)", "1H\n(3)", "2H\n(4)", "3H\n(5)", "4H\n(6)", "5H+\n(7)")

## 限界効果プロット（H5b）
ggplot(genkai_h5b, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", y="オンライン投票導入の限界効果", x=setmlab)
# h5b_genkaikoka_plot（図９）
ggsave("./out/h5b_genkaikoka_plot.png", width = 6, height = 4)

## H5C-a: 政府の責任認識による政府信頼の効果差
genkai_h5c_a <- intereff(m = mh5c_a_1, 
                         main = "govtH", 
                         mod = "gvtresp_1", 
                         modrange = range(dn$gvtresp_1, na.rm=TRUE), 
                         nsim = 4) #GK 実際には4つ値があるので4に変更

# ラベル設定
# setxlab <- "政府信頼の限界効果" #GK 使っていないのでコメントアウト
setmlab <- "政府の責任認識（社会）"
setmlabels <- c("あまりない\n(1)", "少し\n(2)", 
                "ある程度\n(3)", "大きな責任\n(4)")

## 限界効果プロット（H5C-a）
ggplot(genkai_h5c_a, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", y="政府信頼の限界効果", x=setmlab)
# h5c_a_genkaikoka_plot（図１２a）
ggsave("./out/h5c_a_genkaikoka_plot.png", width = 4, height = 4)

## H5C-a_2: 政府の責任認識(自身の暮らし向)による政府信頼の効果差
genkai_h5c_a_2 <- intereff(m = mh5c_a_2, 
                         main = "govtH", 
                         mod = "gvtresp_2", 
                         modrange = range(dn$gvtresp_2, na.rm=TRUE), 
                         nsim = 4) #GK 実際には4つ値があるので4に変更

# ラベル設定
# setxlab <- "政府信頼の限界効果" #GK 使っていないのでコメントアウト
setmlab <- "政府の責任認識（自身）"
setmlabels <- c("あまりない\n(1)", "少し\n(2)", 
                "ある程度\n(3)", "大きな責任\n(4)")

## 限界効果プロット（H5C-a）
ggplot(genkai_h5c_a_2, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", y="政府信頼の限界効果", x=setmlab)
# h5c_a_2_genkaikoka_plot（図13a）
ggsave("./out/h5c_a_2_genkaikoka_plot.png", width = 4, height = 4)

#GK 追加
## H5C-b: 政府の責任認識による政府信頼の効果差
genkai_h5c_b <- intereff(m = mh5c_b_1, 
                         main = "govtH", 
                         mod = "gvtresp_1", 
                         modrange = range(dn$gvtresp_1, na.rm=TRUE), 
                         nsim = 4) #GK 実際には4つ値があるので4に変更

# ラベル設定
setmlab <- "政府の責任認識（社会）"
setmlabels <- c("あまりない\n(1)", "少し\n(2)", 
                "ある程度\n(3)", "大きな責任\n(4)")

## 限界効果プロット（H5C-b）
ggplot(genkai_h5c_b, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：オンライン投票利用意向", y="政府信頼の限界効果", x=setmlab)
# h5c_b_genkaikoka_plot（図12b）
ggsave("./out/h5c_b_genkaikoka_plot.png", width = 4, height = 4)

## H5C-b_2: 政府の責任認識(自身の暮らし向)による政府信頼の効果差
genkai_h5c_b_2 <- intereff(m = mh5c_b_2, 
                         main = "govtH", 
                         mod = "gvtresp_2", 
                         modrange = range(dn$gvtresp_2, na.rm=TRUE), 
                         nsim = 4) #GK 実際には4つ値があるので4に変更

# ラベル設定
setmlab <- "政府の責任認識（自身）"
setmlabels <- c("あまりない\n(1)", "少し\n(2)", 
                "ある程度\n(3)", "大きな責任\n(4)")

## 限界効果プロット（H5C-b）
ggplot(genkai_h5c_b_2, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：オンライン投票利用意向", y="政府信頼の限界効果", x=setmlab)
# h5c_b_2_genkaikoka_plot（図13b）
ggsave("./out/h5c_b_2_genkaikoka_plot.png", width = 4, height = 4)

##################
## 発展的な分析 ##
##################

## 政府の責任意識の分布を出力（本文では言及なし、参考）############

# 表をデータフレームに
t1 <- table(factor(round(dn$gvtresp_1), levels = 1:4))
df1 <- data.frame(値 = names(t1), 度数 = as.numeric(t1))

p1 <- ggplot(df1, aes(x = 値, y = 度数)) +
  geom_col(fill = "steelblue", color = "white") +
  labs(title = "gvtresp_1の分布", x = "gvtresp_1", y = "度数") +
  theme_bw()
# gvtresp_1_hist
ggsave("./out/gvtresp_1_hist.png", p1, width = 5, height = 4, dpi = 120)


# 表をデータフレームに
t2 <- table(factor(round(dn$gvtresp_2), levels = 1:4))
df2 <- data.frame(値 = names(t2), 度数 = as.numeric(t2))

p2 <- ggplot(df2, aes(x = 値, y = 度数)) +
  geom_col(fill = "steelblue", color = "white") +
  labs(title = "gvtresp_2の分布", x = "gvtresp_2", y = "度数") +
  theme_bw()
# gvtresp_2_hist
ggsave("./out/gvtresp_2_hist.png", p2, width = 5, height = 4, dpi = 120)

## H5B(発展): 限界効果のプロット（インターネット） dummy変数version ####
## ※本文では言及なし、参考用

## H5b. 変数の定義
dn$dummy_useinternet <- NA
dn$dummy_useinternet[which(d$useinternet%in%c(1,2,3,4))] <- 0
dn$dummy_useinternet[which(d$useinternet%in%c(5,6,7))] <- 1
## H5b. Y=投票参加意向、X=オンライン投票導入、M=SNSの使用頻度、Z=政府信頼高、技術信頼高: SNSの使用頻度を01に設定
mh5b_dummy <- lm_robust(sankaiko ~ onlinevote*dummy_useinternet + govtH + techH, data = dn)
screenreg(list(mh5b_dummy), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

setmlab <- "インターネット使用量(少/多)"
setmlabels <- c("使用少(0)","使用多(1)")

genkai_h5b_dummy <- intereff(m = mh5b_dummy,
                             main = "onlinevote",
                             mod = "dummy_useinternet",
                             modrange = c(0,1),
                             nsim = 2)
ggplot(genkai_h5b_dummy, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", y="オンライン投票導入の限界効果", x=setmlab)
# h5b_dummy_genkaikoka_plot
ggsave("./out/h5b_dummy_genkaikoka_plot.png", width = 6, height = 4)

## 実験刺激文が機能しているかのチェック（図14）########################

dn$techH <- as.numeric(d$exp2_techH)
dn$techT <- d$exp2_q5

dn$onlinevote <- as.numeric(d$exp2_onlinevote)
dn_online <- subset(dn, onlinevote == 1)
mctechT <- lm_robust(techT ~ techH, data = dn_online)

screenreg(list(mctechT), include.ci = FALSE,
          digits=3, single.row = TRUE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")
## 実験刺激のチェック: techH の 0/1 で techT がどう変わっているか（平均＋95%CI）
yosoku_techT <- genpr(dpr = dn_online, mpr = mctechT, 
                      setx = "techH", setxvals = c(0, 1))

ggplot(yosoku_techT, aes(x = as.factor(x), y = pr)) +
  geom_col(fill = "gray50", width = 0.6) +
  geom_errorbar(aes(ymin = lo95, ymax = up95), width = 0.1, linewidth = 0.5) +
  scale_x_discrete(labels = c("技術信頼低(0)", "技術信頼高(1)")) +
  labs(x = "実験条件（技術への信頼）", 
       y = "オンライン投票システムの信頼度（予測値）",
       subtitle = "技術への信頼度") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))
# mctechT_plot（図14）
ggsave("./out/mctechT_plot.png", width = 6, height = 4)