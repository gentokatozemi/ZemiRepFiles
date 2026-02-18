## 「家族計画における子育て支援政策の認識とその影響」政経セミナー（2026）
## 分析コード
## 作成日：2026/02/19

##########
## 準備 ##
##########

## ワークスペースを掃除する
rm(list=ls())

## ワーキングディレクトリをこのファイルの場所に設定する
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd() # 確認

## 図表出力用のフォルダ（out）を作成
if (!"./out" %in% list.dirs()) dir.create("./out")

## 必要な分析パッケージのインポート
## （エラーが出る場合は install.packages("PACKAGE-NAME")でインストール）
library(stringi) # 全角テキストを半角にするのに使う
library(estimatr) # ロバスト標準誤差付きの回帰モデル
library(ggplot2) # 図の出力
# theme_set(theme_bw()) #デフォルトテーマ（Windows他）
# theme_set(theme_bw(base_family = "HiraKakuProN-W3")) # Macの場合
theme_set(theme_bw(base_family = "Noto Sans CJK JP")) # Linuxの場合
library(texreg) # 回帰表の出力
library(labelled) # 変数ラベルの参照
library(htmltools)

## データをインポートする
d <- readRDS("dataset_g2_main.rds")
dim(d)

##########################
## データ・クリーニング ##
##########################

## 39歳以下対象の調査ですが、様々な要因により、
## 40歳以上の回答者が数人存在しています。
## また、2名の二重回答者が確認されました。
## これらの回答者は、調査対象外なのでデータから除きます。

## 年齢データを調整する。
table(d$v13, exclude=F) 
# 概ね生の値で良いが、たまに全角や「歳」がついた回答がある。
class(d$v13) # characterクラスなので数値じゃない。
d$v13 <- d$v13
d$v13 <- gsub("歳","",d$v13) # 歳を削除
d$v13 <- gsub("才","",d$v13) # 才を削除
d$v13 <- stri_trans_nfkc(d$v13) # 全角数字を半角にする
d$v13 <- as.numeric(d$v13) # 数値化
table(d$v13, exclude=F)
class(d$v13) # numericクラスに変換済。

## 年齢40歳以上をデータから削除する
d <- subset(d, d$v13 <= 39)
table(d$v13) # 40歳以上がいない

## 二重回答者をデータから削除する
d <- subset(d, !d$timestamp_start %in% c("2025/01/27 14:32:07",
                                         "2025/01/29 10:24:40"))

### データサイズを確認
dim(d)

################
## 変数の作成 ##
################

## 新しいデータセットのハコの準備
dn <- data.frame(id = d$id)

## 結婚意向
table(d$v09, exclude=F) ## 分布を確認（値の意味はコードブック確認）
dn$kekkoniko <- 5 - as.numeric(d$v09) # 値を逆転させる
table(dn$kekkoniko) # 事前登録通りか確認

## 子ども意向
table(d$v10, exclude=F) ## 分布を確認（値の意味はコードブック確認）
dn$kodomoiko <- as.numeric(d$v10) - 1  # 生の値から1を引く
table(dn$kodomoiko) # 事前登録通りか確認

## 児童手当（1＝提示、0＝非提示）
table(d$e01, exclude=F) ## 1=統制、2=児童手当、3=養育費、4=両方
dn$jidoteate <- NA
dn$jidoteate[which(d$e01%in%c(1,3))] <- 0
dn$jidoteate[which(d$e01%in%c(2,4))] <- 1
table(dn$jidoteate, exclude=F)

## 養育費（1＝提示、0＝非提示）
table(d$e01, exclude=F) ## 1=統制、2=児童手当、3=養育費、4=両方
dn$yoikuhi <- NA
dn$yoikuhi[which(d$e01%in%c(1,2))] <- 0
dn$yoikuhi[which(d$e01%in%c(3,4))] <- 1
table(dn$yoikuhi, exclude=F)

## 養育費認識
table(d$v04, exclude=F) ## 表記にブレがあるので調整が必要
dn$yoikuhi_ninshiki <- as.character(d$v04) # まずは文字としてコピー
dn$yoikuhi_ninshiki[which(dn$yoikuhi_ninshiki=="")] <- NA # 無回答はNA
dn$yoikuhi_ninshiki <- gsub(",","",dn$yoikuhi_ninshiki,fixed=T) # ,を削除
dn$yoikuhi_ninshiki <- gsub(".","",dn$yoikuhi_ninshiki,fixed=T) # .を削除
dn$yoikuhi_ninshiki <- gsub("万|円","",dn$yoikuhi_ninshiki) # 万or円を削除
dn$yoikuhi_ninshiki <- gsub("くらい","",dn$yoikuhi_ninshiki) # くらいを削除
dn$yoikuhi_ninshiki <- gsub("千","000",dn$yoikuhi_ninshiki) # 千を数字に
dn$yoikuhi_ninshiki <- stri_trans_nfkc(dn$yoikuhi_ninshiki) # 全角を半角に
table(dn$yoikuhi_ninshiki, exclude=F) 
## 金額が高すぎる人がいる。単位を円にしていると考えられるので万円単位に変換
dn$yoikuhi_ninshiki[which(dn$yoikuhi_ninshiki=="10000000")] <- 1000
dn$yoikuhi_ninshiki[which(dn$yoikuhi_ninshiki=="20000000")] <- 2000
dn$yoikuhi_ninshiki[which(dn$yoikuhi_ninshiki=="25000000")] <- 2500
table(dn$yoikuhi_ninshiki, exclude=F) 
## 数値に変換
dn$yoikuhi_ninshiki <- as.numeric(dn$yoikuhi_ninshiki) # 数値に変換
## 変換結果を確認
table(dn$yoikuhi_ninshiki, exclude=F) 
summary(dn$yoikuhi_ninshiki)
## 10億と答えている人がいるが、数値が大きすぎるのでタイポとみなして1億に変換
##（※人為的措置なので検証は必要かも）
dn$yoikuhi_ninshiki[which(dn$yoikuhi_ninshiki==100000)] <- 10000
summary(dn$yoikuhi_ninshiki)
## ヒストグラムの簡易的チェック（そんなに偏りはないので、対数変換はしない）
hist(dn$yoikuhi_ninshiki) 
## 2000を引いて標準偏差で割って新しい変数を作る（事前登録通り）
dn$yoikuhi_ninshiki_std <- 
  (dn$yoikuhi_ninshiki-2000)/sd(dn$yoikuhi_ninshiki, na.rm = T)
summary(dn$yoikuhi_ninshiki_std) ## 確認

######################
## 記述統計の可視化 ##
######################

## 統制群にデータを絞る
dn0 <- subset(dn, dn$jidoteate==0 & dn$yoikuhi==0)

## 図１：従属変数の回答分布（統制群）##

## 出産意向

### プロット用のデータ作成
plotdata <- data.frame(x = names(table(dn0$kodomoiko)), 
                       y = as.numeric(prop.table(table(dn0$kodomoiko))))
plotdata$x <- factor(plotdata$x, levels=c(0,1,2,3),
                     labels = c("0人\n(0)",
                                "1人\n(1)",
                                "2人\n(2)",
                                "3人以上\n(3)"))
plotdata1 <- plotdata

## 結婚意向

### プロット用のデータ作成
plotdata <- data.frame(x = names(table(dn0$kekkoniko)), 
                       y = as.numeric(prop.table(table(dn0$kekkoniko))))
plotdata$x <- factor(plotdata$x, levels=c(1,2,3,4),
                     labels = c("欲しいと\n思わない\n(1)",
                                "どちらか\nと言えば\n欲しいと\n思わない\n(2)",
                                "どちらか\nと言えば\n欲しいと\n思う\n(3)",
                                "欲しいと\n思う\n(4)"))
plotdata2 <- plotdata

## 結合
plotdata1$dv <- "出産意向"
plotdata2$dv <- "結婚意向"
plotdata <- rbind(plotdata1, plotdata2)
plotdata$dv <- factor(plotdata$dv, levels=unique(plotdata$dv))
plotdata$x <- factor(plotdata$x, levels=unique(plotdata$x))
### プロット
ggplot(plotdata, aes(x=x, y=y)) +
  geom_bar(stat = "identity") +
  facet_grid(.~dv, scales = "free") + 
  labs(x = NULL, y="統制群における回答割合") +
  # theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11, face="bold"))
### 保存
ggsave("./out/bunpu_shussankekkoniko.png", width=4.8, height=3.307225)

## *児童手当と養育費はランダムに割り当てられているので省略

## 図２：養育費用認識の回答分布 ##

## 養育費認識

## ラベル作成のために、標準化値を生の値でも表現する
summary(dn$yoikuhi_ninshiki_std) # -2から7まで
sdtorawval <- round(2000 + c(-1:6)*sd(dn$yoikuhi_ninshiki, na.rm = T))
sdtorawval

### ヒストグラムをプロット
ggplot(dn, aes(x=yoikuhi_ninshiki_std)) + 
  geom_histogram(bins = 15, alpha=0.8) +
  geom_vline(aes(xintercept=0), linetype=2) + 
  scale_x_continuous(breaks=c(-1:6),
                     labels=c(paste0(c(-1:6),"SD\n(",
                                     sdtorawval,"\n万円)"))) + 
  labs(x = "養育費用認識", 
       y="回答割合") #+
  # theme_bw()

### 保存
ggsave("./out/bunpu_yoikuhi_ninshiki_std.png", width=4.8, height=3.321136)

##########
## 分析 ##
##########

## H1 

### 分析の実行
m1 <- lm_robust(kodomoiko ~ jidoteate + yoikuhi, data=dn) 

### 簡易的な回帰表
summary(m1)

## H2

### 分析の実行
m2 <- lm_robust(kekkoniko ~ jidoteate + yoikuhi, data=dn) 

### 簡易的な回帰表
summary(m2)

## H3

### 分析の実行
m3 <- lm_robust(kodomoiko ~ yoikuhi*yoikuhi_ninshiki_std + jidoteate, 
                data=dn)

### 簡易的な回帰表
summary(m3)

## H4

### 分析の実行
m4 <- lm_robust(kekkoniko ~ yoikuhi*yoikuhi_ninshiki_std + jidoteate, 
                data=dn)

### 簡易的な回帰表
summary(m4)


## 追加分析

### 分析の実行
m5 <- lm_robust(kodomoiko ~ yoikuhi*jidoteate, 
                data=dn)
m6 <- lm_robust(kekkoniko ~ yoikuhi*jidoteate, 
                data=dn)

### 簡易的な回帰表
summary(m5)
summary(m6)

######################
## 分析結果の可視化 ##
######################

## 回帰表の作成 ##

### まずはジェネリックな表を見る
screenreg(list(m1,m2,m3,m4),
          include.ci=FALSE, digits=3,
          stars = c(0.001,0.01,0.05,0.1), symbol="+",
          custom.header = list("DV:kodomoiko"=1:2,"DV:kekkoniko"=3:4),
          custom.model.names = c("H1","H2","H3","H4")) 

### 変数名ラベルをつける
vnmap <- list("(Intercept)" = " （定数項）",
              "jidoteate" = " 児童手当情報提示",
              "yoikuhi" = " 養育費用情報提示",
              "yoikuhi_ninshiki_std" = " 養育費用認識",
              "yoikuhi:yoikuhi_ninshiki_std" = " 養育費用情報 * 認識",
              "yoikuhi:jidoteate" = " 養育費用 * 児童手当")

### HTMLで表を出力
## 表１：H1とH2の検証結果 ##
browsable(HTML(htmlreg(list(m1,m2),
                       include.ci=FALSE, digits=3, #single.row = TRUE,
                       stars = c(0.001,0.01,0.05,0.1), symbol="+",
                       custom.header = list("出産意向"=1, "結婚意向"=2),
                       custom.model.names = c("H1","H2"),
                       custom.coef.map = vnmap,
                       caption = "", 
                       caption.above = TRUE,
                       custom.note = "%stars。OLS回帰分析結果。括弧内はロバスト標準誤差。")))
## 表２：H3とH4の検証結果 ##
browsable(HTML(htmlreg(list(m3,m4),
                       include.ci=FALSE, digits=3, #single.row = TRUE,
                       stars = c(0.001,0.01,0.05,0.1), symbol="+",
                       custom.header = list("出産意向"=1, "結婚意向"=2),
                       custom.model.names = c("H3","H4"),
                       custom.coef.map = vnmap,
                       caption = "", 
                       caption.above = TRUE,
                       custom.note = "%stars。OLS回帰分析結果。括弧内はロバスト標準誤差。")))
## 表３：追加分析の検証結果 ##
browsable(HTML(htmlreg(list(m5,m6),
                       include.ci=FALSE, digits=3, #single.row = TRUE,
                       stars = c(0.001,0.01,0.05,0.1), symbol="+",
                       # custom.header = list("出産意向"=1, "結婚意向"=2),
                       custom.model.names = c("出産意向","結婚意向"),
                       custom.coef.map = vnmap,
                       caption = "", 
                       caption.above = TRUE,
                       custom.note = "%stars。OLS回帰分析結果。括弧内はロバスト標準誤差。")))

## 予測値プロット ##

## 予測値データ出力用関数（条件付なし）
yosokuchi1 <- function(dpr, # データ
                      mpr, # モデル
                      setx, # 独立変数名
                      setlevx, # 独立変数の値
                      setlabx) { # 独立変数の値ラベル
  
  ## シミュレーション用データの作成
  simdt <- na.omit(dpr[,all.vars(mpr$terms)])
  simx <- sort(unique(simdt[,setx]))
  if(length(simx)>11) simx <- seq(min(simx),max(simx),length=11)
  ###（予測値出力）
  prout <- as.data.frame(t(sapply(simx, function(k) {
    tmpdt <- simdt
    tmpdt[,setx] <- k
    tmp <- colMeans(as.data.frame(predict(mpr, newdata=tmpdt, se.fit=TRUE)))
    tmp <- c(k, tmp[1:2], 
             tmp[1]-tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.975,df=mpr$df[1]))
    names(tmp) <- c("x","pr","se","lowerci","upperci")
    return(tmp)
  })))
  ### (ラベル付き独立変数)
  prout$labelledx <- factor(prout$x,levels=setlevx,labels=setlabx)
  ### 結果の出力 
  return(prout) 
}

## H1
## データ、モデルと変数名を代入する
dpr <- dn ## データ
mpr <- m1 ## モデル
setx <- "jidoteate" # 独立変数名
setlevx <- c(0,1) # 独立変数の値
setlabx <- c("提示なし","提示あり") # 独立変数の値ラベル
## 予測値を出力
proutH1 <- yosokuchi1(dpr, mpr, setx, setlevx, setlabx)
proutH1$dv <- "出産意向（0-3）"

## H2
## データ、モデルと変数名を代入する
dpr <- dn ## データ
mpr <- m2 ## モデル
setx <- "jidoteate" # 独立変数名
setlevx <- c(0,1) # 独立変数の値
setlabx <- c("提示なし","提示あり") # 独立変数の値ラベル
## 予測値を出力
proutH2 <- yosokuchi1(dpr, mpr, setx, setlevx, setlabx)
proutH2$dv <- "結婚意向（1-4）"

## H1、H2をまとめてプロットする　
prout <- rbind(proutH1, proutH2)
prout$dv <- factor(prout$dv, levels=unique(prout$dv))

### 図３：H1とH2に関する予測値プロット ###
library(ggplot2)
p <- ggplot(prout, aes(x=labelledx, y=pr)) + 
  geom_errorbar(aes(ymin=lowerci, ymax=upperci), width=.1) + 
  geom_point() + 
  facet_wrap(vars(dv), scales = "free_y") + 
  labs(x="児童手当情報", 
       y=paste0("従属変数の予測値平均\n（95％信頼区間付き）"),
       caption="注：H1、H2モデルに基づいて算出。ロバスト標準誤差を使用。") + 
  # theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11, face="bold"))
## プロットを保存
p
ggsave("./out/predictionplot_h1h2.png",width=4.8,height=3.307225)

## 予測値データ出力用関数（条件付あり）
yosokuchi2 <- function(dpr,
                       mpr, 
                       setx,
                       setm, 
                       setmvals) {
  
  ### (シミュレーションデータ)
  simdt <- na.omit(dpr[,all.vars(mpr$terms)])
  simx <- sort(unique(simdt[,setx]))
  if(length(simx)>11) simx <- seq(min(simx),max(simx),length=11)
  simm <- sort(unique(simdt[,setm]))
  if(length(simm)>11) simm <- seq(min(simm),max(simm),length=11)
  if (is.numeric(simm)) {
    if (any(!unlist(setmvals)%in%simm)) {
      simm <- sort(unique(c(simm,unlist(setmvals))))
    }
  }
  simv <- data.frame(simx = rep(simx,each=length(simm)), 
                     simm = simm)
  ###（予測値出力）
  prout <- as.data.frame(t(apply(simv, 1, function(k) {
    tmpdt <- simdt
    tmpdt[,setx] <- k[1]
    tmpdt[,setm] <- k[2]
    tmp <- colMeans(as.data.frame(predict(mpr, newdata=tmpdt, se.fit=TRUE)))
    tmp <- c(k, tmp[1:2], 
             tmp[1]-tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.975,df=mpr$df[1]))
    names(tmp) <- c("x","m","pr","se","lowerci","upperci")
    return(tmp)
  })))
  ### (ラベル付き独立変数)
  prout$labelledx <- factor(prout$x,levels=setlevx,labels=setlabx)
  ### (ラベル付き条件付き変数)
  prout$labelledm <- 
    factor(names(setmvals)[match(prout$m,unlist(setmvals))],
           levels = names(setmvals))
  ### 出力
  return(prout)
  
}

## H3
## モデルと変数名を代入する
dpr <- dn ## データ
mpr <- m3 ## モデル
setx <- "yoikuhi" # 独立変数名
setm <- "yoikuhi_ninshiki_std" # 条件付け変数名
setmvals <- list("少なめに\n認識 (-1)" = -1,
                 "実際の平均\nと同じ (0)" = 0,
                 "多めに\n認識 (1)" = 1) #条件付け変数の値
## 予測値を出力
proutH3 <- yosokuchi2(dpr, mpr, setx, setm, setmvals) 
proutH3$dv <- "出産意向（0-3）"

## H4
## モデルと変数名を代入する
dpr <- dn ## データ
mpr <- m4 ## モデル
setx <- "yoikuhi" # 独立変数名
setm <- "yoikuhi_ninshiki_std" # 条件付け変数名
setmvals <- list("少なめに\n認識 (-1)" = -1,
                 "実際の平均\nと同じ (0)" = 0,
                 "多めに\n認識 (1)" = 1) #条件付け変数の値
## 予測値を出力
proutH4 <- yosokuchi2(dpr, mpr, setx, setm, setmvals) 
proutH4$dv <- "結婚意向（1-4）"

## H3、H4をまとめてプロットする　
prout <- rbind(proutH3, proutH4)
prout$dv <- factor(prout$dv, levels=unique(prout$dv))
setmname <- "養育費用\n認識" #条件付け変数ラベル

### 図４：H3とH4に関する予測値プロット ###
p <- ggplot(na.omit(prout), aes(x=labelledx, y=pr)) + 
  geom_errorbar(aes(ymin=lowerci, ymax=upperci,
                    color = labelledm), width=0.1,
                position = position_dodge(width=0.35)) + 
  geom_point(aes(color=labelledm,shape=labelledm),
             position = position_dodge(width=0.35)) + 
  scale_shape_discrete(name = setmname) + 
  scale_linetype_discrete(name = setmname) + 
  scale_color_brewer(name = setmname, type = "qual", palette = 2) + 
  scale_fill_brewer(name = setmname, type = "qual", palette = 2) + 
  facet_wrap(vars(dv), scales = "free_y") + 
  labs(x="養育費用情報", 
       y=paste0("従属変数の予測値平均\n（95％信頼区間付き）"),
       caption="注：H3、H4モデルに基づいて算出。ロバスト標準誤差を使用。") + 
  # theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11, face="bold"),
        legend.position = "bottom")
## プロットを保存
p
ggsave("./out/predictionplot_h3h4.png",width=4.8,height=4.216352)

## 限界効果プロット（M5とM6のみ）##

## 限界効果の出力関数
genkai <- function(mpr, # 独立変数名
                   setx, # 独立変数ラベル
                   setm, # 条件付け変数名
                   setmvals) { # 条件付け変数の値
  ### (限界効果の計算)
  mc <- summary(mpr)$coefficients
  prout <- data.frame(mval = setmvals, 
                      est = NA, se = NA)
  if(paste(setx,setm,sep=":")%in%rownames(mc)) {
    setxm <- paste(setx,setm,sep=":")
  } else {
    setxm <- paste(setm,setx,sep=":")
  }
  for(i in 1:nrow(prout)) {
    prout$est[i] <- 
      mc[which(rownames(mc)==setx),1] + 
      (prout$mval[i])*mc[which(rownames(mc)==setxm),1]
    prout$se[i] <- sqrt(
      mc[which(rownames(mc)==setx),2]^2 + 
        (prout$mval[i])^2 * mc[which(rownames(mc)==setxm),2]^2 + 
        2 * (prout$mval[i]) * vcov(mpr)[which(rownames(mc)==setx),
                                        which(rownames(mc)==setxm)])
  }
  prout$lci <- prout$est - qt(0.975, df=mpr$df[1])*prout$se
  prout$uci <- prout$est + qt(0.975, df=mpr$df[1])*prout$se
  prout$lci90 <- prout$est - qt(0.95, df=mpr$df[1])*prout$se
  prout$uci90 <- prout$est + qt(0.95, df=mpr$df[1])*prout$se
  return(prout)
}

## 児童手当情報 ##

## M5
## モデルと変数名を代入する
mpr <- m5 ## モデル
setx <- "jidoteate" # 独立変数名
setm <- "yoikuhi" # 条件付け変数名
setmvals <- seq(0,1,length = 2) #条件付け変数の値
## 限界効果を出力
prout <- genkai(mpr, setx, setm, setmvals)
prout$m <- factor(c("提示なし","提示あり"),
                  levels=c("提示なし","提示あり"))
prout5 <- prout
prout5$dv <- "出産意向（0-3）"

## M6
## モデルと変数名を代入する
mpr <- m6 ## モデル
setx <- "jidoteate" # 独立変数名
setm <- "yoikuhi" # 条件付け変数名
setmvals <- seq(0,1,length = 2) #条件付け変数の値
## 限界効果を出力
prout <- genkai(mpr, setx, setm, setmvals)
prout$m <- factor(c("提示なし","提示あり"),
                  levels=c("提示なし","提示あり"))
prout6 <- prout
prout6$dv <- "結婚意向（1-4）"

## まとめてプロットする　
prout <- rbind(prout5, prout6)
prout$dv <- factor(prout$dv, levels=unique(prout$dv))
setmname <- "養育費用情報" #条件付け変数ラベル
setcaption <- "注：表３のモデルに基づいて算出。ロバスト標準誤差を使用。"
## 図を保存
## 図５：追加分析の限界効果（児童手当情報）##
p <- ggplot(prout, aes(x=m, y=est)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.1) + 
  geom_errorbar(aes(ymin=lci90, ymax=uci90), 
                width=0, linewidth = 1.5) + 
  geom_point(size=3) + 
  facet_grid(.~dv) + 
  labs(x=setmname, 
       y=paste0("児童手当情報提示の条件付き効果\n（90%、95％信頼区間付き）"),
       caption=setcaption) + 
  # theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11, face="bold"),
        legend.position = "bottom")
p
ggsave("./out/marginaleff_jidoteate56.png", width=4.8, height=3.307225)

## 養育費用情報 ##

## M5
## モデルと変数名を代入する
mpr <- m5 ## モデル
setx <- "yoikuhi" # 独立変数名
setm <- "jidoteate" # 条件付け変数名
setmvals <- seq(0,1,length = 2) #条件付け変数の値
## 限界効果を出力
prout <- genkai(mpr, setx, setm, setmvals)
prout$m <- factor(c("提示なし","提示あり"),
                  levels=c("提示なし","提示あり"))
prout5 <- prout
prout5$dv <- "出産意向（0-3）"

## M6
## モデルと変数名を代入する
mpr <- m6 ## モデル
setx <- "yoikuhi" # 独立変数名
setm <- "jidoteate" # 条件付け変数名
setmvals <- seq(0,1,length = 2) #条件付け変数の値
## 限界効果を出力
prout <- genkai(mpr, setx, setm, setmvals)
prout$m <- factor(c("提示なし","提示あり"),
                  levels=c("提示なし","提示あり"))
prout6 <- prout
prout6$dv <- "結婚意向（1-4）"

## まとめてプロットする　
prout <- rbind(prout5, prout6)
prout$dv <- factor(prout$dv, levels=unique(prout$dv))
setmname <- "児童手当情報" #条件付け変数ラベル
setcaption <- "注：表３のモデルに基づいて算出。ロバスト標準誤差を使用。"
## 図を保存
## 図６：追加分析の限界効果（養育費用情報）##
p <- ggplot(prout, aes(x=m, y=est)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.1) + 
  geom_errorbar(aes(ymin=lci90, ymax=uci90), 
                width=0, linewidth = 1.5) + 
  geom_point(size=3) + 
  facet_grid(.~dv) + 
  labs(x=setmname, 
       y=paste0("養育費用情報提示の条件付き効果\n（90%、95％信頼区間付き）"),
       caption=setcaption) + 
  # theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11, face="bold"),
        legend.position = "bottom")
p
ggsave("./out/marginaleff_yoikuhi56.png", width=4.8, height=3.307225)