## 候補者外見年齢実験　主要分析コード
## 文責：加藤言人
## 作成日：2026/1/13

##########
## 準備 ##
##########

## ワークスペースを掃除する
rm(list=ls())

## ワーキングディレクトリをこのファイルの場所に設定する
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd() # 確認

## 必要な分析パッケージのインポート
## （エラーが出る場合は install.packages("PACKAGE-NAME")でインストール）
library(labelled) # ラベル付き変数の管理
library(estimatr) # ロバスト標準誤差付きの回帰モデル
## 図の文字化け防止にフォントをセット
if (Sys.info()["sysname"]=="Windows") {
  extrafont::font_import(pattern = "Noto")
  extrafont::loadfonts(device="win")
  library(ggplot2) # 図の出力
  theme_set( theme_bw(base_family = "Noto Sans JP")) 
} else if (Sys.info()["sysname"]=="Darwin") {
  library(ggplot2) # 図の出力
  theme_set( theme_bw(base_family = "HiraKakuProN-W3")) 
} else if (Sys.info()["sysname"]=="Linux") { 
  library(ggplot2) # 図の出力
  theme_set( theme_bw(base_family = "Noto Sans CJK JP"))
}
library(superb) # showSignificance
library(texreg) # 回帰表の出力
library(lmtest) # 補助的な回帰表情報の出力

## 限界効果の算出関数
genkaikoka <- function(mme, # 回帰モデルオブジェクト
                       setx, # 独立変数の名前
                       setm,　# 条件付け変数の名前
                       setmvals, # 結果を出力する条件付け変数の値
                       setmlabs = NULL) { # 条件付け変数の値ラベル
  
  ## 交差項変数の名前の抽出
  setxsetm = paste(setx,setm,sep=":")
  if (!setxsetm%in%rownames(vcov(mme))) setxsetm = paste(setm,setx,sep=":")
  
  ## 自由度の抽出
  if ("df"%in%names(mme)) {
    dfset <- mme$df[1] 
    # Assuming that df is the same across all.
    # CAUTION: The above is not true if lm_robust's se_type="CR2".
  } else {
    dfset <- df.residual(mme)
  }
  
  ## 係数と分散共分散マトリックスの抽出
  cfset <- c(coef(mme)[which(names(coef(mme))==setx)],
             coef(mme)[which(names(coef(mme))==setxsetm)])
  vcset <- c(vcov(mme)[which(rownames(vcov(mme))==setx),
                       which(colnames(vcov(mme))==setx)],
             vcov(mme)[which(rownames(vcov(mme))==setxsetm),
                       which(colnames(vcov(mme))==setxsetm)],
             vcov(mme)[which(rownames(vcov(mme))==setx),
                       which(colnames(vcov(mme))==setxsetm)])
  
  ## 限界効果の算出
  out = data.frame(m = setmvals,
                   est = cfset[1]+cfset[2]*setmvals,
                   se = sqrt(vcset[1]+setmvals^2*vcset[2]+2*setmvals*vcset[3]),
                   qt90 = qt(0.95,dfset),
                   qt95 = qt(0.975,dfset))
  ## 条件付け変数にラベル付与（ある場合のみ）
  if (!is.null(setmlabs)) {
    out$m <- factor(out$m, levels=setmvals, labels=setmlabs)
  }
  
  ## 信頼区間の追加
  out$lo90 = out$est-out$se*out$qt90
  out$up90 = out$est+out$se*out$qt90
  out$lo95 = out$est-out$se*out$qt95
  out$up95 = out$est+out$se*out$qt95
  
  ## p値の追加
  out$pval = (1 - pt(abs(out$est/out$se),dfset))*2
  
  ## 結果を返す
  return(out)
}

## 予測値の算出
yosokuchi <- function(dpr, # 予測値算出用データ（元データ）
                      mpr, # 予測値算出用分析結果
                      setx, # 独立変数の名前  
                      setxvals, # 独立変数の値設定（numeric/character）
                      setxlabs=NULL, # 独立変数ラベル（カテゴリ変数の場合）
                      setm=NULL, # 条件付け変数の名前
                      setmvals=NULL, # 条件付変数のシミュレーション用値（list）
                      datalab=NULL) { # データにラベル
  
  ## 予測値算出用の元データの作成
  simdt <- na.omit(dpr[,all.vars(mpr$terms)])
  
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
    ## 2つ以上条件付け変数がある場合
    if (length(setm)>1) {
      for (i in 2:length(setm)) {
        simm <- sapply(setmvals, function(x) x[i])
        names(simm) <- NULL
        simv[,paste0("simm",i)] <- simm
      }
    }
    
  }
  
  ## 予測値の出力
  prout <- as.data.frame(t(apply(simv, 1, function(k) {
    
    ## 予測値算出用仮データ
    tmpdt <- simdt
    tmpdt[,setx] <- k[1] # 独立変数の値割り当て
    if (!is.null(setm)) {
      tmpdt[,setm[1]] <- k[2] # 条件付け変数の値割り当て
      ## ２つ以上ある場合
      if (length(setm)>1) {
        for(i in 2:length(setm)) {
          tmpdt[,setm[i]] <- k[1+i] # 条件付け変数の値割り当て
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
    } else if (length(setm)==1) {
      names(tmp) <- c("x","m","pr","se",
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
        factor(names(setmvals)[match(prout$m,unlist(setmvals))],
               levels = names(setmvals))
    } else {
      
      tmpmvals <- sapply(setmvals, function(x) paste(x, collapse=" "))
      tmpm <- apply(prout[,paste0("m",1:length(setm))], 1, 
                    function(x) paste(x, collapse=" "))
      prout$labelledm <- 
        factor(names(setmvals)[match(tmpm,tmpmvals)],
               levels = names(setmvals))

    }
    
  }
  ### データ自体にラベルを割り当て（ある場合）
  if (!is.null(datalab)) {
    prout$datalab <- datalab
  }
  
  ## 結果を出力
  return(prout)
}

## グラフ用のp値テキストを出力
genptxt <- function(pval) {
  ifelse(pval>.1,"n.s.",
         ifelse(pval<.001,"p<.001",
                paste0("p≈",gsub("^0\\.",".",sprintf("%.3f",pval)))))
}

## NAを空白に変換
natoblank <- function(tgt) ifelse(is.na(tgt),"",tgt)

## データをインポートする
d <- readRDS("dataset_g3_main.rds")

### データサイズを確認
dim(d)

################
## 変数の作成 ##
################

## 新しいデータセットのハコの準備
dn <- data.frame(id = d$id)

## 政策選好（実験群ごとに重複なし）
table(d$e01==01, d$v08, exclude=F) ## 分布を確認（群01）
table(d$e01==02, d$v09, exclude=F) ## 分布を確認（群02）
table(d$e01==03, d$v10, exclude=F) ## 分布を確認（群03）
table(d$e01==04, d$v11, exclude=F) ## 分布を確認（群04）
table(d$e01==05, d$v12, exclude=F) ## 分布を確認（群05）
table(d$e01==06, d$v13, exclude=F) ## 分布を確認（群06）
table(d$e01==07, d$v14, exclude=F) ## 分布を確認（群07）
table(d$e01==08, d$v15, exclude=F) ## 分布を確認（群08）
table(d$e01==09, d$v16, exclude=F) ## 分布を確認（群09）
table(d$e01==10, d$v17, exclude=F) ## 分布を確認（群10）
table(d$e01==11, d$v18, exclude=F) ## 分布を確認（群11）
table(d$e01==12, d$v19, exclude=F) ## 分布を確認（群12）
table(d$e01==13, d$v20, exclude=F) ## 分布を確認（群13）
table(d$e01==14, d$v21, exclude=F) ## 分布を確認（群14）
table(d$e01==15, d$v22, exclude=F) ## 分布を確認（群15）
table(d$e01==16, d$v23, exclude=F) ## 分布を確認（群16）
table(d$e01==17, d$v24, exclude=F) ## 分布を確認（群17）
table(d$e01==18, d$v25, exclude=F) ## 分布を確認（群18）
table(d$e01==19, d$v26, exclude=F) ## 分布を確認（群19）
table(d$e01==20, d$v27, exclude=F) ## 分布を確認（群20）
table(d$e01==21, d$v28, exclude=F) ## 分布を確認（群21）
table(d$e01==22, d$v29, exclude=F) ## 分布を確認（群22）
table(d$e01==23, d$v30, exclude=F) ## 分布を確認（群23）
table(d$e01==24, d$v31, exclude=F) ## 分布を確認（群24）
table(d$e01==25, d$v32, exclude=F) ## 分布を確認（群25）
table(d$e01==26, d$v33, exclude=F) ## 分布を確認（群26）
table(d$e01==27, d$v34, exclude=F) ## 分布を確認（群27）
table(d$e01==28, d$v35, exclude=F) ## 分布を確認（群28）
table(d$e01==29, d$v36, exclude=F) ## 分布を確認（群29）
table(d$e01==20, d$v37, exclude=F) ## 分布を確認（群30）
table(d$e01==21, d$v38, exclude=F) ## 分布を確認（群31）
table(d$e01==22, d$v39, exclude=F) ## 分布を確認（群32）
table(d$e01==23, d$v40, exclude=F) ## 分布を確認（群33）
table(d$e01==24, d$v41, exclude=F) ## 分布を確認（群34）
table(d$e01==25, d$v42, exclude=F) ## 分布を確認（群35）
table(d$e01==26, d$v43, exclude=F) ## 分布を確認（群36）
table(d$e01==27, d$v44, exclude=F) ## 分布を確認（群37）
table(d$e01==28, d$v45, exclude=F) ## 分布を確認（群38）
## 値の割り当て
## ※奇数実験群は候補者２が高齢者向け政策なのでそのまま
## ※偶数実験群は候補者１が高齢者向け政策なので逆転させる
dn$seisakusenko <- NA # まずはすべて欠損の変数を作る
dn$seisakusenko[which(d$e01==01)] <- d$v08[which(d$e01==01)]
dn$seisakusenko[which(d$e01==02)] <- 10 - d$v09[which(d$e01==02)]
dn$seisakusenko[which(d$e01==03)] <- d$v10[which(d$e01==03)]
dn$seisakusenko[which(d$e01==04)] <- 10 - d$v11[which(d$e01==04)]
dn$seisakusenko[which(d$e01==05)] <- d$v12[which(d$e01==05)]
dn$seisakusenko[which(d$e01==06)] <- 10 - d$v13[which(d$e01==06)]
dn$seisakusenko[which(d$e01==07)] <- d$v14[which(d$e01==07)]
dn$seisakusenko[which(d$e01==08)] <- 10 - d$v15[which(d$e01==08)]
dn$seisakusenko[which(d$e01==09)] <- d$v16[which(d$e01==09)]
dn$seisakusenko[which(d$e01==10)] <- 10 - d$v17[which(d$e01==10)]
dn$seisakusenko[which(d$e01==11)] <- d$v18[which(d$e01==11)]
dn$seisakusenko[which(d$e01==12)] <- 10 - d$v19[which(d$e01==12)]
dn$seisakusenko[which(d$e01==13)] <- d$v20[which(d$e01==13)]
dn$seisakusenko[which(d$e01==14)] <- 10 - d$v21[which(d$e01==14)]
dn$seisakusenko[which(d$e01==15)] <- d$v22[which(d$e01==15)]
dn$seisakusenko[which(d$e01==16)] <- 10 - d$v23[which(d$e01==16)]
dn$seisakusenko[which(d$e01==17)] <- d$v24[which(d$e01==17)]
dn$seisakusenko[which(d$e01==18)] <- 10 - d$v25[which(d$e01==18)]
dn$seisakusenko[which(d$e01==19)] <- d$v26[which(d$e01==19)]
dn$seisakusenko[which(d$e01==20)] <- 10 - d$v27[which(d$e01==20)]
dn$seisakusenko[which(d$e01==21)] <- d$v28[which(d$e01==21)]
dn$seisakusenko[which(d$e01==22)] <- 10 - d$v29[which(d$e01==22)]
dn$seisakusenko[which(d$e01==23)] <- d$v30[which(d$e01==23)]
dn$seisakusenko[which(d$e01==24)] <- 10 - d$v31[which(d$e01==24)]
dn$seisakusenko[which(d$e01==25)] <- d$v32[which(d$e01==25)]
dn$seisakusenko[which(d$e01==26)] <- 10 - d$v33[which(d$e01==26)]
dn$seisakusenko[which(d$e01==27)] <- d$v34[which(d$e01==27)]
dn$seisakusenko[which(d$e01==28)] <- 10 - d$v35[which(d$e01==28)]
dn$seisakusenko[which(d$e01==29)] <- d$v36[which(d$e01==29)]
dn$seisakusenko[which(d$e01==30)] <- 10 - d$v37[which(d$e01==30)]
dn$seisakusenko[which(d$e01==31)] <- d$v38[which(d$e01==31)]
dn$seisakusenko[which(d$e01==32)] <- 10 - d$v39[which(d$e01==32)]
dn$seisakusenko[which(d$e01==33)] <- d$v40[which(d$e01==33)]
dn$seisakusenko[which(d$e01==34)] <- 10 - d$v41[which(d$e01==34)]
dn$seisakusenko[which(d$e01==35)] <- d$v42[which(d$e01==35)]
dn$seisakusenko[which(d$e01==36)] <- 10 - d$v43[which(d$e01==36)]
dn$seisakusenko[which(d$e01==37)] <- d$v44[which(d$e01==37)]
dn$seisakusenko[which(d$e01==38)] <- 10 - d$v45[which(d$e01==38)]
table(dn$seisakusenko, exclude=F) ## 分布の確認

## 公約記憶
table(d$v46_1, exclude=F) ## 奨学金返済の支援による教育の格差解消（若者）
table(d$v46_2, exclude=F) ## 学業や仕事を支援する生活費の補助制度（若者）
table(d$v46_3, exclude=F) ## 食事・運動支援プログラムの拡充（高齢者）
table(d$v46_4, exclude=F) ## 公共交通機関利用のための移動サポート制度（高齢者）
## 奇数実験グループは、候補者１＝若者政策、候補者２＝高齢者政策
## 偶数実験グループは、候補者１＝高齢者政策、候補者２＝若者政策
## 正しい記憶は１、間違った記憶は−１、どちらでもない／無回答は０
dn$kioku_1 <- 0
dn$kioku_1[which(d$e01%in%seq(1,37,by=2) & d$v46_1%in%c(1))] <- 1
dn$kioku_1[which(d$e01%in%seq(2,38,by=2) & d$v46_1%in%c(2))] <- 1
dn$kioku_1[which(d$e01%in%seq(1,37,by=2) & d$v46_1%in%c(2))] <- -1
dn$kioku_1[which(d$e01%in%seq(2,38,by=2) & d$v46_1%in%c(1))] <- -1
table(dn$kioku_1, exclude=F) # 分布を確認
dn$kioku_2 <- 0
dn$kioku_2[which(d$e01%in%seq(1,37,by=2) & d$v46_2%in%c(1))] <- 1
dn$kioku_2[which(d$e01%in%seq(2,38,by=2) & d$v46_2%in%c(2))] <- 1
dn$kioku_2[which(d$e01%in%seq(1,37,by=2) & d$v46_2%in%c(2))] <- -1
dn$kioku_2[which(d$e01%in%seq(2,38,by=2) & d$v46_2%in%c(1))] <- -1
table(dn$kioku_2, exclude=F) # 分布を確認
dn$kioku_3 <- 0
dn$kioku_3[which(d$e01%in%seq(1,37,by=2) & d$v46_3%in%c(2))] <- 1
dn$kioku_3[which(d$e01%in%seq(2,38,by=2) & d$v46_3%in%c(1))] <- 1
dn$kioku_3[which(d$e01%in%seq(1,37,by=2) & d$v46_3%in%c(1))] <- -1
dn$kioku_3[which(d$e01%in%seq(2,38,by=2) & d$v46_3%in%c(2))] <- -1
table(dn$kioku_3, exclude=F) # 分布を確認
dn$kioku_4 <- 0
dn$kioku_4[which(d$e01%in%seq(1,37,by=2) & d$v46_4%in%c(2))] <- 1
dn$kioku_4[which(d$e01%in%seq(2,38,by=2) & d$v46_4%in%c(1))] <- 1
dn$kioku_4[which(d$e01%in%seq(1,37,by=2) & d$v46_4%in%c(1))] <- -1
dn$kioku_4[which(d$e01%in%seq(2,38,by=2) & d$v46_4%in%c(2))] <- -1
table(dn$kioku_4, exclude=F) # 分布を確認
## 全ての点数を足し合わせる
dn$kiokuA <- dn$kioku_1 + dn$kioku_2 + dn$kioku_3 + dn$kioku_4
table(dn$kiokuA)
## 点数の偏りが激しいので全問正解か否かで分ける
dn$kioku <- 
  (dn$kioku_1>0 & dn$kioku_2>0 & dn$kioku_3>0 & dn$kioku_4>0)*1
table(dn$kioku)

## 被験者年齢
table(d$v50, exclude=F) #分布を確認（無回答が１人）
dn$nenrei <- as.numeric(d$v50) # 挿入
table(dn$nenrei, exclude=F) # 分布を確認

## 実験群
dn$jikken <- NA
dn$jikken[which(d$e01%in%c(1:2))] <- 1
dn$jikken[which(d$e01%in%c(3:20))] <- 2
dn$jikken[which(d$e01%in%c(21:38))] <- 3
dn$jikken <- factor(dn$jikken, 
                    labels=c("顔写真なし","外見政策一致","外見政策不一致"))
table(dn$jikken)

## 統制群（写真なし）
table(d$e01, exclude=F) # 1-2が写真なし、3-20が顔一致、21-38が顔不一致
dn$tosei <- NA
dn$tosei[which(d$e01%in%c(1:2))] <- 1
dn$tosei[which(d$e01%in%c(3:38))] <- 0
table(dn$tosei, exclude=F)

## 顔一致
table(d$e01, exclude=F) # 1-2が写真なし、3-20が顔一致、21-38が顔不一致
dn$icchi <- NA
dn$icchi[which(d$e01%in%c(3:20))] <- 1
dn$icchi[which(d$e01%in%c(1:2,21:38))] <- 0
table(dn$icchi, exclude=F)

## 顔不一致
table(d$e01, exclude=F) # 1-2が写真なし、3-20が顔一致、21-38が顔不一致
dn$fuicchi <- NA
dn$fuicchi[which(d$e01%in%c(21:38))] <- 1
dn$fuicchi[which(d$e01%in%c(1:20))] <- 0
table(dn$fuicchi, exclude=F)
table(dn$icchi,dn$fuicchi, exclude=F)

## 政治関心
table(d$v02, exclude=F) ## 分布を確認
dn$kanshin <- (4 - as.numeric(d$v02))/3 
## 逆転して挿入、０−１の範囲にリスケール
table(dn$kanshin, exclude=F) ## 分布を確認
# ## ２値化
# dn$kanshin <- ifelse(d$v02<=2,1,0) 
# ## 逆転して挿入、０−１の範囲にリスケール
# table(dn$kanshin, exclude=F) ## 分布を確認

## 政治知識
table(d$v04, exclude=F) ## 2が正解 
table(d$v05, exclude=F) ## 4が正解 
table(d$v07, exclude=F) ## 1が正解 
## 正答は１、誤答／無回答は０
dn$chishiki_1 <- ifelse(d$v04%in%c(2), 1, 0)
table(dn$chishiki_1)
dn$chishiki_2 <- ifelse(d$v05%in%c(4), 1, 0)
table(dn$chishiki_2)
dn$chishiki_3 <- ifelse(d$v07%in%c(1), 1, 0)
table(dn$chishiki_3)
dn$chishiki <- 
  (dn$chishiki_1 + dn$chishiki_2 + dn$chishiki_3)/3
table(dn$chishiki)

## 顔写真ごとの選好（スコアが高いと当該写真を好む）
dn$faceAsenko <- ifelse(d$e01%in%c(3:8),10-dn$seisakusenko,
                        ifelse(d$e01%in%c(21:26),dn$seisakusenko,999))
dn$faceBsenko <- ifelse(d$e01%in%c(9:14),10-dn$seisakusenko,
                        ifelse(d$e01%in%c(27:32),dn$seisakusenko,999))
dn$faceCsenko <- ifelse(d$e01%in%c(15:20),10-dn$seisakusenko,
                        ifelse(d$e01%in%c(33:38),dn$seisakusenko,999))
dn$faceDsenko <- ifelse(d$e01%in%c(3:4,9:10,15:16),dn$seisakusenko,
                        ifelse(d$e01%in%c(21:22,27:28,33:34),
                               10-dn$seisakusenko,999))
dn$faceEsenko <- ifelse(d$e01%in%c(5:6,11:12,17:18),dn$seisakusenko,
                        ifelse(d$e01%in%c(23:24,29:30,35:36),
                               10-dn$seisakusenko,999))
dn$faceFsenko <- ifelse(d$e01%in%c(7:8,13:14,19:20),dn$seisakusenko,
                        ifelse(d$e01%in%c(25:26,31:32,37:38),
                               10-dn$seisakusenko,999))


## Exclude respondents with missing values
dim(dn)
dn <- na.omit(dn)
dim(dn)

## 顔写真ごとの選好にNAを加える
dn$faceAsenko[which(dn$faceAsenko==999)] <- NA
dn$faceBsenko[which(dn$faceBsenko==999)] <- NA
dn$faceCsenko[which(dn$faceCsenko==999)] <- NA
dn$faceDsenko[which(dn$faceDsenko==999)] <- NA
dn$faceEsenko[which(dn$faceEsenko==999)] <- NA
dn$faceFsenko[which(dn$faceFsenko==999)] <- NA

## 実験群ごとの数の確認
table(dn$jikken)
tmp <- d$duration[match(dn$id,d$id)]
tmp <- ifelse(tmp>5000|tmp<10,NA,tmp)
tapply(tmp, dn$jikken, function(x) median(x, na.rm=T))

######################
## 記述統計の可視化 ##
######################

## 政策選好

### プロット用のデータ作成
plotdata <- data.frame(x = names(table(dn$seisakusenko)), 
                       y = as.numeric(prop.table(table(dn$seisakusenko))))
plotdata$x <- factor(plotdata$x, levels=unique(plotdata$x))
plotdata 
### プロット
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity") + 
  labs(x = "市区町村議会議員としてよりふさわしいと思う候補者\n(0=若者向け政策を掲げる候補者; 10=高齢者向け政策を掲げる候補者)", 
       y="回答割合") 
### 保存
ggsave("bunpu_seisakusenko.png", width=6, height=4)


## 公約記憶

### プロット用のデータ作成
plotdata <- data.frame(x = names(table(dn$kioku)), 
                       y = as.numeric(prop.table(table(dn$kioku))))
plotdata$x <- factor(plotdata$x, levels=unique(plotdata$x),
                     labels = c("誤答あり","すべて正答"))
plotdata 
### プロット
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity") + 
  labs(x = "提示された公約についての記憶", 
       y="回答割合") 
### 保存
ggsave("bunpu_kioku.png", width=6, height=4)


## 年齢

quantile(dn$nenrei, prob = c(0.25,0.75))

### ヒストグラムをプロット
ggplot(dn, aes(x=nenrei)) + 
  geom_histogram(bins = 15, alpha=0.8,color="white") +
  labs(x = "被験者年齢", 
       y="回答割合") 

### 保存
ggsave("bunpu_nenrei.png", width=6, height=4)


## *顔一致と顔不一致はランダムに割り当てられているので省略


## 政治関心

### プロット用のデータ作成
plotdata <- data.frame(x = names(table(dn$kanshin)),
                       y = as.numeric(prop.table(table(dn$kanshin))))
plotdata$x <- factor(plotdata$x, levels=unique(plotdata$x),
                     labels = c("関心がない\n（0）",
                                "あまり関心がない\n（1/3）",
                                "ある程度関心がある\n（2/3）",
                                "関心がある\n（1）"))
plotdata
### プロット
ggplot(plotdata, aes(x=x, y=y)) +
  geom_bar(stat = "identity") +
  labs(x = "政治関心",
       y="回答割合") 
### 保存
ggsave("bunpu_kanshin.png", width=6, height=4)


## 政治知識

### プロット用のデータ作成
plotdata <- data.frame(x = names(table(dn$chishiki)),
                       y = as.numeric(prop.table(table(dn$chishiki))))
plotdata$x <- factor(plotdata$x, levels=unique(plotdata$x),
                     labels = c("正答なし\n（0）",
                                "正答１問\n（1/3）",
                                "正答２問\n（2/3）",
                                "すべて正答\n（1）"))
plotdata
### プロット
ggplot(plotdata, aes(x=x, y=y)) +
  geom_bar(stat = "identity") +
  labs(x = "政治知識",
       y="回答割合") 
### 保存
ggsave("bunpu_chishiki.png", width=6, height=4)

## 顔写真ごとの選好

pdt1 <- as.data.frame(
  rbind(coeftest(lm_robust(faceAsenko ~ 1, data=dn)),
        coeftest(lm_robust(faceBsenko ~ 1, data=dn)),
        coeftest(lm_robust(faceCsenko ~ 1, data=dn)),
        coeftest(lm_robust(faceDsenko ~ 1, data=dn)),
        coeftest(lm_robust(faceEsenko ~ 1, data=dn)),
        coeftest(lm_robust(faceFsenko ~ 1, data=dn))))
colnames(pdt1) <- c("est","se","t","p")
pdt2 <- as.data.frame(
  rbind(coefci(lm_robust(faceAsenko ~ 1, data=dn)),
        coefci(lm_robust(faceBsenko ~ 1, data=dn)),
        coefci(lm_robust(faceCsenko ~ 1, data=dn)),
        coefci(lm_robust(faceDsenko ~ 1, data=dn)),
        coefci(lm_robust(faceEsenko ~ 1, data=dn)),
        coefci(lm_robust(faceFsenko ~ 1, data=dn))))
colnames(pdt2) <- c("lo95","up95")
pdt3 <- as.data.frame(
  rbind(coefci(lm_robust(faceAsenko ~ 1, data=dn), level=0.90),
        coefci(lm_robust(faceBsenko ~ 1, data=dn), level=0.90),
        coefci(lm_robust(faceCsenko ~ 1, data=dn), level=0.90),
        coefci(lm_robust(faceDsenko ~ 1, data=dn), level=0.90),
        coefci(lm_robust(faceEsenko ~ 1, data=dn), level=0.90),
        coefci(lm_robust(faceFsenko ~ 1, data=dn), level=0.90)))
colnames(pdt3) <- c("lo90","up90")
plotdata <- cbind(pdt1,pdt2,pdt3)
plotdata$faceID <- as.factor(c("A","B","C",
                               "D","E","F"))

ggplot(plotdata, aes(y=est,x=faceID)) + 
  geom_hline(aes(yintercept=5), linetype=2) + 
  geom_errorbar(aes(ymin=lo95,ymax=up95),width=0.2) + 
  geom_errorbar(aes(ymin=lo90,ymax=up90),linewidth=2,width=0) + 
  geom_point(size=3) + 
  scale_y_continuous(breaks=c(4.5,5,5.5)) +
  labs(x="顔写真ID", y="対象顔写真選好の平均（0-10）",
       caption = "注：定数項のみを含む回帰モデルから算出。エラーバーは90％・95％信頼区間（ロバスト標準誤差を使用）。") + 
  theme(axis.text.x = element_text(size=15))
  
ggsave("facecheck.pdf",width=6,height=4, device=cairo_pdf)

##########
## 分析 ##
##########

## H1 
m1 <- lm_robust(seisakusenko ~ nenrei, # + 
                # male + married + nokid + citysize, 
                data=dn) 
summary(m1)

## H2
m2 <- lm_robust(seisakusenko ~ nenrei*icchi + nenrei*fuicchi, # + 
                # male + married + nokid + citysize, 
                data=dn)
summary(m2)

## H3
m3 <- lm_robust(kioku ~ icchi + fuicchi, # + 
                # male + married + nokid + citysize, 
                data=dn)
summary(m3)
m3x <- lm_robust(kioku ~ tosei + fuicchi, # + 
                 # male + married + nokid + citysize, 
                 data=dn)

## H4
### 分析の実行（H2の条件付け by 関心)
m4_m2a <- lm_robust(seisakusenko ~
                      nenrei + icchi + fuicchi + kanshin + 
                      nenrei*icchi*kanshin + 
                      nenrei*fuicchi*kanshin,
                    # male + married + nokid + citysize, 
                    data=dn)
summary(m4_m2a)
m4_m2a_1 <- lm_robust(seisakusenko ~ 
                        nenrei + icchi + fuicchi + I(1-kanshin) + 
                        nenrei*icchi*I(1-kanshin) + 
                        nenrei*fuicchi*I(1-kanshin), # + 
                      # male + married + nokid + citysize, 
                      data=dn)
### 分析の実行（H2の条件付け by 知識)
m4_m2b <- lm_robust(seisakusenko ~ 
                      nenrei + icchi + fuicchi + chishiki + 
                      nenrei*icchi*chishiki + 
                      nenrei*fuicchi*chishiki, # + 
                    # male + married + nokid + citysize, 
                    data=dn)
summary(m4_m2b)
m4_m2b_1 <- lm_robust(seisakusenko ~ 
                        nenrei + icchi + fuicchi + I(1-chishiki) + 
                        nenrei*icchi*I(1-chishiki) + 
                        nenrei*fuicchi*I(1-chishiki), # + 
                      # male + married + nokid + citysize, 
                      data=dn)

### 分析の実行（H3の条件付け by 関心)
m4_m3a <- lm_robust(kioku ~ 
                      icchi + fuicchi + 
                      icchi*kanshin + 
                      fuicchi*kanshin, # + 
                    # male + married + nokid + citysize, 
                    data=dn)
m4_m3xa <- lm_robust(kioku ~ 
                       tosei + fuicchi + 
                       tosei*kanshin + 
                       fuicchi*kanshin, # + 
                     # male + married + nokid + citysize, 
                     data=dn)
m4_m3a_1 <- lm_robust(kioku ~ 
                        icchi + fuicchi + 
                        icchi*I(1-kanshin) + 
                        fuicchi*I(1-kanshin), # + 
                      # male + married + nokid + citysize, 
                      data=dn)
m4_m3xa_1 <- lm_robust(kioku ~ 
                         tosei + fuicchi + 
                         tosei*I(1-kanshin) + 
                         fuicchi*I(1-kanshin), # + 
                       # male + married + nokid + citysize, 
                       data=dn)

### 分析の実行（H3の条件付け by 知識)
summary(m4_m3a)
m4_m3b <- lm_robust(kioku ~ 
                      icchi + fuicchi + 
                      icchi*chishiki +  
                      fuicchi*chishiki, # + 
                    # male + married + nokid + citysize, 
                    data=dn)
m4_m3xb <- lm_robust(kioku ~ 
                       tosei + fuicchi + 
                       tosei*chishiki +  
                       fuicchi*chishiki, # + 
                     # male + married + nokid + citysize, 
                     data=dn)
m4_m3b_L <- lm_robust(kioku ~ 
                      icchi + fuicchi + 
                      icchi*I(chishiki-0) +  
                      fuicchi*I(chishiki-0), # + 
                    # male + married + nokid + citysize, 
                    data=dn)
m4_m3xb_L <- lm_robust(kioku ~ 
                       tosei + fuicchi + 
                       tosei*I(chishiki-0) +  
                       fuicchi*I(chishiki-0), # + 
                     # male + married + nokid + citysize, 
                     data=dn)
m4_m3b_1 <- lm_robust(kioku ~ 
                        icchi + fuicchi + 
                        icchi*I(1-chishiki) +  
                        fuicchi*I(1-chishiki), # + 
                      # male + married + nokid + citysize, 
                      data=dn)
m4_m3xb_1 <- lm_robust(kioku ~ 
                         tosei + fuicchi + 
                         tosei*I(1-chishiki) +  
                         fuicchi*I(1-chishiki), # + 
                       # male + married + nokid + citysize, 
                       data=dn)

table(dn$kioku, dn$kanshin)

######################
## 分析結果の可視化 ##
######################

## 回帰表の作成 ##

### まずはジェネリックな表を見る
screenreg(list(m1,m2,m3),
          include.ci=FALSE, digits=4,
          stars = c(0.001,0.01,0.05,0.1), symbol="+",
          custom.header = list("DV:seisakusenko" = 1:2,
                               "DV:kioku" = 3),
          custom.model.names = c("H1","H2","H3")) 
screenreg(list(m4_m2a,m4_m2b,m4_m3a,m4_m3b), 
          include.ci=FALSE, digits=4,
          stars = c(0.001,0.01,0.05,0.1), symbol="+",
          custom.header = list("DV:seisakusenko" = 1:2,
                               "DV:kioku" = 3:4),
          custom.model.names = c("H4 (H2)","H4 (H2)",
                                 "H4 (H3)","H4 (H3)")) 

### 変数名ラベルをつける
vnmap <- list("(Intercept)" = " （定数項）",
              "nenrei" = " 被験者年齢",
              "icchi" = " 顔写真一致",
              "fuicchi" = " 顔写真不一致",
              "nenrei:icchi" = " 年齢 x 顔一致",
              "nenrei:fuicchi" = " 年齢 x 顔不一致",
              "kanshin" = "政治関心",
              "icchi:kanshin" = "顔一致 x 関心",
              "fuicchi:kanshin" = "顔不一致 x 関心",
              "nenrei:icchi:kanshin" = " 年齢 x 一致 x 関心",
              "nenrei:fuicchi:kanshin" = " 年齢 x 不一致 x 関心",
              "nenrei:kanshin" = "年齢 x 関心",
              "chishiki" = "政治知識",
              "icchi:chishiki" = "顔一致 x 知識",
              "fuicchi:chishiki" = "顔不一致 x 知識",
              "nenrei:icchi:chishiki" = " 年齢 x 一致 x 知識",
              "nenrei:fuicchi:chishiki" = " 年齢 x 不一致 x 知識",
              "nenrei:chishiki" = "年齢 x 知識")

## R コンソールだとズレる
screenreg(list(m1,m2,m3),
          include.ci=FALSE, digits=3,
          stars = c(0.001,0.01,0.05,0.1), symbol="+",
          custom.header = list("DV:政策選好" = 1:2,
                               "DV:公約記憶" = 3),
          custom.model.names = c("H1","H2","H3"),
          custom.coef.map = vnmap) 
screenreg(list(m4_m2a,m4_m2b,m4_m3a,m4_m3b), 
          include.ci=FALSE, digits=3,
          stars = c(0.001,0.01,0.05,0.1), symbol="+",
          custom.header = list("DV:政策選好" = 1:2,
                               "DV:公約記憶" = 3:4),
          custom.model.names = c("H4 (H2)","H4 (H2)",
                                 "H4 (H3)","H4 (H3)"),
          custom.coef.map = vnmap) 

## LaTeXに出力
texreg(list(m1,m2,m3),
        include.ci=FALSE, digits=4,
        stars = c(0.001,0.01,0.05,0.1), symbol="+",
        custom.header = list("DV:政策選好" = 1:2,
                             "DV:公約記憶" = 3),
        custom.model.names = c("H1","H2","H3"),
        custom.coef.map = vnmap, single.row = TRUE,
        caption = "仮説1〜3の回帰表", caption.above = TRUE,
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE,
       fontsize = "scriptsize", label = "tab_m1to3", float.pos = "ht!",
       file = "m1to3tab.tex") 
texreg(list(m4_m2a,m4_m2b,m4_m3a,m4_m3b), 
        include.ci=FALSE, digits=4,
        stars = c(0.001,0.01,0.05,0.1), symbol="+",
        custom.header = list("DV:政策選好" = 1:2,
                             "DV:公約記憶" = 3:4),
        custom.model.names = c("H4 (H2)","H4 (H2)",
                               "H4 (H3)","H4 (H3)"),
        custom.coef.map = vnmap, single.row = TRUE,
        caption = "仮説4の回帰表", caption.above = TRUE,
        booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE,
        fontsize = "scriptsize", label = "tab_m4", float.pos = "ht!",
        file = "m4tab.tex") 

## 予測値プロット ##

## H1

prout <- yosokuchi(dn, m1, "nenrei", c(18:78))

prout$pr[prout$x==52] - prout$pr[prout$x==36]

sp <- 3.5
ggplot(prout, aes(x=x, y=pr-sp)) + 
  geom_histogram(data=dn,aes(x=nenrei,y=(..count..)*0.0025), 
                 bins = 20, color = "gray30", fill = "white") +
  geom_ribbon(aes(ymin=lo95-sp, ymax=up95-sp), alpha=0.3) + 
  geom_ribbon(aes(ymin=lo90-sp, ymax=up90-sp), alpha=0.4) + 
  geom_line() + 
  coord_cartesian(ylim=c(0,6-sp)) + 
  scale_y_continuous(breaks = seq(0,6-sp,0.5),
                     labels = seq(sp,6,0.5)) +
  annotate("text", x=50-sp, y=5.75-sp, 
           label = paste0("年齢の係数=",round(coeftest(m1)[2,1],4)," (",genptxt(coeftest(m1)[2,4]),")"),
           family= theme_get()$text[["family"]]) + 
  labs(x="年齢", 
       y=paste0("政策選好の予測値平均\n（0=若者 - 10=高齢者）"),
       caption= "注：H1モデルに基づいて算出。シェード部分は90％・95％信頼区間\n（ロバスト標準誤差を使用）。ヒストグラムは年齢の頻度分布。") 

ggsave("predictionplot_h1.pdf",width=3.5, height=3.5, device=cairo_pdf)

## H2

prout <- yosokuchi(dn, m2, "nenrei", c(18:78), NULL,
                   setm = c("icchi","fuicchi"), 
                   setmvals = list("顔写真なし"=c(0,0),
                                   "外見政策一致"=c(1,0),
                                   "外見政策不一致"=c(0,1)))

sigout <- rbind(genkaikoka(m2, "nenrei", "icchi", c(0,1)),
                genkaikoka(m2, "nenrei", "fuicchi", c(0,1))[-1,])
sigout$efest <- c(NA, coeftest(m2)[c(5:6),1])
sigout$efpval <- c(NA, coeftest(m2)[c(5:6),4])
sigout$labelledm <- factor(levels(prout$labelledm), 
                           levels=levels(prout$labelledm))
sigout$x <- 78
sigout$y <- 5.85
sigout$text <- paste0("年齢の条件付き係数=",round(sigout$est,4)," (",genptxt(sigout$pval),")\n",ifelse(is.na(sigout$efest),"",paste0("実験刺激効果=",round(sigout$efest,4)," (",genptxt(sigout$efpval),")")))


setxname <- "被験者年齢"
setyname <- "政策選好の予測値平均（0=若者 - 10=高齢者）"
setmname <- "顔写真の提示" #条件付け変数ラベル
setcaption <- "注：H2モデルに基づいて算出。シェード部分は90％・95％信頼区間（ロバスト標準誤差を使用、n.s.はp>.010）。"
sp <- 3.5; dn$labelledm <- dn$jikken
ggplot(na.omit(prout), aes(x=x, y=pr-sp)) + 
  geom_histogram(data=dn,aes(x=nenrei,y=(..count..)*0.0050), 
                 bins = 20, color = "gray30", fill = "white") +
  geom_ribbon(aes(ymin=lo95-sp, ymax=up95-sp,
                  fill = labelledm), alpha=0.3) + 
  geom_ribbon(aes(ymin=lo90-sp, ymax=up90-sp,
                  fill = labelledm), alpha=0.4) + 
  geom_line(aes(color=labelledm,linetype=labelledm)) + 
  geom_text(data=sigout, aes(x=x-sp,y=y-sp,label=text),size=2.7, hjust=1,
            family= theme_get()$text[["family"]]) + 
  coord_cartesian(ylim=c(0,6-sp)) +
  scale_y_continuous(breaks = seq(0,6-sp,0.5),
                     labels = seq(sp,6,0.5)) +
  facet_grid(.~labelledm) + 
  scale_shape_discrete(name = setmname) + 
  scale_linetype_discrete(name = setmname) + 
  scale_color_brewer(name = setmname, type = "qual", palette = 2) + 
  scale_fill_brewer(name = setmname, type = "qual", palette = 2) + 
  labs(x=setxname, 
       y=paste0(setyname),
       subtitle = setmname,
       caption=setcaption) + 
  theme(legend.position = "none",
        plot.subtitle = element_text(hjust=0.5))

ggsave("predictionplot_h2.pdf",width=7.5,height=3.5, device=cairo_pdf)

## H3
prout <- yosokuchi(dn, m3, "null", NA, NULL,
                    setm = c("icchi","fuicchi"), 
                    setmvals = list("顔写真なし"=c(0,0),
                                    "外見政策一致"=c(1,0),
                                    "外見政策不一致"=c(0,1)))

setyname <- "公約記憶の予測値平均（0=記憶なし 1=記憶あり）"
setxname <- "顔写真の提示" #条件付け変数ラベル
setcaption <- "注：H3モデルに基づいて算出。縦線は90％・95％信頼区間、\n上の数字は実験刺激効果のp値（ロバスト標準誤差を使用、n.s.はp>.10）。"
p <- ggplot(prout, aes(x=labelledm, y=pr)) + 
  geom_col(alpha=0.6) +
  geom_errorbar(aes(ymin=lo95, ymax=up95), width=0.05) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90), width=0, linewidth=1.5) + 
  # geom_point() + 
  coord_cartesian(ylim=c(0,1.05)) +
  labs(x=setxname, 
       y=paste0(setyname),
       caption=setcaption) 
## Showing p-values
pvalout <- as.data.frame(rbind(coeftest(m3)[2:3,],
                               coeftest(m3x)[3,]))
colnames(pvalout) <- c("est","se","t","pval")
setw <- 0.05
pvalout$control <- c(1+setw,1+setw,2+setw)
pvalout$treated <- c(2-setw,3-setw,3-setw)
pvalout$ptxt <- genptxt(pvalout$pval)
pvalout$y <- c(max(prout$up90) + 0.05,
               max(prout$up90) + 0.09,
               max(prout$up90) + 0.05)
for(i in 1:nrow(pvalout)) {
  p <- p + 
    showSignificance(c(as.numeric(pvalout$control[i]),
                       as.numeric(pvalout$treated[i])),
                     pvalout$y[i],-0.02,pvalout$ptxt[i],
                     # panel=list(dv=pvalout$dv[i]),
                     textParams = list(size=3, vjust=-0.3,
                                       color=ifelse(pvalout$ptxt[i]=="n.s.",
                                                    "gray50","black")),
                     segmentParams = list(color=ifelse(pvalout$ptxt[i]=="n.s.",
                                                       "gray50","black")))
}
p

ggsave("predictionplot_h3.pdf",width=6,height=4,device=cairo_pdf)


## H4（H2の政治関心条件付け）

prout0 <- yosokuchi(dn, m4_m2a, "nenrei", c(18:78), NULL,
                    setm = c("icchi","fuicchi","kanshin"), 
                    setmvals = list("顔写真なし"=c(0,0,0),
                                    "外見政策一致"=c(1,0,0),
                                    "外見政策不一致"=c(0,1,0)),
                    datalab = "政治関心低（0）")
prout1 <- yosokuchi(dn, m4_m2a, "nenrei", c(18:78), NULL,
                    setm = c("icchi","fuicchi","kanshin"), 
                    setmvals = list("顔写真なし"=c(0,0,1),
                                    "外見政策一致"=c(1,0,1),
                                    "外見政策不一致"=c(0,1,1)),
                    datalab = "政治関心高（1）")
prout <- rbind(prout0,prout1)
prout$datalab <- factor(prout$datalab, levels=unique(prout$datalab))

sigout <- rbind(genkaikoka(m4_m2a, "nenrei", "icchi", c(0,1)),
                genkaikoka(m4_m2a, "nenrei", "fuicchi", c(0,1))[-1,],
                genkaikoka(m4_m2a_1, "nenrei", "icchi", c(0,1)),
                genkaikoka(m4_m2a_1, "nenrei", "fuicchi", c(0,1))[-1,])
sigout$efest <- c(NA, coeftest(m4_m2a)[c(6,9),1], NA, coeftest(m4_m2a_1)[c(6,9),1])
sigout$efpval <- c(NA, coeftest(m4_m2a)[c(6,9),4],NA, coeftest(m4_m2a_1)[c(6,9),4])
sigout$labelledm <- factor(levels(prout$labelledm), 
                           levels=levels(prout$labelledm))
sigout$datalab <- factor(rep(levels(prout$datalab),each=3), 
                           levels=levels(prout$datalab))
sigout$x <- 78
sigout$y <- 2
sigout$text <- paste0("年齢の条件付き係数=",round(sigout$est,4)," (",genptxt(sigout$pval),")\n",ifelse(is.na(sigout$efest),"",paste0("実験刺激効果=",round(sigout$efest,4)," (",genptxt(sigout$efpval),")")))

setxname <- "被験者年齢" #独立変数ラベル
setmname <- "顔写真の提示" #条件付け変数ラベル
setyname <- "政策選好の予測値平均（0=若者 - 10=高齢者）" #従属変数ラベル
setcaption <- "注：H4(H2)モデルに基づいて算出。シェード部分は90％・95％信頼区間（ロバスト標準誤差を使用）。"
ggplot(na.omit(prout), aes(x=x, y=pr)) + 
  geom_ribbon(aes(ymin=lo95, ymax=up95,
                  fill = labelledm), alpha=0.3) + 
  geom_ribbon(aes(ymin=lo90, ymax=up90,
                  fill = labelledm), alpha=0.4) + 
  geom_line(aes(color=labelledm,linetype=labelledm)) + 
  geom_text(data=sigout, aes(x=x,y=y,label=text), size=3, hjust=1,
            family= theme_get()$text[["family"]]) + 
  coord_cartesian(ylim=c(1,9)) +
  facet_grid(datalab~labelledm) + 
  scale_shape_discrete(name = setmname) + 
  scale_linetype_discrete(name = setmname) + 
  scale_color_brewer(name = setmname, type = "qual", palette = 2) + 
  scale_fill_brewer(name = setmname, type = "qual", palette = 2) + 
  labs(x=setxname, 
       y=paste0(setyname),
       subtitle = setmname,
       caption=setcaption) + 
  theme(legend.position = "none",
        plot.subtitle = element_text(hjust=0.5))

ggsave("predictionplot_h4_h2_kanshin.pdf",width=7.5,height=5,
       device = cairo_pdf)

## H4（H2の政治知識条件付け）

prout0 <- yosokuchi(dn, m4_m2b, "nenrei", c(18:78), NULL,
                    setm = c("icchi","fuicchi","chishiki"), 
                    setmvals = list("顔写真なし"=c(0,0,0),
                                    "外見政策一致"=c(1,0,0),
                                    "外見政策不一致"=c(0,1,0)),
                    datalab = "政治知識低（0）")
prout1 <- yosokuchi(dn, m4_m2b, "nenrei", c(18:78), NULL,
                    setm = c("icchi","fuicchi","chishiki"), 
                    setmvals = list("顔写真なし"=c(0,0,1),
                                    "外見政策一致"=c(1,0,1),
                                    "外見政策不一致"=c(0,1,1)),
                    datalab = "政治知識高（1）")
prout <- rbind(prout0,prout1)
prout$datalab <- factor(prout$datalab, levels=unique(prout$datalab))

sigout <- rbind(genkaikoka(m4_m2b, "nenrei", "icchi", c(0,1)),
                genkaikoka(m4_m2b, "nenrei", "fuicchi", c(0,1))[-1,],
                genkaikoka(m4_m2b_1, "nenrei", "icchi", c(0,1)),
                genkaikoka(m4_m2b_1, "nenrei", "fuicchi", c(0,1))[-1,])
sigout$efest <- c(NA, coeftest(m4_m2b)[c(6,9),1], NA, coeftest(m4_m2b_1)[c(6,9),1])
sigout$efpval <- c(NA, coeftest(m4_m2b)[c(6,9),4],NA, coeftest(m4_m2b_1)[c(6,9),4])
sigout$labelledm <- factor(levels(prout$labelledm), 
                           levels=levels(prout$labelledm))
sigout$datalab <- factor(rep(levels(prout$datalab),each=3), 
                         levels=levels(prout$datalab))
sigout$x <- 78
sigout$y <- 2
sigout$text <- paste0("年齢の条件付き係数=",round(sigout$est,4)," (",genptxt(sigout$pval),")\n",ifelse(is.na(sigout$efest),"",paste0("実験刺激効果=",round(sigout$efest,4)," (",genptxt(sigout$efpval),")")))

setxname <- "被験者年齢" #独立変数ラベル
setmname <- "顔写真の提示" #条件付け変数ラベル
setyname <- "政策選好の予測値平均（0=若者 - 10=高齢者）" #従属変数ラベル
setcaption <- "注：H4(H2)モデルに基づいて算出。シェード部分は90％・95％信頼区間（ロバスト標準誤差を使用）。"
ggplot(na.omit(prout), aes(x=x, y=pr)) + 
  geom_ribbon(aes(ymin=lo95, ymax=up95,
                  fill = labelledm), alpha=0.3) + 
  geom_ribbon(aes(ymin=lo90, ymax=up90,
                  fill = labelledm), alpha=0.3) + 
  geom_line(aes(color=labelledm,linetype=labelledm)) + 
  geom_text(data=sigout, aes(x=x,y=y,label=text),size=3,hjust=1,
            family= theme_get()$text[["family"]]) + 
  coord_cartesian(ylim=c(1,9)) +
  facet_grid(datalab~labelledm) + 
  scale_shape_discrete(name = setmname) + 
  scale_linetype_discrete(name = setmname) + 
  scale_color_brewer(name = setmname, type = "qual", palette = 2) + 
  scale_fill_brewer(name = setmname, type = "qual", palette = 2) + 
  labs(x=setxname, 
       y=paste0(setyname),
       subtitle = setmname,
       caption=setcaption) + 
  theme(legend.position = "none",
        plot.subtitle = element_text(hjust=0.5))

ggsave("predictionplot_h4_h2_chishiki.pdf",width=7.5,height=5,
       device = cairo_pdf)


## H4（H3の政治関心条件付け）

prout0 <- yosokuchi(dn, m4_m3a, "null", NA, NULL,
                    setm = c("icchi","fuicchi","kanshin"), 
                    setmvals = list("顔写真なし"=c(0,0,0),
                                    "外見政策一致"=c(1,0,0),
                                    "外見政策不一致"=c(0,1,0)),
                    datalab = "政治関心低（0）")
prout1 <- yosokuchi(dn, m4_m3a, "null", NA, NULL,
                    setm = c("icchi","fuicchi","kanshin"), 
                    setmvals = list("顔写真なし"=c(0,0,1),
                                    "外見政策一致"=c(1,0,1),
                                    "外見政策不一致"=c(0,1,1)),
                    datalab = "政治関心高（1）")
prout <- rbind(prout0,prout1)
prout$datalab <- factor(prout$datalab, levels=unique(prout$datalab))

setxname <- "顔写真の提示"
setyname <- "公約記憶の予測値平均\n（0=記憶なし 1=記憶あり）"
setcaption <- "注：H4(H3)モデルに基づいて算出。縦線は90％・95％信頼区間、上の数字は実験刺激効果のp値（ロバスト標準誤差、n.s.はp>.10）。"
p <- ggplot(prout, aes(x=labelledm, y=pr)) + 
  geom_col(aes(fill=labelledm), alpha=0.4) +
  geom_errorbar(aes(ymin=lo95, ymax=up95,
                    color = labelledm), width=0.2) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90,
                    color = labelledm), width=0, linewidth=1.5) + 
  # geom_point(aes(color=labelledm,shape=labelledm)) + 
  coord_cartesian(ylim=c(0,1.09)) +
  facet_grid(.~datalab) + 
  scale_shape_discrete(name = setmname) + 
  scale_linetype_discrete(name = setmname) + 
  scale_color_brewer(name = setmname, type = "qual", palette = 2) + 
  scale_fill_brewer(name = setmname, type = "qual", palette = 2) + 
  labs(x=setxname, 
       y=paste0(setyname),
       caption=setcaption) + 
  theme(legend.position = "none",
        plot.subtitle = element_text(hjust=0.5),
        panel.grid = element_blank(),
        panel.grid.major.y = 
          element_line(color="gray80", linewidth = 0.3))

## Showing p-values
pvalout <- as.data.frame(rbind(coeftest(m4_m3a)[2:3,],
                               coeftest(m4_m3xa)[3,],
                               coeftest(m4_m3a_1)[2:3,],
                               coeftest(m4_m3xa_1)[3,]))
colnames(pvalout) <- c("est","se","t","pval")
setw <- 0.05
pvalout$control <- c(1+setw,1+setw,2+setw)
pvalout$treated <- c(2-setw,3-setw,3-setw)
pvalout$datalab <- factor(rep(levels(prout$datalab),each=3),
                          levels=levels(prout$datalab))
pvalout$ptxt <- genptxt(pvalout$pval)
pvalout$y <- c(max(prout$up90) + 0.05,
               max(prout$up90) + 0.12,
               max(prout$up90) + 0.05)
for(i in 1:nrow(pvalout)) {
  p <- p + 
    showSignificance(c(as.numeric(pvalout$control[i]),
                       as.numeric(pvalout$treated[i])),
                     pvalout$y[i],-0.02,pvalout$ptxt[i],
                     panel=list(datalab=pvalout$datalab[i]),
                     textParams = list(size=3, vjust=-0.3,
                                       color=ifelse(pvalout$ptxt[i]=="n.s.",
                                                    "gray50","black")),
                     segmentParams = list(color=ifelse(pvalout$ptxt[i]=="n.s.",
                                                       "gray50","black")))
}
p

ggsave("predictionplot_h4_h3_kanshin.pdf",
       width=7.5,height=3.5, device=cairo_pdf)


## H4（H3の政治知識条件付け）

prout0 <- yosokuchi(dn, m4_m3b, "null", NA, NULL,
                    setm = c("icchi","fuicchi","chishiki"), 
                    setmvals = list("顔写真なし"=c(0,0,0),
                                    "外見政策一致"=c(1,0,0),
                                    "外見政策不一致"=c(0,1,0)),
                    datalab = "政治知識低（0）")
prout1 <- yosokuchi(dn, m4_m3b, "null", NA, NULL,
                    setm = c("icchi","fuicchi","chishiki"), 
                    setmvals = list("顔写真なし"=c(0,0,1),
                                    "外見政策一致"=c(1,0,1),
                                    "外見政策不一致"=c(0,1,1)),
                    datalab = "政治知識高（1）")
prout <- rbind(prout0,prout1)
prout$datalab <- factor(prout$datalab, levels=unique(prout$datalab))

setxname <- "顔写真の提示"
setyname <- "公約記憶の予測値平均\n（0=記憶なし 1=記憶あり）"
setcaption <- "注：H4(H3)モデルに基づいて算出。縦線は90％・95％信頼区間、上の数字は実験刺激効果のp値（ロバスト標準誤差、n.s.はp>.10）。"
p <- ggplot(prout, aes(x=labelledm, y=pr)) + 
  geom_col(aes(fill=labelledm), alpha=0.4) +
  geom_errorbar(aes(ymin=lo95, ymax=up95,
                    color = labelledm), width=0.2) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90,
                    color = labelledm), width=0, linewidth=1.5) + 
  # geom_point(aes(color=labelledm,shape=labelledm)) + 
  coord_cartesian(ylim=c(0,1.09)) +
  facet_grid(.~datalab) + 
  scale_shape_discrete(name = setmname) + 
  scale_linetype_discrete(name = setmname) + 
  scale_color_brewer(name = setmname, type = "qual", palette = 2) + 
  scale_fill_brewer(name = setmname, type = "qual", palette = 2) + 
  labs(x=setxname, 
       y=paste0(setyname),
       caption=setcaption) + 
  theme(legend.position = "none",
        plot.subtitle = element_text(hjust=0.5),
        panel.grid = element_blank(),
        panel.grid.major.y = 
          element_line(color="gray80", linewidth = 0.3))

## Showing p-values
pvalout <- as.data.frame(rbind(coeftest(m4_m3b_L)[2:3,],
                               coeftest(m4_m3xb_L)[3,],
                               coeftest(m4_m3b_1)[2:3,],
                               coeftest(m4_m3xb_1)[3,]))
colnames(pvalout) <- c("est","se","t","pval")
setw <- 0.05
pvalout$control <- c(1+setw,1+setw,2+setw)
pvalout$treated <- c(2-setw,3-setw,3-setw)
pvalout$datalab <- factor(rep(levels(prout$datalab),each=3),
                          levels=levels(prout$datalab))
pvalout$ptxt <- genptxt(pvalout$pval)
pvalout$y <- c(max(prout$up90) + 0.05,
               max(prout$up90) + 0.12,
               max(prout$up90) + 0.05)
for(i in 1:nrow(pvalout)) {
  p <- p + 
    showSignificance(c(as.numeric(pvalout$control[i]),
                       as.numeric(pvalout$treated[i])),
                     pvalout$y[i],-0.02,pvalout$ptxt[i],
                     panel=list(datalab=pvalout$datalab[i]),
                     textParams = list(size=3, vjust=-0.3,
                                       color=ifelse(pvalout$ptxt[i]=="n.s.",
                                                    "gray50","black")),
                     segmentParams = list(color=ifelse(pvalout$ptxt[i]=="n.s.",
                                                       "gray50","black")))
}
p

ggsave("predictionplot_h4_h3_chishiki.pdf",
       width=7.5,height=3.5, device=cairo_pdf)

