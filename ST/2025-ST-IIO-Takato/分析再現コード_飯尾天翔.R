## 地方議会選挙における再立候補行動の決定要因

## 更新履歴：
## 2026-01-23 飯尾天翔（提出版）
## 2026-02-16 加藤言人（最終調整）

## ワーキングディレクトリの設定
dirhere <- dirname(rstudioapi::getActiveDocumentContext()$path) 
setwd(dirhere)
getwd() # 

## 図表出力用のフォルダ（out）を作成
if (!"./out" %in% list.dirs()) dir.create("./out")

## データの読み込み
library(readxl)
dt <- read_xlsx("ゼミ_データ.xlsx", sheet = "全体",
                na = c("","ー"))
dt <- subset(dt, !is.na(ID)) 
nrow(dt)

## 必要なパッケージ
library(estimatr) 
library(marginaleffects) 
library(ggplot2) 
library(texreg) 
library(htmltools) 

## データの修正
names(dt)
# colnames(dt) <-
#   c("ID","City","Population","Election","Year","Name",
#     "Number_of_votes","Result","Sex","Gender_basis",
#     "Age","Candidate_category","Occupation","Political_party",
#     "Candidate_in_the_previous_election",
#     "Number_of_votes_in_the_previous_election",
#     "ID_in_the_previous_election","Remarks")
table(dt$Election)
dt$Election[which(dt$Election=="補欠議員選挙")] <- "補欠選挙"
dt$Election[which(dt$Election=="議員補欠選挙")] <- "補欠選挙"
table(dt$Election)
dt$Election[which(dt$City=="男鹿市"&dt$Year==2013)] <- "補欠選挙"
dt$Next_election <- NA 
dt$Next_election_ID <- NA 
Yearalt <- dt$Year
Yearalt[which(dt$City=="由利本荘市"&dt$Election=="補欠選挙"&dt$Year==2009)] <- 2008
Yearalt[which(dt$City=="由利本荘市"&dt$Election=="補欠選挙"&dt$Year==2013)] <- 2012
Yearalt[which(dt$City=="由利本荘市"&dt$Election=="補欠選挙"&dt$Year==2021)] <- 2020
Yearalt[which(dt$City=="大仙市"&dt$Election=="補欠選挙"&dt$Year==2017)] <- 2016
Yearalt[which(dt$City=="湯沢市"&dt$Election=="補欠選挙"&dt$Year==2009)] <- 2008
Yearalt[which(dt$City=="湯沢市"&dt$Election=="補欠選挙"&dt$Year==2017)] <- 2016
Yearalt[which(dt$City=="湯沢市"&dt$Election=="補欠選挙"&dt$Year==2021)] <- 2020
Yearalt[which(dt$City=="湯沢市"&dt$Election=="補欠選挙"&dt$Year==2025)] <- 2024

for (x in unique(dt$City)) {
  print(x)
  getYear <- unique(subset(Yearalt, dt$City==x)) 
  nontarget <- 1
  if (unique(dt$Election[which(dt$City==x&
                               Yearalt==tail(getYear,1))])=="補欠選挙") {
    nontarget <- 2
  }
  for (i in 1:(length(getYear)-nontarget)) {
    nowY <- getYear[i]
    nextY <- numeric(); stype <- ""; j <- 1
    while(stype!="議員選挙") {
      nextY <- c(nextY,getYear[i+j])
      j <- j + 1
      stype <- dt$Election[which(dt$City==x&Yearalt==tail(nextY,1))][1]
    }
    for (k in subset(dt$Name, dt$City==x & Yearalt==nowY)) {
      loc1 <- which(dt$Name==k&dt$City==x&Yearalt==nowY)
      if (length(loc1)!=1) {
        stop(paste0(x,"の",nowY,
                    "年選挙に同じ名前の人が複数います！"))
      }        
      for (nextYx in nextY) {
        loc2 <- which(dt$Name==k&dt$City==x&Yearalt==nextYx)
        if (length(loc2)==1) {
          dt$Next_election[loc1] <- 1
          dt$Next_election_ID[loc1] <- dt$ID[loc2]
        } else if (length(loc2)==0) {
          dt$Next_election[loc1] <- 0
        } else {
          stop(paste0(x,"の",nextYx,
                      "年選挙に同じ名前の人が複数います！"))
        }
      }
    }
  }
}
table(dt$Year, dt$Next_election, useNA="always")

## 仮説４：再立候補した候補者かどうかの変数を作成 
dt$WinLast <- 0　
dt$LoseLast <- 0 
dt$IDLast <- NA
for(i in which(!is.na(dt$Next_election_ID))) {
  if (dt$Result[i]=="当") {
    if (dt$LoseLast[which(dt$ID==dt$Next_election_ID[i])]%in%0) {
      dt$WinLast[which(dt$ID==dt$Next_election_ID[i])] <- 1
      dt$IDLast[which(dt$ID==dt$Next_election_ID[i])] <- dt$ID[i]
    } else if (dt$LoseLast[which(dt$ID==dt$Next_election_ID[i])]%in%1) {
      dt$WinLast[which(dt$ID==dt$Next_election_ID[i])] <- 1
      dt$LoseLast[which(dt$ID==dt$Next_election_ID[i])] <- 0 
    }
  } else if (dt$Result[i]=="落") {
    dt$LoseLast[which(dt$ID==dt$Next_election_ID[i])] <- 1
    dt$IDLast[which(dt$ID==dt$Next_election_ID[i])] <- dt$ID[i]
  } 
}
(firstElection <- tapply(subset(dt,Election=="議員選挙")$Year,
                         subset(dt,Election=="議員選挙")$City, min))
dt$WinLast[paste(dt$Year,dt$City)%in%
             paste(firstElection,names(firstElection))] <- NA
dt$LoseLast[paste(dt$Year,dt$City)%in%
              paste(firstElection,names(firstElection))] <- NA
dt$WinLast[which(dt$Election=="補欠選挙")] <- NA
dt$LoseLast[which(dt$Election=="補欠選挙")] <- NA
table(dt$LoseLast,dt$WinLast)
table(dt$Candidate_category,dt$WinLast, useNA="always")
as.data.frame(dt[which(dt$Name%in%dt$Name[which(dt$Candidate_category=="元職"&
                                                  dt$WinLast==1)]),
                 c("ID","City","Year","Election","Name",
                   "WinLast","LoseLast","Candidate_category")])
dt$Candidate_category[which(dt$WinLast==1)] <- "現職"
table(dt$Candidate_category,dt$LoseLast, useNA="always")
as.data.frame(dt[which(dt$Name%in%dt$Name[which(dt$Candidate_category=="現職"&
                                                  dt$LoseLast==1)]),
                 c("ID","City","Year","Election","Name",
                   "WinLast","LoseLast","Candidate_category")])

dt$Candidate_category[which(dt$ID=="05203_129")] <- "新人"
dt$Result[which(dt$ID=="05210_31")] <- "当"
dt$LoseLast[which(dt$ID=="05210_83")] <- 0
dt$WinLast[which(dt$ID=="05210_83")] <- 1
dt$Result[which(dt$ID=="05204_31")] <- "当"
dt$LoseLast[which(dt$ID=="05204_74")] <- 0
dt$WinLast[which(dt$ID=="05204_74")] <- 1
dt$Result[which(dt$ID=="05204_122")] <- "当"
dt$LoseLast[which(dt$ID=="05204_141")] <- 0
dt$WinLast[which(dt$ID=="05204_141")] <- 1

table(dt$Candidate_category,dt$WinLast, useNA="always")
table(dt$Candidate_category,dt$LoseLast, useNA="always")
table(dt$IDLast==dt$ID_in_the_previous_election)
as.data.frame(dt[which(dt$Name%in%dt$Name[which(dt$IDLast!=dt$ID_in_the_previous_election)]),
                 c("ID","City","Year","Election","Name",
                   "IDLast","ID_in_the_previous_election")])

table(is.na(dt$Number_of_votes))
dt <- subset(dt, !is.na(Number_of_votes))
dt <- subset(dt, Election != "補欠選挙")


##分析
## 仮説1: 政党に所属する人は、所属しない人よりも再立候補しやすい
## 仮説2: 得票が多くなるほど再立候補しやすいが、その効果は落選した人の中のみ

## 分析用の新しいデータの作成
dn <- 
  data.frame(ID = dt$ID,
             Election = dt$Election,
             Year = dt$Year,
             City = dt$City,
             Name = factor(paste(dt$Name,dt$City),
                            levels=unique(paste(dt$Name,dt$City))))

## 従属変数Y の作成 ##

## 再立候補の有無
dn$Next_election <- dt$Next_election
summary(dn$Next_election)

## 再立候補の有無とその割合のグラフ
ggplot(dn, aes(x=Next_election, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 2, color = "white") + 
  scale_x_continuous(breaks=c(0,1),
                     labels=c("再立候補なし(0)",
                              "再立候補あり(1)")) + 
  labs(x = "再立候補の有無", 
       y="割合") +
  theme_bw()
ggsave("./out/itfig1.png", width=6, height=4)

## 独立変数Xの作成 
## 政党所属の有無
dn$Belong_to_party <- 1
dn$Belong_to_party[which(dt$Political_party=="無所属")] <- 0
summary(dn$Belong_to_party)
## 政党所属の有無とその割合のグラフ
ggplot(dn, aes(x=Belong_to_party, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 2, color = "white") + 
  scale_x_continuous(breaks=c(0,1),
                     labels=c("無所属(0)",
                              "政党所属あり(1)")) + 
  labs(x = "政党所属の有無", 
       y="割合") +
  theme_bw()
ggsave("./out/itfig2.png", width=6, height=4)

## 得票率
dn$Vote_count <- dt$Number_of_votes ## 得票数（参考）
dn$Vote_share <- 
  unlist(tapply(dn$Vote_count,
                factor(paste(dt$City,dt$Year),
                       levels=unique(paste(dt$City,dt$Year))),
                function(x) x/sum(x)*100))

## 全候補者の得票率の分布
ggplot(dn, aes(x=Vote_share, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 20, color = "white") + 
  labs(x = "得票率", 
       y="割合") +
  theme_bw()
ggsave("./out/itfig3.png", width=6, height=4)

## 条件付け変数Mの作成 
## 当選したかどうか
dn$Victory <- NA
dn$Victory[which(dt$Result=="当")] <- 1
dn$Victory[which(dt$Result=="落")] <- 0
table(dn$Victory)

## 選挙の当落とその当落の割合
ggplot(dn, aes(x=Victory, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins = 2, color = "white") + 
  scale_x_continuous(breaks=c(0,1),
                     labels=c("落選(0)",
                              "当選(1)")) + 
  labs(x = "選挙結果", 
       y="割合") +
  theme_bw()

## 交絡変数Zの作成 
## 今回現職だったかどうか（元職は欠損）
dn$Incumbent <- NA
dn$Incumbent[which(dt$Candidate_category=="現職")] <- 1
dn$Incumbent[which(dt$Candidate_category=="新人")] <- 0
table(dn$Incumbent, useNA="always")

## 性別（女性）
dn$Female <- NA
dn$Female[which(dt$Sex=="女")] <- 1
dn$Female[which(dt$Sex=="男")] <- 0
table(dn$Female)

## 年齢（10歳刻み）
dn$Age10 <- dt$Age/10
table(dn$Age10)

## 選挙回（市町村×選挙年）
dn$Election_num <- factor(paste(dt$City,dt$Year),
                       levels = unique(paste(dt$City,dt$Year)))

## 分析 

## 仮説１：基本モデル
m1b <- lm_robust(Next_election ~ Belong_to_party + 
                   as.factor(Election_num),
                   # as.factor(Year) + as.factor(City), 
                 data = dn,
                 clusters = Name, se_type="stata")

## 仮説１：交絡変数あり
m1 <- lm_robust(Next_election ~  Belong_to_party + 
                  Vote_share + Victory + 
                  Incumbent + Female + Age10 + 
                  as.factor(Election_num),
                # as.factor(Year) + as.factor(City), 
                data = dn,
                clusters = Name, se_type="stata")

## 仮説２：基本モデル
m2b <- lm_robust(Next_election ~ 
                   Vote_share * Victory + 
                   as.factor(Election_num),
                 
                 data = dn,
                 clusters = Name, se_type="stata")

## 仮説２：交絡変数あり
m2 <- lm_robust(Next_election ~ 
                  Vote_share * Victory + 
                  Belong_to_party + 
                  Incumbent + Female + Age10 + 
                  as.factor(Election_num),
               
                data = dn, 
                clusters = Name, se_type="stata")


screenreg(list(m1b, m1, m2b, m2), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")
vnmap <- list(#"(Intercept)" = "（定数項）",　#固定効果があるので定数項は省略
              "Belong_to_party" = "政党所属",
              "Vote_share" = "得票率",
              "Vote_share:Victory" = "得票率×当選",
              "Victory" = "当選",
              "Incumbent" = "現職",
              "Female" = "女性",
              "Age10" = "年齢（10歳刻み）")
##仮説1、仮説2の分析結果を表で出力（表１）
tab1 <- HTML(htmlreg(list(m1b, m1, m2b, m2), include.ci = FALSE,
          digits=3, stars = c(0.001,0.01,0.05,0.1), 
          symbol="+",  star.symbol = "*", inline.css = FALSE,
          html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
          single.row = FALSE, caption = "",
          custom.coef.map = vnmap, 
          custom.header = list("仮説１" = 1:2, "仮説２" = 3:4), 
          custom.model.names =  c("基本","拡張","基本","拡張"),
          custom.gof.rows = list("自治体×年固定効果" = rep("有",4)),
          custom.note = "%stars。OLS回帰分析結果。括弧内はクラスタロバスト標準誤差。"))
## 表を仮表示
browsable(tab1)
## 表をoutに保存する
save_html(tab1, "./out/ittab1.html")
webshot(url="./out/ittab1.html", file="./out/ittab1.png", zoom=3, selector="table")

## 得票率が再立候補確率に与える限界効果を当選者と落選者に分けたグラフ（参考）
genkaiout <- slopes(m2, variables="Vote_share", 
                    newdata=datagrid(Victory=c(0,1)))
ggplot(genkaiout, aes(y=estimate, x=as.factor(Victory))) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), 
                width=0.1, linewidth=0.5) + 
  
  geom_point(color="black") + 
  scale_x_discrete(labels = c("落選(0)","当選(1)")) +
  labs(subtitle="従属変数：再立候補",
       y="得票率の限界効果",
       x="選挙の当落",
       caption="注：拡張モデルを使用。エラーバーは、95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))

###########################################################
## 予測値の算出########
##（ここから）#############################################
genpr <- function(dpr,
                  setx,
                  mpr, 
                  setxvals, 
                  setxlabs=NULL, 
                  setm=NULL,
                  setmvals=NULL, 
                  datalab=NULL) { 
  
  simdt <- na.omit(dpr[,all.vars(mpr$terms)])
  simx <- setxvals
  if (is.null(setm)) {
    simv <- data.frame(simx = simx)
  } else {
    simm <- sapply(setmvals, function(x) x[1])
    names(simm) <- NULL
    simv <- data.frame(simx = rep(simx,each=length(simm)), 
                       simm = simm)
  }

  prout <- as.data.frame(t(apply(simv, 1, function(k) {
    tmpdt <- simdt
    if(is.numeric(simv$simx)) {
      tmpdt[,setx] <- as.numeric(k[1])
    } else {
      tmpdt[,setx] <- k[1]
    }
    if (!is.null(setm)) {
      if (is.numeric(simv$simm)) {
        tmpdt[,setm] <- as.numeric(k[2])
      } else {
        tmpdt[,setm] <- k[2]
      }
    }
    tmp <- colMeans(as.data.frame(predict(mpr, newdata=tmpdt, se.fit=TRUE)))
    tmp <- c(k, tmp[1:2], 
             tmp[1]-tmp[2]*qt(0.975,df=mpr$df[2]),
             tmp[1]+tmp[2]*qt(0.975,df=mpr$df[2]),
             tmp[1]-tmp[2]*qt(0.95,df=mpr$df[2]),
             tmp[1]+tmp[2]*qt(0.95,df=mpr$df[2]))
    
    if (is.null(setm)) {
      names(tmp) <- c("x","pr","se",
                      "lo95","up95","lo90","up90")
    } else {
      names(tmp) <- c("x","m","pr","se",
                      "lo95","up95","lo90","up90")
    }
    return(tmp)
  })))
  prout$pr <- as.numeric(prout$pr)
  prout$se <- as.numeric(prout$se)
  prout$lo95 <- as.numeric(prout$lo95)
  prout$up95 <- as.numeric(prout$up95)
  prout$lo90 <- as.numeric(prout$lo90)
  prout$up90 <- as.numeric(prout$up90)
  if(!is.null(setxlabs)) {
    prout$labelledx <- 
      factor(prout$x,levels=setxvals,labels=setxlabs)
  }
  if (!is.null(setm)) {
    prout$labelledm <- 
      factor(names(setmvals)[match(prout$m,unlist(setmvals))],
             levels = names(setmvals))
  }
  if (!is.null(datalab)) {
    prout$datalab <- datalab
  }
  
  return(prout)
}
##（ここまで）#############################################

## 予測確率の計算（仮説１）
yosokuout1 <- genpr(dpr = dn,
                   mpr = m1,
                   setx = "Belong_to_party", 
                   setxvals = seq(0,1,length=2))

## 政党所属の有無による候補者の再立候補率(仮説1)（図4）
ggplot(yosokuout1, aes(x=as.factor(x), y=pr)) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95), 
                width=0.1, linewidth=0.5) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90), 
                width=0, linewidth=2) + 
  geom_point(color="white") + 
  scale_x_discrete(labels = c("無所属(0)","政党所属(1)")) +
  labs(x="政党所属の有無",
       y="予測再立候補率", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/itfig4.png", width=6, height=4)

## 予測確率の計算（仮説２）
quantile(dn$Vote_share[which(dn$Victory==0)], 
         probs = c(0, 0.05,0.95, 1),
         na.rm = TRUE)
quantile(dn$Vote_share[which(dn$Victory==1)], 
         probs = c(0, 0.05,0.95, 1),
         na.rm = TRUE)
yosokuout2 <- genpr(dpr = dn,
                    mpr = m2,
                    setx = "Vote_share", 
                    setxvals = seq(0.5,5.9,by=0.1),
                    setm = "Victory",
                    setmvals = list("落選(0)" = 0,
                                    "当選(1)" = 1))
yosokuout2$x[which(yosokuout2$m==0&
                     yosokuout2$x>4.20)] <- NA
yosokuout2$x[which(yosokuout2$m==1&
                     yosokuout2$x<1.34)] <- NA
## 得票率による再立候補率の落選者と当選者別の割合(仮説2)（図５）
ggplot(yosokuout2, aes(x=x, y=pr)) + 
  geom_ribbon(aes(ymin=lo95, ymax=up95,
                  fill = labelledm), alpha=0.3) + 
  geom_ribbon(aes(ymin=lo90, ymax=up90,
                  fill = labelledm), alpha=0.5) + 
  geom_line(aes(linetype=labelledm)) + 
  coord_cartesian(ylim=c(0,1)) + 
  scale_shape_discrete(name = "選挙の当落") + 
  scale_linetype_discrete(name = "選挙の当落") + 
  scale_color_brewer(name = "選挙の当落", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "選挙の当落", 
                    type = "qual", palette = 2) + 
  labs(x="得票率",
       y="予測再立候補率", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/itfig5.png", width=6, height=4)


## 仮説3：地縁型社会資本の持つ職業の変数作成

table(dt$Occupation); sort(unique(dt$Occupation))
# 地縁型社会資本の高い職業の分類
# high_status_jobs <- c("会社役員", "団体職員","団体役員", "公務員", "市議会議員",
#                       "会社経営", "医療系管理職", "団体役員","政党役員","神職",
#                       "会社社長","行政書士","税理士","国会議員秘書",
#                       "社会保険労務士","タレント","NPO理事","法人理事",
#                       "法人役員","経営コンサルタント","増田町長")
# 
dn$CommunitySC <- dt$`Community-embedded_social_capital`
table(dn$CommunitySC, useNA="always")
unique(dt$Occupation[which(dn$CommunitySC==1)])

## 仮説３の分析
## 仮説３：交絡変数なし
m3b <- lm_robust(Next_election ~ CommunitySC +
                   as.factor(Election_num),
               
                 data = dn,
                 clusters = Name,
                 se_type = "stata")

## 仮説３：交絡変数あり
m3 <- lm_robust(Next_election ~ CommunitySC +
                  Belong_to_party + Vote_share * Victory +
                  Incumbent + Female + Age10 +
                  as.factor(Election_num),
               
                data = dn,
                clusters = Name,
                se_type = "stata")

screenreg(list(m3b, m3), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

vnmap <- list(#"(Intercept)" = "（定数項）",
              "CommunitySC" = "地縁型社会資本",
              "Belong_to_party" = "政党所属",
              "Vote_share" = "得票率",
              "Vote_share:Victory" = "得票率×当選",
              "Victory" = "当選",
              "Incumbent" = "現職",
              "Female" = "女性",
              "Age10" = "年齢（10歳刻み）")

##仮説3の分析結果の表を出力
tab2 <- HTML(htmlreg(list(m3b, m3), include.ci = FALSE,
                       digits=3, stars = c(0.001,0.01,0.05,0.1), 
                       symbol="+",  star.symbol = "*", inline.css = FALSE,
                       html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                       single.row = FALSE, caption = "",
                       custom.coef.map = vnmap, 
                       custom.header = list("仮説３" = 1:2), 
                       custom.model.names =  c("基本","拡張"),
                       custom.gof.rows = list("自治体×年固定効果" = rep("有",2)),
                       custom.note = "%stars。OLS回帰分析結果。括弧内はクラスタロバスト標準誤差。"))
## 表を仮表示
browsable(tab2)
## 表をoutに保存する
save_html(tab2, "./out/ittab2.html")
webshot(url="./out/ittab2.html", file="./out/ittab2.png", zoom=3, selector="table")

## 仮説３用の予測確率の計算
yosokuout3 <- genpr(dpr = dn,
                    mpr = m3,
                    setx = "CommunitySC", 
                    setxvals = seq(0,1,length=2))
## 地縁型社会資本の有無による候補者の再立候補率のグラフ(仮説3)（図６）
ggplot(yosokuout3, aes(x=as.factor(x), y=pr)) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95), 
                width=0.1, linewidth=0.5) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90), 
                width=0, linewidth=2) + 
  geom_point(color="white") + 
  scale_x_discrete(labels = c("なし(0)","あり(1)")) +
  labs(x="地縁型社会資本",
       y="予測再立候補率", 
       caption = "注：拡張モデルを使用。塗りつぶしは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/itfig6.png", width=6, height=4)


## 仮説4：再立候補が得票に与える影響 

dn$WinLast <- dt$WinLast
dn$LoseLast <- dt$LoseLast

tgtnames <- 
  intersect(
    unique(paste(dt$Name,dt$City)[which(dt$LoseLast==0&dt$WinLast==0)]),
    unique(paste(dt$Name,dt$City)[which(dt$LoseLast==1)]))
tgtnames 
dnsub <- subset(dn, Name %in% tgtnames & Incumbent == 0)
dnsub <- dnsub[order(dnsub$Name),]
length(unique(dnsub$Name)) 
dnsub[c("Name","Year",
        "Vote_count","Vote_share",
        "Victory","Incumbent",
        "LoseLast","WinLast")]

dnsub$TimesCand <- NA
for (k in unique(dnsub$Name)) {
  tmploc <- which(dnsub$Name==k)
  dnsub$TimesCand[tmploc] <- c(0:(length(tmploc)-1))
}
table(dnsub$TimesCand) 
dnsub$TimesCand[which(dnsub$TimesCand==2)] <- NA

## 仮説４：交絡変数なし（従属変数：得票率）
m4b <- lm_robust(Vote_share ~  TimesCand + 
                   as.factor(City),
                 data = dnsub,
                 clusters = Name,
                 se_type = "stata")

## 仮説４：交絡変数あり（従属変数：得票率）
m4 <- lm_robust(Vote_share ~  TimesCand + 
                  Belong_to_party + Female + Age10 + 
                  as.factor(City),
                data = dnsub,
                clusters = Name,
                se_type = "stata")

screenreg(list(m4b, m4), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

vnmap <- list(#"(Intercept)" = "（定数項）",
              "TimesCand" = "再立候補=1, 初立候補=0",
              "Belong_to_party" = "政党所属",
              "Female" = "女性",
              "Age10" = "年齢（10歳刻み）")
### 仮説4の分析結果を表で出力
tab3 <- HTML(htmlreg(list(m4b, m4), include.ci = FALSE,
                       digits=3, stars = c(0.001,0.01,0.05,0.1), 
                       symbol="+",  star.symbol = "*", inline.css = FALSE,
                       html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                       single.row = FALSE, caption = "",
                       custom.coef.map = vnmap, 
                       custom.header = list("仮説３" = 1:2), 
                       custom.model.names =  c("基本","拡張"),
                       custom.gof.rows = list("自治体固定効果" = rep("有",2)),
                       custom.note = "%stars。OLS回帰分析結果。括弧内は標準誤差。"))
## 表を仮表示
browsable(tab3)
## 表をoutに保存する
save_html(tab3, "./out/ittab3.html")
webshot(url="./out/ittab3.html", file="./out/ittab3.png", zoom=3, selector="table")

## 仮説４の予測確率計算
yosokuout4 <- genpr(dpr = dnsub,
                    mpr = m4,
                    setx = "TimesCand", 
                    setxvals = seq(0,1,length=2))
## 新人候補者の初出馬時と再立候補時の得票率比較(仮説4)
ggplot(yosokuout4, aes(x=as.factor(x), y=pr)) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95), 
                width=0.1, linewidth=0.5) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90), 
                width=0, linewidth=2) + 
  geom_point(color="white") + 
  scale_x_discrete(labels = c("初立候補(0)","再立候補(1)")) +
  labs(x="再立候補かどうか",
       y="予測得票率", 
       caption = "注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/itfig7.png", width=6, height=4)

