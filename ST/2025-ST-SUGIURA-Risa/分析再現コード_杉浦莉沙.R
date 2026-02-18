##タイトル：立場表明と立場回避は、政治家の意見表明に影響を与えるのか

## 更新履歴：
## 2026/01/22 杉浦莉沙（提出版）
## 2026/02/13 加藤言人（最終調整）

##データをクリーンにする
rm(list=ls())

## このコードファイルがあるディレクトリを取得（RStudioのみ）
dirhere <- dirname(rstudioapi::getActiveDocumentContext()$path) 
## ワーキングディレクトリをセット
setwd(dirhere)
## ワーキングディレクトリを確認
getwd() 

## 図表出力用のフォルダ（out）を作成
if (!"./out" %in% list.dirs()) dir.create("./out")

##################
## データの入手 ##
##################

## 政治家データ（2024年の衆議院）

## データのダウンロード（必要な場合のみ） 
## 東京大学谷口研究室・朝日新聞社共同調査
##https://www.masaki.j.u-tokyo.ac.jp/utas/utasindex.html#2024にてデータを取得
if(!"2024UTASP20241125.csv"%in%list.files()){
  download.file("https://www.masaki.j.u-tokyo.ac.jp/utas/2024UTASP20241125.csv",
                "2024UTASP20241125.csv")
}

## データの読み込み
library(readr)
dtp0 <- read_csv("2024UTASP20241125.csv", locale = locale(encoding="SHIFT_JIS"))

##############################
## 必要な変数をチェックする ##
##############################

##政治家について：現職か否か
##1＝ 新人、2＝ 現職、3= 元職
table(dtp0$INCUMB, exclude=F)

##政治家について：自民党か否か
table(dtp0$PARTY, exclude=F)

## 消費税率を10％よりも高くする (Q3_1)
table(dtp0$Q3_1, exclude=F) ## 政治家　

## 年金や医療費の給付を現行の水準よりも抑制する (Q3_2)
table(dtp0$Q3_2, exclude=F) ## 政治家　

## 競争力のない産業・企業に対する保護を現行の水準よりも削減する (Q3_3)
table(dtp0$Q3_3, exclude=F) ## 政治家　

## 日本銀行は国債の買入れなど量的金融緩和政策を続ける (Q3_4)
table(dtp0$Q3_4, exclude=F) ## 政治家　

## 日本の防衛力はもっと強化すべきだ (Q4_1)
table(dtp0$Q4_1, exclude=F) ## 政治家

## 北朝鮮に対しては対話よりも圧力を優先すべきだ (Q4_2)
table(dtp0$Q4_2, exclude=F) ## 政治家

## 非核三原則を堅持すべきだ (Q4_3)
table(dtp0$Q4_3, exclude=F) ## 政治家

## 首相には靖国神社に参拝してほしい (Q4_4)
table(dtp0$Q4_4, exclude=F) ## 政治家

##社会福祉など政府のサービスが悪くなっても、お金のかからない小さな政府の方が良い  (Q4_5) 
table(dtp0$Q4_5, exclude=F) ## 政治家

## 公共事業による雇用確保は必要だ (Q4_6)
table(dtp0$Q4_6, exclude=F) ## 政治家 

## 当面は財政再建のために歳出を抑えるのではなく、景気対策のために財政出動を行うべきだ (Q4_7)
table(dtp0$Q4_7, exclude=F) ## 政治家　

## 企業が納めている法人税率を引き上げるべきだ (Q4_8)
table(dtp0$Q4_8, exclude=F) ## 政治家　

## 全農の株式会社化や信用事業の分離など、農協の組織改革をさらに進めるべきだ(Q4_9)
table(dtp0$Q4_9, exclude=F) ## 政治家 

## 外国人労働者の受け入れを進めるべきだ (Q4_10)
table(dtp0$Q4_10, exclude=F) ## 政治家 

## 日本銀行は政策金利を引き上げるべきだ (Q4_11)
table(dtp0$Q4_11, exclude=F) ## 政治家 

## 治安を守るためにプライバシーや個人の権利が制約されるのは当然だ (Q4_12)
table(dtp0$Q4_12, exclude=F) ## 政治家 

## 公務員の人数や給与を抑制すべきだ  (Q4_13)
table(dtp0$Q4_13, exclude=F) ## 政治家 

## 雇用の流動化を進めるべきだ (Q4_14)
table(dtp0$Q4_14, exclude=F) ## 政治家 

##  政策研鑽や人材育成のため、党内に派閥／政策集団は必要だ (Q4_15)
table(dtp0$Q4_15, exclude=F) ## 政治家 

## 被選挙権を得られる年齢を引き下げるべきだ (Q4_16)
table(dtp0$Q4_16, exclude=F) ## 政治家 

##  夫婦が望む場合には、結婚後も夫婦がそれぞれ結婚前の名字を称することを、法律で認めるべきだ(Q4_17)
table(dtp0$Q4_17, exclude=F) ## 政治家 

## 男性同士、女性同士の結婚を法律で認めるべきだ  (Q4_18)
table(dtp0$Q4_18, exclude=F) ## 政治家 

## 国会議員の議席や候補者の一定割合を女性に割り当てるクオータ制を導入すべきだ (Q4_19)
table(dtp0$Q4_19, exclude=F) ## 政治家 

##政策立場AorB(Q5_1～Q5_14のデータを確認する)

##(1) A: 危機のときのアメリカによる協力を確実にするため、日米安保体制をもっと強化すべきだ
##B: 日本と関係ない戦争に巻き込まれないように、日米安保体制の強化には慎重であるべきだ(Q5_1)
table(dtp0$Q5_1, exclude=F) ## 政治家


##(2) A: 日本にとって中国は脅威である
##B: 日本にとって中国はパートナーである(Q5_2)
table(dtp0$Q5_2, exclude=F) ## 政治家


##(3) A: 社会的格差が多少あっても、いまは経済競争力の向上を優先すべきだ
##B: 経済競争力を多少犠牲にしても、いまは社会的格差の是正を優先すべきだ(Q5_3)
table(dtp0$Q5_3, exclude=F) ## 政治家


##(4) A: 国内産業を保護すべきだ
##B: 貿易や投資の自由化を進めるべきだ(Q5_4)
table(dtp0$Q5_4, exclude=F) ## 政治家 


##(5) A: 少子化対策の財源の一部を社会保障費の削減でまかなうことは避けられない
##B: 少子化対策の財源確保のため社会保障費に切り込むことはあってはならない(Q5_5)
table(dtp0$Q5_5, exclude=F) ## 政治家 


##(6) A: いますぐ原子力発電を廃止すべきだ
##B: 将来も原子力発電は電力源のひとつとして保つべきだ(Q5_6)
table(dtp0$Q5_6, exclude=F) ## 政治家


##(7) A: 国債は安定的に消化されており、財政赤字を心配する必要はない
##B: 財政赤字は危機的水準であるので、国債発行を抑制すべきだ(Q5_7)
table(dtp0$Q5_7, exclude=F) ## 政治家


##(8) A: 気候変動問題に対応するため、生活水準を犠牲にすることも必要だ
##B: 生活水準を犠牲にするほど、気候変動問題への対応は重要問題ではない(Q5_8)
table(dtp0$Q5_8, exclude=F) ## 政治家


##9) A: 生成AIの活用・普及を推進すべきだ
##B: 生成AIの活用・普及を規制すべきだ(Q5_9)
table(dtp0$Q5_9, exclude=F) ## 政治家


##(10) A: マイナンバーの活用・普及を推進すべきだ
##B: マイナンバーの活用・普及を規制すべきだ(Q5_10)
table(dtp0$Q5_10, exclude=F) ## 政治家 


##(11) A: 企業・団体にも政治活動の自由がある
##B: 企業・団体献金は全面禁止すべきだ(Q5_11)
table(dtp0$Q5_11, exclude=F) ## 政治家


##(12) A: 日本は二大政党制を目指すべきだ
##B: 日本は多党制を目指すべきだ(Q5_12) 
table(dtp0$Q5_12, exclude=F) ## 政治家


##(13) A: わたしは、自分を支持した地域や団体の代表というよりも、すべての国民の代表である
##B: わたしは、すべての国民の代表というよりも、自分を支持した地域や団体の代表である(Q5_13)
table(dtp0$Q5_13, exclude=F) ## 政治家


##(14) A: 選挙戦では、所属政党の政策や実績よりも、自分自身の人となりや政見を重点的に訴えたい
##B: 選挙戦では、自分自身の人となりや政見よりも、所属政党の政策や実績を重点的に訴えたい(Q5_14)
table(dtp0$Q5_14, exclude=F) ## 政治家

##############################
## 新しいデータと変数の作成 ##
##############################

## データを作成（政治家IDの列を追加する）
dtp <- data.frame(id = c(dtp0$ID))

##政治家のinclum属性をデータに組み込む
table(dtp0$INCUMB)
dtp$gensyoku<- NA
dtp$gensyoku[c(dtp0$INCUMB)%in%c(1,2)] <- 0 
dtp$gensyoku[c(dtp0$INCUMB)%in%c(3)] <- 1
table(dtp$gensyoku, exclude=F)

##政治家のparty属性をデータに組み込む
table(dtp0$PARTY)
dtp$jimin<- NA
dtp$jimin[c(dtp0$PARTY)%in%c(2,3,4,5,6,7,8,9,10,11,12)] <- 0 
dtp$jimin[c(dtp0$PARTY)%in%c(1)] <- 1
table(dtp$jimin, exclude=F)

##各政策に対する意見表明度をまとまる
##回答項目を意見表明度として変数に追加する。

## 消費税率を10％よりも高くする (Q3_1)
table(dtp0$Q3_1, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$tax10 <- NA
dtp$tax10[c(dtp0$Q3_1)%in%c(3,99)] <- 0
dtp$tax10[c(dtp0$Q3_1)%in%c(2,4)] <- 1
dtp$tax10[c(dtp0$Q3_1)%in%c(1,5)] <- 2
table(dtp$tax10, exclude=F)

## 年金や医療費の給付を現行の水準よりも抑制する (Q3_2)
table(dtp0$Q3_2, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$pension<- NA
dtp$pension[c(dtp0$Q3_2)%in%c(3,99)] <- 0
dtp$pension[c(dtp0$Q3_2)%in%c(2,4)] <- 1
dtp$pension[c(dtp0$Q3_2)%in%c(1,5)] <- 2
table(dtp$pension, exclude=F)

## 競争力のない産業・企業に対する保護を現行の水準よりも削減する (Q3_3)
table(dtp0$Q3_3, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Competitiveness<- NA
dtp$Competitiveness[c(dtp0$Q3_3)%in%c(3,99)] <- 0
dtp$Competitiveness[c(dtp0$Q3_3)%in%c(2,4)] <- 1
dtp$Competitiveness[c(dtp0$Q3_3)%in%c(1,5)] <- 2
table(dtp$Competitiveness, exclude=F)

## 日本銀行は国債の買入れなど量的金融緩和政策を続ける (Q3_4)
table(dtp0$Q3_4, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$bank<- NA
dtp$bank[c(dtp0$Q3_4)%in%c(3,99)] <- 0
dtp$bank[c(dtp0$Q3_4)%in%c(2,4)] <- 1
dtp$bank[c(dtp0$Q3_4)%in%c(1,5)] <- 2
table(dtp$bank, exclude=F)

## 日本の防衛力はもっと強化すべきだ (Q4_1)
table(dtp0$Q4_1, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Defense<- NA
dtp$Defense[c(dtp0$Q4_1)%in%c(3,99)] <- 0
dtp$Defense[c(dtp0$Q4_1)%in%c(2,4)] <- 1
dtp$Defense[c(dtp0$Q4_1)%in%c(1,5)] <- 2
table(dtp$Defense, exclude=F)

## 北朝鮮に対しては対話よりも圧力を優先すべきだ (Q4_2)
table(dtp0$Q4_2, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$northsouth <- NA
dtp$northsouth[c(dtp0$Q4_2)%in%c(3,99)] <- 0
dtp$northsouth[c(dtp0$Q4_2)%in%c(2,4)] <- 1
dtp$northsouth[c(dtp0$Q4_2)%in%c(1,5)] <- 2
table(dtp$northsouth, exclude=F)

## 非核三原則を堅持すべきだ (Q4_3)
table(dtp0$Q4_3, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$hikaku <- NA
dtp$hikaku[c(dtp0$Q4_3)%in%c(3,99)] <- 0
dtp$hikaku[c(dtp0$Q4_3)%in%c(2,4)] <- 1
dtp$hikaku[c(dtp0$Q4_3)%in%c(1,5)] <- 2
table(dtp$hikaku, exclude=F)

## 首相には靖国神社に参拝してほしい (Q4_4)
table(dtp0$Q4_4, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$yasukuni <- NA
dtp$yasukuni[c(dtp0$Q4_4)%in%c(3,99)] <- 0
dtp$yasukuni[c(dtp0$Q4_4)%in%c(2,4)] <- 1
dtp$yasukuni[c(dtp0$Q4_4)%in%c(1,5)] <- 2
table(dtp$yasukuni, exclude=F)

##社会福祉など政府のサービスが悪くなっても、お金のかからない小さな政府の方が良い  (Q4_5) 
table(dtp0$Q4_5, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Smallgovernment <- NA
dtp$Smallgovernment[c(dtp0$Q4_5)%in%c(3,99)] <- 0
dtp$Smallgovernment[c(dtp0$Q4_5)%in%c(2,4)] <- 1
dtp$Smallgovernment[c(dtp0$Q4_5)%in%c(1,5)] <- 2
table(dtp$Smallgovernment, exclude=F)

## 公共事業による雇用確保は必要だ (Q4_6)
table(dtp0$Q4_6, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Publicworks <- NA
dtp$Publicworks[c(dtp0$Q4_6)%in%c(3,99)] <- 0 ## GK: Smallgovernment->Publicworks
dtp$Publicworks[c(dtp0$Q4_6)%in%c(2,4)] <- 1 ## GK: Smallgovernment->Publicworks
dtp$Publicworks[c(dtp0$Q4_6)%in%c(1,5)] <- 2 ## GK: Smallgovernment->Publicworks
table(dtp$Publicworks, exclude=F) ## GK: Smallgovernment->Publicworks

## 当面は財政再建のために歳出を抑えるのではなく、景気対策のために財政出動を行うべきだ (Q4_7)
table(dtp0$Q4_7, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Fiscalstimulus <- NA
dtp$Fiscalstimulus[c(dtp0$Q4_7)%in%c(3,99)] <- 0
dtp$Fiscalstimulus[c(dtp0$Q4_7)%in%c(2,4)] <- 1
dtp$Fiscalstimulus[c(dtp0$Q4_7)%in%c(1,5)] <- 2
table(dtp$Fiscalstimulus, exclude=F)

## 企業が納めている法人税率を引き上げるべきだ (Q4_8)
table(dtp0$Q4_8, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Corporatetax <- NA
dtp$Corporatetax[c(dtp0$Q4_8)%in%c(3,99)] <- 0
dtp$Corporatetax[c(dtp0$Q4_8)%in%c(2,4)] <- 1
dtp$Corporatetax[c(dtp0$Q4_8)%in%c(1,5)] <- 2
table(dtp$Corporatetax, exclude=F)

## 全農の株式会社化や信用事業の分離など、農協の組織改革をさらに進めるべきだ(Q4_9)
table(dtp0$Q4_9, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$ farm<- NA
dtp$farm[(dtp0$Q4_9)%in%c(3,99)] <- 0
dtp$farm[(dtp0$Q4_9)%in%c(2,4)] <- 1
dtp$farm[(dtp0$Q4_9)%in%c(1,5)] <- 2
table(dtp$farm, exclude=F)

## 外国人労働者の受け入れを進めるべきだ (Q4_10)
table(dtp0$Q4_10, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Foreigner<- NA
dtp$Foreigner[(dtp0$Q4_10)%in%c(3,99)] <- 0
dtp$Foreigner[(dtp0$Q4_10)%in%c(2,4)] <- 1
dtp$Foreigner[(dtp0$Q4_10)%in%c(1,5)] <- 2
table(dtp$Foreigner, exclude=F)

## 日本銀行は政策金利を引き上げるべきだ (Q4_11)
table(dtp0$Q4_11, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$nichigin<- NA
dtp$nichigin[(dtp0$Q4_11)%in%c(3,99)] <- 0
dtp$nichigin[(dtp0$Q4_11)%in%c(2,4)] <- 1
dtp$nichigin[(dtp0$Q4_11)%in%c(1,5)] <- 2
table(dtp$nichigin, exclude=F)

## 治安を守るためにプライバシーや個人の権利が制約されるのは当然だ (Q4_12)
table(dtp0$Q4_12, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Privacy<- NA
dtp$Privacy[(dtp0$Q4_12)%in%c(3,99)] <- 0
dtp$Privacy[(dtp0$Q4_12)%in%c(2,4)] <- 1
dtp$Privacy[(dtp0$Q4_12)%in%c(1,5)] <- 2
table(dtp$Privacy, exclude=F)

## 公務員の人数や給与を抑制すべきだ  (Q4_13)
table(dtp0$Q4_13, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Civilservant<- NA
dtp$Civilservant[(dtp0$Q4_13)%in%c(3,99)] <- 0
dtp$Civilservant[(dtp0$Q4_13)%in%c(2,4)] <- 1
dtp$Civilservant[(dtp0$Q4_13)%in%c(1,5)] <- 2
table(dtp$Civilservant, exclude=F)

## 雇用の流動化を進めるべきだ (Q4_14)
table(dtp0$Q4_14, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Employment<- NA
dtp$Employment[(dtp0$Q4_14)%in%c(3,99)] <- 0
dtp$Employment[(dtp0$Q4_14)%in%c(2,4)] <- 1
dtp$Employment[(dtp0$Q4_14)%in%c(1,5)] <- 2
table(dtp$Employment, exclude=F)

##  政策研鑽や人材育成のため、党内に派閥／政策集団は必要だ (Q4_15)
table(dtp0$Q4_15, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$faction<- NA
dtp$faction[(dtp0$Q4_15)%in%c(3,99)] <- 0
dtp$faction[(dtp0$Q4_15)%in%c(2,4)] <- 1
dtp$faction[(dtp0$Q4_15)%in%c(1,5)] <- 2
table(dtp$faction, exclude=F)

## 被選挙権を得られる年齢を引き下げるべきだ (Q4_16)
table(dtp0$Q4_16, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$vote<- NA
dtp$vote[(dtp0$Q4_16)%in%c(3,99)] <- 0
dtp$vote[(dtp0$Q4_16)%in%c(2,4)] <- 1
dtp$vote[(dtp0$Q4_16)%in%c(1,5)] <- 2
table(dtp$vote, exclude=F)

##  夫婦が望む場合には、結婚後も夫婦がそれぞれ結婚前の名字を称することを、法律で認めるべきだ(Q4_17)
table(dtp0$Q4_17, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$select<- NA
dtp$select[(dtp0$Q4_17)%in%c(3,99)] <- 0
dtp$select[(dtp0$Q4_17)%in%c(2,4)] <- 1
dtp$select[(dtp0$Q4_17)%in%c(1,5)] <- 2
table(dtp$select, exclude=F)

## 男性同士、女性同士の結婚を法律で認めるべきだ  (Q4_18)
table(dtp0$Q4_18, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$samesexmarriage<- NA
dtp$samesexmarriage[(dtp0$Q4_18)%in%c(3,99)] <- 0
dtp$samesexmarriage[(dtp0$Q4_18)%in%c(2,4)] <- 1
dtp$samesexmarriage[(dtp0$Q4_18)%in%c(1,5)] <- 2
table(dtp$samesexmarriage, exclude=F)

## 国会議員の議席や候補者の一定割合を女性に割り当てるクオータ制を導入すべきだ (Q4_19)
table(dtp0$Q4_19, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp0$Quarter<- NA
dtp0$Quarter[(dtp0$Q4_19)%in%c(3,99)] <- 0
dtp0$Quarter[(dtp0$Q4_19)%in%c(2,4)] <- 1
dtp0$Quarter[(dtp0$Q4_19)%in%c(1,5)] <- 2
table(dtp0$Quarter, exclude=F)

##政策立場AorB

##(1) A: 危機のときのアメリカによる協力を確実にするため、日米安保体制をもっと強化すべきだ
##B: 日本と関係ない戦争に巻き込まれないように、日米安保体制の強化には慎重であるべきだ(Q5_1)
table(dtp0$Q5_1, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$america<- NA
dtp$america[(dtp0$Q5_1)%in%c(3,99)] <- 0
dtp$america[(dtp0$Q5_1)%in%c(2,4)] <- 1
dtp$america[(dtp0$Q5_1)%in%c(1,5)] <- 2
table(dtp$america, exclude=F)

##(2) A: 日本にとって中国は脅威である
##B: 日本にとって中国はパートナーである(Q5_2)
table(dtp0$Q5_2, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$china<- NA
dtp$china[(dtp0$Q5_2)%in%c(3,99)] <- 0
dtp$china[(dtp0$Q5_2)%in%c(2,4)] <- 1
dtp$china[(dtp0$Q5_2)%in%c(1,5)] <- 2
table(dtp$china, exclude=F)

##(3) A: 社会的格差が多少あっても、いまは経済競争力の向上を優先すべきだ
##B: 経済競争力を多少犠牲にしても、いまは社会的格差の是正を優先すべきだ(Q5_3)
table(dtp0$Q5_3, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$economy<- NA
dtp$economy[(dtp0$Q5_3)%in%c(3,99)] <- 0
dtp$economy[(dtp0$Q5_3)%in%c(2,4)] <- 1
dtp$economy[(dtp0$Q5_3)%in%c(1,5)] <- 2
table(dtp$economy, exclude=F)

##(4) A: 国内産業を保護すべきだ
##B: 貿易や投資の自由化を進めるべきだ(Q5_4)
table(dtp0$Q5_4, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$domestic<- NA
dtp$domestic[(dtp0$Q5_4)%in%c(3,99)] <- 0
dtp$domestic[(dtp0$Q5_4)%in%c(2,4)] <- 1
dtp$domestic[(dtp0$Q5_4)%in%c(1,5)] <- 2
table(dtp$domestic, exclude=F)

##(5) A: 少子化対策の財源の一部を社会保障費の削減でまかなうことは避けられない
##B: 少子化対策の財源確保のため社会保障費に切り込むことはあってはならない(Q5_5)
table(dtp0$Q5_5, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Decliningbirthrate<- NA
dtp$Decliningbirthrate[(dtp0$Q5_5)%in%c(3,99)] <- 0
dtp$Decliningbirthrate[(dtp0$Q5_5)%in%c(2,4)] <- 1
dtp$Decliningbirthrate[(dtp0$Q5_5)%in%c(1,5)] <- 2
table(dtp$Decliningbirthrate, exclude=F)

##(6) A: いますぐ原子力発電を廃止すべきだ
##B: 将来も原子力発電は電力源のひとつとして保つべきだ(Q5_6)
table(dtp0$Q5_6, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Nucleus<- NA
dtp$Nucleus[(dtp0$Q5_6)%in%c(3,99)] <- 0
dtp$Nucleus[(dtp0$Q5_6)%in%c(2,4)] <- 1
dtp$Nucleus[(dtp0$Q5_6)%in%c(1,5)] <- 2
table(dtp$Nucleus, exclude=F)

##(7) A: 国債は安定的に消化されており、財政赤字を心配する必要はない
##B: 財政赤字は危機的水準であるので、国債発行を抑制すべきだ(Q5_7)
table(dtp0$Q5_7, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Deficit<- NA
dtp$Deficit[(dtp0$Q5_7)%in%c(3,99)] <- 0
dtp$Deficit[(dtp0$Q5_7)%in%c(2,4)] <- 1
dtp$Deficit[(dtp0$Q5_7)%in%c(1,5)] <- 2
table(dtp$Deficit, exclude=F)

##(8) A: 気候変動問題に対応するため、生活水準を犠牲にすることも必要だ
##B: 生活水準を犠牲にするほど、気候変動問題への対応は重要問題ではない(Q5_8)
table(dtp0$Q5_8, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Environment<- NA
dtp$Environment[(dtp0$Q5_8)%in%c(3,99)] <- 0
dtp$Environment[(dtp0$Q5_8)%in%c(2,4)] <- 1
dtp$Environment[(dtp0$Q5_8)%in%c(1,5)] <- 2
table(dtp$Environment, exclude=F)

##9) A: 生成AIの活用・普及を推進すべきだ
##B: 生成AIの活用・普及を規制すべきだ(Q5_9)
table(dtp0$Q5_9, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$AI<- NA
dtp$AI[(dtp0$Q5_9)%in%c(3,99)] <- 0
dtp$AI[(dtp0$Q5_9)%in%c(2,4)] <- 1
dtp$AI[(dtp0$Q5_9)%in%c(1,5)] <- 2
table(dtp$AI, exclude=F)

##(10) A: マイナンバーの活用・普及を推進すべきだ
##B: マイナンバーの活用・普及を規制すべきだ(Q5_10)
table(dtp0$Q5_10, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$number<- NA
dtp$number[(dtp0$Q5_10)%in%c(3,99)] <- 0
dtp$number[(dtp0$Q5_10)%in%c(2,4)] <- 1
dtp$number[(dtp0$Q5_10)%in%c(1,5)] <- 2
table(dtp$number, exclude=F)

##(11) A: 企業・団体にも政治活動の自由がある
##B: 企業・団体献金は全面禁止すべきだ(Q5_11)
table(dtp0$Q5_11, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Donation<- NA
dtp$Donation[(dtp0$Q5_11)%in%c(3,99)] <- 0
dtp$Donation[(dtp0$Q5_11)%in%c(2,4)] <- 1
dtp$Donation[(dtp0$Q5_11)%in%c(1,5)] <- 2
table(dtp$Donation, exclude=F)

##(12) A: 日本は二大政党制を目指すべきだ
##B: 日本は多党制を目指すべきだ(Q5_12) 
table(dtp0$Q5_12, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$party<- NA
dtp$party[(dtp0$Q5_12)%in%c(3,99)] <- 0
dtp$party[(dtp0$Q5_12)%in%c(2,4)] <- 1
dtp$party[(dtp0$Q5_12)%in%c(1,5)] <- 2
table(dtp$party, exclude=F)

##(13) A: わたしは、自分を支持した地域や団体の代表というよりも、すべての国民の代表である
##B: わたしは、すべての国民の代表というよりも、自分を支持した地域や団体の代表である(Q5_13)
table(dtp0$Q5_13, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$Representative<- NA
dtp$Representative[(dtp0$Q5_13)%in%c(3,99)] <- 0
dtp$Representative[(dtp0$Q5_13)%in%c(2,4)] <- 1
dtp$Representative[(dtp0$Q5_13)%in%c(1,5)] <- 2
table(dtp$Representative, exclude=F)

##(14) A: 選挙戦では、所属政党の政策や実績よりも、自分自身の人となりや政見を重点的に訴えたい
##B: 選挙戦では、自分自身の人となりや政見よりも、所属政党の政策や実績を重点的に訴えたい(Q5_14)
table(dtp0$Q5_14, exclude=F) ## 政治家
##2＝強い意見あり、1＝弱い意見在り、0意見なし
dtp$election<- NA
dtp$election[(dtp0$Q5_14)%in%c(3,99)] <- 0
dtp$election[(dtp0$Q5_14)%in%c(2,4)] <- 1
dtp$election[(dtp0$Q5_14)%in%c(1,5)] <- 2
table(dtp$election, exclude=F)

#####交絡変数#####

## 性別変数##

## 1が男性、2が女性、99が無回答
table(dtp0$SEX, exclude=F)
## 性別（女性）
## 1=女性、0=男性
dtp$josei <- NA
dtp$josei[c(dtp0$SEX)%in%c(2)] <- 1 
dtp$josei[c(dtp0$SEX)%in%c(1)] <- 0
table(dtp$josei, exclude=F)

##選挙区変数##
##選挙区(小選挙区か比例区か)
##1＝小選挙区、2＝比例区
dtp$syosen <- NA
dtp$syosen[c(dtp0$PR)%in%c(0)] <- 1 
dtp$syosen[c(dtp0$PR)%in%c(1)] <- 0
table(dtp$syosen, exclude=F)

#### データの形を変換 ####

## 最初のデータを作る
dtpL <- dtp
dtpL$hyomei <- dtp[,4]
dtpL$topic <- colnames(dtp)[4]
## 表明列を縦に足していく
for(i in 5:39) {
  tmp <- dtp
  tmp$hyomei <- dtp[,i]
  tmp$topic <- colnames(dtp)[i]
  dtpL <- rbind(dtpL,tmp)
}

## トピック変数
##政策質問をカテゴリ分けする。
table(dtpL$topic)
dtpL$topic <- factor(dtpL$topic, levels = unique(dtpL$topic))
table(dtpL$topic)

## 経済・社会保障政策ダミー
##経済・社会保障政策に該当する質問項目を「経済・社会保障政策ダミー」としてまとめる。
dtpL$keizai <- 0
dtpL$keizai[which(dtpL$topic%in%c("tax10","pension","Competitiveness","bank",
                                  "Smallgovernment","Publicworks","Fiscalstimulus",
                                  "Corporatetax","farm","nichigin","Employment",
                                  "Civilservant","economy","domestic","Deficit"))] <- 1

## 安全保障・外交政策ダミー
##外交・安全保障政策に該当する質問項目を「外交・安全保障政策ダミー」としてまとめる。
dtpL$anzen <- 0
dtpL$anzen[which(dtpL$topic%in%c("Defense","northsouth","hikaku","america",
                                 "china","Fiscalstimulus"))] <- 2

##新法案・制度政策ダミー
##新法案・制度政策に該当する質問項目を「新法案・制度政策ダミー」としてまとめる。

dtpL$new <- 0
dtpL$new[which(dtpL$topic%in%c("Privacy","Foreigner","Civilservant","faction",
                               "vote","select","samesexmarriage","Nucleus",
                               "Environment","AI","number","donation",
                               "party","Representative","election"))] <- 3

## 他のダミーも同様に作る

##########
## 分析 ##
##########

## GK: やってること自体はいいのですが、
## 以下順番と仮説のナンバリングが混乱していたので整理しました。

## 仮説１：現職→意見表明低
## 仮説２：自民党→意見表明低
## 仮説３：経済政策→仮説１・２が強まる
## 仮説４：外交政策→仮説１・２が弱まる
## 仮説５：外交政策→仮説１・２が強まる

#install.packages("fixest") #インストールされていなければ1回だけ実行
library(fixest) 
# 同じ回答者と同じ政策に対する設問内で回答が似る傾向があると、
# 推定値のブレ（標準誤差：Standard Error = SE）が過少推定されがちになる。
# それを補正するため、lmの代わりにfeolsを使う。
# ※政策固定効果は、問題が複雑になるので除く。

## 仮説１：基本モデル(現職)
m1b <- feols(hyomei ~ gensyoku, #+ as.factor(topic), 
             cluster = ~ id + topic, # 回答者と争点でSEを補正
             data = dtpL)

## 仮説２：基本モデル(自民党)
m2b <- feols(hyomei ~ jimin, #+ as.factor(topic), 
             cluster = ~ id + topic, # 回答者と争点でSEを補正
             data = dtpL)

## 仮説１・２：交絡変数あり(現職+政党+選挙区+性別)
## （現職と自民党は互いに交絡変数と考えられるので、一緒に検証） 
m12 <- feols(hyomei ~ gensyoku + jimin + syosen + josei, #+ as.factor(topic), 
             cluster = ~ id + topic, # 回答者と争点でSEを補正
             data = dtpL)

## 仮説３－１：基本モデル(現職×経済)
m31b <- feols(hyomei ~ gensyoku*keizai, # + as.factor(topic), 
              cluster = ~ id + topic, # 回答者と争点でSEを補正
              data = dtpL)

## 仮説３－２：基本モデル(自民党×経済)
m32b <- feols(hyomei ~ jimin*keizai, #+ as.factor(topic), 
              cluster = ~ id + topic, # 回答者と争点でSEを補正
              data = dtpL)

## 仮説３－１・２：交絡変数あり(現職×経済+政党×経済+選挙区+性別)
## （現職と自民党は互いに交絡変数と考えられるので、一緒に検証） 
m312 <- feols(hyomei ~ gensyoku*keizai + jimin*keizai + 
                syosen + josei, #+ as.factor(topic), 
              cluster = ~ id + topic, # 回答者と争点でSEを補正
              data = dtpL)

## 仮説４－１：基本モデル(現職×安全保障外交)
m41b <- feols(hyomei ~ gensyoku*anzen, #+ as.factor(topic), 
              cluster = ~ id + topic, # 回答者と争点でSEを補正
              data = dtpL)

## 仮説４－２：基本モデル(自民党×安全保障外交)
m42b <- feols(hyomei ~ jimin*anzen, #+ as.factor(topic), 
              cluster = ~ id + topic, # 回答者と争点でSEを補正
              data = dtpL)

## 仮説４－１・２：交絡変数あり(現職×安保+政党×安保+選挙区+性別)
## （現職と自民党は互いに交絡変数と考えられるので、一緒に検証） 
m412 <- feols(hyomei ~ gensyoku*anzen + jimin*anzen + 
                syosen + josei, #+ as.factor(topic), 
              cluster = ~ id + topic, # 回答者と争点でSEを補正
              data = dtpL)

## 仮説５－１：基本モデル(現職×新法)
m51b <- feols(hyomei ~ gensyoku*new, #+ as.factor(topic), 
              cluster = ~ id + topic, # 回答者と争点でSEを補正
              data = dtpL)

## 仮説５－２：基本モデル(自民党×新法)
m52b <- feols(hyomei ~ jimin*new, #+ as.factor(topic), 
              cluster = ~ id + topic, # 回答者と争点でSEを補正
              data = dtpL)

## 仮説５－１・２：交絡変数あり(現職×新法+政党×新法+選挙区+性別)
## （現職と自民党は互いに交絡変数と考えられるので、一緒に検証） 
m512 <- feols(hyomei ~ gensyoku*new + jimin*new + 
                syosen + josei, #+ as.factor(topic), 
              cluster = ~ id + topic, # 回答者と争点でSEを補正
              data = dtpL)

## 簡易的な結果
library(texreg) #HTML表の作成
library(htmltools) #HTML表の表示
library(webshot2) #HTML表を画像で保存

##仮説１、仮説２の結果、基本モデルと拡張モデル
screenreg(list(m1b, m2b, m12), include.ci = FALSE,
          cluster = ~ id + topic, # 回答者と争点でSEを補正
          include.proj.stats = FALSE, # 不必要な情報を省略
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")
##仮説３、仮説４の結果、基本モデルと拡張モデル
screenreg(list(m31b,m32b,m312), include.ci = FALSE,
          cluster = ~ id + topic, # 回答者と争点でSEを補正
          include.proj.stats = FALSE, # 不必要な情報を省略
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")
##仮説５、仮説６の結果、基本モデルと拡張モデル
screenreg(list(m41b,m42b,m412), include.ci = FALSE,
          cluster = ~ id + topic, # 回答者と争点でSEを補正
          include.proj.stats = FALSE, # 不必要な情報を省略
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")
##仮説７、仮説８の結果、基本モデルと拡張モデル
screenreg(list(m51b, m52b, m512), include.ci = FALSE,
          cluster = ~ id + topic, # 回答者と争点でSEを補正
          include.proj.stats = FALSE, # 不必要な情報を省略
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

## もっときれいな結果
### 変数ラベル
### GK：変数マッピングは一種類で構いません。モデルに
### 当該変数が含まれていなければ、表示されないだけです。
vnmap <- list("(Intercept)" = "（定数項）",
              "gensyoku" = "政治家(現職)",
              "gensyoku:keizai" = "現職×経済政策",
              "gensyoku:anzen" = "現職×安全保障外交政策",
              "gensyoku:new" = "現職×新法政策",
              "jimin" = "政治家(自民)",
              "jimin:keizai" = "自民×経済政策",
              "keizai:jimin" = "自民×経済政策", #たまに順番が逆転するのでこちらもラベルを振る
              "jimin:anzen" = "自民×安全保障外交政策",
              "anzen:jimin" = "自民×安全保障外交政策", #たまに順番が逆転するのでこちらもラベルを振る
              "jimin:new" = "自民×新法政策",
              "new:jimin" = "自民×新法政策", #たまに順番が逆転するのでこちらもラベルを振る
              "keizai" = "経済政策ダミー",
              "anzen" = "安全保障外交政策ダミー",
              "new" = "新法政策ダミー",
              "josei" = "性別（女性）",
              "syosen" = "選挙区")

### HTMLで表を出力

### 仮説１・２
###表6：H1とH2の検証結果
tab6 <- HTML(htmlreg(list(m1b, m2b, m12), include.ci = FALSE,
                       cluster = ~ id + topic, # 回答者と争点でSEを補正
                       include.proj.stats = FALSE, # 不必要な情報を省略
                       digits=3, stars = c(0.001,0.01,0.05,0.1), 
                       symbol="+",  star.symbol = "*", inline.css = FALSE,
                       html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                       single.row = FALSE, caption = "",
                       custom.coef.map = vnmap, 
                       custom.header = list("基本" = 1:2, "拡張" = 3), 
                       custom.model.names =  c("モデル１","モデル２","モデル３"),
                       # custom.gof.rows = list("政策固定効果" = 
                       #                          c("有り","有り","有り")),
                       custom.note = "%stars。OLS回帰分析結果。括弧内はクラスタロバスト標準誤差（回答者＋争点）。"))
## 表を仮表示
browsable(tab6)
## 表をoutに保存する
save_html(tab6, "./out/srtab6.html")
webshot(url="./out/srtab6.html", file="./out/srtab6.png", zoom=3, selector="table")

### 仮説３
###表7：H3とH4の検証結果
tab7 <- HTML(htmlreg(list(m31b, m32b, m312), include.ci = FALSE,
                       cluster = ~ id + topic, # 回答者と争点でSEを補正
                       include.proj.stats = FALSE, # 不必要な情報を省略
                       digits=3, stars = c(0.001,0.01,0.05,0.1), 
                       symbol="+",  star.symbol = "*", inline.css = FALSE,
                       html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                       single.row = FALSE, caption = "",
                       custom.coef.map = vnmap, 
                       custom.header = list("基本" = 1:2, "拡張" = 3), 
                       custom.model.names =  c("モデル４","モデル５","モデル６"),
                       # custom.gof.rows = list("政策固定効果" = 
                       #                          c("有り","有り","有り")),
                       custom.note = "%stars。OLS回帰分析結果。括弧内はクラスタロバスト標準誤差（回答者＋争点）。"))
## 表を仮表示
browsable(tab7)
## 表をoutに保存する
save_html(tab7, "./out/srtab7.html")
webshot(url="./out/srtab7.html", file="./out/srtab7.png", zoom=3, selector="table")

### 仮説４
###表8：H5とH6の検証結果
tab8 <- HTML(htmlreg(list(m41b, m42b, m412), include.ci = FALSE,
                       cluster = ~ id + topic, # 回答者と争点でSEを補正
                       include.proj.stats = FALSE, # 不必要な情報を省略
                       digits=3, stars = c(0.001,0.01,0.05,0.1), 
                       symbol="+",  star.symbol = "*", inline.css = FALSE,
                       html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                       single.row = FALSE, caption = "",
                       custom.coef.map = vnmap, 
                       custom.header = list("基本" = 1:2, "拡張" = 3), 
                       custom.model.names =  c("モデル７","モデル８","モデル９"),
                       # custom.gof.rows = list("政策固定効果" = 
                       #                          c("有り","有り","有り")),
                       custom.note = "%stars。OLS回帰分析結果。括弧内はクラスタロバスト標準誤差（回答者＋争点）。"))
## 表を仮表示
browsable(tab8)
## 表をoutに保存する
save_html(tab8, "./out/srtab8.html")
webshot(url="./out/srtab8.html", file="./out/srtab8.png", zoom=3, selector="table")

### 仮説５
###表9：H7とH8の検証結果
tab9 <- HTML(htmlreg(list(m51b, m52b, m512), include.ci = FALSE,
                       cluster = ~ id + topic, # 回答者と争点でSEを補正
                       include.proj.stats = FALSE, # 不必要な情報を省略
                       digits=3, stars = c(0.001,0.01,0.05,0.1), 
                       symbol="+",  star.symbol = "*", inline.css = FALSE,
                       html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
                       single.row = FALSE, caption = "",
                       custom.coef.map = vnmap, 
                       custom.header = list("基本" = 1:2, "拡張" = 3), 
                       custom.model.names =  c("モデル10","モデル11","モデル12"),
                       # custom.gof.rows = list("政策固定効果" = 
                       #                          c("有り","有り","有り")),
                       custom.note = "%stars。OLS回帰分析結果。括弧内はクラスタロバスト標準誤差（回答者＋争点）。"))
## 表を仮表示
browsable(tab9)
## 表をoutに保存する
save_html(tab9, "./out/srtab9.html")
webshot(url="./out/srtab9.html", file="./out/srtab9.png", zoom=3, selector="table")


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
  
  ## cope with "fixest" class results
  if ("fixest"%in%class(mpr)) {
    mpr$terms <- mpr$fml
    mpr$df <- degrees_freedom(mpr, "t")
  }
  
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
yosokuout1 <- genpr(dpr = dtpL,
                    mpr = m12,
                    setx = "gensyoku", 
                    setxvals = seq(0,1,length=2))
##予測値平均の値を確認する
yosokuout1
## 予測値をプロットする
###図１：H1検証結果
library(ggplot2)
ggplot(yosokuout1) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), 
                width=0.1, linewidth=0.5) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), 
                width=0, linewidth=2) + 
  geom_point(aes(x=as.factor(x), y=pr),color="white") + 
  scale_x_discrete(labels=c("新人・元職","現職")) + ## labelsを設定する
  #geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) + 
  #geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) + 
  #geom_line(aes(x=x, y=pr)) + 
  labs(x="属性",
       y="態度表明度（予測値）", 
       caption = "注：モデル3を使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/srfig1.png", width = 6, height = 4)

## 仮説２ ##

## 予測値の算出
yosokuout2 <- genpr(dpr = dtpL,
                    mpr = m12,
                    setx = "jimin", 
                    setxvals = seq(0,1,length=2))
###予測値平均の値を確認する
yosokuout2 
## 予測値をプロットする
###図2：H2検証結果
library(ggplot2)
ggplot(yosokuout2) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), 
                width=0.1, linewidth=0.5) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), 
                width=0, linewidth=2) + 
  geom_point(aes(x=as.factor(x), y=pr),color="white") + 
  scale_x_discrete(labels=c("非自民","自民")) + ## labelsを設定する
  #geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3) + 
  #geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5) + 
  #geom_line(aes(x=x, y=pr)) + 
  labs(x="属性",
       y="態度表明度（予測値）", 
       caption = "注：モデル3を使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/srfig2.png", width = 6, height = 4)

## 仮説３（現職×経済政策） ##

## 予測値の算出
yosokuout31 <- genpr(dpr = dtpL,
                     mpr = m312,
                     setx = "gensyoku", 
                     setxvals = seq(0,1,length=2),
                     setm = "keizai",
                     setmvals = list("その他の政策(0)" = 0,
                                     "経済政策(1)" = 1))
###予測値平均の値を確認する
yosokuout31
## 予測値をプロットする
###図3：H3検証結果
ggplot(yosokuout31, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels=c("新人・元職","現職")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95,
  #                 fill = labelledm), alpha=0.3) + 
  # geom_ribbon(aes(ymin=lo90, ymax=up90,
  #                 fill = labelledm), alpha=0.5) + 
  # geom_line(aes(linetype=labelledm)) + 
  scale_shape_discrete(name = "政策トピック") + 
  scale_linetype_discrete(name = "政策トピック") + 
  scale_color_brewer(name = "政策トピック", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "政策トピック", 
                    type = "qual", palette = 2) + 
  labs(x="候補者属性",
       y="意見表明度", 
       caption = "注：モデル6を使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/srfig3.png", width = 6, height = 4)

## 仮説３（自民×経済政策） ##

## 予測値の算出
yosokuout32 <- genpr(dpr = dtpL,
                     mpr = m312,
                     setx = "jimin", 
                     setxvals = seq(0,1,length=2),
                     setm = "keizai",
                     setmvals = list("その他の政策(0)" = 0,
                                     "経済政策(1)" = 1))
###予測値平均の値を確認する
yosokuout32
## 予測値をプロットする
###図4：H4検証結果
ggplot(yosokuout32, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels=c("自民党以外の政党","自民党")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95,
  #                 fill = labelledm), alpha=0.3) + 
  # geom_ribbon(aes(ymin=lo90, ymax=up90,
  #                 fill = labelledm), alpha=0.5) + 
  # geom_line(aes(linetype=labelledm)) + 
  scale_shape_discrete(name = "政策トピック") + 
  scale_linetype_discrete(name = "政策トピック") + 
  scale_color_brewer(name = "政策トピック", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "政策トピック", 
                    type = "qual", palette = 2) + 
  labs(x="候補者属性",
       y="意見表明度", 
       caption = "注：モデル6を使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/srfig4.png", width = 6, height = 4)

## 仮説４（現職×安全外交保障政策） ##

## 予測値の算出
yosokuout41 <- genpr(dpr = dtpL,
                     mpr = m412,
                     setx = "gensyoku", 
                     setxvals = seq(0,1,length=2),
                     setm = "anzen",
                     setmvals = list("その他の政策(0)" = 0,
                                     "安全保障外交政策(1)" = 1))
###予測値平均の値を確認する
yosokuout41
## 予測値をプロットする
###図5：H5検証結果
ggplot(yosokuout41, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels=c("新人・元職","現職")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95,
  #                 fill = labelledm), alpha=0.3) + 
  # geom_ribbon(aes(ymin=lo90, ymax=up90,
  #                 fill = labelledm), alpha=0.5) + 
  # geom_line(aes(linetype=labelledm)) + 
  scale_shape_discrete(name = "政策トピック") + 
  scale_linetype_discrete(name = "政策トピック") + 
  scale_color_brewer(name = "政策トピック", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "政策トピック", 
                    type = "qual", palette = 2) + 
  labs(x="候補者属性",
       y="意見表明度", 
       caption = "注：モデル9を使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/srfig5.png", width = 6, height = 4)

## 仮説４（自民×安全外交保障政策） ##

## 予測値の算出
yosokuout42 <- genpr(dpr = dtpL,
                     mpr = m412,
                     setx = "jimin", 
                     setxvals = seq(0,1,length=2),
                     setm = "anzen",
                     setmvals = list("その他の政策(0)" = 0,
                                     "安全保障外交政策(1)" = 1))
###予測値平均の値を確認する
yosokuout42
## 予測値をプロットする
###図6：H6検証結果
ggplot(yosokuout42, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels=c("自民党以外の政党","自民党")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95,
  #                 fill = labelledm), alpha=0.3) + 
  # geom_ribbon(aes(ymin=lo90, ymax=up90,
  #                 fill = labelledm), alpha=0.5) + 
  # geom_line(aes(linetype=labelledm)) + 
  scale_shape_discrete(name = "政策トピック") + 
  scale_linetype_discrete(name = "政策トピック") + 
  scale_color_brewer(name = "政策トピック", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "政策トピック", 
                    type = "qual", palette = 2) + 
  labs(x="候補者属性",
       y="意見表明度", 
       caption = "注：モデル9を使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/srfig6.png", width = 6, height = 4)

## 仮説５（現職×新法政策） ##

## 予測値の算出
yosokuout51 <- genpr(dpr = dtpL,
                     mpr = m512,
                     setx = "gensyoku", 
                     setxvals = seq(0,1,length=2),
                     setm = "new",
                     setmvals = list("その他の政策(0)" = 0,
                                     "新法政策(1)" = 1))
###予測値平均の値を確認する
yosokuout51
## 予測値をプロットする
###図7：H7検証結果
ggplot(yosokuout51, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels=c("新人・元職","現職")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95,
  #                 fill = labelledm), alpha=0.3) + 
  # geom_ribbon(aes(ymin=lo90, ymax=up90,
  #                 fill = labelledm), alpha=0.5) + 
  # geom_line(aes(linetype=labelledm)) + 
  scale_shape_discrete(name = "政策トピック") + 
  scale_linetype_discrete(name = "政策トピック") + 
  scale_color_brewer(name = "政策トピック", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "政策トピック", 
                    type = "qual", palette = 2) + 
  labs(x="候補者属性",
       y="意見表明度", 
       caption = "注：モデル12を使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/srfig7.png", width = 6, height = 4)

## 仮説５（自民×新法政策） ##

## 予測値の算出
yosokuout52 <- genpr(dpr = dtpL,
                     mpr = m512,
                     setx = "jimin", 
                     setxvals = seq(0,1,length=2),
                     setm = "new",
                     setmvals = list("その他の政策(0)" = 0,
                                     "新法政策(1)" = 1))
###予測値平均の値を確認する
yosokuout52
## 予測値をプロットする
###図8：H8検証結果
ggplot(yosokuout52, aes(x=x, y=pr)) + 
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
  scale_x_discrete(labels=c("自民党以外の政党","自民党")) + ## labelsを設定する
  # geom_ribbon(aes(ymin=lo95, ymax=up95,
  #                 fill = labelledm), alpha=0.3) + 
  # geom_ribbon(aes(ymin=lo90, ymax=up90,
  #                 fill = labelledm), alpha=0.5) + 
  # geom_line(aes(linetype=labelledm)) + 
  scale_shape_discrete(name = "政策トピック") + 
  scale_linetype_discrete(name = "政策トピック") + 
  scale_color_brewer(name = "政策トピック", 
                     type = "qual", palette = 2) + 
  scale_fill_brewer(name = "政策トピック", 
                    type = "qual", palette = 2) + 
  labs(x="候補者属性",
       y="意見表明度", 
       caption = "注：モデル12を使用。エラーバーは、90％および95％信頼区間を示している。") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/srfig8.png", width = 6, height = 4)


## ※以下は本文では未使用なので参考 #######################

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

## 限界効果の出力（仮説３　現職×経済政策）
genkaiout3 <- intereff(m312,
                        "gensyoku", 
                        "keizai", c(0,1), nsim=2)
## 限界効果プロット
###H3の限界効果
ggplot(genkaiout3, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("その他の政策","経済社会保障政策")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：意見表明度",
       y="現職であることの限界効果",
       x="政策トピック",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/h3_genkaikoka_plot.png", width = 6, height = 4)

## 限界効果の出力（仮説5　現職×外交安全保障政策）
###H5の限界効果
genkaiout5 <- intereff(m412, 
                        "gensyoku", 
                        "anzen", c(0,1), nsim=2)
## 限界効果プロット
ggplot(genkaiout5, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("その他の政策","外交安全保障政策")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：意見表明度",
       y="現職であることの限界効果",
       x="政策トピック",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/h5_genkaikoka_plot.png", width = 6, height = 4)

## 限界効果の出力（仮説7 現職×新法政策）
###H7の限界効果
genkaiout7 <- intereff(m512,
                        "gensyoku",
                        "new", c(0,1), nsim=2)
## 限界効果プロット
ggplot(genkaiout7, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("その他の政策","新法政策")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：意見表明度",
       y="現職であることの限界効果",
       x="政策トピック",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/h7_genkaikoka_plot.png", width = 6, height = 4)

## 限界効果の出力（仮説4　自民×経済政策）
genkaiout4 <- intereff(m312,
                        "jimin", 
                        "keizai", c(0,1), nsim=2)
## 限界効果プロット
###H4の限界効果
ggplot(genkaiout4, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("その他の政策","経済社会保障政策")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：意見表明度",
       y="自民党であることの限界効果",
       x="政策トピック",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/h4_genkaikoka_plot.png", width = 6, height = 4)

## 限界効果の出力（仮説6 自民×外交安全保障政策）
genkaiout6 <- intereff(m412,
                        "jimin",
                        "anzen", c(0,1), nsim=2)
## 限界効果プロット
###H6の限界効果
ggplot(genkaiout6, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("その他の政策","外交安全保障政策")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：意見表明度",
       y="自民党であることの限界効果",
       x="政策トピック",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/h6_genkaikoka_plot.png", width = 6, height = 4)

## 限界効果の出力（仮説8 自民×新法政策）
genkaiout8 <- intereff(m512,
                        "jimin", 
                        "new", c(0,1), nsim=2)
## 限界効果プロット
###H8の限界効果
ggplot(genkaiout8, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=c("その他の政策","新法政策")) + ## labelsを設定する
  # geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3) + 
  # geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5) + 
  # geom_line(aes(x=mod, y=est)) + 
  labs(subtitle="従属変数：意見表明度",
       y="自民党であることの限界効果",
       x="政策トピック",
       caption="注：拡張モデルを使用。エラーバーは、90％および95％信頼区間を示している。") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
## グラフを保存
ggsave("./out/h8_genkaikoka_plot.png", width = 6, height = 4)


