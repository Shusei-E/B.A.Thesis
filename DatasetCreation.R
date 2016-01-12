########全てのaidをベースにdatasetの構築を行うことにする

rm(list = ls())
setwd("/Users/S/Dropbox/Study/Paper/")
# Read Raw Data
AidData <- read.csv("aiddata2_1_donor_recipient_year_purpose.csv")

# Making dataset for Dem Aid
Agg2_2 <- aggregate(AidData$commitment_usd_constant_sum ~ AidData$recipient + AidData$year + 
                      AidData$coalesced_purpose_name + AidData$coalesced_purpose_code, FUN="sum")
write.csv(Agg2_2, "aiddata_country_year_purpose.csv")


Agg_Demrelated_2 <- subset(Agg2_2, (Agg2_2[4]=="15130" | Agg2_2[4]== "15140" | 
                                      Agg2_2[4]== "15161" | Agg2_2[4]== "15162" | Agg2_2[4]== "15163") ) # For Dem Aid 
colnames(Agg_Demrelated_2) <- c("recipient","year","coalesced_purpose_name","coalesced_purpose_code","commitment_usd_constant_sum")

Agg_cleaned <- aggregate(Agg_Demrelated_2$commitment_usd_constant_sum ~ Agg_Demrelated_2$year + Agg_Demrelated_2$recipient , FUN="sum")
colnames(Agg_cleaned) <- c("year","recipient","commitment_usd_constant_sum")
write.csv(Agg_cleaned, "aiddata.csv")


# Making dataset for All Aid
Agg2_2 <- read.csv("aiddata_country_year_purpose.csv")
colnames(Agg2_2) <- c("rnum","recipient","year","coalesced_purpose_name","coalesced_purpose_code","commitment_usd_constant_sum")
Agg_cleaned <- aggregate(Agg2_2$commitment_usd_constant_sum ~ Agg2_2$year + Agg2_2$recipient , FUN="sum")
colnames(Agg_cleaned) <- c("year","recipient","commitment_usd_constant_sum")
#AidData <- subset(Agg_cleaned,(Agg_cleaned[1] >= 1968)) # 2の処理ができるように名前を変えたかった
# Dem Aidは1968年スタートゆえ、こちらもそれに合わせる


#######################################################################
# 2. 欠けている年をNAで入れる
# まず、Dem AidだけまとめたものにたいしてNAを入れる処理をした。
#次に、それとは独立にAll AidのデータについてNAを入れる処理をした。

# 事前にExcellで国ごと、year順にまとめておく
AidData <- read.csv("aiddata.csv", header =TRUE)

# 重複なしで国名を取り出したい
recipient_list <- unique(AidData$recipient)

# yearはどこからどこまでか
summary(AidData$year) # 1968-2011

# 欠けている年はNAとして追加していく
AidData_comp <- data.frame(year = NULL, recipient = NULL,commitment_usd_constant_sum = NULL )

for (i in recipient_list){ #各国に対して処理
  check_dataframe <- subset(AidData, AidData$recipient == i) # 今から処理する国
  
  for (m in 1:51){ # 2011-1961+1=51
    check_length <- length(check_dataframe$year) # Dataframeは追加されていくのでこのタイミングでチェック
    check_year = m+1960
    if (check_dataframe[m,"year"] != check_year | is.na(check_dataframe[m,"year"]) == TRUE){
      # 挿入がよくわからないので、データフレームを2つに分割してrbind()する
      temp <- m-1
      check_dataframe_upper <- check_dataframe[0:temp,]
      temp <- m+1
      if (m <= check_length){ # mが最後の行の時だけは違う処理をする(else以下)
        check_dataframe_lower <- check_dataframe[m:check_length,]
        AidData_add <- data.frame(year = check_year, recipient = i,commitment_usd_constant_sum = NA )
        check_dataframe <- rbind(check_dataframe_upper, AidData_add, check_dataframe_lower)
      }else{
        AidData_add <- data.frame(year = check_year, recipient = i,commitment_usd_constant_sum = NA )
        check_dataframe <- rbind(check_dataframe_upper, AidData_add)
      } # close if(m =< check_length)
      
    } # close if (check_dataframe[m,"year"] != check_year | is.na(check_dataframe[m,"year"]) == TRUE)
    
  } # close m 
  
  AidData_comp <- rbind(AidData_comp, check_dataframe)
} # close i

write.csv(AidData_comp, "aiddata_wNA_61-11.csv") # For Aid Dem
write.csv(AidData_comp, "aiddata_wNA_ALL_61-11.csv") # For Aid ALL

#######################################################################
# DemだけのAidデータとAll Aidのデータをまとめる
rm(list = ls())
Aid_Dem <- read.csv("aiddata_wNA_61-11.csv")
colnames(Aid_Dem) <- c("rnum","year","recipient","Aid_Dem")
Aid_All <- read.csv("aiddata_wNA_ALL_61-11.csv")
colnames(Aid_All) <- c("rnum","year","recipient","Aid_All")

nameChecker(Aid_All$recipient, Aid_Dem$recipient) #下にある自作の関数, # list1にはあって、list2にはないものを取り出す

length(unique(Aid_Dem$recipient)) ; length(unique(Aid_All$recipient))
# ２つのデータのrecipientの数は微妙に違うがAid_Allの方に合わせる
Country_AidAll <- unique(Aid_All$recipient)
Country_AidDem <- unique(Aid_Dem$recipient)


Aid_Allext <- data.frame(rnum= NULL, year = NULL, recipient = NULL, Aid_All = NULL )
for (i in 1:238){ # length(unique(Aid_All$recipient)) = 238
  extract_rownum <- is.element(as.character(Aid_Dem$recipient), as.character(Country_AidAll[i])) # x 中の各要素は集合 y に含まれるか否か
  
  if (table(extract_rownum)["FALSE"] == 10455){ # 各国51年分のデータゆえ、AidAllの国がAidDemにもあれば全体-51=10455個がFalseになる
    temp <- subset(Aid_Dem, extract_rownum) # AidDemにある国だけ抜き出してデータフレームを作る
    rownames(temp) <- NULL
    # subsetは論理値しかとれないので注意
  }else{
    #ここに来ているということは、その国がないということ
    dyear <- rep(1961:2011)
    temp <- data.frame(rnum= i, year = dyear, recipient = as.character(Country_AidAll[i]), Aid_Dem = NA )
    print(as.character(Country_AidAll[i]))
  }
  
  Aid_Allext <- rbind(Aid_Allext, temp)
}

length(unique(Aid_Allext$recipient))
summary((as.character(unique(Aid_Allext$recipient)) == as.character(unique(Aid_All$recipient))))
#全てTRUEとなったので順番も完全一致しているとわかった

AidData <- cbind(Aid_All, Aid_Dem=Aid_Allext$Aid_Dem)
write.csv(AidData, "AidData_AllDem_61-11.csv")

#######################################################################
# Polityの追加 国の名前を揃えて上と同じ方法で？

nameChecker <- function(list1, list2){
  # list1にはあって、list2にはないものを取り出す 
  list1 <- unique(list1)
  list2 <- unique(list2)
  
  vecnum <- is.element(unique(list1), unique(list2))
  subset(list1, !vecnum)
}

Polity4 <- read.csv("p4v2013.csv")
Polity_61to11 <- subset(Polity4, (year>=1961 & year <= 2011))
nameChecker(AidData$recipient, Polity4$country) # AidDataにあってPolityにない
nameChecker(Polity4$country, AidData$recipient) # PolityにあってAidDataにない
nameChecker(Polity_61to11$country, AidData$recipient) # Polity_61to11にあってAidDataにない

# あとは手動で合わせる。(AidDataを基準にした)

# データを読み込み直す
rm(list = ls())
AidData <- read.csv("AidData_AllDem_61-11_Renamed.csv")
Polity <- read.csv("p4v2013_Renamed.csv")
Polity <- subset(Polity, (year>=1961 & year <= 2011)) #年の調整
nameChecker(Polity$country, AidData$recipient) # PolityにあってAidDataにない
nameChecker(AidData$recipient, Polity$country) # AidDataにあってPolityにない

# もう一つの自作関数を利用
AidData <- CountryYearMatching(Data1 = AidData,
                               CountryColumn1 = AidData$recipient,
                               YearColumn1 = AidData$year,
                               Data2 = Polity[,1:18],
                               CountryColumn2 = Polity$country,
                               YearColumn2 = Polity$year,
                               AddColumn = 6:18)

write.csv(AidData, "AidData_w.Polity.csv")
#######################################################################
# Freedomhouseの追加 1972-
# 公式のは使いにくかったので、http://acrowinghen.com/data/ を参考にした

FreedomHouse <- read.csv("fredomhouse_72-14_Renamed.csv")
nameChecker(FreedomHouse$country, AidData$recipient) # FreedomHouseにあってAidDataにない
nameChecker(AidData$recipient, FreedomHouse$country) # AidDataにあってFreedomHouseにない

AidData <- CountryYearMatching(Data1 = AidData,
                               CountryColumn1 = AidData$recipient,
                               YearColumn1 = AidData$year,
                               Data2 = FreedomHouse,
                               CountryColumn2 = FreedomHouse$country,
                               YearColumn2 = FreedomHouse$year,
                               AddColumn = 6:18)
write.csv(AidData, "AidData_w.Polity.FreedomH.csv") #これをver1としても複製して以下どんどん追加していく
#######################################################################
# Gini Index ＜欠損値が非常に多い＞
rm(list = ls())
AidData <- read.csv("AidData_w.Polity.FreedomH.csv")
library(foreign)
Gini <- read.dta("/Users/S/Dropbox/Study/Paper/WorldBankData/Gini/allginis_Oct2014.dta")
Gini <- Gini[,c(1,3,5,56)]
write.csv(Gini, "Gini.csv") #これを手作業で修正していく

Gini <- read.csv("/Users/S/Dropbox/Study/Paper/WorldBankData/Gini/Gini_renamed.csv")
nameChecker(AidData$recipient, Gini$country) #AidDataにあって、Giniにないもの
AidData2 <- CountryYearMatching(Data1 = AidData,
                                CountryColumn1 = AidData$recipient,
                                YearColumn1 = AidData$year,
                                Data2 = Gini,
                                CountryColumn2 = Gini$country,
                                YearColumn2 = Gini$year,
                                AddColumn = 3:4)
#regionはカットしておく
AidData2 <- AidData2[,1:32]

#write.csv(AidData2, "AidData.ver2.csv")
# 変数にp4_やfh_をつけて見やすくした
#######################################################################
#Regime Data
rm(list = ls())

#Geddes Regime Data
library(foreign)
RegimeData <- read.dta("/Users/S/Dropbox/Study/Paper/RegimeData/GWFRegime/GWF_AllPoliticalRegimes.dta")
RegimeData <- subset(RegimeData, (year>=1961 & year <= 2011)) #年の調整

#対応するコードがあるので、Chibub(-08まで)のregimedataと合体させる
# cf. UN Regional Code: http://unstats.un.org/unsd/methods/m49/m49regin.htm#asia
CGV <- read.csv("/Users/S/Dropbox/Study/Paper/RegimeData/Chibub/ddrevisited_data_v1.csv")
RegimeData2 <- CountryYearMatching(Data1 = RegimeData,
                                   CountryColumn1 = RegimeData$cowcode,
                                   YearColumn1 = RegimeData$year,
                                   Data2 = CGV,
                                   CountryColumn2 = CGV$cowcode,
                                   YearColumn2 = CGV$year,
                                   AddColumn = 3:9)
RegimeData3 <- RegimeData2[,c(2:16,18:23)] 
#write.csv(RegimeData3, "/Users/S/Dropbox/Study/Paper/RegimeData/Regimes_GWF_CGV.csv")

rm(list = ls())
AidData <- read.csv("AidData.ver2.csv") #オリジナル、実際は順番が前後してVer4を用いてる
AidData <- AidData[,3:33]#余計な列を消す
RegimeData <- read.csv("/Users/S/Dropbox/Study/Paper/RegimeData/Regimes_GWF_CGV.csv")
RegimeData <- RegimeData[,2:22]#余計な列を消す

nameChecker(AidData$recipient, RegimeData$gwf_country)
#ここからは手動で名前を修正
RegimeData <- read.csv("/Users/S/Dropbox/Study/Paper/RegimeData/Regimes_GWF_CGV_Rename.csv", stringsAsFactors=FALSE) #もう一度読み込み
AidData2 <- CountryYearMatching(Data1 = AidData,
                                CountryColumn1 = AidData$recipient,
                                YearColumn1 = AidData$year,
                                Data2 = RegimeData,
                                CountryColumn2 = RegimeData$gwf_country,
                                YearColumn2 = RegimeData$year,
                                AddColumn = 4:21)
#write.csv(AidData2, "AidData.ver3.csv")
# un-country / region　codeは不完全なものがあったので手動で修正
# 修正したものがver.4
Data <- easyFillUp(Data=Data, 
                   MatchObjectColNum=2,
                   WhatToMatch=3)
AidData <- read.csv("AidData.ver5.csv", stringsAsFactors=FALSE)
#######################################################################
# Calculate Winning Coalition and Selectolate size
# psDataという関数にWinset Creatorがあるのでそれを使わせてもらう
library(psData)
Winset <- WinsetCreator(PolityUrl = "http://www.systemicpeace.org/inscr/p4v2013.sav",
                        DpiUrl = "http://siteresources.worldbank.org/INTRES/Resources/469232-1107449512766/DPI2012.dta", 
                        OutCountryID = "iso2c", na.rm = TRUE)
#write.csv(Winset,"Winset.csv",row.names = FALSE)

WinsetData <- read.csv("Winset.csv", stringsAsFactors=FALSE)

nameChecker(AidData$recipient, Winset$country) #AidDataにあって、Winsetにないもの
WinsetData <- read.csv("Winset_Renamed.csv", stringsAsFactors=FALSE)
AidData2 <- CountryYearMatching(Data1 = AidData,
                                CountryColumn1 = AidData$recipient,
                                YearColumn1 = AidData$year,
                                Data2 = WinsetData,
                                CountryColumn2 = WinsetData$country,
                                YearColumn2 = WinsetData$year,
                                AddColumn = 3:5)
AidData3 <- easyFillUp(Data=AidData2, 
                       MatchObjectColNum=2, 
                       WhatToMatch=52)
#write.csv(AidData3,"AidData.ver6.csv",row.names = FALSE)
rm(list = ls())
AidData <- read.csv("AidData.ver6.csv", stringsAsFactors=FALSE)
#######################################################################
#Corruption Data
CPI <- read.csv("/Users/S/Dropbox/Study/Paper/CorruptionPerceptionIndex/CPIFull.csv", stringsAsFactors=FALSE)
CPI2 <- CPI[,c(1,2,37:3)]

CPI2[,2] <- c("Corruption")
CPI3 <- reshapeTSCS(Data=CPI2, 
                    CountryColumn=CPI2$country, 
                    YearColumn=CPI2[,3:37], 
                    VariableNamesColumn=CPI2$Jurisdiction)

library(stringi) #国名の空白を削除
CPI3$country <- stri_trim_right(CPI3$country)
#write.csv(CPI3,"/Users/S/Dropbox/Study/Paper/CorruptionPerceptionIndex/CPIFull_Reshaped.csv",row.names = FALSE)

nameChecker(AidData$recipient, CPI3$country) #AidDataにあってCorruptionにない
CPIRename <- read.csv("/Users/S/Dropbox/Study/Paper/CorruptionPerceptionIndex/CPIFull_Reshaped_Renamed.csv", stringsAsFactors=FALSE)
CPI4 <- CountryYearMatching(Data1 = AidData,
                            CountryColumn1 = AidData$recipient,
                            YearColumn1 = AidData$year,
                            Data2 = CPIRename,
                            CountryColumn2 = CPIRename$country,
                            YearColumn2 = CPIRename$year,
                            AddColumn = 3:4) 
CPI5 <- CPI4[,-56] #CountryYearMatchingは2列以上じゃないといけないので、意図的に不要な列を加えていた
#write.csv(CPI5,"AidData.ver7.csv",row.names = FALSE)

#######################################################################
# Data From World Bank
#先にWorld Bank系を全て一緒にしてからAidDataに統合する
rm(list = ls())
AidData <- read.csv("AidData.ver7.csv", stringsAsFactors=FALSE)

# EducationHealthの整形(年が列に来ているタイプだった)
rm(list = ls())
EH <- read.csv("/Users/S/Dropbox/Study/Paper/WorldBankData/EducationHealth/EducationHealth74-11.csv")
EHreplace <- reshapeTSCS(Data=EH, 
                         CountryColumn=EH$country, 
                         YearColumn=EH[,3:40], 
                         VariableNamesColumn=EH$Variables)
#write.csv(EHreplace, "/Users/S/Dropbox/Study/Paper/WorldBankData/EducationHealth/EducationHealth74-11Replace.csv")

# PovertyInequalityの整形(変える必要なし)
rm(list = ls())
PI <- read.csv("/Users/S/Dropbox/Study/Paper/WorldBankData/PovertyInequality/PovertyInequality74-11.csv")
summary(PI)
#どちらも欠損値が多くて使い物にならなそう(Populationくらい)

#WorldDevelopmentIndicator 色々入ってる
WDI <- read.csv("/Users/S/Dropbox/Study/Paper/WorldBankData/WorldDevelopmentIndicator/WDI.csv")
WDIreshape <- reshapeTSCS(Data=WDI, 
                          CountryColumn=WDI$country, 
                          YearColumn=WDI[,3:53], 
                          VariableNamesColumn=WDI$variables)
#write.csv(WDIreshape, "/Users/S/Dropbox/Study/Paper/WorldBankData/WorldDevelopmentIndicator/WDIreshape.csv")

#とりえずWDIを追加
WDI <- read.csv("/Users/S/Dropbox/Study/Paper/WorldBankData/WorldDevelopmentIndicator/WDIreshape.csv", stringsAsFactors=FALSE)
nameChecker(AidData$recipient, WDI$country)
#名前を手動で整える
WDI <- read.csv("/Users/S/Dropbox/Study/Paper/WorldBankData/WorldDevelopmentIndicator/WDIreshape_Renamed.csv", stringsAsFactors=FALSE)
AidData2 <- CountryYearMatching(Data1 = AidData,
                                CountryColumn1 = AidData$recipient,
                                YearColumn1 = AidData$year,
                                Data2 = WDI,
                                CountryColumn2 = WDI$country,
                                YearColumn2 = WDI$year,
                                AddColumn = 3:21) 
#write.csv(AidData2,"AidData.ver8.csv",row.names = FALSE)