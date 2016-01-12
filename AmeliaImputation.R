##Amelia
AidData <- read.csv("AidData17.csv", stringsAsFactors=FALSE) #国ごとに考える(intercs=TRUE)ので、Aid_ALLの欠損値が半分以上のものは除外した
#さらに、polity_2 FH の値が期間中一つもないのを除いたものがver.16
#同じことをgwf_regimetypeにしたのがver17
AidData$Population.total <- log(AidData$Population.total) 
AidData$GDP.per.capita.constant.2005.US <- log(AidData$GDP.per.capita.constant.2005.US)
AidData$Aid_All <- log(AidData$Aid_All)
AidData$Aid_Dem <- log(AidData$Aid_Dem)

AidData <- stringTOfactor(AidData)
AidData <- subset(AidData, year<2011) #2011はgwf_regimetypeが揃っていない
summary(AidData[,c(1:52, 53:62,65,68:74)])
bds <- t(matrix(c(33,0,80, 55,0,10, 56,0,100, 57,0,100, 58,0,100, 59,0,100, 
                  60,0,100, 61,0,100, 63,0,100, 64,0,100, 65,0,60, 66,0,100, 67,0,100, 68,0,60,
                  69,0,100), nrow=3, ncol=15)) #boundaryを設定
library(Amelia)
AidData.AllRegime1 <- amelia(AidData[,c(1:52, 53:62,65,69:74)], m=1, ts="year", cs="recipient", 
                             idvars=c("un_region", "un_continent", "un_region.1", "un_continent.1", 
                                      "fh_pr", "fh_cl", "fh_mean", "fh_inverse_mean", "fh_sum", "fh_status",
                                      "fh_inverse_sum", "fh_max", "fh_inverse_min", "fh_inverse_max",
                                      "gwf_next", "gwf_prior", "gwf_party", "gwf_military", "gwf_nonautocracy", "gwf_monarchy", "gwf_personal",
                                      "gwf_disagree","gwf_spell", "gwf_duration",
                                      "cgv_collect",  "cgv_democracy", "cgv_nmil",
                                      "WinningSelec_CountryCode", 
                                      "p4_polity", "p4_flag","p4_democ", "p4_autoc", "p4_xconst", "p4_parcomp", "p4_durable",
                                      "p4_xrreg", "p4_xrcomp", "p4_xropen", "p4_parreg", #nominalで10以上のcategoryになるので無視することに
                                      "Gross.savings.current.US"
                             ),
                             ords=c("p4_polity2", "fh_inverse_pr", "fh_inverse_cl"
                             ),
                             noms=c("cgv_regime", "WinningCoalition", "Selectorate","gwf_regimetype"),
                             polytime=2,
                             bounds=bds, max.resample=5000) 
save(AidData.AllRegime1, file="AidDataImputations_AidData.AllRegime2.RData")
write.amelia(obj=AidData.AllRegime1, file.stem="AidData.AllRegime2")
