# Stan Version Up, Online

#Offlineでの前処理
AidData <- readyData("AidData.AllRegime21.csv")
write.csv(AidData, "/Users/S/Dropbox/Study/Paper/OnlineRstudio/AidData1.csv", row.names=FALSE)
#ここからOnlineで読み込み
AidData <- read.csv("/Users/S/Dropbox/Study/Paper/OnlineRstudio/AidData1.csv", row.names=FALSE)

rm(list = ls())
gc() #念のためにメモリを綺麗にしておく
library(rstan)
library(coda)
library(parallel)


# 自分のパソコンで整形したデータを読み込むのならここから
setwd("~/Dropbox/Study/Paper/")
AidDataTemp <- AidData
AidData <- read.csv("StanData1.csv", header=T)

Aid_All <- AidData$Aid_All
p4_polity2 <- AidData$p4_polity2
polity_lag <- AidData$polity_lag
Giniall <- AidData$Giniall
Corruption <- AidData$Corruption
GDP <- log(AidData$GDP.per.capita.constant.2005.US)
TotalRents <- AidData$Total.natural.resources.rents.of.GDP
Population <- log(AidData$Population.total)
regime1 <- AidData$regime1 + 1  #Dem=2, NonDem=1

WinningCoalition <- AidData$WinningCoalition
Selectorate <- AidData$Selectorate

time = length(Aid_All) #Time length
n = length(Aid_All) #国の数
L = length(unique(regime1)) # グループの数

data<- list(n=n, time=time, L=L, ll=regime1, Aid_All=Aid_All, p4_polity2=p4_polity2, polity_lag=polity_lag, Population=Population,
            Giniall=Giniall, Corruption=Corruption, GDP=GDP, TotalRents=TotalRents,
            #regime1=regime1, regime1_aid=regime1_aid,
            WinningCoalition=WinningCoalition, Selectorate=Selectorate #D=2
)
#y1, y2, y3という列が個体、で50個の行はt時点の観測
#それを変数xについても同じように作っている。(列が3個体を示しており、行がt時点での観測)

#polity_lagを使うことでAR(1)になっている
#別ファイルのモデルを実行
source("StanModelStage1.R")
source("StanModelStage2.R")

stan_fit <- stan(model_name="AR(1) stan", model_code = stancode, data=data, chains=0)
#save(stan_fit, file="~/StanCompiledD60Stage2.RData", compress=TRUE, compression_level=9)

time_before <- proc.time()
sflist <- mclapply(1:3, mc.cores = 4,
                   function(i) stan(
                     fit=stan_fit, data=data,
                     #pars=c("ineq", "AidAll", "reg2", "reg3", "reg4", "reg5", "reg6", "regaid2", "regaid3", "regaid4", "regaid5", "regaid6"),
                     iter=50000, warmup=25000, thin=1,
                     seed = i, chains = 1, chain_id = i, refresh = -1
                   )
)
fit <- sflist2stanfit(sflist)
rm(sflist) #細くメモリの管理
gc()
calc_time <- proc.time() - time_before
#save(fit, file="~/Dropbox/Study/Paper/OnlineRStudio/StanFit_1.RData")
#save.image("~/StanResults_D62Stage1.RData")

fit.coda <- mcmc.list(lapply(1:ncol(fit),function(x) mcmc(as.array(fit)[,x,])))
rm(fit)
gc()
#fit.coda.mc <- to.mcmc(fit.coda)
save(fit.coda, file="~/StanResults_D232DifIncpStage2.RData", compress=TRUE, compression_level=9)
#plot(fit.coda) #plotは自分のPCで確認
#plot(fit.coda[, c("ineq", "AidAll", "reg1", "regaid1")])
#plot(fit.coda[, c("ineq[1]")])
#plot(fit.coda[, c("ineq[2]")])
#plot(fit.coda[, c("alpha[1]")])
#plot(fit.coda[, c("alpha[2]")])
#plot(fit.coda[, c("pol_log")])
#plot(fit.coda[, c("gdp")])
#plot(fit.coda[, c("AidAll[1]")])
#plot(fit.coda[, c("AidAll[2]")])
#plot(fit.coda[, c("reg1")])
#plot(fit.coda[, c("regaid1")])

#Dem=2, NonDem=1
summary(fit.coda[, c("ineq[1]", "ineq[2]")])
gelman.diag(fit.coda[, c("ineq[1]", "ineq[2]")])

summary(fit.coda[, c("AidAll[1]", "AidAll[2]")])
gelman.diag(fit.coda[, c("AidAll[1]", "AidAll[2]")])

summary(fit.coda)
gelman.diag(fit.coda)

mean(as.matrix(fit.coda[, c("ineq[1]")]) > 0) #緑本 p.213
mean(as.matrix(fit.coda[, c("ineq[2]")]) > 0)

geweke.diag(fit.coda[, c("ineq")])
raftery.diag(fit.coda[, c("ineq", "pol_lag", "population", "gdp", "rents")])

traceplot(fit, pars = c("ineq", "AidAll"), inc_warmup = TRUE)
