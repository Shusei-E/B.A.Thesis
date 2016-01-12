####データが５つ
# 分布を合成する
#これが参考になりそう http://stat.biopapyrus.net/ggplot/geom-density.html
fit.coda1 <- fit.coda
fit.coda2 <- fit.coda
fit.coda3 <- fit.coda
fit.coda4 <- fit.coda
fit.coda5 <- fit.coda

#Coef ineq
S1data1 <- data.frame(
  Data1 = as.matrix(fit.coda1[, c("ineq[1]")]),
  Data2 = as.matrix(fit.coda2[, c("ineq[1]")]),
  Data3 = as.matrix(fit.coda3[, c("ineq[1]")]),
  Data4 = as.matrix(fit.coda4[, c("ineq[1]")]),
  Data5 = as.matrix(fit.coda5[, c("ineq[1]")])
)
colnames(S1data1) <- c("Data1", "Data2", "Data3", "Data4", "Data5") # S1data1 <- data.frame(Data1, Data2, Data3)
# データを ggplot 用に変換
df <- melt(S1data1) 
qplot(df[,2], geom="histogram", main = "Histogram for ineq(Non-Dem)", 
      xlab = "coefficient",  fill=I("#cd5c5c"))
mean(S1data1 > 0)

S1data2 <- data.frame(
  Data1 = as.matrix(fit.coda1[, c("ineq[2]")]),
  Data2 = as.matrix(fit.coda2[, c("ineq[2]")]),
  Data3 = as.matrix(fit.coda3[, c("ineq[2]")]),
  Data4 = as.matrix(fit.coda4[, c("ineq[2]")]),
  Data5 = as.matrix(fit.coda5[, c("ineq[2]")])
)
colnames(S1data2) <- c("Data1", "Data2", "Data3", "Data4", "Data5")
# データを ggplot 用に変換
df <- melt(S1data2) 
qplot(df[,2], geom="histogram", main = "Histogram for ineq(Dem)", 
      xlab = "coefficient",  fill=I("#6495ed"))
mean(S1data2 > 0)

#Coef All Aid
S2data1 <- data.frame(
  Data1 = as.matrix(fit.coda1[, c("AidAll[1]")]),
  Data2 = as.matrix(fit.coda2[, c("AidAll[1]")]),
  Data3 = as.matrix(fit.coda3[, c("AidAll[1]")]),
  Data4 = as.matrix(fit.coda4[, c("AidAll[1]")]),
  Data5 = as.matrix(fit.coda5[, c("AidAll[1]")])
)
colnames(S2data1) <- c("Data1", "Data2", "Data3", "Data4", "Data5")
# データを ggplot 用に変換
df <- melt(S2data1) 
qplot(df[,2], geom="histogram", main = "Histogram for AidAll(Non-Dem)", 
      xlab = "coefficient",  fill=I("#cd5c5c"))
mean(S2data1 > 0)

S2data2 <- data.frame(
  Data1 = as.matrix(fit.coda1[, c("AidAll[2]")]),
  Data2 = as.matrix(fit.coda2[, c("AidAll[2]")]),
  Data3 = as.matrix(fit.coda3[, c("AidAll[2]")]),
  Data4 = as.matrix(fit.coda4[, c("AidAll[2]")]),
  Data5 = as.matrix(fit.coda5[, c("AidAll[2]")])
)
colnames(S2data2) <- c("Data1", "Data2", "Data3","Data4", "Data5")
# データを ggplot 用に変換
df <- melt(S2data2) 
qplot(df[,2], geom="histogram", main = "Histogram for AidAll(Dem)", 
      xlab = "coefficient",  fill=I("#6495ed"))
mean(S2data2 > 0)

#Intercept Stage1
S1data1a <- data.frame(
  Data1 = as.matrix(fit.coda1[, c("alpha[1]")]),
  Data2 = as.matrix(fit.coda2[, c("alpha[1]")]),
  Data3 = as.matrix(fit.coda3[, c("alpha[1]")]),
  Data4 = as.matrix(fit.coda4[, c("alpha[1]")]),
  Data5 = as.matrix(fit.coda5[, c("alpha[1]")])
)
colnames(S1data1a) <- c("Data1", "Data2", "Data3", "Data4", "Data5") # S1data1 <- data.frame(Data1, Data2, Data3)

# データを ggplot 用に変換
df <- melt(S1data1a) 
qplot(df[,2], geom="histogram", main = "Histogram for ineq(Non-Dem)", 
      xlab = "coefficient",  fill=I("#cd5c5c"))
mean(S1data1a > 0)

S1data2a <- data.frame(
  Data1 = as.matrix(fit.coda1[, c("alpha[2]")]),
  Data2 = as.matrix(fit.coda2[, c("alpha[2]")]),
  Data3 = as.matrix(fit.coda3[, c("alpha[2]")]),
  Data4 = as.matrix(fit.coda4[, c("alpha[2]")]),
  Data5 = as.matrix(fit.coda5[, c("alpha[2]")])
)
colnames(S1data2a) <- c("Data1", "Data2", "Data3", "Data4", "Data5")

# データを ggplot 用に変換
df <- melt(S1data2a) 
qplot(df[,2], geom="histogram", main = "Histogram for ineq(Dem)", 
      xlab = "coefficient",  fill=I("#6495ed"))
mean(S1data2a > 0)

#Intercept Stage2
S2data1a <- data.frame(
  Data1 = as.matrix(fit.coda1[, c("alpha[1]")]),
  Data2 = as.matrix(fit.coda2[, c("alpha[1]")]),
  Data3 = as.matrix(fit.coda3[, c("alpha[1]")]),
  Data4 = as.matrix(fit.coda4[, c("alpha[1]")]),
  Data5 = as.matrix(fit.coda5[, c("alpha[1]")])
)
colnames(S2data1a) <- c("Data1", "Data2", "Data3", "Data4", "Data5")

# データを ggplot 用に変換
df <- melt(S2data1a) 
qplot(df[,2], geom="histogram", main = "Histogram for AidAll(Non-Dem)", 
      xlab = "coefficient",  fill=I("#cd5c5c"))
mean(S2data1a > 0)

S2data2a <- data.frame(
  Data1 = as.matrix(fit.coda1[, c("alpha[2]")]),
  Data2 = as.matrix(fit.coda2[, c("alpha[2]")]),
  Data3 = as.matrix(fit.coda3[, c("alpha[2]")]),
  Data4 = as.matrix(fit.coda4[, c("alpha[2]")]),
  Data5 = as.matrix(fit.coda5[, c("alpha[2]")])
)
colnames(S2data2a) <- c("Data1", "Data2", "Data3", "Data4", "Data5")

# データを ggplot 用に変換
df <- melt(S2data2a) 
qplot(df[,2], geom="histogram", main = "Histogram for AidAll(Dem)", 
      xlab = "coefficient",  fill=I("#6495ed"))
mean(S2data2a > 0)


##### violin plotを試みる cf. http://tjo.hatenablog.com/entry/2014/01/27/235048
#####Dataが5つ
#Stage1 Coef
bs<-data.frame(Democracy= as.vector(t(S1data2)), Non_Democracy=as.vector(t(S1data1))) #データの種類はここで調整
bs.melt<-melt(bs,id=c(),variable.name="Regime")
bs.qua.melt<-ddply(bs.melt,.(Regime),summarize,
                   median=median(value),
                   ymax=quantile(value,prob=0.975),
                   ymin=quantile(value,prob=0.025))
colnames(bs.qua.melt)[2]<-"value"
N.mcmc <- 10000
bs.melt<-data.frame(bs.melt,ymax=rep(0,N.mcmc),ymin=rep(0,N.mcmc))  
g<-ggplot(bs.melt,aes(x=Regime,y=value,group=Regime,ymax=ymax,ymin=ymin,color=Regime))
g<-g+geom_violin(trim=F,fill="#5B423D",linetype="blank",alpha=I(1/3), adjust=2)
g<-g+geom_pointrange(data=bs.qua.melt,size=0.75)
g<-g + labs(x="",y="") + theme_bw()  + theme(axis.text.x=element_text(size=13), axis.text.y=element_text(size=13))
g<-g + ggtitle("Coefficient of Inequality on Polity Score")
#g<-g + ggtitle("Intercept on Polity Score") 
g<-g+geom_abline(intercept=0,slope=-0)  + annotate("text",label="74.2%\n  (coefficient>0)",x=1.00,y=0.022) 
plot(g)

#Stage2 Coef
bs<-data.frame(Democracy= as.vector(t(S2data2)), Non_Democracy=as.vector(t(S2data1)))
bs.melt<-melt(bs,id=c(),variable.name="Regime")
bs.qua.melt<-ddply(bs.melt,.(Regime),summarize,
                   median=median(value),
                   ymax=quantile(value,prob=0.975),
                   ymin=quantile(value,prob=0.025))
colnames(bs.qua.melt)[2]<-"value"
N.mcmc <- 10000
bs.melt<-data.frame(bs.melt,ymax=rep(0,N.mcmc),ymin=rep(0,N.mcmc))  
g<-ggplot(bs.melt,aes(x=Regime,y=value,group=Regime,ymax=ymax,ymin=ymin,color=Regime))
g<-g+geom_violin(trim=F,fill="#5B423D",linetype="blank",alpha=I(1/3))
g<-g+geom_pointrange(data=bs.qua.melt,size=0.75) + annotate("text",label="1.5%\n  (coefficient<0)",x=1.78,y=-0.140)
#g<-g+geom_pointrange(data=bs.qua.melt,size=0.75) + annotate("text",label="1.5%\n  (intercept<0)",x=1.38,y=-2.80)
g<-g + labs(x="",y="") + theme_bw()  + theme(axis.text.x=element_text(size=13), axis.text.y=element_text(size=13))
g<-g + ggtitle("Coefficient of All_Aid on Inequality")
#g<-g + ggtitle("Intercept of All_Aid on Inequality")
g<-g+geom_abline(intercept=0,slope=-0)
plot(g)
