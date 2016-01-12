#係数の推定値 cf. https://gist.github.com/dsparks/4332698
#######
#1段目
##Demについて
formula <- p4_polity2 ~ polity_lag + log(GDP.per.capita.constant.2005.US) + log(Population.total) + Total.natural.resources.rents.of.GDP +
  Giniall*regime1
model1 <- lm(formula, data = AidData1)
model2 <- lm(formula, data = AidData2)
model3 <- lm(formula, data = AidData3) 
model4 <- lm(formula, data = AidData4)
model5 <- lm(formula, data = AidData5)

model1Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model1$coef[6] + model1$coef[8]*1,
                          SE = sqrt(vcov(model1)[6,6] + 1^2 * vcov(model1)[8,8] + 2*1*vcov(model1)[6,8]),
                          Dataset = "Imputed Data1")
model2Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model2$coef[6] + model2$coef[8]*1,
                          SE = sqrt(vcov(model2)[6,6] + 1^2 * vcov(model2)[8,8] + 2*1*vcov(model2)[6,8]),
                          Dataset = "Imputed Data2")
model3Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model3$coef[6] + model3$coef[8]*1,
                          SE = sqrt(vcov(model3)[6,6] + 1^2 * vcov(model3)[8,8] + 2*1*vcov(model3)[6,8]),
                          Dataset = "Imputed Data3")
model4Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model4$coef[6] + model4$coef[8]*1,
                          SE = sqrt(vcov(model4)[6,6] + 1^2 * vcov(model4)[8,8] + 2*1*vcov(model4)[6,8]),
                          Dataset = "Imputed Data4")
model5Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model5$coef[6] + model5$coef[8]*1,
                          SE = sqrt(vcov(model5)[6,6] + 1^2 * vcov(model5)[8,8] + 2*1*vcov(model5)[6,8]),
                          Dataset = "Imputed Data5")
model6Frame <- data.frame(Regime = "in\nDemocracy",  #先にAmeliaのデータを合わせておく(functionあり)
                          Coefficient = combined.results1$q.mi[6] + combined.results1$q.mi[8]*1,
                          SE = sqrt((combined.results1$se.mi[6])^2 + 1^2 *(combined.results1$se.mi[8])^2),
                          Dataset = "Combined Datasets")

# Combine these data.frames
allModelFrameSt1Dem <- data.frame(rbind(model1Frame, model2Frame, model3Frame, model4Frame, model5Frame ,model6Frame))  # etc.
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
zp1 <- ggplot(allModelFrameSt1Dem, aes(colour = Dataset))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes( ymin = Coefficient - SE*interval1, x = Regime,
                                 ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes( y = Coefficient, ymin = Coefficient - SE*interval2, x = Regime,
                                  ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + theme_bw() #+ coord_flip()
zp1 <- zp1 + ggtitle("Coefficient of Inequality")
print(zp1)  # The trick to these is position_dodge()

##NonDemについて
model1Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model1$coef[6],
                          SE = sqrt(vcov(model1)[6,6]),
                          Dataset = "Imputed Data1")
model2Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model2$coef[6],
                          SE = sqrt(vcov(model2)[6,6]),
                          Dataset = "Imputed Data2")
model3Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model3$coef[6],
                          SE = sqrt(vcov(model3)[6,6]),
                          Dataset = "Imputed Data3")
model4Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model4$coef[6],
                          SE = sqrt(vcov(model4)[6,6]),
                          Dataset = "Imputed Data4")
model5Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model5$coef[6],
                          SE = sqrt(vcov(model5)[6,6]),
                          Dataset = "Imputed Data5")
model6Frame <- data.frame(Regime = "in\nNon-Democracy",  #先にAmeliaのデータを合わせておく(functionあり)
                          Coefficient = combined.results1$q.mi[6],
                          SE = sqrt((combined.results1$se.mi[6])^2),
                          Dataset = "Combined Datasets")

# Combine these data.frames
allModelFrameSt1NonDem <- data.frame(rbind(model1Frame, model2Frame, model3Frame, model4Frame, model5Frame, model6Frame))  # etc.
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
zp1 <- ggplot(allModelFrameSt1NonDem, aes(colour = Dataset))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes( ymin = Coefficient - SE*interval1, x = Regime,
                                 ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes( y = Coefficient, ymin = Coefficient - SE*interval2, x = Regime,
                                  ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + theme_bw() #+ coord_flip()
zp1 <- zp1 + ggtitle("Coefficient of Inequality")
print(zp1)  # The trick to these is position_dodge()

# Stage1まとめてPlot
allModelFrameSt1<- data.frame(rbind(allModelFrameSt1Dem, allModelFrameSt1NonDem)) 
zp1 <- ggplot(allModelFrameSt1, aes(colour = Dataset))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes( ymin = Coefficient - SE*interval1, x = Regime,
                                 ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes( y = Coefficient, ymin = Coefficient - SE*interval2, x = Regime,
                                  ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + theme_bw()  + theme(axis.text.x=element_text(size=13), axis.text.y=element_text(size=13))   #+ coord_flip() 
zp1 <- zp1 + ggtitle("Coefficient of Inequality on Polity Score")
print(zp1)  # The trick to these is position_dodge()

#2段目
##Demについて
formula <- Giniall ~ WinningCoalition + Selectorate + Corruption + regime1*Aid_All
model1 <- lm(formula, data = AidData1)
model2 <- lm(formula, data = AidData2)
model3 <- lm(formula, data = AidData3)  # These are just arbitrary examples
model4 <- lm(formula, data = AidData4) 
model5 <- lm(formula, data = AidData5) 

model1Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model1$coef[6] + model1$coef[7]*1,
                          SE = sqrt(vcov(model1)[6,6] + 1^2 * vcov(model1)[7,7] + 2*1*vcov(model1)[6,7]),
                          Dataset = "Imputed Data1")
model2Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model2$coef[6] + model2$coef[7]*1,
                          SE = sqrt(vcov(model2)[6,6] + 1^2 * vcov(model2)[7,7] + 2*1*vcov(model2)[6,7]),
                          Dataset = "Imputed Data2")
model3Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model3$coef[6] + model3$coef[7]*1,
                          SE = sqrt(vcov(model3)[6,6] + 1^2 * vcov(model3)[7,7] + 2*1*vcov(model3)[6,7]),
                          Dataset = "Imputed Data3")
model4Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model4$coef[6] + model4$coef[7]*1,
                          SE = sqrt(vcov(model4)[6,6] + 1^2 * vcov(model4)[7,7] + 2*1*vcov(model4)[6,7]),
                          Dataset = "Imputed Data4")
model5Frame <- data.frame(Regime = "in\nDemocracy",
                          Coefficient = model5$coef[6] + model5$coef[7]*1,
                          SE = sqrt(vcov(model5)[6,6] + 1^2 * vcov(model5)[7,7] + 2*1*vcov(model5)[6,7]),
                          Dataset = "Imputed Data5")
model6Frame <- data.frame(Regime = "in\nDemocracy",  #先にAmeliaのデータを合わせておく(functionあり)
                          Coefficient = combined.results2$q.mi[4] + combined.results2$q.mi[5]*1,
                          SE = sqrt((combined.results2$se.mi[4])^2 + 1^2 *(combined.results2$se.mi[5])^2),
                          Dataset = "Combined Datasets")

# Combine these data.frames
allModelFrameSt2Dem <- data.frame(rbind(model1Frame, model2Frame, model3Frame, model4Frame, model5Frame, model6Frame))  # etc.
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
zp1 <- ggplot(allModelFrameSt2Dem, aes(colour = Dataset))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes( ymin = Coefficient - SE*interval1, x = Regime,
                                 ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes( y = Coefficient, ymin = Coefficient - SE*interval2, x = Regime,
                                  ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + theme_bw() #+ coord_flip()
zp1 <- zp1 + ggtitle("Coefficient of Aid_All")
print(zp1)  # The trick to these is position_dodge()

##NonDemについて
model1Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model1$coef[6],
                          SE = sqrt(vcov(model1)[6,6]),
                          Dataset = "Imputed Data1")
model2Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model2$coef[6],
                          SE = sqrt(vcov(model2)[6,6]),
                          Dataset = "Imputed Data2")
model3Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model3$coef[6],
                          SE = sqrt(vcov(model3)[6,6]),
                          Dataset = "Imputed Data3")
model4Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model4$coef[6],
                          SE = sqrt(vcov(model4)[6,6]),
                          Dataset = "Imputed Data4")
model5Frame <- data.frame(Regime = "in\nNon-Democracy",
                          Coefficient = model5$coef[6],
                          SE = sqrt(vcov(model5)[6,6]),
                          Dataset = "Imputed Data5")
model6Frame <- data.frame(Regime = "in\nNon-Democracy",  #先にAmeliaのデータを合わせておく(functionあり)
                          Coefficient = combined.results2$q.mi[4],
                          SE = sqrt((combined.results2$se.mi[4])^2),
                          Dataset = "Combined Datasets")

# Combine these data.frames
allModelFrameSt2NonDem <- data.frame(rbind(model1Frame, model2Frame, model3Frame, model4Frame, model5Frame, model6Frame))  # etc.
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
zp1 <- ggplot(allModelFrameSt2NonDem, aes(colour = Dataset))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes( ymin = Coefficient - SE*interval1, x = Regime,
                                 ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes( y = Coefficient, ymin = Coefficient - SE*interval2, x = Regime,
                                  ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + theme_bw() #+ coord_flip()
zp1 <- zp1 + ggtitle("Coefficient of Aid_All on Inequality")
print(zp1)  # The trick to these is position_dodge()

# Stage2まとめてPlot
allModelFrameSt2<- data.frame(rbind(allModelFrameSt2Dem, allModelFrameSt2NonDem)) 
zp1 <- ggplot(allModelFrameSt2, aes(colour = Dataset))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes( ymin = Coefficient - SE*interval1, x = Regime,
                                 ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes( y = Coefficient, ymin = Coefficient - SE*interval2, x = Regime,
                                  ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + theme_bw() + theme(axis.text.x=element_text(size=13), axis.text.y=element_text(size=13)) #+ coord_flip()
zp1 <- zp1 + ggtitle("Coefficient of Aid_All on Inequality")
print(zp1)  # The trick to these is position_dodge()
