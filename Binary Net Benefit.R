###################################################################
################### Net Benefit ###################################
###################################################################
# LR
average_predictions_m1<- read.csv('')
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=average_predictions_m1[,2], outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit<- append(benefit,benefit1)
}

# LR with transformations
average_predictions_m2<- read.csv('')
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit2<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=average_predictions_m2[,2], outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit2<- append(benefit2,benefit1)
}

# Ridge
average_predictions_m3<-  read.csv('')
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit3<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=average_predictions_m3[,2], outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit3<- append(benefit3,benefit1)
}

#Firth
average_predictions_m4<-  read.csv('')
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit4<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=average_predictions_m4[,2], outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit4<- append(benefit4,benefit1)
}

# CART
average_predictions_m5<-  read.csv('')
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit5<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=average_predictions_m5[,2], outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit5<- append(benefit5,benefit1)
}

# RF
average_predictions_m6<-  read.csv('')
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit6<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=average_predictions_m6[,2], outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit6<- append(benefit6,benefit1)
}


# XGB
average_predictions_m7<-  read.csv('')
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit7<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=average_predictions_m7[,2], outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit7<- append(benefit7,benefit1)
}

# SVM
average_predictions_m8<- read.csv('')
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit8<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=average_predictions_m8[,2], outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit8<-append(benefit8,benefit1)
}

# NN
average_predictions_m9<-  read.csv('')
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit9<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=average_predictions_m9[,2], outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit9<- append(benefit9,benefit1)
}


# Treat all
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit_all<- c()
benefit1<-c()
for (i in thresholds){
  benefit1<- 362.8/2894 - 2531.2/2894*i/(1-i)
  benefit_all<- append(benefit_all,benefit1)
}


## Make graph
x = seq(0, 0.1, by = 0.05)
y = seq(0, 1, by = 0.05)
tiff("Binary Net Benefit.tiff", width = 14, height = 14, units = "cm", res = 300)
plot(y=benefit, x=thresholds, xlim = c(0.03,0.1), ylim = c(0, 0.11), type = "l", col = "#e6194b", lwd = 2, lty = 4, 
     xlab = "Threshold", ylab = "Net Benefit", cex.lab = 1, cex.axis = 1, las = 1) 
lines(y=benefit2, x=thresholds, lwd = 2,lty=4, col = "#3cb44b")
lines(y=benefit3, x=thresholds, lwd = 2, lty=1, col = "#4363d8")
lines(y=benefit4, x=thresholds, lwd=1, lty=1, col="#f58231")
lines(y=benefit5, x=thresholds, lwd=2, lty=1, col="#911eb4")
lines(y=benefit6, x=thresholds, lwd=1,lty=1,  col="#f032e6")
lines(y=benefit7, x=thresholds, lwd=2, lty=1, col="#80CDC1")
lines(y=benefit8, x=thresholds, lwd=1, lty=1, col="#01665E")
lines(y=benefit9, x=thresholds, lwd=2, col="#FDE725FF")
lines(y=benefit_all, x=thresholds, lwd=2.5, col="#000000")
lines(x=thresholds,y=c(0,0,0,0,0,0,0,0), lwd=1, col="#888888")
legend(0.03,0.06, legend = c( "LR", "LR with Transformations", "Ridge LR", "Firth LR", "CART", "RF", "XGBoost", "SVM", "NN", 'Treat all', 'Treat none'),
       col = c("#e6194b", "#3cb44b", "#4363d8","#f58231", "#911eb4", "#f032e6", "#80CDC1", "#01665E", "#FDE725FF", "#000000", '#888888'),
       lty = c(4,4,1,1,1,1,1,1,1,1,1), lwd = c(2,2,2,1,2,1,2,1,2,2.5,1), cex = 0.7, bty = "n", ncol = 1)
dev.off()  