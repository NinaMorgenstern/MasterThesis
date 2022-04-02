
#############################
######### NET BENEFIT #######
#############################

# load in predictions of models
predictions_1 <-read.csv('predictions/pred_multinomial_LR.csv')
mydata['mm1_pred']<-predictions_1[,2]

predictions_2 <-read.csv('predictions/pred_multinomial_LR_with_Transformations.csv')
mydata['mm2_pred']<-predictions_2[,2]

predictions_3 <-read.csv('predictions/pred_multinomial_Ridge.csv')
mydata['mm3_pred']<-predictions_3[,2]

predictions_4 <-read.csv('predictions/pred_multinomial_Firth.csv')
mydata['mm4_pred']<-predictions_4[,2]

predictions_5 <-read.csv('predictions/pred_multinomial_CART.csv')
mydata['mm5_pred']<-predictions_5[,2]


predictions_6 <-read.csv('predictions/pred_multinomial_RF.csv')
mydata['mm6_pred']<-predictions_6[,2] 


predictions_7 <-read.csv('predictions/pred_multinomial_XGB.csv')
mydata['mm7_pred']<-predictions_7[,2]

predictions_8 <-read.csv('predictions/pred_multinomial_SVM.csv')
mydata['mm8_pred']<-predictions_8[,2]

predictions_9 <-read.csv('predictions/pred_multinomial_NN.csv')
mydata['mm9_pred']<-predictions_9[,2]

# LR 
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=mm1_pred, outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit<- append(benefit,benefit1)
}

# LR with transformations
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit2<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=mm2_pred, outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit2<- append(benefit2,benefit1)
}

# Ridge
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit3<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=mm3_pred, outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit3<- append(benefit3,benefit1)
}

#Firth
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit4<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=mm4_pred, outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit4<- append(benefit4,benefit1)
}

#CART
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit5<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=mm5_pred, outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit5<- append(benefit5,benefit1)
}

# RF
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit6<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=mm6_pred, outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit6<- append(benefit6,benefit1)
}

# XGB
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit7<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=mm7_pred, outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit7<- append(benefit7,benefit1)
}

# SVM
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit8<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=mm8_pred, outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
  benefit1<- sum(performance_1[["CenterPer"]][["TP"]])/2894 - (sum(performance_1[["CenterPer"]][["FP"]]) / 2894)*i/(1-i)
  benefit8<-append(benefit8,benefit1)
}

# NN
thresholds<-c(0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
benefit9<- c()
for (i in thresholds){
  performance_1<-SS.imp(pred=mm9_pred, outcome= Outcome2, threshold=i, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
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
tiff("Multinomial Net Benefit.tiff", width = 14, height = 14, units = "cm", res = 300)
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
