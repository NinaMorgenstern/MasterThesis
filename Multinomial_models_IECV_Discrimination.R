
###########################################
############# IECV ########################
###########################################

set.seed(123)
center_names<-c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")

######## Logistic Regression without transformations #####################
prediction_matrix <- matrix(nrow = 0, ncol = 30) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)
for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=30)
  mods11 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_1[mydata_1$Centre_numeric==i,]), ncol = 0)
    mods11[[i]]  <- multinom(Outcome3~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore,data=mydata_1[mydata_1$Centre_numeric!=i,]) #Build the model on all but one
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict(mods11[[i]] , type="probs", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer, pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
}

# this step needs to be done to average the right predictions
mydata$EP_mm1  <- rowMeans(as.data.frame(prediction_matrix)[,c(1,4,7,10,13,16,19,22,25,28)])
mydata$FPUL_mm1  <- rowMeans(as.data.frame(prediction_matrix)[,c(2,5,8,11,14,17,20,23,26,29)])
mydata$IUP_mm1  <- rowMeans(as.data.frame(prediction_matrix)[,c(3,6,9,12,15,18,21,24,27,30)])

write.csv(mydata[c('EP_mm1', 'FPUL_mm1', 'IUP_mm1')], "predictions/pred_multinomial_LR.csv")

mydata$mm1_pred <- 1-(mydata$FPUL_mm1+mydata$IUP_mm1)
IECVmm1auc <- AUCimp.IOTA(pred=mm1_pred,outcome = Outcome2, imp=.imp,center=Centre, data=mydata)
IECVmm1auc$Performance['AUC']


############### LR with transformations ######################
prediction_matrix <- matrix(nrow = 0, ncol = 30) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)
for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=30)
  mods22 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_1[mydata_1$Centre_numeric==i,]), ncol = 0)
    mods22[[i]]  <- multinom(Outcome3~log.hCG0num+ log.prog0num + I(log.prog0num^2) + lhCGratio +  I(lhCGratio^2)  + lhCGratio_log.prog0num  + Agesd + I(Agesd^2) + Vaginal.bleeding + Histep + Painscore + Painscore2, data=mydata_1[mydata_1$Centre_numeric!=i,]) #Build the model on all but one
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods22[[i]] , type="probs", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer, pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}


mydata$EP_mm2  <- rowMeans(as.data.frame(prediction_matrix)[,c(1,4,7,10,13,16,19,22,25,28)])
mydata$FPUL_mm2  <- rowMeans(as.data.frame(prediction_matrix)[,c(2,5,8,11,14,17,20,23,26,29)])
mydata$IUP_mm2  <- rowMeans(as.data.frame(prediction_matrix)[,c(3,6,9,12,15,18,21,24,27,30)])

write.csv(mydata[c('EP_mm2', 'FPUL_mm2', 'IUP_mm2')], "predictions/pred_multinomial_LR_with_Transformations.csv")

mydata$mm2_pred <- 1-(mydata$FPUL_mm2+mydata$IUP_mm2)
IECVmm2auc <- AUCimp.IOTA(pred=mm2_pred,outcome = Outcome2, imp=.imp,center=Centre, data=mydata)
IECVmm2auc$Performance['AUC']



################### Ridge Regression ###########################
prediction_matrix <- matrix(nrow = 0, ncol = 30) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)

y_train_std <- mydata[, c(".imp","Centre_numeric","Outcome3")]
x_train_std <- mydata[, c(".imp","Centre_numeric","log.hCG0num","log.prog0num","lhCGratio","Agesd","Vaginal.bleeding","Histep","Painscore","lhCGratio_log.prog0num",'Painscore2')]
x_train_std$X2.log.prog0num <-x_train_std$log.prog0num^2
x_train_std$X2.lhCGratio <-x_train_std$lhCGratio^2
x_train_std$X2.Agesd<- x_train_std$Agesd^2

for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=30)
  mods33 <- vector(mode="list", length=8)
  x_train_std_1<-x_train_std[x_train_std$.imp==j,]
  y_train_std_1<-y_train_std[y_train_std$.imp==j,]
  for (i in 1:8){
    X<-x_train_std_1[x_train_std_1$Centre_numeric!=i,]
    Y<-y_train_std_1[y_train_std_1$Centre_numeric!=i,]
    Y_final <-Y[,-c(1,2)]
    X_final <-data.matrix(X[,-c(1,2)])
    cv_train_std <- cv.glmnet(X_final, Y_final, nfolds=10, type.measure="deviance", alpha=0, family="multinomial")
    lambda <- cv_train_std$lambda.min
    mods33[[i]] <- glmnet(X_final, Y_final, family="multinomial", lambda=lambda, alpha=0)
    answer<-matrix(nrow =nrow(x_train_std_1[x_train_std_1$Centre_numeric==i,]), ncol = 0)
    for (k in 1:10){
      x_train_std_k<-x_train_std[x_train_std$.imp==k,]
      X_new<-x_train_std_k[x_train_std_k$Centre_numeric==i,]
      X_new1 <- as.matrix(X_new[,-c(1,2)])
      pred <- predict( mods33[[i]] , type="response", newx=X_new1)
      pred<-drop(pred)
      answer <- cbind(answer, pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}


mydata$EP_mm3  <- rowMeans(as.data.frame(prediction_matrix)[,c(1,4,7,10,13,16,19,22,25,28)])
mydata$FPUL_mm3  <- rowMeans(as.data.frame(prediction_matrix)[,c(2,5,8,11,14,17,20,23,26,29)])
mydata$IUP_mm3  <- rowMeans(as.data.frame(prediction_matrix)[,c(3,6,9,12,15,18,21,24,27,30)])

write.csv(mydata[c('EP_mm3', 'FPUL_mm3', 'IUP_mm3')], "predictions/pred_multinomial_Ridge.csv")

mydata$mm3_pred <-1-(mydata$FPUL_mm3+mydata$IUP_mm3)
IECVmm3auc <- AUCimp.IOTA(pred=mm3_pred,outcome = Outcome2, imp=.imp, center=Centre,data=mydata)
IECVmm3auc$Performance['AUC']


###################### FIRTH ##########################
prediction_matrix <- matrix(nrow = 0, ncol = 30) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)
for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=30)
  mods44 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_1[mydata_1$Centre_numeric==i,]), ncol = 0)
    mods44[[i]]  <- multinom(Outcome3~log.hCG0num+ log.prog0num + I(log.prog0num^2) + lhCGratio +  I(lhCGratio^2)  + lhCGratio_log.prog0num  + Agesd + I(Agesd^2) + Vaginal.bleeding + Histep + Painscore + Painscore2, method="brglmFit", data=mydata_1[mydata_1$Centre_numeric!=i,]) #Build the model on all but one
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods44[[i]] , type="probs", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer, pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

mydata$EP_mm4  <- rowMeans(as.data.frame(prediction_matrix)[,c(1,4,7,10,13,16,19,22,25,28)])
mydata$FPUL_mm4  <- rowMeans(as.data.frame(prediction_matrix)[,c(2,5,8,11,14,17,20,23,26,29)])
mydata$IUP_mm4  <- rowMeans(as.data.frame(prediction_matrix)[,c(3,6,9,12,15,18,21,24,27,30)])

write.csv(mydata[c('EP_mm4', 'FPUL_mm4', 'IUP_mm4')], "predictions/pred_multinomial_Firth.csv")

mydata$mm4_pred <- 1-(mydata$FPUL_mm4+mydata$IUP_mm4)
IECVmm4auc <- AUCimp.IOTA(pred=mm4_pred,outcome = Outcome2, imp=.imp,center=Centre, data=mydata)
IECVmm4auc$Performance['AUC']


##################### CART ########################
set.seed(123)
ctrl <- trainControl(method="cv",   # 10fold cross validation
                     number=10,         
                     summaryFunction=LogLoss,
                     classProbs=TRUE, search="random")

prediction_matrix <- matrix(nrow = 0, ncol = 30) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)

for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=30)
  mods55 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_1[mydata_1$Centre_numeric==i,]), ncol = 0)
    mods55[[i]]<- train(make.names(Outcome3)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, data=mydata_1[mydata_1$Centre_numeric!=i,], method="rpart", trControl=ctrl, tuneLength = 40)
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods55[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

mydata$EP_mm5  <- rowMeans(as.data.frame(prediction_matrix)[,c(1,4,7,10,13,16,19,22,25,28)])
mydata$FPUL_mm5  <- rowMeans(as.data.frame(prediction_matrix)[,c(2,5,8,11,14,17,20,23,26,29)])
mydata$IUP_mm5  <- rowMeans(as.data.frame(prediction_matrix)[,c(3,6,9,12,15,18,21,24,27,30)])

write.csv(mydata[c('EP_mm5', 'FPUL_mm5', 'IUP_mm5')], "predictions/pred_multinomial_CART.csv")

mydata$mm5_pred <- 1-(mydata$FPUL_mm5+mydata$IUP_mm5)
IECVmm5auc <- AUCimp.IOTA(pred=mm5_pred,outcome = Outcome2,center=Centre, imp=.imp, data=mydata)
IECVmm5auc$Performance['AUC']



#################### RANDOM FOREST ##############################
set.seed(123)
control <- trainControl(method="cv", number=10, classProbs=TRUE, search='grid')
hyper_grid <- expand.grid(mtry= c(1:4), min.node.size = c(1, 4, 6, 8, 10), splitrule="gini")

prediction_matrix <- matrix(nrow = 0, ncol = 30) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)

for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=30)
  mods66 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_1[mydata_1$Centre_numeric==i,]), ncol = 0)
    mods66[[i]]<-train(make.names(Outcome3)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, 
                       data=mydata_1[mydata_1$Centre_numeric!=i,],
                       method = "ranger",
                       trControl = control,
                       tuneGrid = hyper_grid)
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods66[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}


mydata$EP_mm6  <- rowMeans(as.data.frame(prediction_matrix)[,c(1,4,7,10,13,16,19,22,25,28)])
mydata$FPUL_mm6  <- rowMeans(as.data.frame(prediction_matrix)[,c(2,5,8,11,14,17,20,23,26,29)])
mydata$IUP_mm6  <- rowMeans(as.data.frame(prediction_matrix)[,c(3,6,9,12,15,18,21,24,27,30)])

write.csv(mydata[c('EP_mm6', 'FPUL_mm6', 'IUP_mm6')], "predictions/pred_multinomial_RF.csv")

mydata$mm6_pred <- 1-(mydata$FPUL_mm6+mydata$IUP_mm6)
IECVmm6auc <- AUCimp.IOTA(pred=mm6_pred,outcome = Outcome2, imp=.imp,center=Centre, data=mydata)
IECVmm6auc$Performance['AUC']




######################### XGB ###############################
set.seed(123)
xgb_trcontrol = trainControl(method = "cv", number = 10, 
                             verboseIter = FALSE,
                             classProbs=TRUE, allowParallel = TRUE, returnData = FALSE)

xgbgrid <- expand.grid(nrounds=100,
                       eta=c(0.01, 0.001,0.0001, 0.1, 0.15, 0.2, 0.3),
                       max_depth=6, #6 is default c(5,7,9,11),
                       gamma=0,
                       colsample_bytree=1,
                       min_child_weight=1,
                       subsample=1)

prediction_matrix <- matrix(nrow = 0, ncol = 30) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)

for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=30)
  mods77 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_1[mydata_1$Centre_numeric==i,]), ncol = 0)
    mods77[[i]]<-train(as.factor(make.names(Outcome3))~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore,
                       trControl = xgb_trcontrol, 
                       method = "xgbTree",
                       tuneLength=10,
                       #tuneGrid=xgbgrid,
                       verbosity = 0,
                       data=mydata_1[mydata_1$Centre_numeric!=i,])
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods77[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}


mydata$EP_mm7  <- rowMeans(as.data.frame(prediction_matrix)[,c(1,4,7,10,13,16,19,22,25,28)])
mydata$FPUL_mm7  <- rowMeans(as.data.frame(prediction_matrix)[,c(2,5,8,11,14,17,20,23,26,29)])
mydata$IUP_mm7  <- rowMeans(as.data.frame(prediction_matrix)[,c(3,6,9,12,15,18,21,24,27,30)])

write.csv(mydata[c('EP_mm7', 'FPUL_mm7', 'IUP_mm7')], "predictions/pred_multinomial_XGB.csv")

mydata$mm7_pred <- 1-(mydata$FPUL_mm7+mydata$IUP_mm7)
IECVmm7auc <- AUCimp.IOTA(pred=mm7_pred,outcome = Outcome2,center=Centre, imp=.imp, data=mydata)
IECVmm7auc$Performance['AUC']


######################### SVM ###############################
set.seed(123)
ctrl <- trainControl(method="cv",   # 10fold cross validation
                     number=10,         
                     search="random",
                     classProbs=TRUE)
prediction_matrix <- matrix(nrow = 0, ncol = 30) 

for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=30)
  mods88 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  for (i in 1:8){
    mods88[[i]] <- train(make.names(Outcome3)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore,
                         method = "svmRadial",# Radial basis function kernel
                         tuneLength = 10,
                         preProcess = c("center", "scale"),
                         data=mydata_1[mydata_1$Centre_numeric!=i,],
                         trControl=ctrl) 
    answer<-matrix(nrow =nrow(mydata[mydata$Centre_numeric==i & mydata$.imp==k,]), ncol = 0)
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods88[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

mydata$EP_mm8  <- rowMeans(as.data.frame(prediction_matrix)[,c(1,4,7,10,13,16,19,22,25,28)])
mydata$FPUL_mm8  <- rowMeans(as.data.frame(prediction_matrix)[,c(2,5,8,11,14,17,20,23,26,29)])
mydata$IUP_mm8  <- rowMeans(as.data.frame(prediction_matrix)[,c(3,6,9,12,15,18,21,24,27,30)])

write.csv(mydata[c('EP_mm8', 'FPUL_mm8', 'IUP_mm8')], "predictions/pred_multinomial_SVM.csv")

mydata$mm8_pred <- 1-(mydata$FPUL_mm8+mydata$IUP_mm8)
IECVmm8auc <- AUCimp.IOTA(pred=mm8_pred,outcome = Outcome2, imp=.imp,center=Centre, data=mydata)
IECVmm8auc$Performance['AUC']



##################### Neural Network ##########################
set.seed(123)
ctrl <- trainControl(method="cv", 
                     number=10,         
                     search="random",
                     classProbs=TRUE, summaryFunction=multiClassSummary)
prediction_matrix <- matrix(nrow = 0, ncol = 30) 


for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=30)
  mods99 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  for (i in 1:8){
    mods99[[i]] <- train(make.names(Outcome3)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore,
                         method = "nnet",
                         tuneLength = 10,
                         preProcess = c("center", "scale"),
                         trace = FALSE,
                         metric="ROC",
                         maxit = 1000,
                         linout = FALSE,
                         data= mydata_1[mydata_1$Centre_numeric!=i,],
                         trControl=ctrl) 
    answer<-matrix(nrow =nrow(mydata[mydata$Centre_numeric==i & mydata$.imp==k,]), ncol = 0)
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods99[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

mydata$EP_mm9  <- rowMeans(as.data.frame(prediction_matrix)[,c(1,4,7,10,13,16,19,22,25,28)])
mydata$FPUL_mm9  <- rowMeans(as.data.frame(prediction_matrix)[,c(2,5,8,11,14,17,20,23,26,29)])
mydata$IUP_mm9  <- rowMeans(as.data.frame(prediction_matrix)[,c(3,6,9,12,15,18,21,24,27,30)])

write.csv(mydata[c('EP_mm9', 'FPUL_mm9', 'IUP_mm9')], "predictions/pred_multinomial_NN.csv")

mydata$mm9_pred <- 1-(mydata$FPUL_mm9+mydata$IUP_mm9)
IECVmm9auc <- AUCimp.IOTA(pred=mm9_pred,outcome = Outcome2,center=Centre, imp=.imp, data=mydata)
IECVmm9auc$Performance['AUC']


#######################################
####### Forest plots cEP ##############
#######################################

#### LR ######
IECVmm1auc$Plot
IECVmm1auc$Plot[11, "RRauc"] <- "   "
IECVmm1auc$Plot[12, "RRauc"] <- "   "
IECVmm1auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "cEP (95 % CI)", "95 % Prediction Interval")


tiff("multinomial_LR_IECV_cEP.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECVmm1auc$Plot,
           align = c("l", "c", "c"),
           mean = IECVmm1auc$dataPlot$AUC,
           lower = IECVmm1auc$dataPlot$LL,
           upper = IECVmm1auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECVmm1auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Multinomial LR: IECV cEP",
           xlab = "cEP",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECVmm1auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECVmm1auc$Performance$AUC)
dev.off()




############## LR with transformations #################
IECVmm2auc$Plot
IECVmm2auc$Plot[11, "RRauc"] <- "   "
IECVmm2auc$Plot[12, "RRauc"] <- "   "
IECVmm2auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "cEP (95 % CI)", "95 % Prediction Interval")


tiff("multinomial_LR_with_Transformations_IECV_cEP.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECVmm2auc$Plot,
           align = c("l", "c", "c"),
           mean = IECVmm2auc$dataPlot$AUC,
           lower = IECVmm2auc$dataPlot$LL,
           upper = IECVmm2auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECVmm2auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Multinomial LR with Transformations: IECV cEP",
           xlab = "cEP",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECVmm2auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECVmm2auc$Performance$AUC)
dev.off()


############### Ridge LR ########################
IECVmm3auc$Plot
IECVmm3auc$Plot[11, "RRauc"] <- "   "
IECVmm3auc$Plot[12, "RRauc"] <- "   "
IECVmm3auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "cEP (95 % CI)", "95 % Prediction Interval")

tiff("multinomial_Ridge_IECV_cEP.tiff", width = 31, height = 20, units = "cm", res = 300)
forestplot(IECVmm3auc$Plot,
           align = c("l", "c", "c"),
           mean = IECVmm3auc$dataPlot$AUC,
           lower = IECVmm3auc$dataPlot$LL,
           upper = IECVmm3auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECVmm3auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Multinomial Ridge LR: IECV cEP",
           xlab = "cEP",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECVmm3auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECVmm3auc$Performance$AUC)
dev.off()


################ Firth ##################
IECVmm4auc$Plot
IECVmm4auc$Plot[11, "RRauc"] <- "   "
IECVmm4auc$Plot[12, "RRauc"] <- "   "
IECVmm4auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "cEP (95 % CI)", "95 % Prediction Interval")

tiff("multinomial_Firth_IECV_cEP.tiff", width = 31, height = 20, units = "cm", res = 300)
forestplot(IECVmm4auc$Plot,
           align = c("l", "c", "c"),
           mean = IECVmm4auc$dataPlot$AUC,
           lower = IECVmm4auc$dataPlot$LL,
           upper = IECVmm4auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECVmm4auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Multinomial Firth LR: IECV cEP",
           xlab = "cEP",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECVmm4auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECVmm4auc$Performance$AUC)
dev.off()


############# CART ##################
IECVmm5auc$Plot
IECVmm5auc$Plot[11, "RRauc"] <- "   "
IECVmm5auc$Plot[12, "RRauc"] <- "   "
IECVmm5auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "cEP (95 % CI)", "95 % Prediction Interval")

tiff("multinomial_CART_IECV_cEP.tiff", width = 31, height = 20, units = "cm", res = 300)
forestplot(IECVmm5auc$Plot,
           align = c("l", "c", "c"),
           mean = IECVmm5auc$dataPlot$AUC,
           lower = IECVmm5auc$dataPlot$LL,
           upper = IECVmm5auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECVmm5auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Multinomial CART: IECV cEP",
           xlab = "cEP",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECVmm5auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECVmm5auc$Performance$AUC)
dev.off()



################### RF ############################
IECVmm6auc$Plot
IECVmm6auc$Plot[11, "RRauc"] <- "   "
IECVmm6auc$Plot[12, "RRauc"] <- "   "
IECVmm6auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "cEP (95 % CI)", "95 % Prediction Interval")

tiff("multinomial_RF_IECV_cEP.tiff", width = 31, height = 20, units = "cm", res = 300)
forestplot(IECVmm6auc$Plot,
           align = c("l", "c", "c"),
           mean = IECVmm6auc$dataPlot$AUC,
           lower = IECVmm6auc$dataPlot$LL,
           upper = IECVmm6auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECVmm6auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Multinomial RF: IECV cEP",
           xlab = "cEP",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECVmm6auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECVmm6auc$Performance$AUC)
dev.off()


############# XGB ###############
IECVmm7auc$Plot
IECVmm7auc$Plot[11, "RRauc"] <- "   "
IECVmm7auc$Plot[12, "RRauc"] <- "   "
IECVmm7auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "cEP (95 % CI)", "95 % Prediction Interval")

tiff("multinomial_XGB_IECV_cEP.tiff", width = 31, height = 20, units = "cm", res = 300)
forestplot(IECVmm7auc$Plot,
           align = c("l", "c", "c"),
           mean = IECVmm7auc$dataPlot$AUC,
           lower = IECVmm7auc$dataPlot$LL,
           upper = IECVmm7auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECVmm7auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Multinomial XGB: IECV cEP",
           xlab = "cEP",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECVmm7auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECVmm7auc$Performance$AUC)
dev.off()



################# SVM ######################
IECVmm8auc$Plot
IECVmm8auc$Plot[11, "RRauc"] <- "   "
IECVmm8auc$Plot[12, "RRauc"] <- "   "
IECVmm8auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "cEP (95 % CI)", "95 % Prediction Interval")

tiff("multinomial_SVM_IECV_cEP.tiff", width = 31, height = 20, units = "cm", res = 300)
forestplot(IECVmm8auc$Plot,
           align = c("l", "c", "c"),
           mean = IECVmm8auc$dataPlot$AUC,
           lower = IECVmm8auc$dataPlot$LL,
           upper = IECVmm8auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECVmm8auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Multinomial SVM: IECV cEP",
           xlab = "cEP",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECVmm8auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECVmm8auc$Performance$AUC)
dev.off()


############## NN ######################
IECVmm9auc$Plot
IECVmm9auc$Plot[11, "RRauc"] <- "   "
IECVmm9auc$Plot[12, "RRauc"] <- "   "
IECVmm9auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "cEP (95 % CI)", "95 % Prediction Interval")

tiff("multinomial_NN_IECV_cEP.tiff", width = 31, height = 20, units = "cm", res = 300)
forestplot(IECVmm9auc$Plot,
           align = c("l", "c", "c"),
           mean = IECVmm9auc$dataPlot$AUC,
           lower = IECVmm9auc$dataPlot$LL,
           upper = IECVmm9auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECVmm9auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Multinomial NN: IECV cEP",
           xlab = "cEP",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECVmm9auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECVmm9auc$Performance$AUC)
dev.off()



###################################
#### Overall Forest plot cEP ######
###################################

NA.forest <- IECVmm1auc$Performance[1,]
NA.forest <- NA
Summary.AUC <- rbind(NA.forest, IECVmm1auc$Performance[1,], IECVmm2auc$Performance[1,], IECVmm3auc$Performance[1,], IECVmm4auc$Performance[1,],IECVmm5auc$Performance[1,], IECVmm6auc$Performance[1,], IECVmm7auc$Performance[1,], IECVmm8auc$Performance[1,], IECVmm9auc$Performance[1,])
Summary.AUC$Model <- c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
Summary.AUC.PI <- rbind(NA.forest, IECVmm1auc$Performance[2,], IECVmm2auc$Performance[2,], IECVmm3auc$Performance[2,], IECVmm4auc$Performance[2,], IECVmm5auc$Performance[2,], IECVmm6auc$Performance[2,], IECVmm7auc$Performance[2,], IECVmm8auc$Performance[2,], IECVmm9auc$Performance[2,])
Summary.AUC.PI$Model <-  c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
tabletext <- cbind(
  c('Model', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN"),
  c('cEP (95% CI)', 
    paste(format(round(IECVmm1auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECVmm1auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECVmm1auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECVmm2auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECVmm2auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECVmm2auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECVmm3auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECVmm3auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECVmm3auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECVmm4auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECVmm4auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECVmm4auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECVmm5auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECVmm5auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECVmm5auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECVmm6auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECVmm6auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECVmm6auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECVmm7auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECVmm7auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECVmm7auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECVmm8auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECVmm8auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECVmm8auc$Performance$UL[1], 2), nsmall = 2), ")", sep= ""),
    paste(format(round(IECVmm9auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECVmm9auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECVmm9auc$Performance$UL[1], 2), nsmall = 2), ")", sep="")),
  c('95% PI', 
    paste0("(", format(round(IECVmm1auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECVmm1auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECVmm2auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECVmm2auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECVmm3auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECVmm3auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECVmm4auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECVmm4auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECVmm5auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECVmm5auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECVmm6auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECVmm6auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECVmm7auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECVmm7auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECVmm8auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECVmm8auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECVmm9auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECVmm9auc$Performance$UL[2], 2), nsmall = 2), ")")))

tiff("graphs/IECV_multinomial_AUC_all_models.tiff", width = 31, height = 13.75, units = "cm", res = 300)
forestplot(labeltext = tabletext,
           title = "cEP per model",
           mean = round(Summary.AUC$AUC, 3),
           lower = round(Summary.AUC$LL, 3),
           upper = round(Summary.AUC$UL, 3),
           is.summary = c(FALSE, TRUE, TRUE, TRUE, TRUE,TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
           xlab = "cEP",
           boxsize = .5,
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55, fontface = "plain"), xlab = gpar(cex = 1.5, fontface = "plain"), label = gpar(cex = 1.5, fontface = "bold"),
                            ticks = gpar(cex = 1.5, fontface = "plain"), title = gpar(cex = 1.75)),
           graphwidth = unit(9, "cm"),
           graph.pos = 3,
           xticks = c(0.70,0.8, 0.9, 0.9, 1), xlog = TRUE, clip = c(0.6, 1))     
dev.off()


#####################################
############# PDI ###################
#####################################

########## LR #############
mm1_predapp<-read.csv("predictions/pred_multinomial_LR.csv")[ ,2:4]
mydata<-cbind(mydata, mm1_predapp)
PDI_mm1 <- matrix(ncol = 4, nrow = 0)
for (i in 1:8){
  mydata_center<- mydata[mydata$Centre_numeric==i,]
  se<-c()
  value<-c()
  for (i in 1:10){
    mydata1<-mydata_center[mydata_center$.imp==i,]
    mm1_predapp_1<-mydata1[, c('EP_mm1', 'FPUL_mm1', 'IUP_mm1')]
    PDI<- ests(y=mydata1$Outcome3, d=mm1_predapp_1, acc="pdi", level=0.95, method="prob", k=3)
    value<-append(value,PDI$value)
    se<-append(se,PDI$se)
    print(i)
  }
  PDIcombined <- matrix(ncol = 3, nrow = 1)
  colnames(PDIcombined) <- c('PDI', 'LL', 'UL')
  PDIcombined <- data.frame(PDIcombined) # Rubin's rule to pool PDIs on imputetd datasets
  WithinVar <- mean(se^2)
  BetweenVar <- var(value)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/10
  PooledSE <- sqrt(PooledVar)
  PDIcombined$LL <- mean(value) - 1.96*PooledSE
  PDIcombined$UL <- mean(value) + 1.96*PooledSE
  PDIcombined$PDI<-mean(value)
  PDIcombined$PooledSE<-PooledSE
  PDI_mm1<-rbind(PDIcombined, PDI_mm1)
}
fit.RE_1 = rma.uni(PDI_mm1$PDI, sei = PDI_mm1$PooledSE, method = 'REML') # meta analysis for overall PDI




########### LR with transformations #################
mm1_predapp<-read.csv("predictions/pred_multinomial_LR_with_Transformations.csv")[ ,2:4]
mydata<-cbind(mydata, mm1_predapp)
PDI_mm1 <- matrix(ncol = 4, nrow = 0)
for (i in 1:8){
  mydata_center<- mydata[mydata$Centre_numeric==i,]
  se<-c()
  value<-c()
  for (i in 1:10){
    mydata1<-mydata_center[mydata_center$.imp==i,]
    mm1_predapp_1<-mydata1[, c('EP_mm2', 'FPUL_mm2', 'IUP_mm2')]
    PDI<- ests(y=mydata1$Outcome3, d=mm1_predapp_1, acc="pdi", level=0.95, method="prob", k=3)
    value<-append(value,PDI$value)
    se<-append(se,PDI$se)
    print(i)
  }
  PDIcombined <- matrix(ncol = 3, nrow = 1)
  colnames(PDIcombined) <- c('PDI', 'LL', 'UL')
  PDIcombined <- data.frame(PDIcombined)
  WithinVar <- mean(se^2)
  BetweenVar <- var(value)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/10
  PooledSE <- sqrt(PooledVar)
  PDIcombined$LL <- mean(value) - 1.96*PooledSE
  PDIcombined$UL <- mean(value) + 1.96*PooledSE
  PDIcombined$PDI<-mean(value)
  PDIcombined$PooledSE<-PooledSE
  PDI_mm1<-rbind(PDIcombined, PDI_mm1)
}
fit.RE_2 = rma.uni(PDI_mm1$PDI, sei = PDI_mm1$PooledSE, method = 'REML')


############### Ridge #####################
mm1_predapp<-read.csv("predictions/pred_multinomial_Ridge.csv")[ ,2:4]
mydata<-cbind(mydata, mm1_predapp)
PDI_mm1 <- matrix(ncol = 4, nrow = 0)
for (i in 1:8){
  mydata_center<- mydata[mydata$Centre_numeric==i,]
  se<-c()
  value<-c()
  for (i in 1:10){
    mydata1<-mydata_center[mydata_center$.imp==i,]
    mm1_predapp_1<-mydata1[, c('EP_mm3', 'FPUL_mm3', 'IUP_mm3')]
    PDI<- ests(y=mydata1$Outcome3, d=mm1_predapp_1, acc="pdi", level=0.95, method="prob", k=3)
    value<-append(value,PDI$value)
    se<-append(se,PDI$se)
    print(i)
  }
  PDIcombined <- matrix(ncol = 3, nrow = 1)
  colnames(PDIcombined) <- c('PDI', 'LL', 'UL')
  PDIcombined <- data.frame(PDIcombined)
  WithinVar <- mean(se^2)
  BetweenVar <- var(value)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/10
  PooledSE <- sqrt(PooledVar)
  PDIcombined$LL <- mean(value) - 1.96*PooledSE
  PDIcombined$UL <- mean(value) + 1.96*PooledSE
  PDIcombined$PDI<-mean(value)
  PDIcombined$PooledSE<-PooledSE
  PDI_mm1<-rbind(PDIcombined, PDI_mm1)
}
fit.RE_3 = rma.uni(PDI_mm1$PDI, sei = PDI_mm1$PooledSE, method = 'REML')


################ Firth #####################
mm1_predapp<-read.csv("predictions/pred_multinomial_Firth.csv")[ ,2:4]
mydata<-cbind(mydata, mm1_predapp)
PDI_mm1 <- matrix(ncol = 4, nrow = 0)
for (i in 1:8){
  mydata_center<- mydata[mydata$Centre_numeric==i,]
  se<-c()
  value<-c()
  for (i in 1:10){
    mydata1<-mydata_center[mydata_center$.imp==i,]
    mm1_predapp_1<-mydata1[, c('EP_mm4', 'FPUL_mm4', 'IUP_mm4')]
    PDI<- ests(y=mydata1$Outcome3, d=mm1_predapp_1, acc="pdi", level=0.95, method="prob", k=3)
    value<-append(value,PDI$value)
    se<-append(se,PDI$se)
    print(i)
  }
  PDIcombined <- matrix(ncol = 3, nrow = 1)
  colnames(PDIcombined) <- c('PDI', 'LL', 'UL')
  PDIcombined <- data.frame(PDIcombined)
  WithinVar <- mean(se^2)
  BetweenVar <- var(value)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/10
  PooledSE <- sqrt(PooledVar)
  PDIcombined$LL <- mean(value) - 1.96*PooledSE
  PDIcombined$UL <- mean(value) + 1.96*PooledSE
  PDIcombined$PDI<-mean(value)
  PDIcombined$PooledSE<-PooledSE
  PDI_mm1<-rbind(PDIcombined, PDI_mm1)
}
fit.RE_4 = rma.uni(PDI_mm1$PDI, sei = PDI_mm1$PooledSE, method = 'REML')



############ CART ####################
mm1_predapp<-read.csv("predictions/pred_multinomial_CART.csv")[ ,2:4]
mydata<-cbind(mydata, mm1_predapp)
PDI_mm1 <- matrix(ncol = 4, nrow = 0)
for (i in 1:8){
  mydata_center<- mydata[mydata$Centre_numeric==i,]
  se<-c()
  value<-c()
  for (i in 1:10){
    mydata1<-mydata_center[mydata_center$.imp==i,]
    mm1_predapp_1<-mydata1[, c('EP_mm5', 'FPUL_mm5', 'IUP_mm5')]
    PDI<- ests(y=mydata1$Outcome3, d=mm1_predapp_1, acc="pdi", level=0.95, method="prob", k=3)
    value<-append(value,PDI$value)
    se<-append(se,PDI$se)
    print(i)
  }
  PDIcombined <- matrix(ncol = 3, nrow = 1)
  colnames(PDIcombined) <- c('PDI', 'LL', 'UL')
  PDIcombined <- data.frame(PDIcombined)
  WithinVar <- mean(se^2)
  BetweenVar <- var(value)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/10
  PooledSE <- sqrt(PooledVar)
  PDIcombined$LL <- mean(value) - 1.96*PooledSE
  PDIcombined$UL <- mean(value) + 1.96*PooledSE
  PDIcombined$PDI<-mean(value)
  PDIcombined$PooledSE<-PooledSE
  PDI_mm1<-rbind(PDIcombined, PDI_mm1)
}
fit.RE_5 = rma.uni(PDI_mm1$PDI, sei = PDI_mm1$PooledSE, method = 'REML')


############# RF ######################
mm1_predapp<-read.csv("predictions/pred_multinomial_RF.csv")[ ,2:4]
mydata<-cbind(mydata, mm1_predapp)
PDI_mm1 <- matrix(ncol = 4, nrow = 0)
for (i in 1:8){
  mydata_center<- mydata[mydata$Centre_numeric==i,]
  se<-c()
  value<-c()
  for (i in 1:10){
    mydata1<-mydata_center[mydata_center$.imp==i,]
    mm1_predapp_1<-mydata1[, c('EP_mm6', 'FPUL_mm6', 'IUP_mm6')]
    PDI<- ests(y=mydata1$Outcome3, d=mm1_predapp_1, acc="pdi", level=0.95, method="prob", k=3)
    value<-append(value,PDI$value)
    se<-append(se,PDI$se)
    print(i)
  }
  PDIcombined <- matrix(ncol = 3, nrow = 1)
  colnames(PDIcombined) <- c('PDI', 'LL', 'UL')
  PDIcombined <- data.frame(PDIcombined)
  WithinVar <- mean(se^2)
  BetweenVar <- var(value)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/10
  PooledSE <- sqrt(PooledVar)
  PDIcombined$LL <- mean(value) - 1.96*PooledSE
  PDIcombined$UL <- mean(value) + 1.96*PooledSE
  PDIcombined$PDI<-mean(value)
  PDIcombined$PooledSE<-PooledSE
  PDI_mm1<-rbind(PDIcombined, PDI_mm1)
}
fit.RE_6 = rma.uni(PDI_mm1$PDI, sei = PDI_mm1$PooledSE, method = 'REML')




################# XGB ########################
mm1_predapp<-read.csv("predictions/pred_multinomial_XGB.csv")[ ,2:4]
mydata<-cbind(mydata, mm1_predapp)
PDI_mm1 <- matrix(ncol = 4, nrow = 0)
for (i in 1:8){
  mydata_center<- mydata[mydata$Centre_numeric==i,]
  se<-c()
  value<-c()
  for (i in 1:10){
    mydata1<-mydata_center[mydata_center$.imp==i,]
    mm1_predapp_1<-mydata1[, c('EP_mm7', 'FPUL_mm7', 'IUP_mm7')]
    PDI<- ests(y=mydata1$Outcome3, d=mm1_predapp_1, acc="pdi", level=0.95, method="prob", k=3)
    value<-append(value,PDI$value)
    se<-append(se,PDI$se)
    print(i)
  }
  PDIcombined <- matrix(ncol = 3, nrow = 1)
  colnames(PDIcombined) <- c('PDI', 'LL', 'UL')
  PDIcombined <- data.frame(PDIcombined)
  WithinVar <- mean(se^2)
  BetweenVar <- var(value)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/10
  PooledSE <- sqrt(PooledVar)
  PDIcombined$LL <- mean(value) - 1.96*PooledSE
  PDIcombined$UL <- mean(value) + 1.96*PooledSE
  PDIcombined$PDI<-mean(value)
  PDIcombined$PooledSE<-PooledSE
  PDI_mm1<-rbind(PDIcombined, PDI_mm1)
}
fit.RE_7 = rma.uni(PDI_mm1$PDI, sei = PDI_mm1$PooledSE, method = 'REML')




############## SVM ####################
mm1_predapp<-read.csv("predictions/pred_multinomial_SVM.csv")[ ,2:4]
mydata<-cbind(mydata, mm1_predapp)
PDI_mm1 <- matrix(ncol = 4, nrow = 0)
for (i in 1:8){
  mydata_center<- mydata[mydata$Centre_numeric==i,]
  se<-c()
  value<-c()
  for (i in 1:10){
    mydata1<-mydata_center[mydata_center$.imp==i,]
    mm1_predapp_1<-mydata1[, c('EP_mm8', 'FPUL_mm8', 'IUP_mm8')]
    PDI<- ests(y=mydata1$Outcome3, d=mm1_predapp_1, acc="pdi", level=0.95, method="prob", k=3)
    value<-append(value,PDI$value)
    se<-append(se,PDI$se)
    print(i)
  }
  PDIcombined <- matrix(ncol = 3, nrow = 1)
  colnames(PDIcombined) <- c('PDI', 'LL', 'UL')
  PDIcombined <- data.frame(PDIcombined)
  WithinVar <- mean(se^2)
  BetweenVar <- var(value)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/10
  PooledSE <- sqrt(PooledVar)
  PDIcombined$LL <- mean(value) - 1.96*PooledSE
  PDIcombined$UL <- mean(value) + 1.96*PooledSE
  PDIcombined$PDI<-mean(value)
  PDIcombined$PooledSE<-PooledSE
  PDI_mm1<-rbind(PDIcombined, PDI_mm1)
}
fit.RE_8 = rma.uni(PDI_mm1$PDI, sei = PDI_mm1$PooledSE, method = 'REML')



################ NN ###################
mm1_predapp<-read.csv("predictions/pred_multinomial_NN.csv")[ ,2:4]
mydata<-cbind(mydata, mm1_predapp)
PDI_mm1 <- matrix(ncol = 4, nrow = 0)
for (i in 1:8){
  mydata_center<- mydata[mydata$Centre_numeric==i,]
  se<-c()
  value<-c()
  for (i in 1:10){
    mydata1<-mydata_center[mydata_center$.imp==i,]
    mm1_predapp_1<-mydata1[, c('EP_mm9', 'FPUL_mm9', 'IUP_mm9')]
    PDI<- ests(y=mydata1$Outcome3, d=mm1_predapp_1, acc="pdi", level=0.95, method="prob", k=3)
    value<-append(value,PDI$value)
    se<-append(se,PDI$se)
    print(i)
  }
  PDIcombined <- matrix(ncol = 3, nrow = 1)
  colnames(PDIcombined) <- c('PDI', 'LL', 'UL')
  PDIcombined <- data.frame(PDIcombined)
  WithinVar <- mean(se^2)
  BetweenVar <- var(value)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/10
  PooledSE <- sqrt(PooledVar)
  PDIcombined$LL <- mean(value) - 1.96*PooledSE
  PDIcombined$UL <- mean(value) + 1.96*PooledSE
  PDIcombined$PDI<-mean(value)
  PDIcombined$PooledSE<-PooledSE
  PDI_mm1<-rbind(PDIcombined, PDI_mm1)
}
fit.RE_9 = rma.uni(PDI_mm1$PDI, sei = PDI_mm1$PooledSE, method = 'REML')



##############################
########### cIF #############
#############################

# function for pooling cIF of different imputations and meta analysis for overall estimates of cIF
# adjusted from: https://github.com/benvancalster/IOTA5modelvalidation2020/blob/master/Functions%20IOTA5.R

AUCimp.IOTA.cIF <- function(pred, outcome, center, imp, data, method.MA = "REML", titleGraph = "AUC per center"){
  
  arguments <- as.list(match.call())[-1]
  pred = eval(arguments$pred, data)
  outcome <- eval(arguments$outcome, data)
  center <- eval(arguments$center, data)
  imp <- eval(arguments$imp, data)
  
  Df = data.frame(p = pred, y = outcome, center = center, imp = imp, stringsAsFactors = F)
  
  centers <- unique(Df$center)
  NRimp <- length(unique(Df$imp))
  
  AUCimp <- list()
  for(i in 1:NRimp){
    AUCimp[[i]] <- list()
  }
  
  PrevalenceOverall <- matrix(nrow = NRimp, ncol = 1)
  
  # AUC per center
  for(j in 1:NRimp){
    cat("Imputation", j, " of ", NRimp, "\n\n")
    
    AUCcenter <- matrix(ncol = 6, nrow = length(centers))
    colnames(AUCcenter) <- c('Center', 'SampleSize', 'Prevalence', 'AUC', 'LL', 'UL')
    AUCcenter <- data.frame(AUCcenter)
    AUCcenter$Center <- centers
    
    for(i in seq_along(centers)){
      AUCcenter[i, 4:6] <- auc.nonpara.mw(Df$p[Df$center == centers[i] & Df$y == 'IUP' & Df$imp == j], Df$p[Df$center == centers[i] & Df$y == 'FPUL' & Df$imp == j], method = "pepe")
      AUCcenter[i, 2]   <- nrow(Df[Df$center == centers[i] & Df$imp == j,])
      AUCcenter[i, 3]   <- round(nrow(Df[Df$y == 'IUP' & Df$center == centers[i] & Df$imp == j,])/nrow(Df[Df$center == centers[i] & Df$imp == j,])*100)
      
      ## Additional part for AUCs of 1
      if(AUCcenter[i, 4] == 1){
        AUCcenter[i, 4:6] <- auc.nonpara.mw(Df$p[Df$center == centers[i] & Df$y == 'IUP' & Df$imp == j], Df$p[Df$center == centers[i] & Df$y == 'FPUL' & Df$imp == j], method = "newcombe") # Newcombe ipv pepe
      } else{
        AUCcenter[i, 4:6] <- auc.nonpara.mw(Df$p[Df$center == centers[i] & Df$y == 'IUP' & Df$imp == j], Df$p[Df$center == centers[i] & Df$y == 'FPUL' & Df$imp == j], method = "pepe")
      }
      
      if(AUCcenter$AUC[i] != 1){
        AUCcenter$logit.AUC[i] <- logit(AUCcenter$AUC[i])
        AUCcenter$logit.se[i]  <- (logit(AUCcenter$AUC[i]) - logit(AUCcenter$LL[i]))/1.96
        AUCcenter$logit.var[i] <- AUCcenter$logit.se[i]^2
      } else{
        AUCcenter$logit.AUC[i] <- logit(0.999)
        AUCcenter$logit.se[i]  <- (logit(0.999) - logit(AUCcenter$LL[i]))/1.96
        AUCcenter$logit.var[i] <- AUCcenter$logit.se[i]^2
      }
      
    }
    AUCcenter <- AUCcenter[order(AUCcenter$SampleSize, decreasing = TRUE),]
    
    AUCimp[[j]] <- AUCcenter
    
    PrevalenceOverall[j] <- round(nrow(Df[Df$y == 'IUP' & Df$imp == j,])/nrow(Df[Df$imp == j,])*100)
  }
  
  AUCimpLong <- rbindlist(AUCimp, fill = TRUE)
  
  # Combine results with Rubin's rule
  AUCcombined <- matrix(ncol = 6, nrow = length(centers))
  colnames(AUCcombined) <- c('Center', 'SampleSize', 'Prevalence', 'logit.AUC', 'logit.LL', 'logit.UL')
  AUCcombined <- data.frame(AUCcombined)
  AUCcombined$Center <- centers
  
  
  for(i in seq_along(centers)){
    AUCcombined$SampleSize[i] <- unique(AUCimpLong$SampleSize[AUCimpLong$Center == centers[i]])
    AUCcombined$Prevalence[i] <- round(mean(AUCimpLong$Prevalence[AUCimpLong$Center == centers[i]]))
    AUCcombined[i, 4] <- mean(AUCimpLong$logit.AUC[AUCimpLong$Center == centers[i]])
    WithinVar <- mean(AUCimpLong$logit.var[AUCimpLong$Center == centers[i]])
    BetweenVar <- var(AUCimpLong$logit.AUC[AUCimpLong$Center == centers[i]])
    PooledVar <- WithinVar + BetweenVar + BetweenVar/NRimp
    AUCcombined$PooledSE[i] <- sqrt(PooledVar)
    AUCcombined$logit.LL[i] <- AUCcombined$logit.AUC[i] - 1.96*AUCcombined$PooledSE[i]
    AUCcombined$logit.UL[i] <- AUCcombined$logit.AUC[i] + 1.96*AUCcombined$PooledSE[i]
  }
  
  AUCcombined$AUC <- inv.logit(AUCcombined$logit.AUC)
  AUCcombined$LL <- inv.logit(AUCcombined$logit.LL)
  AUCcombined$UL <- inv.logit(AUCcombined$logit.UL)
  AUCcombined <- AUCcombined[order(AUCcombined$SampleSize, decreasing = TRUE),]
  
  AUCoverall <- matrix(nrow = 2, ncol = 6)
  colnames(AUCoverall) <- c('Center', 'SampleSize', 'Prevalence', 'AUC', 'LL', 'UL')
  AUCoverall <- data.frame(AUCoverall)
  AUCoverall$Center <- c("Meta-analysis", "95% Prediction interval")
  AUCoverall$SampleSize <- nrow(Df[Df$imp == 1,])
  AUCoverall$Prevalence <- round(mean(PrevalenceOverall))
  
  # Meta-analyse voor overall estimate
  fit.RE = rma.uni(AUCcombined$logit.AUC, sei = AUCcombined$PooledSE, method = method.MA)
  PI = predict(fit.RE, transf = transf.ilogit)
  
  AUCoverall$AUC[1] <- inv.logit(coef(fit.RE))
  AUCoverall$LL[1] <- inv.logit(fit.RE$ci.lb)
  AUCoverall$UL[1] <- inv.logit(fit.RE$ci.ub)
  AUCoverall$AUC[2] <- PI$pred
  AUCoverall$LL[2] <- PI$cr.lb
  AUCoverall$UL[2] <- PI$cr.ub
  AUCoverall
  
  NAforest <- matrix(nrow = 1, ncol = 6)
  colnames(NAforest) <- c('Center', 'SampleSize', 'Prevalence', 'AUC', 'LL', 'UL')
  NAforest <- data.frame(NAforest)
  NAforest <- NA
  
  AUC <- rbind(AUCcombined[, c('Center', 'SampleSize', 'Prevalence', 'AUC', 'LL', 'UL')], NAforest, NAforest, AUCoverall)
  
  # Layout for forestplot
  RRcenter <- c('Other', 'Meta-analysis')
  nrobs <- nrow(AUC)
  RRcenter <- 1:nrobs
  for(i in 1:nrobs){
    RRcenter[i] <- AUC$Center[i]
  }
  RRauc <- 1:nrobs
  for(i in 1:nrobs){
    RRauc[i] <- paste(format(round(AUC$AUC[i], 2), nsmall = 2), " (", format(round(AUC$LL[i], 2), nsmall = 2), " to ", format(round(AUC$UL[i], 2), nsmall = 2), ")", sep = "")
  }
  RRprev <- 1:nrobs
  for(i in 1:nrobs){
    RRprev[i] <- AUC$SampleSize[i]
  }
  
  Labels <- c('Centre', 'AUC (95% CI)', 'N') 
  Combined <- cbind(RRcenter, RRauc, RRprev)
  Tabletext <- rbind(Labels, NAforest, Combined)
  
  AUCna <- rbind(NAforest, NAforest, AUC)
  
  return(structure(list(Performance = AUCoverall, ModelFit = fit.RE, AUCcenters = AUCcombined[, c('Center', 'SampleSize', 'Prevalence', 'AUC', 'LL', 'UL')], IncludedCenters = centers, dataPlot = AUCna, Plot = Tabletext, data = Df)))
  
}

# getting the cIF for each of the 9 models
mm1_predapp<-read.csv("predictions/pred_multinomial_LR.csv")[ ,3:4]
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
IECV_cIF_1<-AUCimp.IOTA.cIF(pred=mm1_cIF, outcome=Outcome3, center=Centre_numeric, imp=.imp, data=mydata)


mm1_predapp<-read.csv("predictions/pred_multinomial_LR_with_Transformations.csv")[ ,3:4]
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
IECV_cIF_2<-AUCimp.IOTA.cIF(pred=mm1_cIF, outcome=Outcome3, center=Centre_numeric, imp=.imp, data=mydata)

mm1_predapp<-read.csv("predictions/pred_multinomial_Ridge.csv")[ ,3:4]
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
IECV_cIF_3<-AUCimp.IOTA.cIF(pred=mm1_cIF, outcome=Outcome3, center=Centre_numeric, imp=.imp, data=mydata)


mm1_predapp<-read.csv("predictions/pred_multinomial_Firth.csv")[ ,3:4]
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
IECV_cIF_4<-AUCimp.IOTA.cIF(pred=mm1_cIF, outcome=Outcome3, center=Centre_numeric, imp=.imp, data=mydata)

#need to correct for 0,1 probs in CART
mm1_predapp<-read.csv("predictions/pred_multinomial_CART.csv")[ ,3:4]
mm1_predapp$FPUL_mm5[mm1_predapp$FPUL_mm5 == 0] <- 0.001
mm1_predapp$IUP_mm5[mm1_predapp$IUP_mm5 == 0] <- 0.001
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
IECV_cIF_5<-AUCimp.IOTA.cIF(pred=mm1_cIF, outcome=Outcome3, center=Centre_numeric, imp=.imp, data=mydata)


mm1_predapp<-read.csv("predictions/pred_multinomial_RF.csv")[ ,3:4]
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
IECV_cIF_6<-AUCimp.IOTA.cIF(pred=mm1_cIF, outcome=Outcome3, center=Centre_numeric, imp=.imp, data=mydata)

mm1_predapp<-read.csv("predictions/pred_multinomial_XGB.csv")[ ,3:4]
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
IECV_cIF_7<-AUCimp.IOTA.cIF(pred=mm1_cIF, outcome=Outcome3, center=Centre_numeric, imp=.imp, data=mydata)


mm1_predapp<-read.csv("predictions/pred_multinomial_SVM.csv")[ ,3:4]
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
IECV_cIF_8<-AUCimp.IOTA.cIF(pred=mm1_cIF, outcome=Outcome3, center=Centre_numeric, imp=.imp, data=mydata)


mm1_predapp<-read.csv("predictions/pred_multinomial_NN.csv")[ ,3:4]
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
IECV_cIF_9<-AUCimp.IOTA.cIF(pred=mm1_cIF, outcome=Outcome3, center=Centre_numeric, imp=.imp, data=mydata)




### overall Forest plot IECV performance cIF ###
NA.forest <- IECV_cIF_1$Performance[1,]
NA.forest <- NA
Summary.AUC <- rbind(NA.forest, IECV_cIF_1$Performance[1,], IECV_cIF_2$Performance[1,], IECV_cIF_3$Performance[1,], IECV_cIF_4$Performance[1,],IECV_cIF_5$Performance[1,], IECV_cIF_6$Performance[1,], IECV_cIF_7$Performance[1,], IECV_cIF_8$Performance[1,], IECV_cIF_9$Performance[1,])
Summary.AUC$Model <- c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
Summary.AUC.PI <- rbind(NA.forest, IECV_cIF_1$Performance[2,], IECV_cIF_2$Performance[2,], IECV_cIF_3$Performance[2,], IECV_cIF_4$Performance[2,], IECV_cIF_5$Performance[2,], IECV_cIF_6$Performance[2,], IECV_cIF_7$Performance[2,], IECV_cIF_8$Performance[2,], IECV_cIF_9$Performance[2,])
Summary.AUC.PI$Model <-  c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
tabletext <- cbind(
  c('Model', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN"),
  c('cIF (95% CI)', 
    paste(format(round(IECV_cIF_1$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV_cIF_1$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV_cIF_1$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV_cIF_2$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV_cIF_2$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV_cIF_2$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV_cIF_3$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV_cIF_3$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV_cIF_3$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV_cIF_4$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV_cIF_4$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV_cIF_4$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV_cIF_5$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV_cIF_5$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV_cIF_5$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV_cIF_6$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV_cIF_6$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV_cIF_6$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV_cIF_7$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV_cIF_7$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV_cIF_7$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV_cIF_8$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV_cIF_8$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV_cIF_8$Performance$UL[1], 2), nsmall = 2), ")", sep= ""),
    paste(format(round(IECV_cIF_9$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV_cIF_9$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV_cIF_9$Performance$UL[1], 2), nsmall = 2), ")", sep="")),
  c('95% PI', 
    paste0("(", format(round(IECV_cIF_1$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV_cIF_1$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECV_cIF_2$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV_cIF_2$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECV_cIF_3$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV_cIF_3$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECV_cIF_4$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV_cIF_4$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECV_cIF_5$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV_cIF_5$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECV_cIF_6$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV_cIF_6$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECV_cIF_7$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV_cIF_7$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECV_cIF_8$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV_cIF_8$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECV_cIF_9$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV_cIF_9$Performance$UL[2], 2), nsmall = 2), ")")))

tiff("graphs/IECV_multinomial_cIF_all_models", width = 31, height = 13.75, units = "cm", res = 300)
forestplot(labeltext = tabletext,
           title = "cIF per model",
           mean = round(Summary.AUC$AUC, 3),
           lower = round(Summary.AUC$LL, 3),
           upper = round(Summary.AUC$UL, 3),
           is.summary = c(FALSE, TRUE, TRUE, TRUE, TRUE,TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
           xlab = "cIF",
           boxsize = .5,
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55, fontface = "plain"), xlab = gpar(cex = 1.5, fontface = "plain"), label = gpar(cex = 1.5, fontface = "bold"),
                            ticks = gpar(cex = 1.5, fontface = "plain"), title = gpar(cex = 1.75)),
           graphwidth = unit(9, "cm"),
           graph.pos = 3,
           xticks = c(0.70,0.8, 0.9, 0.9, 1), xlog = TRUE, clip = c(0.6, 1))     
dev.off()



#############################################
###Classification performance 5% threshold###
############################################

performance_1<-SS.imp(pred=mm1_pred, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_1[["CenterPer"]][["TP"]])/ sum(performance_1[["CenterPer"]][["TP"]] + performance_1[["CenterPer"]][["FP"]])
#npv
sum(performance_1[["CenterPer"]][["TN"]])/ sum(performance_1[["CenterPer"]][["TN"]] + performance_1[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_1[["CenterPer"]][["TP"]]+ performance_1[["CenterPer"]][["FP"]])/ 2894

performance_2<-SS.imp(pred=mm2_pred, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_2[["CenterPer"]][["TP"]])/ sum(performance_2[["CenterPer"]][["TP"]] + performance_2[["CenterPer"]][["FP"]])
#npv
sum(performance_2[["CenterPer"]][["TN"]])/ sum(performance_2[["CenterPer"]][["TN"]] + performance_2[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_2[["CenterPer"]][["TP"]]+ performance_2[["CenterPer"]][["FP"]])/ 2894

performance_3<-SS.imp(pred=mm3_pred, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_3[["CenterPer"]][["TP"]])/ sum(performance_3[["CenterPer"]][["TP"]] + performance_3[["CenterPer"]][["FP"]])
#npv
sum(performance_3[["CenterPer"]][["TN"]])/ sum(performance_3[["CenterPer"]][["TN"]] + performance_3[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_3[["CenterPer"]][["TP"]]+ performance_3[["CenterPer"]][["FP"]])/ 2894

performance_4<-SS.imp(pred=mm4_pred, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_4[["CenterPer"]][["TP"]])/ sum(performance_4[["CenterPer"]][["TP"]] + performance_4[["CenterPer"]][["FP"]])
#npv
sum(performance_4[["CenterPer"]][["TN"]])/ sum(performance_4[["CenterPer"]][["TN"]] + performance_4[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_4[["CenterPer"]][["TP"]]+ performance_4[["CenterPer"]][["FP"]])/ 2894

performance_5<-SS.imp(pred=mm5_pred, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_5[["CenterPer"]][["TP"]])/ sum(performance_5[["CenterPer"]][["TP"]] + performance_5[["CenterPer"]][["FP"]])
#npv
sum(performance_5[["CenterPer"]][["TN"]])/ sum(performance_5[["CenterPer"]][["TN"]] + performance_5[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_5[["CenterPer"]][["TP"]]+ performance_5[["CenterPer"]][["FP"]])/ 2894

performance_6<-SS.imp(pred=mm6_pred, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_6[["CenterPer"]][["TP"]])/ sum(performance_6[["CenterPer"]][["TP"]] + performance_6[["CenterPer"]][["FP"]])
#npv
sum(performance_6[["CenterPer"]][["TN"]])/ sum(performance_6[["CenterPer"]][["TN"]] + performance_6[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_6[["CenterPer"]][["TP"]]+ performance_6[["CenterPer"]][["FP"]])/ 2894

performance_7<-SS.imp(pred=mm7_pred, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_7[["CenterPer"]][["TP"]])/ sum(performance_7[["CenterPer"]][["TP"]] + performance_7[["CenterPer"]][["FP"]])
#npv
sum(performance_7[["CenterPer"]][["TN"]])/ sum(performance_7[["CenterPer"]][["TN"]] + performance_7[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_7[["CenterPer"]][["TP"]]+ performance_7[["CenterPer"]][["FP"]])/ 2894


performance_8<-SS.imp(pred= mm8_pred, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_8[["CenterPer"]][["TP"]])/ sum(performance_8[["CenterPer"]][["TP"]] + performance_8[["CenterPer"]][["FP"]])
#npv
sum(performance_8[["CenterPer"]][["TN"]])/ sum(performance_8[["CenterPer"]][["TN"]] + performance_8[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_8[["CenterPer"]][["TP"]]+ performance_8[["CenterPer"]][["FP"]])/ 2894


performance_9<-SS.imp(pred=mm9_pred, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_9[["CenterPer"]][["TP"]])/ sum(performance_9[["CenterPer"]][["TP"]] + performance_9[["CenterPer"]][["FP"]])
#npv
sum(performance_9[["CenterPer"]][["TN"]])/ sum(performance_9[["CenterPer"]][["TN"]] + performance_9[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_9[["CenterPer"]][["TP"]]+ performance_9[["CenterPer"]][["FP"]])/ 2894




