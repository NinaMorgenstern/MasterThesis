
##########################
########## IECV# ########
########################
set.seed(123)
center_names<-c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")

######## Logistic Regression without transformations ###################
prediction_matrix <- matrix(nrow = 0, ncol = 10) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)
for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=10)
  mods11 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_k[mydata_k$Centre_numeric==i,]), ncol = 0)
    mods11[[i]]  <- glm(Outcome2~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, family=binomial(link="logit"), data=mydata_1[mydata_1$Centre_numeric!=i,]) #Build the model on all but one
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods11[[i]] , type="response", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer, pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
}

average_predictions <- rowMeans(prediction_matrix)
mydata['average_predictions_m1'] <-average_predictions 
IECV1auc<-AUCimp.IOTA(mydata$prediction_1, outcome = mydata$Outcome2, data=mydata, center=Centre, imp=mydata$.imp)
IECV1auc$Performance['AUC']
write.csv(mydata['average_predictions_m1'], "pred_binary_LR.csv")

# Forest Plot AUC per center
IECV1auc$Plot
IECV1auc$Plot[11, "RRauc"] <- "   "
IECV1auc$Plot[12, "RRauc"] <- "   "
IECV1auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "AUC (95 % CI)", "95 % Prediction Interval")


tiff("binary_LR_IECV_AUC.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECV1auc$Plot,
           align = c("l", "c", "c"),
           mean = IECV1auc$dataPlot$AUC,
           lower = IECV1auc$dataPlot$LL,
           upper = IECV1auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECV1auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Binary LR: IECV performance",
           xlab = "AUC",
           xlog = TRUE,
           xticks = c(0.6, 0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECV1auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECV1auc$Performance$AUC)
dev.off()




##################################### LR with Transformations ######################################

set.seed(123)
center_names<-c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")

prediction_matrix <- matrix(nrow = 0, ncol = 10) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)
for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=10)
  mods22 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_k[mydata_k$Centre_numeric==i,]), ncol = 0)
    mods22[[i]]  <- glm(Outcome2~log.hCG0num+ log.prog0num + I(log.prog0num^2) + lhCGratio +  I(lhCGratio^2)  + lhCGratio_log.prog0num  + Agesd + I(Agesd^2) + Vaginal.bleeding + Histep + Painscore + Painscore2 , family=binomial(link="logit"), data=mydata_1[mydata_1$Centre_numeric!=i,]) #Build the model on all but one
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods22[[i]] , type="response", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer, pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

average_predictions <- rowMeans(prediction_matrix)
mydata['average_predictions_m2'] <-average_predictions 

IECV67auc<-AUCimp.IOTA(mydata$prediction_1, outcome = mydata$Outcome2, data=mydata, center=Centre, imp=mydata$.imp)
IECV2auc$Performance['AUC']
write.csv(mydata['average_predictions_m2'], "predictions/pred_binary_LR_with_Transformations.csv")


# Forest Plot AUC per center
IECV2auc$Plot
IECV2auc$Plot[11, "RRauc"] <- "   "
IECV2auc$Plot[12, "RRauc"] <- "   "
IECV2auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "AUC (95 % CI)", "95 % Prediction Interval")

tiff("binary_LR_withTransformations_IECV_AUC.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECV2auc$Plot,
           align = c("l", "c", "c"),
           mean = IECV2auc$dataPlot$AUC,
           lower = IECV2auc$dataPlot$LL,
           upper = IECV2auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECV2auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Binary LR with Transformations: IECV performance",
           xlab = "AUC",
           xlog = TRUE,
           xticks = c(0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECV2auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECV2auc$Performance$AUC)
dev.off()





###################### Ridge Regression ###########################

set.seed(123)
center_names<-c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")
prediction_matrix <- matrix(nrow = 0, ncol = 10) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)

y_train_std <- mydata[, c(".imp","Centre_numeric","Outcome2")]
x_train_std <- mydata[, c(".imp","Centre_numeric","log.hCG0num","log.prog0num","lhCGratio","Agesd","Vaginal.bleeding","Histep","Painscore","lhCGratio_log.prog0num",'Painscore2')]
x_train_std$X2.log.prog0num <-x_train_std$log.prog0num^2
x_train_std$X2.lhCGratio <-x_train_std$lhCGratio^2
x_train_std$X2.Agesd<- x_train_std$Agesd^2

for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=10)
  mods33 <- vector(mode="list", length=8)
  x_train_std_1<-x_train_std[x_train_std$.imp==j,]
  y_train_std_1<-y_train_std[y_train_std$.imp==j,]
  for (i in 1:8){
    X<-x_train_std_1[x_train_std_1$Centre_numeric!=i,]
    Y<-y_train_std_1[y_train_std_1$Centre_numeric!=i,]
    Y_final <-Y[,-c(1,2)]
    X_final <-data.matrix(X[,-c(1,2)])
    cv_train_std <- cv.glmnet(X_final, Y_final, nfolds=10, type.measure="deviance", alpha=0, family="binomial")
    lambda <- cv_train_std$lambda.min
    mods33[[i]] <- glmnet(X_final, Y_final, family="binomial", lambda=lambda, alpha=0)
    answer<-matrix(nrow =nrow(mydata[mydata$Centre_numeric==i & mydata$.imp==k,]), ncol = 0)
    for (k in 1:10){
      x_train_std_k<-x_train_std[x_train_std$.imp==k,]
      X_new<-x_train_std_k[x_train_std_k$Centre_numeric==i,]
      X_new1 <- as.matrix(X_new[,-c(1,2)])
      pred <- predict( mods33[[i]] , type="response", newx=X_new1)
      answer <- cbind(answer, pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

average_predictions <- rowMeans(prediction_matrix)
mydata['average_predictions_m3'] <-average_predictions 
IECV3auc<-AUCimp.IOTA(mydata$average_predictions_m3, outcome = mydata$Outcome2, data=mydata, center=Centre, imp=mydata$.imp)
IECV3auc$Performance['AUC']
write.csv(mydata['average_predictions_m3'], "predictions/pred_binary_Ridge.csv")

# Forest Plot AUC per center
IECV3auc$Plot
IECV3auc$Plot[11, "RRauc"] <- "   "
IECV3auc$Plot[12, "RRauc"] <- "   "
IECV3auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "AUC (95 % CI)", "95 % Prediction Interval")

tiff("binary_Ridge_IECV_AUC.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECV3auc$Plot,
           align = c("l", "c", "c"),
           mean = IECV3auc$dataPlot$AUC,
           lower = IECV3auc$dataPlot$LL,
           upper = IECV3auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECV3auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Binary Ridge: IECV performance",
           xlab = "AUC",
           xlog = TRUE,
           xticks = c(0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECV3auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECV3auc$Performance$AUC)
dev.off()

######################### Firth Regression ################################
set.seed(123)
center_names<-c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")

prediction_matrix <- matrix(nrow = 0, ncol = 10) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)
for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=10)
  mods44 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_k[mydata_k$Centre_numeric==i,]), ncol = 0)
    mods44[[i]]  <- glm(Outcome2~log.hCG0num+ log.prog0num + I(log.prog0num^2) + lhCGratio +  I(lhCGratio^2)  + lhCGratio_log.prog0num  + Agesd + I(Agesd^2) + Vaginal.bleeding + Histep + Painscore + Painscore2 ,  family = binomial, method="brglmFit", data=mydata_1[mydata_1$Centre_numeric!=i,]) #Build the model on all but one
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods44[[i]] , type="response", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer, pred)
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

average_predictions <- rowMeans(prediction_matrix)
mydata['average_predictions_m4'] <-average_predictions 
IECV4auc<-AUCimp.IOTA(mydata$average_predictions_m4, outcome = mydata$Outcome2, data=mydata, center=Centre, imp=mydata$.imp)
IECV4auc$Performance['AUC']
write.csv(mydata['average_predictions_m4'], "predictions/pred_binary_Firth.csv")


# Forest Plot AUC per center
IECV4auc$Plot
IECV4auc$Plot[11, "RRauc"] <- "   "
IECV4auc$Plot[12, "RRauc"] <- "   "
IECV4auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "AUC (95 % CI)", "95 % Prediction Interval")

tiff("binary_Firth_IECV_AUC.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECV4auc$Plot,
           align = c("l", "c", "c"),
           mean = IECV4auc$dataPlot$AUC,
           lower = IECV4auc$dataPlot$LL,
           upper = IECV4auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECV4auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Binary Firth: IECV performance",
           xlab = "AUC",
           xlog = TRUE,
           xticks = c(0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECV4auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECV4auc$Performance$AUC)
dev.off()



############################## CART #####################################


set.seed(123)
ctrl <- trainControl(method="cv",   # 10fold cross validation
                     number=10,         
                     summaryFunction=LogLoss,
                     classProbs=TRUE)

prediction_matrix <- matrix(nrow = 0, ncol = 10) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)

for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=10)
  mods55 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_k[mydata_k$Centre_numeric==i,]), ncol = 0)
    mods55[[i]]<- train(make.names(Outcome2)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, data=mydata_1[mydata_1$Centre_numeric!=i,], method="rpart",metric="LogLoss", trControl=ctrl, tuneLength = 40)
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods55[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred[,2])
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

average_predictions <- rowMeans(prediction_matrix)
mydata['average_predictions_m5'] <-average_predictions 
IECV5auc<-AUCimp.IOTA(mydata$average_predictions_m5, outcome = mydata$Outcome2, data=mydata, center=Centre, imp=mydata$.imp)
IECV5auc$Performance['AUC']
write.csv(mydata['average_predictions_m5'], "predictions/pred_binary_CART.csv")

# Forest Plot AUC per center
IECV5auc$Plot
IECV5auc$Plot[11, "RRauc"] <- "   "
IECV5auc$Plot[12, "RRauc"] <- "   "
IECV5auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "AUC (95 % CI)", "95 % Prediction Interval")

tiff("binary_CART_IECV_AUC.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECV5auc$Plot,
           align = c("l", "c", "c"),
           mean = IECV5auc$dataPlot$AUC,
           lower = IECV5auc$dataPlot$LL,
           upper = IECV5auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECV5auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Binary CART: IECV performance",
           xlab = "AUC",
           xlog = TRUE,
           xticks = c(0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECV5auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECV5auc$Performance$AUC)
dev.off()




########################## RANDFOM FOREST ###############################
set.seed(123)
control <- trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=LogLoss,search='grid')
hyper_grid <- expand.grid(mtry= c(1:4), min.node.size = c(1, 4, 6, 8, 10), splitrule="gini")

prediction_matrix <- matrix(nrow = 0, ncol = 10) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)

for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=10)
  mods66 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_k[mydata_k$Centre_numeric==i,]), ncol = 0)
    mods66[[i]]<-train(make.names(Outcome2)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, 
                       data=mydata_1[mydata_1$Centre_numeric!=i,],
                       method = "ranger",
                       trControl = control,
                       tuneGrid = hyper_grid,
                       metric="LogLoss")
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods66[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred[,2])
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

average_predictions <- rowMeans(prediction_matrix)
mydata['average_predictions_m6'] <-average_predictions 
IECV6auc<-AUCimp.IOTA(mydata$average_predictions_m6, outcome = mydata$Outcome2, data=mydata, center=Centre, imp=mydata$.imp)
IECV6auc$Performance['AUC']
write.csv(mydata['average_predictions_m6'], "predictions/pred_binary_RF.csv")

# Forest Plot AUC per center
IECV6auc$Plot
IECV6auc$Plot[11, "RRauc"] <- "   "
IECV6auc$Plot[12, "RRauc"] <- "   "
IECV6auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "AUC (95 % CI)", "95 % Prediction Interval")

tiff("binary_RF_IECV_AUC.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECV6auc$Plot,
           align = c("l", "c", "c"),
           mean = IECV6auc$dataPlot$AUC,
           lower = IECV6auc$dataPlot$LL,
           upper = IECV6auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECV6auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Binary RF: IECV performance",
           xlab = "AUC",
           xlog = TRUE,
           xticks = c(0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECV6auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECV6auc$Performance$AUC)
dev.off()


################################ XGBoost #####################################
set.seed(123)
xgb_trcontrol = trainControl(method = "cv", number = 10, 
                             verboseIter = FALSE, summaryFunction=LogLoss,
                             classProbs=TRUE, allowParallel = TRUE, returnData = FALSE)

xgbgrid <- expand.grid(nrounds=100,
                       eta=c(0.01, 0.001,0.0001, 0.1, 0.15, 0.2, 0.3),
                       max_depth=6, #6 is default c(5,7,9,11),
                       gamma=0,
                       colsample_bytree=1,
                       min_child_weight=1,
                       subsample=1)

prediction_matrix <- matrix(nrow = 0, ncol = 10) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)

for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=10)
  mods77 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  mydata_1$Centre_numeric<-as.numeric(mydata_1$Centre)
  for (i in 1:8){
    answer<-matrix(nrow =nrow(mydata_k[mydata_k$Centre_numeric==i,]), ncol = 0)
    mods77[[i]]<-train(as.factor(make.names(Outcome2))~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore,
                       trControl = xgb_trcontrol, 
                       method = "xgbTree",
                       tuneLength=10,
                       #tuneGrid=xgbgrid,
                       data=mydata_1[mydata_1$Centre_numeric!=i,],
                       metric="LogLoss")
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods77[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred[,2])
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}

average_predictions <- rowMeans(prediction_matrix)
mydata['average_predictions_m7'] <-average_predictions 
IECV7auc<-AUCimp.IOTA(mydata$average_predictions_m7, outcome = mydata$Outcome2, data=mydata, center=Centre, imp=mydata$.imp)
IECV7auc$Performance['AUC']
write.csv(mydata['average_predictions_m7'], "predictions/pred_binary_XGB.csv")

# Forest Plot AUC per center
IECV7auc$Plot
IECV7auc$Plot[11, "RRauc"] <- "   "
IECV7auc$Plot[12, "RRauc"] <- "   "
IECV7auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "AUC (95 % CI)", "95 % Prediction Interval")

tiff("binary_XGB_IECV_AUC.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECV7auc$Plot,
           align = c("l", "c", "c"),
           mean = IECV7auc$dataPlot$AUC,
           lower = IECV7auc$dataPlot$LL,
           upper = IECV7auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECV7auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Binary XGB: IECV performance",
           xlab = "AUC",
           xlog = TRUE,
           xticks = c(0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECV7auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECV7auc$Performance$AUC)
dev.off()



############################## SVM #######################################

set.seed(123)
ctrl <- trainControl(method="cv",   # 10fold cross validation
                     number=10,         
                     summaryFunction=LogLoss,
                     search="random",
                     classProbs=TRUE)
center_names<-c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")
prediction_matrix <- matrix(nrow = 0, ncol = 10) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)


for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=10)
  mods88 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  for (i in 1:8){
    mods88[[i]] <- train(make.names(Outcome2)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore,
                         method = "svmRadial",# Radial basis function kernel (gaussian)
                         tuneLength = 10,
                         preProcess = c("center", "scale"),
                         metric = "LogLoss",
                         data=mydata_1[mydata_1$Centre_numeric!=i,],
                         trControl=ctrl) 
    answer<-matrix(nrow =nrow(mydata[mydata$Centre_numeric==i & mydata$.imp==k,]), ncol = 0)
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods88[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred[,2])
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}


average_predictions <- rowMeans(prediction_matrix)
mydata['average_predictions_m8'] <-average_predictions 
IECV8auc<-AUCimp.IOTA(mydata$average_predictions_m8, outcome = mydata$Outcome2, data=mydata, center=Centre, imp=mydata$.imp)
IECV8auc$Performance['AUC']
write.csv(mydata['average_predictions_m8'], "predictions/pred_binary_SVM.csv")

# Forest Plot AUC per center
IECV8auc$Plot
IECV8auc$Plot[11, "RRauc"] <- "   "
IECV8auc$Plot[12, "RRauc"] <- "   "
IECV8auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "AUC (95 % CI)", "95 % Prediction Interval")

tiff("binary_SVM_IECV_AUC.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECV8auc$Plot,
           align = c("l", "c", "c"),
           mean = IECV8auc$dataPlot$AUC,
           lower = IECV8auc$dataPlot$LL,
           upper = IECV8auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECV8auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Binary SVM: IECV performance",
           xlab = "AUC",
           xlog = TRUE,
           xticks = c(0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECV8auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECV8auc$Performance$AUC)
dev.off()







############################## NN #######################################


set.seed(123)
ctrl <- trainControl(method="cv", 
                     number=10,         
                     summaryFunction=LogLoss,
                     search="random",
                     classProbs=TRUE)
center_names<-c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")
prediction_matrix <- matrix(nrow = 0, ncol = 10) 
mydata$Centre_numeric<-as.numeric(mydata$Centre)


for (j in 1:10){
  answer2<- matrix(nrow=0, ncol=10)
  mods99 <- vector(mode="list", length=8)
  mydata_1<-mydata[mydata$.imp==j,]
  for (i in 1:8){
    mods99[[i]] <- train(make.names(Outcome2)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore,
                         method = "nnet",
                         tuneLength = 10,
                         preProcess = c("center", "scale"),
                         metric = "LogLoss",
                         trace = FALSE,
                         maxit = 1000,
                         linout = FALSE,
                         data= mydata_1[mydata_1$Centre_numeric!=i,],
                         trControl=ctrl) 
    answer<-matrix(nrow =nrow(mydata[mydata$Centre_numeric==i & mydata$.imp==k,]), ncol = 0)
    for (k in 1:10){
      mydata_k<-mydata[mydata$.imp==k,]
      pred <- predict( mods99[[i]] , type="prob", newdata=mydata_k[mydata_k$Centre_numeric==i,])
      answer <- cbind(answer,  pred[,2])
    }
    answer2<-rbind(answer2, answer)
  }
  prediction_matrix<-rbind(prediction_matrix, answer2)
  print(j)
}


average_predictions <- rowMeans(prediction_matrix)
mydata['average_predictions_m9'] <-average_predictions 
IECV9auc<-AUCimp.IOTA(mydata$average_predictions_m9, outcome = mydata$Outcome2, data=mydata, center=Centre, imp=mydata$.imp)
IECV9auc$Performance['AUC']
write.csv(mydata['average_predictions_m9'], "predictions/pred_binary_NN.csv")


# Forest Plot AUC per center
IECV9auc$Plot
IECV9auc$Plot[11, "RRauc"] <- "   "
IECV9auc$Plot[12, "RRauc"] <- "   "
IECV9auc$Plot[, "RRcenter"] <-c("Centre", "",  "St. Marys", "Hillingdon","Chelsea and Westminster", "Queen Charlotte's and Chelsea", "Wexham Park","West Middlesex","Royal Surrey", "North Middlesex", "",  "Meta-analysis", "AUC (95 % CI)", "95 % Prediction Interval")

tiff("binary_NN_IECV_AUC.tiff", width = 31, height = 20, units = "cm", res = 300)

forestplot(IECV9auc$Plot,
           align = c("l", "c", "c"),
           mean = IECV9auc$dataPlot$AUC,
           lower = IECV9auc$dataPlot$LL,
           upper = IECV9auc$dataPlot$UL,
           is.summary = c(TRUE, FALSE, rep(FALSE, length(IECV9auc$IncludedCenters)), TRUE, TRUE, TRUE),
           title = "Binary NN: IECV performance",
           xlab = "AUC",
           xlog = TRUE,
           xticks = c(0.7, 0.8,0.9,1),
           clip = c(0.55, 1),
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55), xlab = gpar(cex = 1.5), label = gpar(cex = 1.5),
                            ticks = gpar(cex = 1.5), title = gpar(cex = 1.75)),
           graphwidth = unit(6, "cm"),
           col = fpColors(box = "gray", lines = "black", summary = "black"),
           fn.ci_sum = c(as.list(rep("fpDrawSummaryCI", length(IECV9auc$IncludedCenters) + 4)),
                         fpDrawSummaryCI,
                         fpDrawBarCI),
           zero = IECV9auc$Performance$AUC)
dev.off()



##################################
######## Overall Forest plot ####
#################++#############

NA.forest <- IECV1auc$Performance[1,]
NA.forest <- NA
Summary.AUC <- rbind(NA.forest, IECV1auc$Performance[1,], IECV2auc$Performance[1,], IECV3auc$Performance[1,], IECV4auc$Performance[1,],IECV5auc$Performance[1,], IECV6auc$Performance[1,], IECV7auc$Performance[1,], IECV8auc$Performance[1,], IECV9auc$Performance[1,])
Summary.AUC$Model <- c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
Summary.AUC.PI <- rbind(NA.forest, IECV1auc$Performance[2,], IECV2auc$Performance[2,], IECV3auc$Performance[2,], IECV4auc$Performance[2,], IECV5auc$Performance[2,], IECV6auc$Performance[2,], IECV7auc$Performance[2,], IECV8auc$Performance[2,], IECV9auc$Performance[2,])
Summary.AUC.PI$Model <-  c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
tabletext <- cbind(
  c('Model', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN"),
  c('AUC (95% CI)', 
    paste(format(round(IECV1auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV1auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV1auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV2auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV2auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV2auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV3auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV3auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV3auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV4auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV4auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV4auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV5auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV5auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV5auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV6auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV6auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV6auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV7auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV7auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV7auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(IECV8auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV8auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV8auc$Performance$UL[1], 2), nsmall = 2), ")", sep= ""),
    paste(format(round(IECV9auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(IECV9auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(IECV9auc$Performance$UL[1], 2), nsmall = 2), ")", sep="")),
  c('95% PI', 
    paste0("(", format(round(IECV1auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV1auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECV2auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV2auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECV3auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV3auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECV4auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV4auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(IECV5auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV5auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECV6auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV6auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECV7auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV7auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECV8auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV8auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(IECV9auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(IECV9auc$Performance$UL[2], 2), nsmall = 2), ")")))

tiff("graphs/IECV_binary_AUC_all_models.tiff", width = 31, height = 13.75, units = "cm", res = 300)
forestplot(labeltext = tabletext,
           title = "AUC per model",
           mean = round(Summary.AUC$AUC, 3),
           lower = round(Summary.AUC$LL, 3),
           upper = round(Summary.AUC$UL, 3),
           is.summary = c(FALSE, TRUE, TRUE, TRUE, TRUE,TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
           xlab = "AUC",
           boxsize = .5,
           txt_gp = fpTxtGp(summary = gpar(cex = 1.55, fontface = "plain"), xlab = gpar(cex = 1.5, fontface = "plain"), label = gpar(cex = 1.5, fontface = "bold"),
                            ticks = gpar(cex = 1.5, fontface = "plain"), title = gpar(cex = 1.75)),
           graphwidth = unit(9, "cm"),
           graph.pos = 3,
           xticks = c(0.70,0.8, 0.9, 0.9, 1), xlog = TRUE, clip = c(0.6, 1))     
dev.off()


############################
### Get Predictions ########
############################

predictions_1 <-read.csv('predictions/pred_binary_LR.csv')
mydata['prediction_1']<-predictions_1[,2]

predictions_2 <-read.csv('predictions/pred_binary_LR_with_Transformations.csv')
mydata['prediction_2']<-predictions_2[,2]

predictions_3 <-read.csv('predictions/pred_binary_Ridge.csv')
mydata['prediction_3']<-predictions_3[,2]

predictions_4 <-read.csv('predictions/pred_binary_Firth.csv')
mydata['prediction_4']<-predictions_4[,2]

predictions_5 <-read.csv('predictions/pred_binary_CART.csv')
mydata['prediction_5']<-predictions_5[,2]

predictions_6 <-read.csv('predictions/pred_binary_RF.csv')
mydata['prediction_6']<-predictions_6[,2] 

predictions_7 <-read.csv('predictions/pred_binary_XGB.csv')
mydata['prediction_7']<-predictions_7[,2]

predictions_8 <-read.csv('predictions/pred_binary_SVM.csv')
mydata['prediction_8']<-predictions_8[,2]

predictions_9 <-read.csv('predictions/pred_binary_NN.csv')
mydata['prediction_9']<-predictions_9[,2]




##############################
##Classification performance##
##############################

performance_1<-SS.imp(pred=average_predictions_m1, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_1[["CenterPer"]][["TP"]])/ sum(performance_1[["CenterPer"]][["TP"]] + performance_1[["CenterPer"]][["FP"]])
#npv
sum(performance_1[["CenterPer"]][["TN"]])/ sum(performance_1[["CenterPer"]][["TN"]] + performance_1[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_1[["CenterPer"]][["TP"]]+ performance_1[["CenterPer"]][["FP"]])/ 2894

performance_2<-SS.imp(pred=average_predictions_m2, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_2[["CenterPer"]][["TP"]])/ sum(performance_2[["CenterPer"]][["TP"]] + performance_2[["CenterPer"]][["FP"]])
#npv
sum(performance_2[["CenterPer"]][["TN"]])/ sum(performance_2[["CenterPer"]][["TN"]] + performance_2[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_2[["CenterPer"]][["TP"]]+ performance_2[["CenterPer"]][["FP"]])/ 2894

performance_3<-SS.imp(pred=average_predictions_m3, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_3[["CenterPer"]][["TP"]])/ sum(performance_3[["CenterPer"]][["TP"]] + performance_3[["CenterPer"]][["FP"]])
#npv
sum(performance_3[["CenterPer"]][["TN"]])/ sum(performance_3[["CenterPer"]][["TN"]] + performance_3[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_3[["CenterPer"]][["TP"]]+ performance_3[["CenterPer"]][["FP"]])/ 2894

performance_4<-SS.imp(pred=average_predictions_m4, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_4[["CenterPer"]][["TP"]])/ sum(performance_4[["CenterPer"]][["TP"]] + performance_4[["CenterPer"]][["FP"]])
#npv
sum(performance_4[["CenterPer"]][["TN"]])/ sum(performance_4[["CenterPer"]][["TN"]] + performance_4[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_4[["CenterPer"]][["TP"]]+ performance_4[["CenterPer"]][["FP"]])/ 2894

performance_5<-SS.imp(pred=average_predictions_m5, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_5[["CenterPer"]][["TP"]])/ sum(performance_5[["CenterPer"]][["TP"]] + performance_5[["CenterPer"]][["FP"]])
#npv
sum(performance_5[["CenterPer"]][["TN"]])/ sum(performance_5[["CenterPer"]][["TN"]] + performance_5[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_5[["CenterPer"]][["TP"]]+ performance_5[["CenterPer"]][["FP"]])/ 2894

performance_6<-SS.imp(pred=average_predictions_m6, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_6[["CenterPer"]][["TP"]])/ sum(performance_6[["CenterPer"]][["TP"]] + performance_6[["CenterPer"]][["FP"]])
#npv
sum(performance_6[["CenterPer"]][["TN"]])/ sum(performance_6[["CenterPer"]][["TN"]] + performance_6[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_6[["CenterPer"]][["TP"]]+ performance_6[["CenterPer"]][["FP"]])/ 2894

performance_7<-SS.imp(pred=average_predictions_m7, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_7[["CenterPer"]][["TP"]])/ sum(performance_7[["CenterPer"]][["TP"]] + performance_7[["CenterPer"]][["FP"]])
#npv
sum(performance_7[["CenterPer"]][["TN"]])/ sum(performance_7[["CenterPer"]][["TN"]] + performance_7[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_7[["CenterPer"]][["TP"]]+ performance_7[["CenterPer"]][["FP"]])/ 2894


performance_8<-SS.imp(pred=average_predictions_m8, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_8[["CenterPer"]][["TP"]])/ sum(performance_8[["CenterPer"]][["TP"]] + performance_8[["CenterPer"]][["FP"]])
#npv
sum(performance_8[["CenterPer"]][["TN"]])/ sum(performance_8[["CenterPer"]][["TN"]] + performance_8[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_8[["CenterPer"]][["TP"]]+ performance_8[["CenterPer"]][["FP"]])/ 2894


performance_9<-SS.imp(pred=average_predictions_m9, outcome= Outcome2, threshold=0.05, center=Centre, imp=.imp,data=mydata, method.MA = "REML")
#ppv
sum(performance_9[["CenterPer"]][["TP"]])/ sum(performance_9[["CenterPer"]][["TP"]] + performance_9[["CenterPer"]][["FP"]])
#npv
sum(performance_9[["CenterPer"]][["TN"]])/ sum(performance_9[["CenterPer"]][["TN"]] + performance_9[["CenterPer"]][["FN"]])
#percent high risk
sum(performance_9[["CenterPer"]][["TP"]]+ performance_9[["CenterPer"]][["FP"]])/ 2894

