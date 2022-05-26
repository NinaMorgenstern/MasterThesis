################################
######## Load Packages #########
################################

library(lme4)
library(glmnet)
library(brglm2)
library(randomForest)
library(caret)
library(nnet)
library(rpart)
library(rattle)
library(xgboost)
library(kernlab)
library(dplyr)
library(auRoc)
library(car)
library(metafor)
library(boot)
library(forestplot)
library(tiff)
library(parameters)
library(plotrix)
library(readxl)
library(dplyr)
library(car)
library(data.table)
library(knitr)
library(fastDummies)
library(ranger)
library(jtools)
library(ggstance)
library(broom.mixed)
library(mice)
library(glmnetUtils)
library(parameters)
library(plyr)
library(rema)
library(MLmetrics)
library(Matrix)
library(mcca)
library(devtools)
library(mcca)

###############
###IMPORTANT###
###############
# The functions used in this analysis to obtain AUC scores are taken from:
# https://github.com/benvancalster/IOTA5modelvalidation2020/blob/769c160a8377dd8fcc7df0027f16bab4ba14d089/Functions%20IOTA5.R
# credits to Jolien Ceusters.


################################
####### Data Preparation #######
################################

getwd()
setwd("")
mydata =  read.delim("")

# Only use 10 imputations
mydata<-mydata%>%
  filter(.imp %in% 1:10)

# Remove Age below 16 and unnecessary columns
mydata<- mydata %>% 
  filter(Age>=16) %>%
  select(-LFU,-Type.of.PUL, -ET..initial.US.)

# Check classes
sapply(mydata, class)

# Factor categorical data
factors <- c("Outcome3", "Centre")
for (i in factors) {
  mydata[, i] <- factor(mydata[, i])
}

# Build transformations
mydata['Painscore2'] =  ifelse(mydata$Painscore==0, 0,1)
mydata['lhCGratio_log.prog0num']= mydata$lhCGratio*mydata$log.prog0num

# Standardize 
mydata$Agesd <- scale(mydata$Age, center=TRUE, scale=TRUE)
mydata$hCG0numsd <- scale(mydata$hCG0num, center=TRUE, scale=TRUE)
mydata$prog0numsd <- scale(mydata$prog0num, center=TRUE, scale=TRUE)
mydata$hCGratiosd <- scale(mydata$hCGratio, center=TRUE, scale=TRUE) 
mydata$Painscoresd <- scale(mydata$Painscore, center=TRUE, scale=TRUE) 

# Relevel the Outcome with 3 categories, reference catgeory is EP
mydata['Outcome2'] =  ifelse(mydata$Outcome3=='EP', 1,0)
mydata$Outcome3 <- relevel(mydata$Outcome3, ref = "EP")



###########################################
########### Multinomial Models ############
###########################################
###########################################
############# IECV ########################
###########################################
set.seed(123)
center_names<-c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")

############ Logistic Regression without transformations #####################
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

########### LR #############
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


############### Ridge LR #####################
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



##################################################################
#######################CALIBRATION ###############################
##################################################################

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


# prepare
mydata$Center <- factor(mydata$Centre, labels=c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park"))
center <- c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")

# chose colors and linetypes
IncludedCenters <- unique(mydata$Center)
#selecting line types, colors and width
lty.centers <- c(1,1,1,1,1,1,1,1)
col.centers <- c("#d42942", "#f79a7d", "#feac5e", "#fee292", "#e6f698", "#a7db9e", "#68c3a5", "#5198c5")
lwd.centers <- c(2,1,2,1,2,1,2,1)


# 1. Logistic Regression
# need to take care of 0,1 probabilities
mydata$prediction_1[mydata$mm1_pred == 0] <- 0.001
mydata$prediction_1[mydata$mm1_pred == 1] <- 0.999
mydata$LP_m1 <- logit(mydata$mm1_pred)

slopem1 <- glmer(Outcome2 ~ LP_m1 + (1+LP_m1 | Center), data = mydata, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(slopem1)
coef(slopem1)
ranef(slopem1)

slopecoefm1 <- coef(slopem1)
slopecoefm1 <- data.frame(matrix(unlist(slopecoefm1), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)
slopecoefsem1<- standard_error(slopem1, effects="random")
slopecoefsem1 <- data.frame(matrix(unlist(slopecoefsem1), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)


m1slopes <- matrix(ncol=3, nrow=length(center))
colnames(m1slopes) <- c("Center", "slope", "se" )
m1slopes <- data.frame(m1slopes)
m1slopes$Center <- center
m1slopes$slope <-as.numeric(slopecoefm1$X2)
m1slopes$se <- as.numeric(slopecoefsem1$X2)
slopemam1 <- rma.uni(m1slopes$slope, sei=m1slopes$se, method="REML")
slopemam1

#intercept
m1slopesinter <- matrix(ncol=3, nrow=length(center))
colnames(m1slopesinter) <- c("Center", "intercept", "se" )
m1slopesinter <- data.frame(m1slopesinter)
m1slopesinter$Center <- center
m1slopesinter$intercept <-as.numeric(slopecoefm1$X1)
m1slopesinter$se <- as.numeric(slopecoefsem1$X1)
slopeintermam1 <- rma.uni(m1slopesinter$intercept, sei=m1slopesinter$se, method="REML")
slopeintermam1

dataplotm1 <- data.frame(LP_m1 = seq(min(mydata$LP_m1), max(mydata$LP_m1), length = 500))

newdatam1 <- data.frame(LP_m1 = rep(dataplotm1$LP_m1, length(IncludedCenters)), 
                        Center = sort(rep(IncludedCenters, 500)))

obsm1 <- predict(slopem1, newdata = newdatam1, re.form = ~(LP_m1 |Center), allow.new.levels = T, type = "response")
resultm1 <- cbind.data.frame(x = inv.logit(newdatam1$LP_m1), y = obsm1,center = newdatam1$Center)
resultm1 <- split(resultm1, resultm1$center)

tiff("Multinomial_LR_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm1[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Multinomial LR: Calibration per Center", line =3, cex.main=0.95)
dev.off()

#Random intercept
interceptm1 = glmer(Outcome2 ~ 1 + (1 | Center), data = mydata, family = "binomial", 
                    offset = LP_m1, control = glmerControl(optimizer = "bobyqa"))
summary(interceptm1)
coef(interceptm1)

m1interc <- matrix(ncol=3, nrow=length(center))
colnames(m1interc) <- c("Center", "intercept", "se" )
m1interc <- data.frame(m1interc)
m1interc$Center <- center

intercm1 <- coef(interceptm1)
intercm1 <- data.frame(unlist(intercm1), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

intercm1se <- standard_error(interceptm1, effects="random")
intercm1se <-data.frame(unlist(intercm1se), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

m1interc$intercept <-as.numeric(intercm1$unlist.intercm1.)
m1interc$se <- as.numeric(intercm1se$unlist.intercm1se.)

intercmam1 <- rma.uni(m1interc$intercept, sei=m1interc$se, method="REML")
intercmam1



# 2. Logistic Regression with transformations
mydata$mm2_pred[mydata$mm2_pred == 0] <- 0.001
mydata$mm2_pred[mydata$mm2_pred == 1] <- 0.999
mydata$LP_m2 <- logit(mydata$mm2_pred)

slopem2 <- glmer(Outcome2 ~ LP_m2 + (1+LP_m2 | Center), data = mydata, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(slopem2)
coef(slopem2)
ranef(slopem2)

slopecoefm2 <- coef(slopem2)
slopecoefm2 <- data.frame(matrix(unlist(slopecoefm2), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)
slopecoefsem2<- standard_error(slopem2, effects="random")
slopecoefsem2 <- data.frame(matrix(unlist(slopecoefsem2), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)


m2slopes <- matrix(ncol=3, nrow=length(center))
colnames(m2slopes) <- c("Center", "slope", "se" )
m2slopes <- data.frame(m2slopes)
m2slopes$Center <- center
m2slopes$slope <-as.numeric(slopecoefm2$X2)
m2slopes$se <- as.numeric(slopecoefsem2$X2)
slopemam2 <- rma.uni(m2slopes$slope, sei=m2slopes$se, method="REML")
slopemam2

#intercept
m2slopesinter <- matrix(ncol=3, nrow=length(center))
colnames(m2slopesinter) <- c("Center", "intercept", "se" )
m2slopesinter <- data.frame(m2slopesinter)
m2slopesinter$Center <- center
m2slopesinter$intercept <-as.numeric(slopecoefm2$X1)
m2slopesinter$se <- as.numeric(slopecoefsem2$X1)
slopeintermam2 <- rma.uni(m2slopesinter$intercept, sei=m2slopesinter$se, method="REML")
slopeintermam2

dataplotm2 <- data.frame(LP_m2 = seq(min(mydata$LP_m2), max(mydata$LP_m2), length = 500))

newdatam2 <- data.frame(LP_m2 = rep(dataplotm2$LP_m2, length(IncludedCenters)), 
                        Center = sort(rep(IncludedCenters, 500)))

obsm2 <- predict(slopem2, newdata = newdatam2, re.form = ~(LP_m2 |Center), allow.new.levels = T, type = "response")
resultm2 <- cbind.data.frame(x = inv.logit(newdatam2$LP_m2), y = obsm2,center = newdatam2$Center)
resultm2 <- split(resultm2, resultm2$center)

tiff("Multinomial_LR_withTransformations_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm2[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Multinomial LR with Transformations: Calibration per Center", line =3, cex.main=0.95)
dev.off()

#Random intercept
interceptm2 = glmer(Outcome2 ~ 1 + (1 | Center), data = mydata, family = "binomial", 
                    offset = LP_m2, control = glmerControl(optimizer = "bobyqa"))
summary(interceptm2)
coef(interceptm2)

m2interc <- matrix(ncol=3, nrow=length(center))
colnames(m2interc) <- c("Center", "intercept", "se" )
m2interc <- data.frame(m2interc)
m2interc$Center <- center

intercm2 <- coef(interceptm2)
intercm2 <- data.frame(unlist(intercm2), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

intercm2se <- standard_error(interceptm2, effects="random")
intercm2se <-data.frame(unlist(intercm2se), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

m2interc$intercept <-as.numeric(intercm2$unlist.intercm2.)
m2interc$se <- as.numeric(intercm2se$unlist.intercm2se.)

intercmam2 <- rma.uni(m2interc$intercept, sei=m2interc$se, method="REML")
intercmam2




# 3. Ridge logistic Regression
mydata$mm3_pred[mydata$mm3_pred == 0] <- 0.001
mydata$mm3_pred[mydata$mm3_pred == 1] <- 0.999
mydata$LP_m3 <- logit(mydata$mm3_pred)

slopem3 <- glmer(Outcome2 ~ LP_m3 + (1+LP_m3 | Center), data = mydata, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(slopem3)
coef(slopem3)
ranef(slopem3)

slopecoefm3 <- coef(slopem3)
slopecoefm3 <- data.frame(matrix(unlist(slopecoefm3), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)
slopecoefsem3<- standard_error(slopem3, effects="random")
slopecoefsem3 <- data.frame(matrix(unlist(slopecoefsem3), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)


m3slopes <- matrix(ncol=3, nrow=length(center))
colnames(m3slopes) <- c("Center", "slope", "se" )
m3slopes <- data.frame(m3slopes)
m3slopes$Center <- center
m3slopes$slope <-as.numeric(slopecoefm3$X2)
m3slopes$se <- as.numeric(slopecoefsem3$X2)
slopemam3 <- rma.uni(m3slopes$slope, sei=m3slopes$se, method="REML")
slopemam3

#intercept
m3slopesinter <- matrix(ncol=3, nrow=length(center))
colnames(m3slopesinter) <- c("Center", "intercept", "se" )
m3slopesinter <- data.frame(m3slopesinter)
m3slopesinter$Center <- center
m3slopesinter$intercept <-as.numeric(slopecoefm3$X1)
m3slopesinter$se <- as.numeric(slopecoefsem3$X1)
slopeintermam3 <- rma.uni(m3slopesinter$intercept, sei=m3slopesinter$se, method="REML")
slopeintermam3

dataplotm3 <- data.frame(LP_m3 = seq(min(mydata$LP_m3), max(mydata$LP_m3), length = 500))

newdatam3 <- data.frame(LP_m3 = rep(dataplotm3$LP_m3, length(IncludedCenters)), 
                        Center = sort(rep(IncludedCenters, 500)))

obsm3 <- predict(slopem3, newdata = newdatam3, re.form = ~(LP_m3 |Center), allow.new.levels = T, type = "response")
resultm3 <- cbind.data.frame(x = inv.logit(newdatam3$LP_m3), y = obsm3, center = newdatam3$Center)
resultm3 <- split(resultm3, resultm3$center)

tiff("Multinomial_Ridge_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm3[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Multinomial Ridge Regression: Calibration per Center", line =3, cex.main=0.95)
dev.off()

#Random intercept
interceptm3 = glmer(Outcome2 ~ 1 + (1 | Center), data = mydata, family = "binomial", 
                    offset = LP_m3, control = glmerControl(optimizer = "bobyqa"))
summary(interceptm3)
coef(interceptm3)

m3interc <- matrix(ncol=3, nrow=length(center))
colnames(m3interc) <- c("Center", "intercept", "se" )
m3interc <- data.frame(m3interc)
m3interc$Center <- center

intercm3 <- coef(interceptm3)
intercm3 <- data.frame(unlist(intercm3), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

intercm3se <- standard_error(interceptm3, effects="random")
intercm3se <-data.frame(unlist(intercm3se), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

m3interc$intercept <-as.numeric(intercm3$unlist.intercm3.)
m3interc$se <- as.numeric(intercm3se$unlist.intercm3se.)

intercmam3 <- rma.uni(m3interc$intercept, sei=m3interc$se, method="REML")
intercmam3


# 4. Firth logistic Regression
mydata$mm4_pred[mydata$mm4_pred == 0] <- 0.001
mydata$mm4_pred[mydata$mm4_pred == 1] <- 0.999
mydata$LP_m4 <- logit(mydata$mm4_pred)

slopem4 <- glmer(Outcome2 ~ LP_m4 + (1+LP_m4 | Center), data = mydata, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(slopem4)
coef(slopem4)
ranef(slopem4)

slopecoefm4 <- coef(slopem4)
slopecoefm4 <- data.frame(matrix(unlist(slopecoefm4), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)
slopecoefsem4<- standard_error(slopem4, effects="random")
slopecoefsem4 <- data.frame(matrix(unlist(slopecoefsem4), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)


m4slopes <- matrix(ncol=3, nrow=length(center))
colnames(m4slopes) <- c("Center", "slope", "se" )
m4slopes <- data.frame(m4slopes)
m4slopes$Center <- center
m4slopes$slope <-as.numeric(slopecoefm4$X2)
m4slopes$se <- as.numeric(slopecoefsem4$X2)
slopemam4 <- rma.uni(m4slopes$slope, sei=m4slopes$se, method="REML")
slopemam4

#intercept
m4slopesinter <- matrix(ncol=3, nrow=length(center))
colnames(m4slopesinter) <- c("Center", "intercept", "se" )
m4slopesinter <- data.frame(m4slopesinter)
m4slopesinter$Center <- center
m4slopesinter$intercept <-as.numeric(slopecoefm4$X1)
m4slopesinter$se <- as.numeric(slopecoefsem4$X1)
slopeintermam4 <- rma.uni(m4slopesinter$intercept, sei=m4slopesinter$se, method="REML")
slopeintermam4

dataplotm4 <- data.frame(LP_m4 = seq(min(mydata$LP_m4), max(mydata$LP_m4), length = 500))

newdatam4 <- data.frame(LP_m4 = rep(dataplotm4$LP_m4, length(IncludedCenters)), 
                        Center = sort(rep(IncludedCenters, 500)))

obsm4 <- predict(slopem4, newdata = newdatam4, re.form = ~(LP_m4 |Center), allow.new.levels = T, type = "response")
resultm4 <- cbind.data.frame(x = inv.logit(newdatam4$LP_m4), y = obsm4,center = newdatam4$Center)
resultm4 <- split(resultm4, resultm4$center)

tiff("Multinomial_Firth_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm4[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Multinomial Firth LR: Calibration per Center", line =3, cex.main=0.95)
dev.off()

#Random intercept
interceptm4 = glmer(Outcome2 ~ 1 + (1 | Center), data = mydata, family = "binomial", 
                    offset = LP_m4, control = glmerControl(optimizer = "bobyqa"))
summary(interceptm4)
coef(interceptm4)

m4interc <- matrix(ncol=3, nrow=length(center))
colnames(m4interc) <- c("Center", "intercept", "se" )
m4interc <- data.frame(m4interc)
m4interc$Center <- center

intercm4 <- coef(interceptm4)
intercm4 <- data.frame(unlist(intercm4), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

intercm4se <- standard_error(interceptm4, effects="random")
intercm4se <-data.frame(unlist(intercm4se), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

m4interc$intercept <-as.numeric(intercm4$unlist.intercm4.)
m4interc$se <- as.numeric(intercm4se$unlist.intercm4se.)

intercmam4 <- rma.uni(m4interc$intercept, sei=m4interc$se, method="REML")
intercmam4


# 5. CART
mydata$mm5_pred[mydata$mm5_pred == 0] <- 0.001
mydata$mm5_pred[mydata$mm5_pred == 1] <- 0.999
mydata$LP_m5 <- logit(mydata$mm5_pred)

slopem5 <- glmer(Outcome2 ~ LP_m5 + (1+LP_m5 | Center), data = mydata, family = "binomial")
summary(slopem5)
coef(slopem5)
ranef(slopem5)

slopecoefm5 <- coef(slopem5)
slopecoefm5 <- data.frame(matrix(unlist(slopecoefm5), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)
slopecoefsem5<- standard_error(slopem5, effects="random")
slopecoefsem5 <- data.frame(matrix(unlist(slopecoefsem5), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)

m5slopes <- matrix(ncol=3, nrow=length(center))
colnames(m5slopes) <- c("Center", "slope", "se" )
m5slopes <- data.frame(m5slopes)
m5slopes$Center <- center
m5slopes$slope <-as.numeric(slopecoefm5$X2)
m5slopes$se <- as.numeric(slopecoefsem5$X2)
slopemam5 <- rma.uni(m5slopes$slope, sei=m5slopes$se, method="REML")
slopemam5

#intercept
m5slopesinter <- matrix(ncol=3, nrow=length(center))
colnames(m5slopesinter) <- c("Center", "intercept", "se" )
m5slopesinter <- data.frame(m5slopesinter)
m5slopesinter$Center <- center
m5slopesinter$intercept <-as.numeric(slopecoefm5$X1)
m5slopesinter$se <- as.numeric(slopecoefsem5$X1)
slopeintermam5 <- rma.uni(m5slopesinter$intercept, sei=m5slopesinter$se, method="REML")
slopeintermam5

dataplotm5 <- data.frame(LP_m5 = seq(min(mydata$LP_m5, na.rm=T), max(mydata$LP_m5, na.rm=T), length = 500))

newdatam5 <- data.frame(LP_m5 = rep(dataplotm5$LP_m5, length(IncludedCenters)), 
                        Center = sort(rep(IncludedCenters, 500)))

obsm5 <- predict(slopem5, newdata = newdatam5, re.form = ~(LP_m5 |Center), allow.new.levels = T, type = "response")
resultm5 <- cbind.data.frame(x = inv.logit(newdatam5$LP_m5), y = obsm5,center = newdatam5$Center)
resultm5 <- split(resultm5, resultm5$center)

tiff("Multinomial_CART_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm5[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Multinomial CART: Calibration per Center", line =3, cex.main=0.95)
dev.off()

#Random intercept
interceptm5 = glmer(Outcome2 ~ 1 + (1 | Center), data = mydata, family = "binomial", 
                    offset = LP_m5, control = glmerControl(optimizer = "bobyqa"))
summary(interceptm5)
coef(interceptm5)

m5interc <- matrix(ncol=3, nrow=length(center))
colnames(m5interc) <- c("Center", "intercept", "se" )
m5interc <- data.frame(m5interc)
m5interc$Center <- center

intercm5 <- coef(interceptm5)
intercm5 <- data.frame(unlist(intercm5), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

intercm5se <- standard_error(interceptm5, effects="random")
intercm5se <-data.frame(unlist(intercm5se), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

m5interc$intercept <-as.numeric(intercm5$unlist.intercm5.)
m5interc$se <- as.numeric(intercm5se$unlist.intercm5se.)

intercmam5 <- rma.uni(m5interc$intercept, sei=m5interc$se, method="REML")
intercmam5


# 6. Random Forest
mydata$mm6_pred[mydata$mm6_pred == 0] <- 0.001
mydata$mm6_pred[mydata$mm6_pred == 1] <- 0.999
mydata$LP_m6 <- logit(mydata$mm6_pred)

slopem6 <- glmer(Outcome2 ~ LP_m6 + (1+LP_m6 | Center), data = mydata, family = "binomial")
summary(slopem6)
coef(slopem6)
ranef(slopem6)

slopecoefm6 <- coef(slopem6)
slopecoefm6 <- data.frame(matrix(unlist(slopecoefm6), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)
slopecoefsem6<- standard_error(slopem6, effects="random")
slopecoefsem6 <- data.frame(matrix(unlist(slopecoefsem6), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)


m6slopes <- matrix(ncol=3, nrow=length(center))
colnames(m6slopes) <- c("Center", "slope", "se" )
m6slopes <- data.frame(m6slopes)
m6slopes$Center <- center
m6slopes$slope <-as.numeric(slopecoefm6$X2)
m6slopes$se <- as.numeric(slopecoefsem6$X2)
slopemam6 <- rma.uni(m6slopes$slope, sei=m6slopes$se, method="REML")
slopemam6

#intercept
m6slopesinter <- matrix(ncol=3, nrow=length(center))
colnames(m6slopesinter) <- c("Center", "intercept", "se" )
m6slopesinter <- data.frame(m6slopesinter)
m6slopesinter$Center <- center
m6slopesinter$intercept <-as.numeric(slopecoefm6$X1)
m6slopesinter$se <- as.numeric(slopecoefsem6$X1)
slopeintermam6 <- rma.uni(m6slopesinter$intercept, sei=m6slopesinter$se, method="REML")
slopeintermam6

dataplotm6 <- data.frame(LP_m6 = seq(min(mydata$LP_m6, na.rm=T), max(mydata$LP_m6, na.rm=T), length = 500))

newdatam6 <- data.frame(LP_m6 = rep(dataplotm6$LP_m6, length(IncludedCenters)), 
                        Center = sort(rep(IncludedCenters, 500)))

obsm6 <- predict(slopem6, newdata = newdatam6, re.form = ~(LP_m6 |Center), allow.new.levels = T, type = "response")
resultm6 <- cbind.data.frame(x = inv.logit(newdatam6$LP_m6), y = obsm6,center = newdatam6$Center)
resultm6 <- split(resultm6, resultm6$center)

tiff("Multinomial_RF_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm6[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Multinomial RF: Calibration per Center", line =3, cex.main=0.95)
dev.off()

#Random intercept
interceptm6 = glmer(Outcome2 ~ 1 + (1 | Center), data = mydata, family = "binomial", 
                    offset = LP_m6, control = glmerControl(optimizer = "bobyqa"))
summary(interceptm6)
coef(interceptm6)

m6interc <- matrix(ncol=3, nrow=length(center))
colnames(m6interc) <- c("Center", "intercept", "se" )
m6interc <- data.frame(m6interc)
m6interc$Center <- center

intercm6 <- coef(interceptm6)
intercm6 <- data.frame(unlist(intercm6), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

intercm6se <- standard_error(interceptm6, effects="random")
intercm6se <-data.frame(unlist(intercm6se), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

m6interc$intercept <-as.numeric(intercm6$unlist.intercm6.)
m6interc$se <- as.numeric(intercm6se$unlist.intercm6se.)

intercmam6 <- rma.uni(m6interc$intercept, sei=m6interc$se, method="REML")
intercmam6


# 7. XGBoost
mydata$mm7_pred[mydata$mm7_pred == 0] <- 0.001
mydata$mm7_pred[mydata$mm7_pred == 1] <- 0.999
mydata$LP_m7 <- logit(mydata$mm7_pred)

slopem7 <- glmer(Outcome2 ~ LP_m7 + (1+LP_m7 | Center), data = mydata, family = "binomial")
summary(slopem7)
coef(slopem7)
ranef(slopem7)

slopecoefm7 <- coef(slopem7)
slopecoefm7 <- data.frame(matrix(unlist(slopecoefm7), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)
slopecoefsem7<- standard_error(slopem7, effects="random")
slopecoefsem7 <- data.frame(matrix(unlist(slopecoefsem7), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)


m7slopes <- matrix(ncol=3, nrow=length(center))
colnames(m7slopes) <- c("Center", "slope", "se" )
m7slopes <- data.frame(m7slopes)
m7slopes$Center <- center
m7slopes$slope <-as.numeric(slopecoefm7$X2)
m7slopes$se <- as.numeric(slopecoefsem7$X2)
slopemam7 <- rma.uni(m7slopes$slope, sei=m7slopes$se, method="REML")
slopemam7

#intercept
m7slopesinter <- matrix(ncol=3, nrow=length(center))
colnames(m7slopesinter) <- c("Center", "intercept", "se" )
m7slopesinter <- data.frame(m7slopesinter)
m7slopesinter$Center <- center
m7slopesinter$intercept <-as.numeric(slopecoefm7$X1)
m7slopesinter$se <- as.numeric(slopecoefsem7$X1)
slopeintermam7 <- rma.uni(m7slopesinter$intercept, sei=m7slopesinter$se, method="REML")
slopeintermam7

dataplotm7 <- data.frame(LP_m7 = seq(min(mydata$LP_m7, na.rm=T), max(mydata$LP_m7, na.rm=T), length = 500))

newdatam7 <- data.frame(LP_m7 = rep(dataplotm7$LP_m7, length(IncludedCenters)), 
                        Center = sort(rep(IncludedCenters, 500)))

obsm7 <- predict(slopem7, newdata = newdatam7, re.form = ~(LP_m7 |Center), allow.new.levels = T, type = "response")
resultm7 <- cbind.data.frame(x = inv.logit(newdatam7$LP_m7), y = obsm7,center = newdatam7$Center)
resultm7 <- split(resultm7, resultm7$center)

tiff("Multinomial_XGB_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm7[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Multinomial XGB: Calibration per Center", line =3, cex.main=0.95)
dev.off()

#Random intercept
interceptm7 = glmer(Outcome2 ~ 1 + (1 | Center), data = mydata, family = "binomial", 
                    offset = LP_m7, control = glmerControl(optimizer = "bobyqa"))
summary(interceptm7)
coef(interceptm7)

m7interc <- matrix(ncol=3, nrow=length(center))
colnames(m7interc) <- c("Center", "intercept", "se" )
m7interc <- data.frame(m7interc)
m7interc$Center <- center

intercm7 <- coef(interceptm7)
intercm7 <- data.frame(unlist(intercm7), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

intercm7se <- standard_error(interceptm7, effects="random")
intercm7se <-data.frame(unlist(intercm7se), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

m7interc$intercept <-as.numeric(intercm7$unlist.intercm7.)
m7interc$se <- as.numeric(intercm7se$unlist.intercm7se.)

intercmam7 <- rma.uni(m7interc$intercept, sei=m7interc$se, method="REML")
intercmam7



# 8. SVM
mydata$mm8_pred[mydata$mm8_pred == 0] <- 0.001
mydata$mm8_pred[mydata$mm8_pred == 1] <- 0.999
mydata$LP_m8 <- logit(mydata$mm8_pred)

slopem8 <- glmer(Outcome2 ~ LP_m8 + (1+LP_m8 | Center), data = mydata, family = "binomial")
summary(slopem8)
coef(slopem8)
ranef(slopem8)

slopecoefm8 <- coef(slopem8)
slopecoefm8 <- data.frame(matrix(unlist(slopecoefm8), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)
slopecoefsem8<- standard_error(slopem8, effects="random")
slopecoefsem8 <- data.frame(matrix(unlist(slopecoefsem8), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)


m8slopes <- matrix(ncol=3, nrow=length(center))
colnames(m8slopes) <- c("Center", "slope", "se" )
m8slopes <- data.frame(m8slopes)
m8slopes$Center <- center
m8slopes$slope <-as.numeric(slopecoefm8$X2)
m8slopes$se <- as.numeric(slopecoefsem8$X2)
slopemam8 <- rma.uni(m8slopes$slope, sei=m8slopes$se, method="REML")
slopemam8

#intercept
m8slopesinter <- matrix(ncol=3, nrow=length(center))
colnames(m8slopesinter) <- c("Center", "intercept", "se" )
m8slopesinter <- data.frame(m8slopesinter)
m8slopesinter$Center <- center
m8slopesinter$intercept <-as.numeric(slopecoefm8$X1)
m8slopesinter$se <- as.numeric(slopecoefsem8$X1)
slopeintermam8 <- rma.uni(m8slopesinter$intercept, sei=m8slopesinter$se, method="REML")
slopeintermam8

dataplotm8 <- data.frame(LP_m8 = seq(min(mydata$LP_m8, na.rm=T), max(mydata$LP_m8, na.rm=T), length = 500))

newdatam8 <- data.frame(LP_m8 = rep(dataplotm8$LP_m8, length(IncludedCenters)), 
                        Center = sort(rep(IncludedCenters, 500)))

obsm8 <- predict(slopem8, newdata = newdatam8, re.form = ~(LP_m8 |Center), allow.new.levels = T, type = "response")
resultm8 <- cbind.data.frame(x = inv.logit(newdatam8$LP_m8), y = obsm8,center = newdatam8$Center)
resultm8 <- split(resultm8, resultm8$center)

tiff("Multinomial_SVM_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm8[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Multinomial SVM: Calibration per Center", line =3, cex.main=0.95)
dev.off()

#Random intercept
interceptm8 = glmer(Outcome2 ~ 1 + (1 | Center), data = mydata, family = "binomial", 
                    offset = LP_m8, control = glmerControl(optimizer = "bobyqa"))
summary(interceptm8)
coef(interceptm8)

m8interc <- matrix(ncol=3, nrow=length(center))
colnames(m8interc) <- c("Center", "intercept", "se" )
m8interc <- data.frame(m8interc)
m8interc$Center <- center

intercm8 <- coef(interceptm8)
intercm8 <- data.frame(unlist(intercm8), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

intercm8se <- standard_error(interceptm8, effects="random")
intercm8se <-data.frame(unlist(intercm8se), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

m8interc$intercept <-as.numeric(intercm8$unlist.intercm8.)
m8interc$se <- as.numeric(intercm8se$unlist.intercm8se.)

intercmam8 <- rma.uni(m8interc$intercept, sei=m8interc$se, method="REML")
intercmam8



# 9. Neural Network
mydata$mm9_pred[mydata$mm9_pred == 0] <- 0.001
mydata$mm9_pred[mydata$mm9_pred == 1] <- 0.999
mydata$LP_m9 <- logit(mydata$mm9_pred)


slopem9 <- glmer(Outcome2 ~ LP_m9 + (1+LP_m9 | Center), data = mydata, family = "binomial")
summary(slopem9)
coef(slopem9)
ranef(slopem9)

slopecoefm9 <- coef(slopem9)
slopecoefm9 <- data.frame(matrix(unlist(slopecoefm9), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)
slopecoefsem9<- standard_error(slopem9, effects="random")
slopecoefsem9 <- data.frame(matrix(unlist(slopecoefsem9), nrow=8, byrow=FALSE),stringsAsFactors=FALSE)


m9slopes <- matrix(ncol=3, nrow=length(center))
colnames(m9slopes) <- c("Center", "slope", "se" )
m9slopes <- data.frame(m9slopes)
m9slopes$Center <- center
m9slopes$slope <-as.numeric(slopecoefm9$X2)
m9slopes$se <- as.numeric(slopecoefsem9$X2)
slopemam9 <- rma.uni(m9slopes$slope, sei=m9slopes$se, method="REML")
slopemam9

#intercept
m9slopesinter <- matrix(ncol=3, nrow=length(center))
colnames(m9slopesinter) <- c("Center", "intercept", "se" )
m9slopesinter <- data.frame(m9slopesinter)
m9slopesinter$Center <- center
m9slopesinter$intercept <-as.numeric(slopecoefm9$X1)
m9slopesinter$se <- as.numeric(slopecoefsem9$X1)
slopeintermam9 <- rma.uni(m9slopesinter$intercept, sei=m9slopesinter$se, method="REML")
slopeintermam9

dataplotm9 <- data.frame(LP_m9 = seq(min(mydata$LP_m9, na.rm=T), max(mydata$LP_m9, na.rm=T), length = 500))

newdatam9 <- data.frame(LP_m9 = rep(dataplotm9$LP_m9, length(IncludedCenters)), 
                        Center = sort(rep(IncludedCenters, 500)))

obsm9 <- predict(slopem9, newdata = newdatam9, re.form = ~(LP_m9 |Center), allow.new.levels = T, type = "response")
resultm9 <- cbind.data.frame(x = inv.logit(newdatam9$LP_m9), y = obsm9,center = newdatam9$Center)
resultm9 <- split(resultm9, resultm9$center)

tiff("Multinomial_NN_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm9[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Multinomial NN: Calibration per Center", line =3, cex.main=0.95)
dev.off()

#Random intercept
interceptm9 = glmer(Outcome2 ~ 1 + (1 | Center), data = mydata, family = "binomial", 
                    offset = LP_m9, control = glmerControl(optimizer = "bobyqa"))
summary(interceptm9)
coef(interceptm9)

m9interc <- matrix(ncol=3, nrow=length(center))
colnames(m9interc) <- c("Center", "intercept", "se" )
m9interc <- data.frame(m9interc)
m9interc$Center <- center

intercm9 <- coef(interceptm9)
intercm9 <- data.frame(unlist(intercm9), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

intercm9se <- standard_error(interceptm9, effects="random")
intercm9se <-data.frame(unlist(intercm9se), nrow=8, byrow=TRUE,stringsAsFactors=FALSE)

m9interc$intercept <-as.numeric(intercm9$unlist.intercm9.)
m9interc$se <- as.numeric(intercm9se$unlist.intercm9se.)

intercmam9 <- rma.uni(m9interc$intercept, sei=m9interc$se, method="REML")
intercmam9


#########################################
########### Overall Calibration #########
#########################################

## Predicting the outcome for the calibration curves
# LR
p.LR = seq(min(mydata$mm1_pred), max(mydata$mm1_pred), length = 500)
X = cbind(1, logit(p.LR))
FE = c(coef(slopeintermam1), coef(slopemam1))
OverallCal.LR = inv.logit(X[order(p.LR), ] %*% FE)
p.LR = p.LR[order(p.LR)]

# LR with transformations  
p.LR2 = seq(min(mydata$mm2_pred), max(mydata$mm2_pred), length = 500)
X = cbind(1, logit(p.LR2))
FE = c(coef(slopeintermam2), coef(slopemam2))
OverallCal.LR2 = inv.logit(X[order(p.LR2), ] %*% FE)
p.LR2 = p.LR2[order(p.LR2)]

# Ridge
p.ridge = seq(min(mydata$mm3_pred), max(mydata$mm3_pred), length = 500)
X = cbind(1, logit(p.ridge))
FE = c(coef(slopeintermam3), coef(slopemam3))
OverallCal.ridge = inv.logit(X[order(p.ridge), ] %*% FE)
p.ridge = p.ridge[order(p.ridge)]

# Firth
p.firth = seq(min(mydata$mm4_pred), max(mydata$mm4_pred), length = 500)
X = cbind(1, logit(p.firth))
FE = c(coef(slopeintermam4), coef(slopemam4))
OverallCal.firth = inv.logit(X[order(p.firth), ] %*% FE)
p.firth = p.firth[order(p.firth)]

# CART
p.ca = seq(min(mydata$mm5_pred), max(mydata$mm5_pred), length = 500)
X = cbind(1, logit(p.ca))
FE = c(coef(slopeintermam5), coef(slopemam5))
OverallCal.ca = inv.logit(X[order(p.ca), ] %*% FE)
p.ca = p.ca[order(p.ca)]

# RF
p.rf = seq(min(mydata$mm6_pred), max(mydata$mm6_pred), length = 500)
X = cbind(1, logit(p.rf))
FE = c(coef(slopeintermam6), coef(slopemam6))
OverallCal.rf = inv.logit(X[order(p.rf), ] %*% FE)
p.rf = p.rf[order(p.rf)]

# XGB
p.xg = seq(min(mydata$mm7_pred), max(mydata$mm7_pred), length = 500)
X = cbind(1, logit(p.xg))
FE = c(coef(slopeintermam7), coef(slopemam7))
OverallCal.xg = inv.logit(X[order(p.xg), ] %*% FE)
p.xg = p.xg[order(p.xg)]


# SVM
p.svm = seq(min(mydata$mm8_pred), max(mydata$mm8_pred), length = 500)
X = cbind(1, logit(p.svm))
FE = c(coef(slopeintermam8), coef(slopemam8))
OverallCal.svm = inv.logit(X[order(p.svm), ] %*% FE)
p.svm = p.svm[order(p.svm)]

# NN
p.nn = seq(min(mydata$mm9_pred), max(mydata$mm9_pred), length = 500)
X = cbind(1, logit(p.nn))
FE = c(coef(slopeintermam9), coef(slopemam9))
OverallCal.nn = inv.logit(X[order(p.nn), ] %*% FE)
p.nn = p.nn[order(p.nn)]


table <- matrix(ncol = 3, nrow = 9)
colnames(table) <- c('Model', 'Intercept (95% CI)', 'Slope (95% CI)')
table[, 1] <- c('LR', 'LR w tran', 'Ridge LR',"Firth LR", "CART", "RF", "XGBoost", "SVM", "NN")
table[1, 2:3] <- c(paste0(format(round(coef(intercmam1), 2), nsmall = 2), " (", format(round(intercmam1$ci.lb, 2), nsmall = 2), " to ", format(round(intercmam1$ci.ub, 2), nsmall = 2), ")"), paste0(format(round(coef(slopemam1), 2), nsmall = 2), " (", format(round(slopemam1$ci.lb, 2), nsmall = 2), " to ", format(round(slopemam1$ci.ub, 2), nsmall = 2), ")"))
table[2, 2:3] <- c(paste0(format(round(coef(intercmam2), 2), nsmall = 2), " (", format(round(intercmam2$ci.lb, 2), nsmall = 2), " to ", format(round(intercmam2$ci.ub, 2), nsmall = 2), ")"), paste0(format(round(coef(slopemam2), 2), nsmall = 2), " (", format(round(slopemam2$ci.lb, 2), nsmall = 2), " to ", format(round(slopemam2$ci.ub, 2), nsmall = 2), ")"))
table[3, 2:3] <- c(paste0(format(round(coef(intercmam3), 2), nsmall = 2), " (", format(round(intercmam3$ci.lb, 2), nsmall = 2), " to ", format(round(intercmam3$ci.ub, 2), nsmall = 2), ")"), paste0(format(round(coef(slopemam3), 2), nsmall = 2), " (", format(round(slopemam3$ci.lb, 2), nsmall = 2), " to ", format(round(slopemam3$ci.ub, 2), nsmall = 2), ")"))
table[4, 2:3] <- c(paste0(format(round(coef(intercmam4), 2), nsmall = 2), " (", format(round(intercmam4$ci.lb, 2), nsmall = 2), " to ", format(round(intercmam4$ci.ub, 2), nsmall = 2), ")"), paste0(format(round(coef(slopemam4), 2), nsmall = 2), " (", format(round(slopemam4$ci.lb, 2), nsmall = 2), " to ", format(round(slopemam4$ci.ub, 2), nsmall = 2), ")"))
table[5, 2:3] <- c(paste0(format(round(coef(intercmam5), 2), nsmall = 2), " (", format(round(intercmam5$ci.lb, 2), nsmall = 2), " to ", format(round(intercmam5$ci.ub, 2), nsmall = 2), ")"), paste0(format(round(coef(slopemam5), 2), nsmall = 2), " (", format(round(slopemam5$ci.lb, 2), nsmall = 2), " to ", format(round(slopemam5$ci.ub, 2), nsmall = 2), ")"))

table[6, 2:3] <- c(paste0(format(round(coef(intercmam6), 2), nsmall = 2), " (", format(round(intercmam6$ci.lb, 2), nsmall = 2), " to ", format(round(intercmam6$ci.ub, 2), nsmall = 2), ")"), paste0(format(round(coef(slopemam6), 2), nsmall = 2), " (", format(round(slopemam6$ci.lb, 2), nsmall = 2), " to ", format(round(slopemam6$ci.ub, 2), nsmall = 2), ")"))
table[7, 2:3] <- c(paste0(format(round(coef(intercmam7), 2), nsmall = 2), " (", format(round(intercmam7$ci.lb, 2), nsmall = 2), " to ", format(round(intercmam7$ci.ub, 2), nsmall = 2), ")"), paste0(format(round(coef(slopemam7), 2), nsmall = 2), " (", format(round(slopemam7$ci.lb, 2), nsmall = 2), " to ", format(round(slopemam7$ci.ub, 2), nsmall = 2), ")"))
table[8, 2:3] <- c(paste0(format(round(coef(intercmam8), 2), nsmall = 2), " (", format(round(intercmam8$ci.lb, 2), nsmall = 2), " to ", format(round(intercmam8$ci.ub, 2), nsmall = 2), ")"), paste0(format(round(coef(slopemam8), 2), nsmall = 2), " (", format(round(slopemam8$ci.lb, 2), nsmall = 2), " to ", format(round(slopemam8$ci.ub, 2), nsmall = 2), ")"))
table[9, 2:3] <- c(paste0(format(round(coef(intercmam9), 2), nsmall = 2), " (", format(round(intercmam9$ci.lb, 2), nsmall = 2), " to ", format(round(intercmam9$ci.ub, 2), nsmall = 2), ")"), paste0(format(round(coef(slopemam9), 2), nsmall = 2), " (", format(round(slopemam9$ci.lb, 2), nsmall = 2), " to ", format(round(slopemam9$ci.ub, 2), nsmall = 2), ")"))

## put all in graph
x = seq(0, 1, by = 0.05)
y = seq(0, 1, by = 0.05)
tiff("Multinomial Overall Calibration.tiff", width = 14, height = 14, units = "cm", res = 300)
plot(x, y, xlim = c(0,1), ylim = c(0,1), type = "l", col = "gray50", lwd = 1, lty = 1, 
     xlab = "Estimated risk", ylab = "Observed proportion",
     main = "Multinomial Overall Calibration", cex.lab = 1, cex.axis = 1, las = 1) 
lines(p.LR, OverallCal.LR, lwd = 2,lty=4 ,col = "#e6194b")
lines(p.LR2, OverallCal.LR2, lwd = 2,lty=4, col = "#3cb44b")
lines(p.ridge, OverallCal.ridge, lwd = 1, lty=1, col = "#4363d8")
lines(p.firth, OverallCal.firth, lwd=1, lty=1, col="#f58231")
lines(p.ca, OverallCal.ca, lwd=1, lty=1, col="#911eb4")
lines(p.rf, OverallCal.rf, lwd=2,lty=1,  col="#f032e6")
lines(p.xg, OverallCal.xg, lwd=1, lty=1, col="#80CDC1")
lines(p.nn, OverallCal.svm, lwd=2, lty=1, col="#01665E")
lines(p.svm, OverallCal.nn, lwd=1,lty=1, col="#FDE725FF")
legend(x = -0.035, y = 1, legend = c( "Ideal", "LR", "LR w tran", "Ridge LR", "Firth LR", "CART", "RF", "XGBoost", "SVM", "NN"),
       col = c("gray50", "#e6194b", "#3cb44b", "#4363d8","#f58231", "#911eb4", "#f032e6", "#80CDC1", "#01665E", "#FDE725FF"),
       lty = c(1,4,4,1,1,1,1,1,1,1,1), lwd = c(1,2,2,1,1,1,2,1,2,1), cex = 0.7, bty = "n", ncol = 1)
addtable2plot(x = 0.40, y = -0.02, table = table, display.colnames= TRUE, cex = 0.60) 
dev.off()  



###############################
##FLEXIBLE MULTINOMIAL CURVES##
###############################
cols <- c("#39568CFF","#73D055FF","#FDE725FF")

# LR 
mm1_pred<-read.csv("predictions/pred_multinomial_LR.csv")
LPmm1 <- logitlink(data.matrix(mm1_pred[,-1]))
Polcal(outcome=mydata$Outcome3, k=3, p=data.matrix(mm1_pred[,-1]), LP=LPmm1[,-1], smoothpar=1, datapoints=FALSE)

# LR WITHOUT TRANSFORMATIONS
mm2_pred<-read.csv("predictions/pred_multinomial_LR_with_Transformations.csv")
LPmm2 <- logitlink(data.matrix(mm2_pred[,-1]))
Polcal(outcome=mydata$Outcome3, k=3, p=data.matrix(mm2_pred[,-1]), LP=LPmm2[,-1], smoothpar=1, datapoints=FALSE)

# Ridge
mm3_pred<-read.csv("predictions/pred_multinomial_Ridge.csv")
LPmm3 <- logitlink(data.matrix(mm3_pred[,-1]))
Polcal(outcome=mydata$Outcome3, k=3, p=data.matrix(mm3_pred[,-1]), LP=LPmm3[,-1], smoothpar=1, datapoints=FALSE)

#Firth LR
mm4_pred<-read.csv("predictions/pred_multinomial_Firth.csv")
LPmm4 <- logitlink(data.matrix(mm4_pred[,-1]))
Polcal(outcome=mydata$Outcome3, k=3, p=data.matrix(mm4_pred[,-1]), LP=LPmm4[,-1], smoothpar=1, datapoints=FALSE)

# CART
mm5_pred<-read.csv("predictions/pred_multinomial_CART.csv")
mm5_pred$EP_mm5[mm5_pred$EP_mm5 == 0] <- 0.001 # take care of 0,1 predictions
mm5_pred$EP_mm5[mm5_pred$EP_mm5 == 1] <- 0.999
mm5_pred$FPUL_mm5[mm5_pred$FPUL_mm5 == 0] <- 0.001
mm5_pred$FPUL_mm5[mm5_pred$FPUL_mm5 == 1] <- 0.999
mm5_pred$IUP_mm5[mm5_pred$IUP_mm5 == 0] <- 0.001
mm5_pred$IUP_mm5[mm5_pred$IUP_mm5 == 1] <- 0.999
LPmm5 <- logitlink(data.matrix(mm5_pred[,-1]))
Polcal(outcome=mydata$Outcome3, k=3, p=data.matrix(mm5_pred[,-1]), LP=LPmm5[,-1], smoothpar=1, datapoints=FALSE)

#RF
mm6_pred<-read.csv("predictions/pred_multinomial_RF.csv")
mm6_pred$EP_mm6[mm6_pred$EP_mm6 == 0] <- 0.001
mm6_pred$EP_mm6[mm6_pred$EP_mm6 == 1] <- 0.999
mm6_pred$FPUL_mm6[mm6_pred$FPUL_mm6 == 0] <- 0.001
mm6_pred$FPUL_mm6[mm6_pred$FPUL_mm6 == 1] <- 0.999
mm6_pred$IUP_mm6[mm6_pred$IUP_mm6 == 0] <- 0.001
mm6_pred$IUP_mm6[mm6_pred$IUP_mm6 == 1] <- 0.999
LPmm6 <- logitlink(data.matrix(mm6_pred[,-1]))
Polcal(outcome=mydata$Outcome3, k=3, p=data.matrix(mm6_pred[,-1]), LP=LPmm6[,-1], smoothpar=1, datapoints=FALSE)

#XGB
mm7_pred<-read.csv("predictions/pred_multinomial_XGB.csv")
LPmm7 <- logitlink(data.matrix(mm7_pred[,-1]))
Polcal(outcome=mydata$Outcome3, k=3, p=data.matrix(mm7_pred[,-1]), LP=LPmm7[,-1], smoothpar=1, datapoints=FALSE)

#SVM
mm8_pred<-read.csv("predictions/pred_multinomial_SVM.csv")
LPmm8 <- logitlink(data.matrix(mm8_pred[,-1]))
Polcal(outcome=mydata$Outcome3, k=3, p=data.matrix(mm8_pred[,-1]), LP=LPmm8[,-1], smoothpar=1, datapoints=FALSE)

#NN
mm9_pred<-read.csv("predictions/pred_multinomial_NN.csv")
LPmm9 <- logitlink(data.matrix(mm9_pred[,-1]))
Polcal(outcome=mydata$Outcome3, k=3, p=data.matrix(mm9_pred[,-1]), LP=LPmm9[,-1], smoothpar=1, datapoints=FALSE)



# violinplot of predicted risks
tiff("Multinomial Predicted Risk.tiff", width = 24, height = 14, units = "cm", res = 300)
vioplot(mydata$mm1_pred, mydata$mm2_pred, mydata$mm3_pred, mydata$mm4_pred,mydata$mm5_pred,mydata$mm6_pred,mydata$mm7_pred,mydata$mm8_pred,mydata$mm9_pred, names=c('LR', 'LR w tran', 'Ridge LR',"Firth LR", "CART", "RF", "XGBoost", "SVM", "NN"))
title("Violin Plots of predicted risks for EP")
dev.off()  
