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

# Group FPUL and IUP together
mydata['Outcome2'] =  ifelse(mydata$Outcome3=='EP', 1,0)

# Check classes
sapply(mydata, class)

# Factor categorical data
factors <- c("Outcome2", "Outcome3", "Centre")
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


##########################
########## IECV ##########
##########################
set.seed(123)
center_names<-c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")

########### Logistic Regression without transformations ###################
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





##################################################################
#######################CALIBRATION ###############################
##################################################################

mydata$Center <- factor(mydata$Centre, labels=c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park"))
center <- c("Chelsea and Westminster", "Hillingdon", "North Middlesex", "Queen Charlotte's and Chelsea", "Royal Surrey", "St. Marys", "West Middlesex", "Wexham Park")

IncludedCenters <- unique(mydata$Center)
#selecting line types, colors and width
lty.centers <- c(1,1,1,1,1,1,1,1)
col.centers <- c("#d42942", "#f79a7d", "#feac5e", "#fee292", "#e6f698", "#a7db9e", "#68c3a5", "#5198c5")
lwd.centers <- c(2,1,2,1,2,1,2,1)


# 1. Logistic Regression
# need to take care of 0,1 probabilities
mydata$prediction_1[mydata$prediction_1 == 0] <- 0.001
mydata$prediction_1[mydata$prediction_1 == 1] <- 0.999
mydata$LP_m1 <- logit(mydata$prediction_1)
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

# ploting
tiff("Binary_LR_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)
for (i in IncludedCenters) lines(resultm1[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Binary LR: Calibration per Center", line =3, cex.main=0.95)
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
mydata$prediction_2[mydata$prediction_2 == 0] <- 0.001
mydata$prediction_2[mydata$prediction_2 == 1] <- 0.999
mydata$LP_m2 <- logit(mydata$prediction_2)

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

#plot
tiff("Binary_LR_withTransformations_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm2[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Binary LR with Transformations: Calibration per Center", line =3, cex.main=0.95)
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
mydata$prediction_3[mydata$prediction_3 == 0] <- 0.001
mydata$prediction_3[mydata$prediction_3 == 1] <- 0.999
mydata$LP_m3 <- logit(mydata$prediction_3)

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

tiff("Binary_Ridge_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm3[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Binary Ridge Regression: Calibration per Center", line =3, cex.main=0.95)
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
mydata$prediction_4[mydata$prediction_4 == 0] <- 0.001
mydata$prediction_4[mydata$prediction_4 == 1] <- 0.999
mydata$LP_m4 <- logit(mydata$prediction_4)

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

tiff("Binary_Firth_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm4[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Binary Firth LR: Calibration per Center", line =3, cex.main=0.95)
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
mydata$prediction_5[mydata$prediction_5 == 0] <- 0.001
mydata$prediction_5[mydata$prediction_5 == 1] <- 0.999
mydata$LP_m5 <- logit(mydata$prediction_5)

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

tiff("Binary_CART_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm5[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Binary CART: Calibration per Center", line =3, cex.main=0.95)
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
mydata$prediction_6[mydata$prediction_6 == 0] <- 0.001
mydata$prediction_6[mydata$prediction_6 == 1] <- 0.999
mydata$LP_m6 <- logit(mydata$prediction_6)

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

tiff("Binary_RF_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm6[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Binary RF: Calibration per Center", line =3, cex.main=0.95)
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
mydata$prediction_7[mydata$prediction_7 == 0] <- 0.001
mydata$prediction_7[mydata$prediction_7 == 1] <- 0.999
mydata$LP_m7 <- logit(mydata$prediction_7)

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

tiff("Binary_XGB_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm7[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Binary XGB: Calibration per Center", line =3, cex.main=0.95)
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
mydata$prediction_8[mydata$prediction_8 == 0] <- 0.001
mydata$prediction_8[mydata$prediction_8 == 1] <- 0.999
mydata$LP_m8 <- logit(mydata$prediction_8)

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

tiff("Binary_SVM_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm8[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Binary SVM: Calibration per Center", line =3, cex.main=0.95)
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
mydata$prediction_9[mydata$prediction_9 == 0] <- 0.001
mydata$prediction_9[mydata$prediction_9 == 1] <- 0.999
mydata$LP_m9 <- logit(mydata$prediction_9)

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

tiff("Binary_NN_IECV_calibration.tiff", width = 13, height = 13, units = "cm", res = 300)
par(mar = c(5.1,4.1,4.1,9), xpd=FALSE, pty = 's')
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "Estimated risk", 
     ylab = "Observed proportion", type = "n")
clip(0, 1, 0, 1)
abline(0, 1, lty = 1, col = "black", lwd = 1.75)

for (i in IncludedCenters) lines(resultm9[[i]], col = col.centers[which(IncludedCenters == i)], lty = lty.centers[which(IncludedCenters == i)], 
                                 lwd = lwd.centers[which(IncludedCenters == i)])

legend("topright", c("Ideal",as.character(IncludedCenters)) , lty = c(1, lty.centers), cex = 0.6,bty = "n", lwd = c(1.75, lwd.centers), 
       col = c("black", col.centers),  inset = c(-0.7, -0.05),  xpd=NA)
title(main="Binary NN: Calibration per Center", line =3, cex.main=0.95)
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
p.LR = seq(min(mydata$prediction_1), max(mydata$prediction_1), length = 500)
X = cbind(1, logit(p.LR))
FE = c(coef(slopeintermam1), coef(slopemam1))
OverallCal.LR = inv.logit(X[order(p.LR), ] %*% FE)
p.LR = p.LR[order(p.LR)]

#LR with transformations  
p.LR2 = seq(min(mydata$prediction_2), max(mydata$prediction_2), length = 500)
X = cbind(1, logit(p.LR2))
FE = c(coef(slopeintermam2), coef(slopemam2))
OverallCal.LR2 = inv.logit(X[order(p.LR2), ] %*% FE)
p.LR2 = p.LR2[order(p.LR2)]

# Ridge
p.ridge = seq(min(mydata$prediction_3), max(mydata$prediction_3), length = 500)
X = cbind(1, logit(p.ridge))
FE = c(coef(slopeintermam3), coef(slopemam3))
OverallCal.ridge = inv.logit(X[order(p.ridge), ] %*% FE)
p.ridge = p.ridge[order(p.ridge)]

# Firth
p.firth = seq(min(mydata$prediction_4), max(mydata$prediction_4), length = 500)
X = cbind(1, logit(p.firth))
FE = c(coef(slopeintermam4), coef(slopemam4))
OverallCal.firth = inv.logit(X[order(p.firth), ] %*% FE)
p.firth = p.firth[order(p.firth)]

# CART
p.ca = seq(min(mydata$prediction_5), max(mydata$prediction_5), length = 500)
X = cbind(1, logit(p.ca))
FE = c(coef(slopeintermam5), coef(slopemam5))
OverallCal.ca = inv.logit(X[order(p.ca), ] %*% FE)
p.ca = p.ca[order(p.ca)]

# RF
p.rf = seq(min(mydata$prediction_6), max(mydata$prediction_6), length = 500)
X = cbind(1, logit(p.rf))
FE = c(coef(slopeintermam6), coef(slopemam6))
OverallCal.rf = inv.logit(X[order(p.rf), ] %*% FE)
p.rf = p.rf[order(p.rf)]

# XGB
p.xg = seq(min(mydata$prediction_7), max(mydata$prediction_7), length = 500)
X = cbind(1, logit(p.xg))
FE = c(coef(slopeintermam7), coef(slopemam7))
OverallCal.xg = inv.logit(X[order(p.xg), ] %*% FE)
p.xg = p.xg[order(p.xg)]


# SVM
p.svm = seq(min(mydata$prediction_8), max(mydata$prediction_8), length = 500)
X = cbind(1, logit(p.svm))
FE = c(coef(slopeintermam8), coef(slopemam8))
OverallCal.svm = inv.logit(X[order(p.svm), ] %*% FE)
p.svm = p.svm[order(p.svm)]

# NN
p.nn = seq(min(mydata$prediction_9), max(mydata$prediction_9), length = 500)
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
tiff("Binary Overall Calibration.tiff", width = 14, height = 14, units = "cm", res = 300)
plot(x, y, xlim = c(0,1), ylim = c(0,1), type = "l", col = "gray50", lwd = 1, lty = 1, 
     xlab = "Estimated risk", ylab = "Observed proportion",
     main = "Binary Overall Calibration", cex.lab = 1, cex.axis = 1, las = 1) 
lines(p.LR, OverallCal.LR, lwd = 2,lty=4 ,col = "#e6194b")
lines(p.LR2, OverallCal.LR2, lwd = 2,lty=4, col = "#3cb44b")
lines(p.ridge, OverallCal.ridge, lwd = 1, lty=1, col = "#4363d8")
lines(p.firth, OverallCal.firth, lwd=2, lty=1, col="#f58231")
lines(p.ca, OverallCal.ca, lwd=1, lty=1, col="#911eb4")
lines(p.rf, OverallCal.rf, lwd=2,lty=1,  col="#f032e6")
lines(p.xg, OverallCal.xg, lwd=1, lty=1, col="#80CDC1")
lines(p.nn, OverallCal.svm, lwd=2, lty=1, col="#01665E")
lines(p.svm, OverallCal.nn, lwd=1,lty=1, col="#FDE725FF")
legend(x = -0.035, y = 1, legend = c( "Ideal", "LR", "LR w tran", "Ridge LR", "Firth LR", "CART", "RF", "XGBoost", "SVM", "NN"),
       col = c("gray50", "#e6194b", "#3cb44b", "#4363d8","#f58231", "#911eb4", "#f032e6", "#80CDC1", "#01665E", "#FDE725FF"),
       lty = c(1,4,4,1,1,1,1,1,1,1,1), lwd = c(1,2,2,1,2,1,2,1,2,1), cex = 0.7, bty = "n", ncol = 1)
addtable2plot(x = 0.40, y = -0.02, table = table, display.colnames= TRUE, cex = 0.60) 
dev.off()  


# Violinplot of predicted risks
tiff("Binary Predicted Risk.tiff", width = 24, height = 14, units = "cm", res = 300)
vioplot(mydata$prediction_1, mydata$prediction_2, mydata$prediction_3, mydata$prediction_4,mydata$prediction_5,mydata$prediction_6,mydata$prediction_7,mydata$prediction_8,mydata$prediction_9, names=c('LR', 'LR w tran', 'Ridge LR',"Firth LR", "CART", "RF", "XGBoost", "SVM", "NN"))
title("Violin Plots of predicted risks for EP")
dev.off()  









