# Load Required Packages
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

# Only use imputations
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

# Relevel the Outcome with 3 categroies, reference catgeory is EP
mydata['Outcome2'] =  ifelse(mydata$Outcome3=='EP', 1,0)
mydata$Outcome3 <- relevel(mydata$Outcome3, ref = "EP")


################################
###### Multinomial Models ######
################################
################################
##### Apparent Performance #####
################################

# 1. Logistic Regression
set.seed(123)
answer=matrix(nrow=0,ncol=3)
mods1 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods1[[i]]  <- multinom(Outcome3~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, data=mydata[mydata$.imp==i,])
  pred <- predict(mods1[[i]],  type="probs")
  answer <- rbind(answer, pred)
  
}
write.csv(answer, "predictions/pred_multinomial_apparent_LR.csv")
mydata$mm1_predapp <- rowSums(answer[,2:3])
apparentmm1auc <- AUC.imp(pred=mm1_predapp,outcome = Outcome2, imp=.imp, data=mydata)
apparentmm1auc$Performance['AUC']



# 2. Logistic Regression with transformations
answer<-matrix(nrow=0,ncol=3)
mods2 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods2[[i]] <- multinom(Outcome3~log.hCG0num+ log.prog0num + I(log.prog0num^2) + lhCGratio +  I(lhCGratio^2)  + lhCGratio_log.prog0num  + Agesd + I(Agesd^2) + Vaginal.bleeding + Histep + Painscore + Painscore2, data=mydata[mydata$.imp==i,])
  pred <- predict(mods2[[i]], type="probs") 
  answer <- rbind(answer, pred)
  
}
write.csv(answer, "predictions/pred_multinomial_apparent_LR_with_Transformations.csv")
mydata$mm2_predapp <- rowSums(answer[,2:3])
apparentmm2auc <- AUC.imp(pred=mm2_predapp,outcome = Outcome2, imp=.imp, data=mydata)
apparentmm2auc$Performance['AUC']


# 3. Ridge logistic Regression
# Prepare data 
y_train_std <- mydata[, c(".imp","Outcome3")]
x_train_std <- mydata[, c(".imp","log.hCG0num","log.prog0num","lhCGratio","Agesd","Vaginal.bleeding","Histep","Painscore","lhCGratio_log.prog0num",'Painscore2')]
x_train_std$X2.log.prog0num <-x_train_std$log.prog0num^2
x_train_std$X2.lhCGratio <-x_train_std$lhCGratio^2
x_train_std$X2.Agesd<- x_train_std$Agesd^2


answer<-matrix(nrow=0,ncol=3)
mods3 <- vector(mode="list", length=10)
for (i in 1:10)
{
  x_train_std_imp<- x_train_std[x_train_std$.imp==i,]
  X <- as.matrix(x_train_std_imp[,-1])
  y_train_std_imp<- y_train_std[y_train_std$.imp==i,]
  Y <- as.matrix(y_train_std_imp[,-1])
  cv_train_std <- cv.glmnet(X, Y, nfolds=10, type.measure="deviance", alpha=0, family="multinomial")
  lambda <- cv_train_std$lambda.min
  mods3[[i]] <- glmnet(X, Y, family="multinomial", lambda=lambda, alpha=0)
  pred <- predict(mods3[[i]], X, type="response", s=lambda)
  pred<-drop(pred)
  answer <- rbind(answer, pred)
}
write.csv(answer, "predictions/pred_multinomial_apparent_Ridge.csv")
mydata$mm3_predapp <- rowSums(answer[,2:3])
apparentmm3auc <- AUC.imp(pred=mm3_predapp, outcome = Outcome2, imp=.imp, data=mydata)
apparentmm3auc$Performance['AUC']

#to get the lambda values of each model
bestlam= c()
for (i in 1:10)
{
  bestlam <- append(bestlam, mods3[[i]]$lambda )
}


# 4. Firth logistic Regression
answer<-matrix(nrow=0,ncol=3)
mods4 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods4[[i]] <- multinom(Outcome3~log.hCG0num+ log.prog0num + I(log.prog0num^2) + lhCGratio +  I(lhCGratio^2)  + lhCGratio_log.prog0num  + Agesd + I(Agesd^2) + Vaginal.bleeding + Histep + Painscore + Painscore2, data=mydata[mydata$.imp==i,], family = binomial, method="brglmFit")
  pred <- predict(mods4[[i]], type="probs") 
  answer <- rbind(answer, pred)
  
}
write.csv(answer, "predictions/pred_multinomial_apparent_Firth.csv")
mydata$mm4_predapp <- rowSums(answer[,2:3])
apparentmm4auc <- AUC.imp(pred=mm4_predapp,outcome = Outcome2, imp=.imp, data=mydata)
apparentmm4auc$Performance['AUC']
#0.8959424


######## VISUALIZE REGRESSION COEFFICIENTS ########
png(filename="graphs/multinomial_logistic_regression_apparent_performace_model_coefficients", res=200, width=800, height=1024)
plot_summs(pool(mods1), scale = TRUE, coefs = c("Intercept"= "(Intercept)","hCG" = "hCG0num","progesterone" = "prog0num", "hCG ratio" = "hCGratio","Age"="Age", "Vaginal bleeding"="Vaginal.bleeding", "History EP"="Histep", "Painscore"="Painscore"))
dev.off()
png(filename="graphs/multinomial_Logwithtrans_and_Firth_apparent_performace_model_coefficients", res=200, width=1800, height=1024)
plot_summs(pool(mods2),pool(mods4), scale = TRUE, coefs = c("Intercept"= "(Intercept)", "log hCG" = "log.hCG0num", "log progesterone" = "log.prog0num","log progesterone^2" = "I(log.prog0num^2)","log hCG ratio" = "lhCGratio",'log hCG ratio^2'='I(lhCGratio^2)',"log hCG ratio*log progesterone" = "lhCGratio_log.prog0num","Age (standardized)" = "Agesd","Age (standardized)^2" = "I(Agesd^2)"," Vaginal bleeding" = "Vaginal.bleeding","History EP" = "Histep", "Painscore continous" = "Painscore", "Painscore binary" = "Painscore2" ), model.names = c("Logistic Regression with transformations", "Firth Regression"))
dev.off()


# 5. CART
set.seed(1234)
ctrl <- trainControl(method="cv",   # 10fold cross validation
                     number=10,         
                     summaryFunction=LogLoss,
                     classProbs=TRUE,
                     search="random")

answer<-matrix(nrow=0,ncol=3)
mods5 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods5[[i]]<- train(make.names(Outcome3)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, data=mydata[mydata$.imp==i,], method="rpart", trControl=ctrl, tuneLength = 40)
  pred <- predict(mods5[[i]], type="prob") 
  answer <- rbind(answer, pred)
}
write.csv(answer, "predictions/pred_multinomial_apparent_CART.csv")
mydata$mm5_predapp <- rowSums(answer[,2:3])
apparentmm5auc <- AUC.imp(pred=mm5_predapp,outcome = Outcome2, imp=.imp, data=mydata)
apparentmm5auc$Performance['AUC']


# 6. Random Forest
set.seed(123)
control <- trainControl(method="cv", number=10, classProbs=TRUE,search='grid')
hyper_grid <- expand.grid(mtry= c(1:4), min.node.size = c(1, 4, 6, 8, 10), splitrule="gini")

answer<-matrix(nrow=0,ncol=3)
mods6 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods6[[i]]<- train(make.names(Outcome3)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, 
                     data=mydata[mydata$.imp==i,],
                     method = "ranger",
                     trControl = control,
                     tuneGrid = hyper_grid)
  pred <- predict(mods6[[i]], type="prob") 
  answer <- rbind(answer, pred)
  print(i)
}
write.csv(answer, "predictions/pred_multinomial_apparent_RF.csv")
mydata$mm6_predapp <- rowSums(answer[,2:3])
apparentmm6auc <- AUC.imp(pred=mm6_predapp,outcome = Outcome2, imp=.imp, data=mydata)
apparentmm6auc$Performance['AUC']

# 7. XGBoost
set.seed(1234)
xgb_trcontrol = trainControl(method = "cv", number = 10, verboseIter = FALSE,classProbs=TRUE, allowParallel = TRUE, returnData = FALSE)
xgbgrid <- expand.grid(nrounds=100,
                       eta=c(0.01, 0.001,0.0001, 0.1, 0.15, 0.2, 0.3),
                       max_depth=6, #6 is default c(5,7,9,11),
                       gamma=0,
                       colsample_bytree=1,
                       min_child_weight=1,
                       subsample=1)
answer<-matrix(nrow=0,ncol=3)
mods7 <- vector(mode="list", length=10)


for (i in 1:10)
{
  X <- subset(mydata, select=c(Outcome3,.imp,hCG0num,prog0num,hCGratio,Age,Vaginal.bleeding,Histep,Painscore))
  mods7[[i]]<-train(as.factor(make.names(Outcome3))~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore,
                    trControl = xgb_trcontrol, 
                    method = "xgbTree",
                    tuneLength=10, #make higher?
                    #tuneGrid=xgbgrid,
                    verbosity = 0,
                    data = X[X$.imp==i,])
  
  data<-X[X$.imp==i, 3:9]
  pred <- predict(mods7[[i]], type="prob", newdata=data)
  answer <- rbind(answer, pred)
  print(i)
}
write.csv(answer, "predictions/pred_multinomial_apparent_XGB.csv")
mydata$mm7_predapp <- rowSums(answer[,2:3])
apparentmm7auc <- AUC.imp(pred=mm7_predapp,outcome = Outcome2, imp=.imp, data=mydata)
apparentmm7auc$Performance['AUC']


# 8. SVM
set.seed(123)
ctrl <- trainControl(method="cv",   # 10fold cross validation
                     number=10,   
                     search="random",
                     classProbs=TRUE)

answer<-matrix(nrow=0,ncol=3)
mods8 <- vector(mode="list", length=10)

system.time(for (i in 1:10)
{
  X <- subset(mydata, select=c(Outcome3,.imp,hCG0num,prog0num,hCGratio,Age,Vaginal.bleeding,Histep,Painscore))
  X$Outcome3 <-as.factor(X$Outcome3)
  X<-X[X$.imp==i,]
  X <- X[,-2]
  mods8[[i]] <- train(make.names(Outcome3)~.,
                      method = "svmRadial",# Radial basis function kernel
                      tuneLength = 10,
                      preProcess = c("center", "scale"),
                      data=X,
                      trControl=ctrl) 
  pred <- predict(mods8[[i]], type="prob",newdata= X)
  answer <- rbind(answer, pred)
  print(i)
})
write.csv(answer, "predictions/pred_multinomial_apparent_SVM.csv")
mydata$mm8_predapp <- rowSums(answer[,2:3])
apparentmm8auc <- AUC.imp(pred=mm8_predapp,outcome = Outcome2, imp=.imp, data=mydata)
apparentmm8auc$Performance['AUC']


# 9. Neural Network
mods9 <- vector(mode="list", length=10)
ctrl <- trainControl(method="cv", 
                     number=10,       
                     search="random",
                     classProbs=TRUE, summaryFunction=multiClassSummary)

answer<-matrix(nrow=0,ncol=3)
for (i in 1:10) {
  X <- subset(mydata, select=c(Outcome3,.imp,hCG0num,prog0num,hCGratio,Age,Vaginal.bleeding,Histep,Painscore))
  X$Outcome2 <-as.factor(X$Outcome3)
  X<-X[X$.imp==i,]
  X <- X[,-2]
  mods9[[i]] <- train(make.names(Outcome3)~.,
                      method = "nnet",
                      tuneLength = 30,
                      preProcess = c("center", "scale"),
                      trace = FALSE,
                      maxit = 1000,
                      linout = FALSE,
                      data=X,
                      trControl=ctrl, metric="ROC") 
  pred <- predict(mods9[[i]], type="prob",newdata= X)
  answer <- rbind(answer, pred)
  print(i)
}
write.csv(answer, "predictions/pred_multinomial_apparent_NN.csv")
mydata$mm9_predapp <- rowSums(answer[,2:3])
apparentmm9auc <- AUC.imp(pred=mm9_predapp,outcome = Outcome2, imp=.imp, data=mydata)
apparentmm9auc$Performance['AUC']



### Forest plot apparent model performance
NA.forest <- apparentmm1auc$Performance[1,]
NA.forest <- NA
Summary.AUC <- rbind(NA.forest, apparentmm1auc$Performance[1,], apparentmm2auc$Performance[1,], apparentmm3auc$Performance[1,], apparentmm4auc$Performance[1,],apparentmm5auc$Performance[1,], apparentmm6auc$Performance[1,], apparentmm7auc$Performance[1,], apparentmm8auc$Performance[1,], apparentmm9auc$Performance[1,])
Summary.AUC$Model <- c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
Summary.AUC.PI <- rbind(NA.forest, apparentmm1auc$Performance[2,], apparentmm2auc$Performance[2,], apparentmm3auc$Performance[2,], apparentmm4auc$Performance[2,], apparentmm5auc$Performance[2,], apparentmm6auc$Performance[2,], apparentmm7auc$Performance[2,], apparentmm8auc$Performance[2,], apparentmm9auc$Performance[2,])
Summary.AUC.PI$Model <-  c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
tabletext <- cbind(
  c('Model', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN"),
  c('cEP (95% CI)', 
    paste(format(round(apparentmm1auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentmm1auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentmm1auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentmm2auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentmm2auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentmm2auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentmm3auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentmm3auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentmm3auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentmm4auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentmm4auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentmm4auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentmm5auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentmm5auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentmm5auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentmm6auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentmm6auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentmm6auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentmm7auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentmm7auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentmm7auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentmm8auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentmm8auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentmm8auc$Performance$UL[1], 2), nsmall = 2), ")", sep= ""),
    paste(format(round(apparentmm9auc$Performance$AUC[1], 2), nsmall = 2), " (", "1.00", " to ", format(round(apparentmm9auc$Performance$UL[1], 2), nsmall = 2), ")", sep="")),
  c('95% PI', 
    paste0("(", format(round(apparentmm1auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentmm1auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparentmm2auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentmm2auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparentmm3auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentmm3auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparentmm4auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentmm4auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparentmm5auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentmm5auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparentmm6auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentmm6auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparentmm7auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentmm7auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparentmm8auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentmm8auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparentmm9auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentmm9auc$Performance$UL[2], 2), nsmall = 2), ")")))

tiff("graphs/apparent_multinomial_AUC_all_models", width = 31, height = 13.75, units = "cm", res = 300)
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


############################
##### Apparent PDI #########
############################

#LR
mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_LR.csv")[ ,2:4]

se<-c()
value<-c()
for (i in 1:10){
  mydata1<-mydata[mydata$.imp==i,]
  mm1_predapp_1<-mm1_predapp[(((i-1)*2894)+1):(i*2894),]
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
PDI_mm1<-PDIcombined


# LR with transformations
mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_LR_with_Transformations.csv")[ ,2:4]

se<-c()
value<-c()
for (i in 1:10){
  mydata1<-mydata[mydata$.imp==i,]
  mm1_predapp_1<-mm1_predapp[(((i-1)*2894)+1):(i*2894),]
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
PDI_mm2<-PDIcombined


# RIDGE 
mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_Ridge.csv")[ ,2:4]

se<-c()
value<-c()
for (i in 1:10){
  mydata1<-mydata[mydata$.imp==i,]
  mm1_predapp_1<-mm1_predapp[(((i-1)*2894)+1):(i*2894),]
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
PDI_mm3<-PDIcombined




# FIRTH 
mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_Firth.csv")[ ,2:4]

se<-c()
value<-c()
for (i in 1:10){
  mydata1<-mydata[mydata$.imp==i,]
  mm1_predapp_1<-mm1_predapp[(((i-1)*2894)+1):(i*2894),]
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
PDI_mm4<-PDIcombined

# CART 
mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_CART.csv")[ ,2:4]

se<-c()
value<-c()
for (i in 1:10){
  mydata1<-mydata[mydata$.imp==i,]
  mm1_predapp_1<-mm1_predapp[(((i-1)*2894)+1):(i*2894),]
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
PDI_mm5<-PDIcombined

# RF 
mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_RF.csv")[ ,2:4]

se<-c()
value<-c()
for (i in 1:10){
  mydata1<-mydata[mydata$.imp==i,]
  mm1_predapp_1<-mm1_predapp[(((i-1)*2894)+1):(i*2894),]
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
PDI_mm6<-PDIcombined

# XGB 
mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_XGB.csv")[ ,2:4]

se<-c()
value<-c()
for (i in 1:10){
  mydata1<-mydata[mydata$.imp==i,]
  mm1_predapp_1<-mm1_predapp[(((i-1)*2894)+1):(i*2894),]
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
PDI_mm7<-PDIcombined

# SVM
mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_SVM.csv")[ ,2:4]

se<-c()
value<-c()
for (i in 1:10){
  mydata1<-mydata[mydata$.imp==i,]
  mm1_predapp_1<-mm1_predapp[(((i-1)*2894)+1):(i*2894),]
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
PDI_mm8<-PDIcombined


# NN
mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_NN.csv")[ ,2:4]

se<-c()
value<-c()
for (i in 1:10){
  mydata1<-mydata[mydata$.imp==i,]
  mm1_predapp_1<-mm1_predapp[(((i-1)*2894)+1):(i*2894),]
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
PDI_mm9<-PDIcombined



#######################
### apparent cIF #####
#####################

# function for pooling cIF of different imputation
# adjusted from: https://github.com/benvancalster/IOTA5modelvalidation2020/blob/master/Functions%20IOTA5.R
AUC.imp.cIF <- function(pred, outcome, imp, data){
  
  arguments <- as.list(match.call())[-1]
  pred = eval(arguments$pred, data)
  outcome <- eval(arguments$outcome, data)
  imp <- eval(arguments$imp, data)
  
  Df = data.frame(p = pred, y = outcome, imp = imp, stringsAsFactors = F)
  
  NRimp <- length(unique(Df$imp))
  
  AUCimp <- matrix(ncol = 3, nrow = NRimp)
  colnames(AUCimp) <- c('AUC', 'LL', 'UL')
  AUCimp <- data.frame(AUCimp)
  
  # AUC per center
  for(j in 1:NRimp){
    cat("Imputation", j, " of ", NRimp, "\n\n")
    AUC <- auc.nonpara.mw(Df$p[Df$y == 'IUP' & Df$imp == j], Df$p[Df$y == 'FPUL' & Df$imp == j], method = "pepe")
    
    if(AUC[1] < 0.50){
      AUC2 <- auc.nonpara.mw(Df$p[Df$y == 'IUP' & Df$imp == j], Df$p[Df$y == 'FPUL' & Df$imp == j], method = "pepe")
      AUCimp[j, 1:3] <- AUC2 
    } 
    
    ## Additional part for AUCs of 1
    else{
      if(AUC[1] == 1){
        AUC3 <- auc.nonpara.mw(Df$p[Df$y == 'IUP' & Df$imp == j], Df$p[Df$y == 'FPUL' & Df$imp == j], method = "newcombe") # Newcombe ipv pepe
        AUCimp[j, 1:3] <- AUC3 
      } else{
        AUCimp[j, 1:3] <- AUC 
      }
    }
    
    if(AUCimp$AUC[j] != 1){
      AUCimp$logit.AUC[j] <- logit(AUCimp$AUC[j])
      AUCimp$logit.se[j]  <- (logit(AUCimp$AUC[j]) - logit(AUCimp$LL[j]))/1.96
      AUCimp$logit.var[j] <- AUCimp$logit.se[j]^2
    } else{
      AUCimp$logit.AUC[j] <- logit(0.999)
      AUCimp$logit.se[j]  <- (logit(0.999) - logit(AUCimp$LL[j]))/1.96
      AUCimp$logit.var[j] <- AUCimp$logit.se[j]^2
    }
    AUCimp$Mal[j] <- nrow(Df[Df$y == 1 & Df$imp == j,])
  }
  
  # Combine results with Rubin's rule
  AUCcombined <- matrix(ncol = 3, nrow = 1)
  colnames(AUCcombined) <- c('logit.AUC', 'logit.LL', 'logit.UL')
  AUCcombined <- data.frame(AUCcombined)
  
  AUCcombined$logit.AUC <- mean(AUCimp$logit.AUC)
  WithinVar <- mean(AUCimp$logit.var)
  BetweenVar <- var(AUCimp$logit.AUC)
  PooledVar <- WithinVar + BetweenVar + BetweenVar/NRimp
  AUCcombined$PooledSE <- sqrt(PooledVar)
  AUCcombined$logit.LL <- AUCcombined$logit.AUC - 1.96*AUCcombined$PooledSE
  AUCcombined$logit.UL <- AUCcombined$logit.AUC + 1.96*AUCcombined$PooledSE
  
  # Transform back to original scale
  AUCcombined$AUC <- inv.logit(AUCcombined$logit.AUC)
  AUCcombined$LL <- inv.logit(AUCcombined$logit.LL)
  AUCcombined$UL <- inv.logit(AUCcombined$logit.UL)
  
  return(structure(list(Performance = AUCcombined, imp = AUCimp)))
  
}

mm1_predapp<-read.csv("predictions/pred_multinomial_apparent_LR.csv")[ ,3:4]
mydata$mm1_cIF <- mm1_predapp[, 2] / (mm1_predapp[,2] + mm1_predapp[,1])
apparent_cIF_1<-AUC.imp.cIF(pred=mm1_cIF, outcome=Outcome3, imp=.imp, data=mydata)

mm2_predapp<-read.csv("predictions/pred_multinomial_apparent_LR_with_Transformations.csv")[ ,3:4]
mydata$mm2_cIF <- mm2_predapp[, 2] / (mm2_predapp[,2] + mm2_predapp[,1])
apparent_cIF_2<-AUC.imp.cIF(pred=mm2_cIF, outcome=Outcome3, imp=.imp, data=mydata)

mm3_predapp<-read.csv("predictions/pred_multinomial_apparent_Ridge.csv")[ ,3:4]
mydata$mm3_cIF <- mm3_predapp[, 2] / (mm3_predapp[,2] + mm3_predapp[,1])
apparent_cIF_3<-AUC.imp.cIF(pred=mm3_cIF, outcome=Outcome3, imp=.imp, data=mydata)

mm4_predapp<-read.csv("predictions/pred_multinomial_apparent_Firth.csv")[ ,3:4]
mydata$mm4_cIF <- mm4_predapp[, 2] / (mm4_predapp[,2] + mm4_predapp[,1])
apparent_cIF_4<-AUC.imp.cIF(pred=mm4_cIF, outcome=Outcome3, imp=.imp, data=mydata)

mm5_predapp<-read.csv("predictions/pred_multinomial_apparent_CART.csv")[ ,3:4]
mydata$mm5_cIF <- mm5_predapp[, 2] / (mm5_predapp[,2] + mm5_predapp[,1])
apparent_cIF_5<-AUC.imp.cIF(pred=mm5_cIF, outcome=Outcome3, imp=.imp, data=mydata)

mm6_predapp<-read.csv("predictions/pred_multinomial_apparent_RF.csv")[ ,3:4]
mydata$mm6_cIF <- mm6_predapp[, 2] / (mm6_predapp[,2] + mm6_predapp[,1])
apparent_cIF_6<-AUC.imp.cIF(pred=mm6_cIF, outcome=Outcome3, imp=.imp, data=mydata)

mm7_predapp<-read.csv("predictions/pred_multinomial_apparent_XGB.csv")[ ,3:4]
mydata$mm7_cIF <- mm7_predapp[, 2] / (mm7_predapp[,2] + mm7_predapp[,1])
apparent_cIF_7<-AUC.imp.cIF(pred=mm7_cIF, outcome=Outcome3, imp=.imp, data=mydata)

mm8_predapp<-read.csv("predictions/pred_multinomial_apparent_SVM.csv")[ ,3:4]
mydata$mm8_cIF <- mm8_predapp[, 2] / (mm8_predapp[,2] + mm8_predapp[,1])
apparent_cIF_8<-AUC.imp.cIF(pred=mm8_cIF, outcome=Outcome3, imp=.imp, data=mydata)


mm9_predapp<-read.csv("predictions/pred_multinomial_apparent_NN.csv")[ ,3:4]
mydata$mm9_cIF <- mm9_predapp[, 2] / (mm9_predapp[,2] + mm9_predapp[,1])
apparent_cIF_9<-AUC.imp.cIF(pred=mm9_cIF, outcome=Outcome3, imp=.imp, data=mydata)





### Forest plot apparent model performance cIF
NA.forest <- apparent_cIF_1$Performance[1,]
NA.forest <- NA
Summary.AUC <- rbind(NA.forest, apparent_cIF_1$Performance[1,], apparent_cIF_2$Performance[1,], apparent_cIF_3$Performance[1,], apparent_cIF_4$Performance[1,],apparent_cIF_5$Performance[1,], apparent_cIF_6$Performance[1,], apparent_cIF_7$Performance[1,], apparent_cIF_8$Performance[1,], apparent_cIF_9$Performance[1,])
Summary.AUC$Model <- c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
Summary.AUC.PI <- rbind(NA.forest, apparent_cIF_1$Performance[2,], apparent_cIF_2$Performance[2,], apparent_cIF_3$Performance[2,], apparent_cIF_4$Performance[2,], apparent_cIF_5$Performance[2,], apparent_cIF_6$Performance[2,], apparent_cIF_7$Performance[2,], apparent_cIF_8$Performance[2,], apparent_cIF_9$Performance[2,])
Summary.AUC.PI$Model <-  c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
tabletext <- cbind(
  c('Model', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN"),
  c('cIF (95% CI)', 
    paste(format(round(apparent_cIF_1$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparent_cIF_1$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparent_cIF_1$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparent_cIF_2$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparent_cIF_2$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparent_cIF_2$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparent_cIF_3$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparent_cIF_3$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparent_cIF_3$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparent_cIF_4$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparent_cIF_4$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparent_cIF_4$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparent_cIF_5$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparent_cIF_5$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparent_cIF_5$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparent_cIF_6$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparent_cIF_6$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparent_cIF_6$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparent_cIF_7$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparent_cIF_7$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparent_cIF_7$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparent_cIF_8$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparent_cIF_8$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparent_cIF_8$Performance$UL[1], 2), nsmall = 2), ")", sep= ""),
    paste(format(round(apparent_cIF_9$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparent_cIF_9$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparent_cIF_9$Performance$UL[1], 2), nsmall = 2), ")", sep="")),
  c('95% PI', 
    paste0("(", format(round(apparent_cIF_1$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparent_cIF_1$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparent_cIF_2$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparent_cIF_2$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparent_cIF_3$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparent_cIF_3$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparent_cIF_4$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparent_cIF_4$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparent_cIF_5$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparent_cIF_5$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparent_cIF_6$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparent_cIF_6$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparent_cIF_7$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparent_cIF_7$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparent_cIF_8$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparent_cIF_8$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparent_cIF_9$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparent_cIF_9$Performance$UL[2], 2), nsmall = 2), ")")))

tiff("graphs/apparent_multinomial_cIF_all_models", width = 31, height = 13.75, units = "cm", res = 300)
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
