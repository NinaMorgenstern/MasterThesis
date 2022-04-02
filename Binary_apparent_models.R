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


################################
######## Binary Models #########
################################
################################
##### Apparent Performance #####
################################

# 1. Logistic Regression
set.seed(123)
answer=c()
mods1 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods1[[i]]  <- glm(Outcome2~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, family=binomial(link="logit"), data=mydata[mydata$.imp==i,])
  pred <- predict( mods1[[i]] , type="response")
  answer <- append(answer, pred)
  i=i+1
}
mydata['predictions'] <-answer
apparentm1auc <- AUC.imp(mydata$predictions,outcome = mydata$Outcome2, imp=mydata$.imp, data=mydata)
apparentm1auc$Performance['AUC']


# 2. Logistic Regression with transformations
answer<-c()
mods2 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods2[[i]] <- glm(Outcome2~log.hCG0num+ log.prog0num + I(log.prog0num^2) + lhCGratio +  I(lhCGratio^2)  + lhCGratio_log.prog0num  + Agesd + I(Agesd^2) + Vaginal.bleeding + Histep + Painscore + Painscore2 , family=binomial(link="logit"), data=mydata[mydata$.imp==i,])
  pred <- predict(mods2[[i]], type="response") 
  answer <- append(answer, pred)
  i=i+1
}
mydata['predictions'] <-answer

apparentm2auc <- AUC.imp(mydata$predictions,outcome = mydata$Outcome2, imp=mydata$.imp, data=mydata)
apparentm2auc$Performance['AUC']


# 3. Ridge logistic Regression
# Prepare data 
y_train_std <- mydata[, c(".imp","Outcome2")]
x_train_std <- mydata[, c(".imp","log.hCG0num","log.prog0num","lhCGratio","Agesd","Vaginal.bleeding","Histep","Painscore","lhCGratio_log.prog0num",'Painscore2')]
x_train_std$X2.log.prog0num <-x_train_std$log.prog0num^2
x_train_std$X2.lhCGratio <-x_train_std$lhCGratio^2
x_train_std$X2.Agesd<- x_train_std$Agesd^2

answer=c()
mods3 <- vector(mode="list", length=10)
for (i in 1:10)
{
  x_train_std_imp<- x_train_std[x_train_std$.imp==i,]
  X <- as.matrix(x_train_std_imp[,-1])
  y_train_std_imp<- y_train_std[y_train_std$.imp==i,]
  Y <- as.matrix(y_train_std_imp[,-1])
  cv_train_std <- cv.glmnet(X, Y, nfolds=10, type.measure="deviance", alpha=0, family="binomial")
  lambda <- cv_train_std$lambda.min
  mods3[[i]] <- glmnet(X, Y, family="binomial", lambda=lambda, alpha=0)
  pred <- predict(mods3[[i]],lambda, type="response",newx= X)
  answer <- append(answer, pred)
  i=i+1
}
mydata['predictions'] <-answer

apparentm3auc <- AUC.imp(mydata$predictions,outcome = mydata$Outcome2, imp=mydata$.imp, data=mydata)
apparentm3auc$Performance['AUC']

bestlam= c()
for (i in 1:10)
{
  bestlam <- append(bestlam, mods3[[i]]$lambda )
}

# 4. Firth logistic Regression
answer=c()
mods4 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods4[[i]] = glm(Outcome2~log.hCG0num+ log.prog0num + I(log.prog0num^2) + lhCGratio +  I(lhCGratio^2)  + lhCGratio_log.prog0num  + Agesd + I(Agesd^2) + Vaginal.bleeding + Histep + Painscore + Painscore2, data=mydata[mydata$.imp==i,], family = binomial, method="brglmFit")
  pred <- predict(mods4[[i]], type="response")
  answer <- append(answer, pred)
  i=i+1
}
mydata['predictions'] <-answer

apparentm4auc <- AUC.imp(mydata$predictions,outcome = mydata$Outcome2, imp=mydata$.imp, data=mydata)
apparentm4auc$Performance['AUC']


######## VISUALIZE REGRESSION COEFFICIENTS ########
png(filename="graphs/logistic_regression_apparent_performace_model_coefficients", res=200, width=800, height=1024)
plot_summs(pool(mods1), scale = TRUE, coefs = c("Intercept"= "(Intercept)","hCG" = "hCG0num","progesterone" = "prog0num", "hCG ratio" = "hCGratio","Age"="Age", "Vaginal bleeding"="Vaginal.bleeding", "History EP"="Histep", "Painscore"="Painscore"))
dev.off()
png(filename="graphs/Logwithtrans_and_Firth_apparent_performace_model_coefficients", res=200, width=1800, height=1024)
plot_summs(pool(mods2),pool(mods4), scale = TRUE, coefs = c("Intercept"= "(Intercept)", "log hCG" = "log.hCG0num", "log progesterone" = "log.prog0num","log progesterone^2" = "I(log.prog0num^2)","log hCG ratio" = "lhCGratio",'log hCG ratio^2'='I(lhCGratio^2)',"log hCG ratio*log progesterone" = "lhCGratio_log.prog0num","Age (standardized)" = "Agesd","Age (standardized)^2" = "I(Agesd^2)"," Vaginal bleeding" = "Vaginal.bleeding","History EP" = "Histep", "Painscore continous" = "Painscore", "Painscore binary" = "Painscore2" ), model.names = c("Logistic Regression with transformations", "Firth Regression"))
dev.off()

##Ridge regression results cannot be pooled
ridgecoef<-matrix(nrow=13, ncol=0)
for (i in 1:10)
{
  ridgecoef <- cbind(ridgecoef, coef(mods3[[i]]))
}
b = as.data.frame(summary(ridgecoef))
b[,'i']<-factor(b[,'i'], labels=c("Intercept", "log hCG", "log progesterone","log hCG ratio","Age (standardized)","Vaginal bleeding","History EP", "Painscore continous" ,"log hCG ratio*log progesterone" , "Painscore binary","log progesterone^2",'log hCG ratio^2',"Age (standardized)^2"))
png(filename="graphs/RidgeRegression_apparent_performace_model_coefficients", res=200, width=1500, height=1024)
ggplot(b, aes(x=x,y=i)) + geom_point() + xlab('Odds ratios') +ylab('Coefficients')
dev.off()

# 5. CART
# Hyperparameters: minimum cases in leaf node, minimum cases in node to split
set.seed(123)
ctrl <- trainControl(method="cv",   # 10fold cross validation
                     number=10,         
                     summaryFunction=LogLoss,
                     classProbs=TRUE)

set.seed(1234)

answer=c()
mods5 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods5[[i]]<- train(make.names(Outcome2)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, data=mydata[mydata$.imp==i,], method="rpart",metric="LogLoss", trControl=ctrl, tuneLength = 40)
  pred <- predict(mods5[[i]], type="prob") 
  answer <- append(answer, pred[,2])
}
mydata['predictions'] <-answer

apparentm5auc <- AUC.imp(mydata$predictions,outcome = mydata$Outcome2, imp=mydata$.imp, data=mydata)
apparentm5auc$Performance['AUC']


# 6. Random Forest
set.seed(123)
control <- trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=LogLoss,search='grid')
hyper_grid <- expand.grid(mtry= c(1:4), min.node.size = c(1, 4, 6, 8, 10), splitrule="gini")

answer=c()
mods6 <- vector(mode="list", length=10)
for (i in 1:10)
{
  mods6[[i]]<- train(make.names(Outcome2)~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore, 
                     data=mydata[mydata$.imp==i,],
                     method = "ranger",
                     trControl = control,
                     tuneGrid = hyper_grid,
                     metric="LogLoss")
  pred <- predict(mods6[[i]], type="prob") 
  answer <- append(answer, pred[,2])
  print(i)
}
mydata['predictions'] <-answer

apparentm6auc <- AUC.imp(mydata$predictions,outcome = mydata$Outcome2, imp=mydata$.imp, data=mydata)
apparentm6auc$Performance['AUC']

# 7. XGBoost
set.seed(1234)

xgb_trcontrol = trainControl(method = "cv", number = 10, 
                             verboseIter = FALSE, summaryFunction=LogLoss,   
                             classProbs=TRUE, allowParallel = TRUE, returnData = FALSE)

xgbgrid <- expand.grid(nrounds=100,
                       eta=c(0.01, 0.001,0.0001, 0.1, 0.15, 0.2, 0.3),
                       max_depth=6, 
                       gamma=0,
                       colsample_bytree=1,
                       min_child_weight=1,
                       subsample=1)
answer=c()
mods7 <- vector(mode="list", length=10)


for (i in 1:10)
{
  X <- subset(mydata, select=c(Outcome2,.imp,hCG0num,prog0num,hCGratio,Age,Vaginal.bleeding,Histep,Painscore))
  mods7[[i]]<-train(as.factor(make.names(Outcome2))~hCG0num+prog0num+hCGratio+Age+Vaginal.bleeding+Histep+Painscore,
                    trControl = xgb_trcontrol, 
                    method = "xgbTree",
                    tuneLength=10,
                    #tuneGrid=xgbgrid,
                    data = X[X$.imp==i,],
                    metric="LogLoss")
  
  data<-X[X$.imp==i, 3:9]
  pred <- predict(mods7[[i]], type="prob", newdata=data)
  answer <- append(answer, pred[,2])
  print(i)
}
mydata['predictions'] <-answer
apparentm7auc <- AUC.imp(mydata$predictions,outcome = mydata$Outcome2, imp=mydata$.imp, data=mydata)
apparentm7auc$Performance['AUC']
apparentm7auc$Performance['LL']
apparentm7auc$Performance$PooledSE

# 8. SVM
set.seed(123)
ctrl <- trainControl(method="cv",   # 10fold cross validation
                     number=10,         
                     summaryFunction=LogLoss,
                     search="random",
                     classProbs=TRUE)

answer=c()
mods8 <- vector(mode="list", length=10)

system.time(for (i in 1:10)
{
  X <- subset(mydata, select=c(Outcome2,.imp,hCG0num,prog0num,hCGratio,Age,Vaginal.bleeding,Histep,Painscore))
  X$Outcome2 <-as.factor(X$Outcome2)
  X<-X[X$.imp==i,]
  X <- X[,-2]
  mods8[[i]] <- train(make.names(Outcome2)~.,
                      method = "svmRadial",# Radial basis function kernel
                      tuneLength = 10,
                      preProcess = c("center", "scale"),
                      metric = "LogLoss",
                      data=X,
                      trControl=ctrl) 
  pred <- predict(mods8[[i]], type="prob",newdata= X)
  answer <- append(answer, pred[,2])
  i=i+1
})
mydata['predictions'] <-answer
apparentm8auc <- AUC.imp(mydata$predictions,outcome = mydata$Outcome2, imp=mydata$.imp, data=mydata)
apparentm8auc$Performance['AUC']


# 9. Neural Network
mods9 <- vector(mode="list", length=10)
ctrl <- trainControl(method="cv", 
                     number=10,         
                     summaryFunction=LogLoss,
                     search="random",
                     classProbs=TRUE)

answer=c()
for (i in 1:10) {
  X <- subset(mydata, select=c(Outcome2,.imp,hCG0num,prog0num,hCGratio,Age,Vaginal.bleeding,Histep,Painscore))
  X$Outcome2 <-as.factor(X$Outcome2)
  X<-X[X$.imp==i,]
  X <- X[,-2]
  mods9[[i]] <- train(make.names(Outcome2)~.,
                      method = "nnet",
                      tuneLength = 10,
                      preProcess = c("center", "scale"),
                      metric = "LogLoss",
                      trace = FALSE,
                      maxit = 1000,
                      linout = FALSE,
                      data=X,
                      trControl=ctrl) 
  pred <- predict(mods9[[i]], type="prob",newdata= X)
  answer <- append(answer, pred[,2])
  i=i+1
}

mydata['predictions'] <-answer
apparentm9auc <- AUC.imp(mydata$predictions,outcome = mydata$Outcome2, imp=mydata$.imp, data=mydata)
apparentm9auc$Performance['AUC']



### Forest plot apparent model performance
NA.forest <- apparentm1auc$Performance[1,]
NA.forest <- NA
Summary.AUC <- rbind(NA.forest, apparentm1auc$Performance[1,], apparentm2auc$Performance[1,], apparentm3auc$Performance[1,], apparentm4auc$Performance[1,],apparentm5auc$Performance[1,], apparentm6auc$Performance[1,], apparentm7auc$Performance[1,], apparentm8auc$Performance[1,], apparentm9auc$Performance[1,])
Summary.AUC$Model <- c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
Summary.AUC.PI <- rbind(NA.forest, apparentm1auc$Performance[2,], apparentm2auc$Performance[2,], apparentm3auc$Performance[2,], apparentm4auc$Performance[2,], apparentm5auc$Performance[2,], apparentm6auc$Performance[2,], apparentm7auc$Performance[2,], apparentm8auc$Performance[2,], apparentm9auc$Performance[2,])
Summary.AUC.PI$Model <-  c('', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN")
tabletext <- cbind(
  c('Model', 'LR', 'LR with transformations', 'Ridge Regression', 'Firth LR', 'CART', "RF", "XGBoost", "SVM", "NN"),
  c('AUC (95% CI)', 
    paste(format(round(apparentm1auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentm1auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentm1auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentm2auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentm2auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentm2auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentm3auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentm3auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentm3auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentm4auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentm4auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentm4auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentm5auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentm5auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentm5auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentm6auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentm6auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentm6auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentm7auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentm7auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentm7auc$Performance$UL[1], 2), nsmall = 2), ")", sep = ""),
    paste(format(round(apparentm8auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentm8auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentm8auc$Performance$UL[1], 2), nsmall = 2), ")", sep= ""),
    paste(format(round(apparentm9auc$Performance$AUC[1], 2), nsmall = 2), " (", format(round(apparentm9auc$Performance$LL[1], 2), nsmall = 2), " to ", format(round(apparentm9auc$Performance$UL[1], 2), nsmall = 2), ")", sep="")),
  c('95% PI', 
    paste0("(", format(round(apparentm1auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentm1auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparentm2auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentm2auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparentm3auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentm3auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparentm4auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentm4auc$Performance$UL[2], 2), nsmall = 2), ")"), 
    paste0("(", format(round(apparentm5auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentm5auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparentm6auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentm6auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparentm7auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentm7auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparentm8auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentm8auc$Performance$UL[2], 2), nsmall = 2), ")"),
    paste0("(", format(round(apparentm9auc$Performance$LL[2], 2), nsmall = 2), " to ", format(round(apparentm9auc$Performance$UL[2], 2), nsmall = 2), ")")))

tiff("graphs/apparent_binary_AUC_all_models", width = 31, height = 13.75, units = "cm", res = 300)
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