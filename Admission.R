df  <- read.csv("C:/Users/JOGESH MISHRA/Desktop/R projects/ADMISSION/Admission_Predict_Ver1.1.csv",header = TRUE)
View(df)
df<-df[,-1]  # REMOVING THE Seriel.No.

## EDD ANALYSIS ##
summary(df)

## BARPLOT ##

barplot(table(df$Research)) ## CATEGORICAL VALUE

## SCATTER PLOT ##

plot(df$CGPA,df$Chance.of.Admit)
plot(df$GRE.Score,df$Chance.of.Admit)
plot(df$TOEFL.Score,df$Chance.of.Admit)

## DUMMY VARIABLE CREATION ##
library(dummies)
df<-dummy.data.frame(df)
View(df)

cor(df)               # CO-RELATION MATRIX
round(cor(df),2)

## TEST-TRAIN SPLIT ##

library(caTools)
split<-sample.split(df,SplitRatio = 0.8)

train_set=subset(df,split==TRUE)
test_Set=subset(df,split==FALSE)

## LINEAR REGRESSION ##

lm <-lm(Chance.of.Admit~.,data=train_set)
View(lm)

summary(lm)

pred_lm = predict(lm,test_Set)

mse_lm=mean((pred_lm-test_Set$Chance.of.Admit)**2)
mse_lm
adjr2_lm=0.8201
plot(lm)
  
## BEST SUBSET SEECTION METHOD ##

library(leaps)


lm_best <- regsubsets(Chance.of.Admit~.,data=train_set)
summary(lm_best)

summary(lm_best)$adjr2

which.max(summary(lm_best)$adjr2)
coefficients(lm_best,6)

lm_best_model<- lm(Chance.of.Admit~GRE.Score+TOEFL.Score+University.Rating+LOR+CGPA+Research,data=train_set)
summary(lm_best_model)

lm_best_pred=predict(lm_best_model,test_Set)

mse_best=mean((lm_best_pred-test_Set$Chance.of.Admit)**2)
mse_best
plot(lm_best_model)
adjr2_best = 0.8206

##forward ##
lm_forward <- regsubsets(Chance.of.Admit~.,data=train_set,method="forward")
summary(lm_forward)

summary(lm_forward)$adjr2

which.max(summary(lm_forward)$adjr2)
coefficients(lm_forward,6)

lm_forward_model<- lm(Chance.of.Admit~GRE.Score+TOEFL.Score+University.Rating+LOR+CGPA+Research,data=train_set,method="forward")
summary(lm_forward_model)

lm_forward_pred=predict(lm_forward_model,test_Set)

mse_forward=mean((lm_forward_pred-test_Set$Chance.of.Admit)**2)
mse_forward

plot(lm_forward_model)

##backward##
lm_back <- regsubsets(Chance.of.Admit~.,data=train_set,method="backward")
summary(lm_back)

summary(lm_back)$adjr2

which.max(summary(lm_back)$adjr2)
coefficients(lm_back,6)

lm_back_model<- lm(Chance.of.Admit~GRE.Score+TOEFL.Score+University.Rating+LOR+CGPA+Research,data=train_set,method="backward")
summary(lm_back_model)

lm_back_pred=predict(lm_back_model,test_Set)

mse_back=mean((lm_back_pred-test_Set$Chance.of.Admit)**2)
mse_back

plot(lm_back_model)

## RIDGE REGRESSION ##

library(glmnet)
x =model.matrix(Chance.of.Admit~.,data=train_set)[,-7]
y=train_set$Chance.of.Admit

grid= 10**seq(10,-2,length=100)

lm_ridge = glmnet(x,y,alpha=0,lambda=grid)
summary(lm_ridge)

cv_fit=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv_fit)

opt_lambda=cv_fit$lambda.min

y_pred =predict(lm_ridge,s=opt_lambda,newx=x)

plot(lm_ridge)

tss=mean((y-test_Set$Chance.of.Admit)**2)
rss=mean((y-y_pred)**2)
rsq= 1-(rss/tss)
rsq

##lasso##
library(glmnet)
x =model.matrix(Chance.of.Admit~.,data=train_set)[,-7]
y=train_set$Chance.of.Admit

grid= 10**seq(10,-2,length=100)

lm_lasso = glmnet(x,y,alpha=1,lambda=grid)
summary(lm_lasso)

cv_fit_lasso=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv_fit_lasso)

opt_lambda_lasso=cv_fit_lasso$lambda.min

y_lasso =predict(lm_lasso,s=opt_lambda,newx=x)

plot(lm_lasso)

tss_lasso=mean((y-test_Set$Chance.of.Admit)**2)
rss_lasso=mean((y-y_lasso)**2)
rsq_lasso= 1-(rss_lasso/tss_lasso)
rsq_lasso

############################# SUPPORT VECTOR MACHINES ###################################

##linear

library(e1071)
svm.fit<- svm(Chance.of.Admit~.,data=train_set,kernel="linear",cost=0.01,scale=TRUE)
summary(svm.fit)
test_pred_1=predict(svm.fit,test_Set)

mse_svm_1=mean((test_pred_1-test_Set$Chance.of.Admit)**2)
mse_svm_1

##polynomial

svm.fit_2<- svm(Chance.of.Admit~.,data=train_set,kernel="polynomial",cost=0.01,scale=TRUE)
summary(svm.fit_2)
test_pred_2=predict(svm.fit_2,test_Set)

mse_svm_2=mean((test_pred_2-test_Set$Chance.of.Admit)**2)
mse_svm_2

## radial

svm.fit_3<- svm(Chance.of.Admit~.,data=train_set,kernel="radial",cost=0.01,scale=TRUE)
summary(svm.fit_3)
test_pred_3=predict(svm.fit_3,test_Set)

mse_svm_3=mean((test_pred_3-test_Set$Chance.of.Admit)**2)
mse_svm_3

################################ DESICION TREE #####################################
library(rpart)
library(rpart.plot)

regtree = rpart(Chance.of.Admit~.,data=train_set,control=rpart.control(maxdepth = 10))
rpart.plot(regtree,box.palette = "RdBu")

tree_pred=predict(regtree,test_Set,type="vector")

mse2=mean((tree_pred-test_Set$Chance.of.Admit)**2)
mse2

### PRUNNED TREE 

fulltree= rpart(Chance.of.Admit~.,data=train_set,control = rpart.control(cp=0))
rpart.plot(fulltree)

printcp(fulltree)

min_cp<- regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]

prun_tree <- prune(fulltree,min_cp)
rpart.plot(prun_tree)

prun_tree_pred=predict(prun_tree,test_Set)
mse2_prun = mean((prun_tree_pred-test_Set$Chance.of.Admit)**2)
mse2_prun

#################################### FINAL OBSERVATIONS  ##########################################

mse_lm      # 0.003447107
mse_best    # 0.003190961
mse_forward # 0.003144861
mse_back    # 0.003162873

rss         # 0.8753968
rss_lasso   # 0.8715021

mse_svm_1   # 0.003702995
mse_svm_2   # 0.008232018
mse_svm_3   # 0.009493333

mse2        # 0.004565448
mse2_prun   # 0.004489796
