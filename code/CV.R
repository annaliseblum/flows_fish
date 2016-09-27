##Cross Validation
##August 19, 2016

##first for frequentist model: A.FWC_RR

save(A.FWC_RR,file="output/A.FWC_RR.rdata")
load("output/A.FWC_RR.rdata")

##scale all the variables: now have data set AbuPFlows

#### 1 - start with LOO-CV ####
Abu_preds<-rep(NA, length=nrow(AbuPFlows))

for (i in 1:nrow(AbuPFlows)){
  #1 Subset data sets
  omitted_data<-AbuPFlows[i,] #pull that one row/observation out
  data_CV <- AbuPFlows[-i,] #obtain data set omitting one observation
  
  #2 Estimate model

  # CV_mod5<-lmer(RYOYAbu ~(1|site_no)+fallPmin7day+ winterTempStd+springTempStd
  #              ,data=data_CV,REML=T)
  
  # CV_mod5<-glm(RYOYAbu ~fallPmin7day+ winterTempStd+springTempStd
  #               ,data=data_CV,family=poisson) #doesn't work with poisson for some reason...
  
  CV_mod5<- lm(EstYOYAbu ~fallPmin7day+ winterTempStd+springTempStd
                  ,data=data_CV) #try with non-rounded
  
  # CV_mod5<- lm(RYOYAbu ~fallPmin7day+ winterTempStd+springTempStd
  #                 ,data=data_CV)
  
  #3 - Predict annual 7day min flows based on the 3 models and save
  Abu_preds[i]<-predict(CV_mod5,omitted_data,allow.new.levels = T)
}

#add
AbuPFlows$Abu_preds<-Abu_preds

#plot obs vs predicted
plot(AbuPFlows$RYOYAbu,AbuPFlows$Abu_preds)
abline(0,1)

#plot residuals: resids<-obs-predicted
AbuPFlows$residuals<-AbuPFlows$RYOYAbu-AbuPFlows$Abu_preds
plot(AbuPFlows$Abu_preds,AbuPFlows$residuals)
abline(0,0)

#how many are >0 ?
sum(AbuPFlows$residuals>0)/length(AbuPFlows$residuals)
sum(AbuPFlows$residuals<0)/length(AbuPFlows$residuals)

#### 2 - k-fold CV #### 
#only have nrow(AbuPFlows) = 411
#Abu_predsGLM<-rep(NA, length=nrow(AbuPFlows))

AbuPFlows400<-AbuPFlows[1:400,] #practice with an even number

#Randomly shuffle the data
AbuPFlows400<-AbuPFlows400[sample(nrow(AbuPFlows400)),]

#Create 10 equally size folds
n.breaks<-40
folds <- cut(seq(1,nrow(AbuPFlows400)),breaks=n.breaks,labels=FALSE)
Abu_preds <- matrix(NA, nrow = nrow(AbuPFlows400)/n.breaks, ncol = n.breaks)

#Perform 10 fold cross validation
for(k in 1:n.breaks){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==k,arr.ind=TRUE)
  testData <- AbuPFlows400[testIndexes, ]
  trainData <- AbuPFlows400[-testIndexes, ]
  
  #Use the test and train data partitions however you desire...
  #2 Estimate model
  Train_mod5<- lm(EstYOYAbu ~fallPmin7day+ winterTempStd+springTempStd
               ,data=trainData) #try with non-rounded
  
  #3 - Predict annual 7day min flows based on the 3 models and save
  Abu_preds[,k]<-predict(Train_mod5,testData,allow.new.levels = T)
}

#reshape preds to match dataset
Abu_predsVector<-melt(Abu_preds)
AbuPFlows400$Kfold40Preds<-Abu_predsVector$value

#plot obs vs predicted
plot(AbuPFlows400$RYOYAbu,AbuPFlows400$Kfold40Pred)
abline(0,1)

#plot residuals: resids<-obs-predicted
AbuPFlows400$residuals<-AbuPFlows400$RYOYAbu-AbuPFlows400$Kfold40Pred
plot(AbuPFlows400$Kfold40Pred,AbuPFlows400$residuals)
abline(0,0)

#how many are >0 ?
sum(AbuPFlows400$residuals>0)/length(AbuPFlows400$residuals)
sum(AbuPFlows400$residuals<0)/length(AbuPFlows400$residuals)

#### 3 - randomly drop 10 % of data CV #### 
#only have nrow(AbuPFlows) = 411
#Abu_predsGLM<-rep(NA, length=nrow(AbuPFlows))

#figure out why RanAbuPFlows has lower RSME than AbuPFlows #####START HERE

#how many sites to drop?
n.drop<-round(nrow(AbuPFlows)*.1)

#find RMSE without dropping any data
#4 run model
All_mod5<- lm(EstYOYAbu ~fallPmin7day+ winterTempStd+springTempStd
                ,data=RanAbuPFlows) #try with non-rounded

#summary(All_mod5)
#5 - Predict annual 7day min flows based on the 3 models and save
Abu_predsALL<-predict(All_mod5,RanAbuPFlows ,allow.new.levels = T) #testData
RanAbuPFlows$Abu_predsALL<-Abu_predsALL

#4 find RMSE
RMSEall<-(mean((RanAbuPFlows$EstYOYAbu- RanAbuPFlows$Abu_predsALL)^2))^.5
RMSEall

#1 - Randomly shuffle the data
RanAbuPFlows<-AbuPFlows[sample(nrow(AbuPFlows)),]

#2 - Create variable for omit the first 10% of randomly shuffled data
RanAbuPFlows$omit<-0
RanAbuPFlows$omit[1:n.drop]<-1

#3 - Build test and training data sets
testData <- RanAbuPFlows[RanAbuPFlows$omit==1, ] #41 randomly-selected sites
trainData <- RanAbuPFlows[RanAbuPFlows$omit==0, ] #the rest of the data

#4 run model
Train_mod5<- lm(EstYOYAbu ~fallPmin7day+ winterTempStd+springTempStd
                ,data=trainData) #try with non-rounded

summary(Train_mod5)
#5 - Predict annual 7day min flows based on the 3 models and save
Abu_preds<-predict(Train_mod5,RanAbuPFlows ,allow.new.levels = T) #testData
RanAbuPFlows$Abu_preds2<-Abu_preds

#4 find RMSE
RMSE<-(mean((RanAbuPFlows$EstYOYAbu- RanAbuPFlows$Abu_preds2)^2))^.5
RMSE

###from other section
#reshape preds to match dataset
Abu_predsVector<-melt(Abu_preds)
AbuPFlows400$Kfold40Preds<-Abu_predsVector$value

#plot obs vs predicted
plot(AbuPFlows400$RYOYAbu,AbuPFlows400$Kfold40Pred)
abline(0,1)

#plot residuals: resids<-obs-predicted
AbuPFlows400$residuals<-AbuPFlows400$RYOYAbu-AbuPFlows400$Kfold40Pred
plot(AbuPFlows400$Kfold40Pred,AbuPFlows400$residuals)
abline(0,0)

#how many are >0 ?
sum(AbuPFlows400$residuals>0)/length(AbuPFlows400$residuals)
sum(AbuPFlows400$residuals<0)/length(AbuPFlows400$residuals)

##### CV EXAMPLES: #####
#http://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation

#Randomly shuffle the data
yourData<-yourData[sample(nrow(yourData)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- yourData[testIndexes, ]
  trainData <- yourData[-testIndexes, ]
  #Use the test and train data partitions however you desire...
}

#https://www.r-bloggers.com/cross-validation-for-predictive-analytics-using-r/
set.seed(seed)

n_train <- 100
xy <- gen_data(n_train, beta, sigma_eps)
x <- xy$x
y <- xy$y

fitted_models <- apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
mse <- sapply(fitted_models, function(obj) deviance(obj)/nobs(obj))

n_test <- 10000
xy_test <- gen_data(n_test, beta, sigma_eps)
pred <- mapply(function(obj, degf) predict(obj, data.frame(x = xy_test$x)), 
               fitted_models, df)
te <- sapply(as.list(data.frame(pred)), function(y_hat) mean((xy_test$y - y_hat)^2))

n_folds <- 10
folds_i <- sample(rep(1:n_folds, length.out = n_train))
cv_tmp <- matrix(NA, nrow = n_folds, ncol = length(df))
for (k in 1:n_folds) {
  test_i <- which(folds_i == k)
  train_xy <- xy[-test_i, ]
  test_xy <- xy[test_i, ]
  x <- train_xy$x
  y <- train_xy$y
  fitted_models <- apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
  x <- test_xy$x
  y <- test_xy$y
  pred <- mapply(function(obj, degf) predict(obj, data.frame(ns(x, df = degf))), 
                 fitted_models, df)
  cv_tmp[k, ] <- sapply(as.list(data.frame(pred)), function(y_hat) mean((y - 
                                                                           y_hat)^2))
}