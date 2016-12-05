##Cross Validation
###FOR Impact of Extreme Streamflows on Brook Trout Young-of-Year Abundance
### Annalise G Blum
##Created August 19, 2016; Last modified: Nov 27, 2016
##Data sets created in this file: 

#load("output/UVAA.FishNNPreds.rdata") #with UVA sites used in NN
load("output/A.FishNNPreds.rdata") #just USGS sites used in NN

##first for frequentist model: Need file with all flow predictions for fish sites
summary(A.FishNNPreds); names(A.FishNNPreds)
A.FishNNPredsCC<-A.FishNNPreds[complete.cases(A.FishNNPreds),] #remove observations with ANY missing values

##scale all the variables: now have data set AbuPFlows

#quick residuals plots
All_mod5<- lmer(log(EstYOYAbu)~(1|site_no)+log(p95summer)+log(p5fall)+log(p95winter)+log(p95spring)
                +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
                ,data=AbuPFlows)


#### Randomly drop 10 % of data CV #### 
#only have nrow(AbuPFlows) = 411
#Abu_predsGLM<-rep(NA, length=nrow(AbuPFlows))


#how many sites to drop?
n.drop<-round(nrow(AbuPFlows)*.1)

#find RMSE without dropping any data - 
#4 run model
All_mod5<- lmer(log(EstYOYAbu)~(1|site_no)+log(p95summer)+log(p5fall)+log(p95winter)+log(p95spring)
                +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
                ,data=AbuPFlows)
  
  #Seasonal weather
  # lmer(log(EstYOYAbu)~(1|site_no)+log(Psummer)+log(Pfall)+log(Pwinter)+log(Pspring) 
  #      +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
  #Duration
  # lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFfall+.05)
  #              +log(MaxTfall)+log(MaxTspring)
  #              +log(maxP1fall)+log(maxP1winter)
              

#summary(All_mod5)
#5 - Predict EstYOYAbu
Abu_predsALL<-exp(predict(All_mod5,AbuPFlows)) #for whole model, no omitted data
AbuPFlows$Abu_predsALL<-Abu_predsALL

#4 find RMSE
RMSEall<-(mean((AbuPFlows$EstYOYAbu- AbuPFlows$Abu_predsALL)^2))^.5
RMSEall 

#Seasonal weather: 99.15
#avg flows: 96.46053
#magnitude: 96.85063
#Duration: 95.430- good, this matches even when shuffled!

#1 - Randomly shuffle the data
RanAbuPFlows<-AbuPFlows[sample(nrow(AbuPFlows)),]

#2 - Create variable for omit the first 10% of randomly shuffled data
RanAbuPFlows$omit<-0
RanAbuPFlows$omit[1:n.drop]<-1

#3 - Build test and training data sets
testData <- RanAbuPFlows[RanAbuPFlows$omit==1, ] #116 randomly-selected sites
trainData <- RanAbuPFlows[RanAbuPFlows$omit==0, ] #the rest of the data

#4 run model
Train_mod5<- lmer(log(EstYOYAbu)~(1|site_no)+log(p95summer)+log(p5fall)+log(p95winter)+log(p95spring)
                  +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
                  ,data=trainData)

# Avg flows
# lmer(log(EstYOYAbu)~(1|site_no)+log(AvgQsummer)+log(AvgQfall)+log(AvgQwinter)+log(AvgQspring) 
#           +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
#Seasonal weather
# lmer(log(EstYOYAbu)~(1|site_no)+log(Psummer)+log(Pfall)+log(Pwinter)+log(Pspring) 
#      +log(MaxTsummer) +log(MaxTfall)+log(MaxTwinter)+log(MaxTspring)
#Duration
# lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFfall+.05)
#              +log(MaxTfall)+log(MaxTspring)
#              +log(maxP1fall)+log(maxP1winter)

#summary(Train_mod5)
#5 - Predict annual 7day min flows based on the 3 models and save

testData$Abu_predsT1<-exp(predict(Train_mod5,testData ,allow.new.levels = T)) #testData
trainData$Abu_predsT1<-exp(predict(Train_mod5,trainData)) #trainData

TT1_comb<-rbind(testData,trainData)

#4 find RMSE
RMSE<-(mean((TT1_comb$EstYOYAbu- TT1_comb$Abu_predsT1)^2))^.5
RMSE 

#seasonal weather: 99.24443; 98.89725, 98.58086, 100.0126
#Avg flows: 96.59602, 97.32278, 97.27307, 97.27307,  96.04661
#magnitude: 97.44834, 97.55723, 97.20289, 97.31532
#Duration: 95.09962; then 95.52; 95.75; 96.37908


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

#### OLD #####
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
#### 1 - start with LOO-CV ####
AbuPFlows<-A.FishNNPredsCC
Abu_preds<-rep(NA, length=nrow(AbuPFlows))

for (i in 1:nrow(AbuPFlows)){
  #1 Subset data sets
  omitted_data<-AbuPFlows[i,] #pull that one row/observation out
  data_CV <- AbuPFlows[-i,] #obtain data set omitting one observation
  
  #2 Estimate model - See code/Regressions.R
  
  # CV_mod5<-lmer(RYOYAbu ~(1|site_no)+fallPmin7day+ winterTempStd+springTempStd
  #              ,data=data_CV,REML=T)
  
  # CV_mod5<-glm(RYOYAbu ~fallPmin7day+ winterTempStd+springTempStd
  #               ,data=data_CV,family=poisson) #doesn't work with poisson for some reason...
  
  CV_mod5<- lmer(log(EstYOYAbu)~(1|site_no)+log(DurLFfall+.05)+log(DurHFwinter+.05)+log(DurHFfall+.05) 
                 +log(MaxTfall)+log(MaxTspring)
                 +log(maxP1fall)+log(maxP1winter)
                 ,data=data_CV)
  # CV_mod5<- lm(RYOYAbu ~fallPmin7day+ winterTempStd+springTempStd
  #                 ,data=data_CV)
  
  #3 - Predict annual 7day min flows based on the 3 models and save
  Abu_preds[i]<-exp(predict(CV_mod5,omitted_data,allow.new.levels = T))
}

#add
AbuPFlows$Abu_preds<-Abu_preds

#plot obs vs predicted
plot(AbuPFlows$EstYOYAbu,AbuPFlows$Abu_preds)
abline(0,1)
NSE(AbuPFlows$Abu_preds,AbuPFlows$EstYOYAbu) #NSE=0.22
sum(AbuPFlows$Abu_preds>AbuPFlows$EstYOYAbu)/length(AbuPFlows$Abu_preds) #bias? Nope 0.48

#plot residuals: resids<-obs-predicted
AbuPFlows$residuals<-AbuPFlows$EstYOYAbu-AbuPFlows$Abu_preds
plot(AbuPFlows$Abu_preds,AbuPFlows$residuals)
abline(0,0)

#log space - looks good
AbuPFlows$Log_residuals<-log(AbuPFlows$EstYOYAbu)-log(AbuPFlows$Abu_preds)
plot(log(AbuPFlows$Abu_preds),AbuPFlows$Log_residuals)
abline(0,0)

#how many are >0 ?
sum(AbuPFlows$residuals>0)/length(AbuPFlows$residuals)
sum(AbuPFlows$residuals<0)/length(AbuPFlows$residuals)

#### 2 - k-fold CV - THIS IS STILL OLD #### 
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