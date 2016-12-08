##Machine Learning methods

##RANDOM FORESTS
RFfit <- randomForest(EstYOYAbu ~ maxP1winter + maxP1winter + Psummer + Pwinter + Pfall + Pspring,
                      data=A.FishNNPredsCC1, 
                      importance=TRUE, 
                      ntree=2000)
print(RFfit)
importance(RFfit)
plot(RFfit)
plot( importance(RFfit), lty=2, pch=16)
lines(importance(RFfit))
imp = importance(RFfit)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))
for (i in seq_along(impvar)) {
  partialPlot(RFfit, raw, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 1))
}


#machine learning examples
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

## randomForest
library(randomForest)
fit.rf = randomForest(frmla, data=raw)
print(fit.rf)
importance(fit.rf)
plot(fit.rf)
plot( importance(fit.rf), lty=2, pch=16)
lines(importance(fit.rf))
imp = importance(fit.rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))
for (i in seq_along(impvar)) {
  partialPlot(fit.rf, raw, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 1))
}

##################
# REEMtree
# This package is useful for longitudinal studies where random effects exist.  
# This example uses the pbkphData dataset available in the longRPart package.
## REEMtree Random Effects for Longitudinal Data
library(REEMtree)
pbkphData.sub = subset(pbkphData, !is.na(pbkphData$pbkph))
reem.tree = REEMtree(pbkph~Time, data=pbkphData.sub, random=~1|Subject)
plot(reem.tree)
ranef(reem.tree) #random effects

reem.tree = REEMtree(pbkph~Time, data=pbkphData.sub, random=~1|Subject,
                     correlation=corAR1())
plot(reem.tree)

install.packages("randomForest")

A.FishTest<-A.Fish[!is.na(A.Fish),]
library(randomForest)
train = sample(1:nrow(A.FishTest), nrow(A.FishTest)/2) #set training data

set.seed(1)
bag.fish1=randomForest(EstYOYAbu~.,data=A.FishTest,subset=train,mtry=13,importance=TRUE) #how to cluster sites??

#bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)