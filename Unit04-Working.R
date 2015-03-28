#Unit 4  - Trees

#Lecture Sequence 1 - JUDGE, JURY AND CLASSIFIER

## Video 4 CART IN R
### Or, putting the CART before the R
setwd("/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 04 Data Files")
getwd()
stevens = read.csv("stevens.csv")
str(stevens)
summary(stevens$Petitioner)
library(caTools)
set.seed(3000)		#for consistent results with Video
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)
message("Check record count:")
nrow(Train) + nrow(Test) - nrow(stevens) == 0
library(rpart)
#require(rpart.plot) || install.packages("rpart.plot")
library(rpart.plot)

#First, create classification tree:
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
 	data=Train, method="class", minbucket=25) 	# method="class" ==> classification tree (rather than regression tree)
prp(StevensTree)		#plots tree

#Next, test it against predictions:
PredictCART = predict(StevensTree, newdata=Test, type="class")	
table(Test$Reverse, PredictCART)
Accuracy = (41+71)/ sum(table(Test$Reverse, PredictCART))
Accuracy

library(ROCR)
PredictROC = predict(StevensTree, newdata=Test)	
PredictROC		#gives probability of each outcome (Reverse = 0 or1) for each record in Test

pred = prediction(PredictROC[,2], Test$Reverse)	#2nd col of PredictROC is predicted odds of Reverse = 1
perf = performance(pred, "tpr","fpr")
plot(perf)

#quick question
AUC = as.numeric(performance(pred, "auc")@y.values)
AUC
prp(StevensTree)		#plots tree, it has 7 splits

StevensTree_qq4 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
 	data=Train, method="class", minbucket=5)
prp(StevensTree_qq4)		#lower minbucket ==> more splits: 16
StevensTree_qq4b = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
 	data=Train, method="class", minbucket=100)
prp(StevensTree_qq4b)		#higher minbucket ==> fewer splits: 1

#Video 5 RANDOM FORESTS
#install.packages("randomForest")
library(randomForest)

StevenForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
	data = Train, nodesize=25, ntree = 200)
#	Warning message:
#	In randomForest.default(m, y, ...) :
#	  The response has five or fewer unique values.  Are you sure you want to do regression?
#
# We want to do classification, so we must first 
#  convert Reverse to a factor variable to make classification tree
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
StevenForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
	data = Train, nodesize=25, ntree = 200)
PredictForest = predict(StevenForest, newdata=Test)
table(Test$Reverse,PredictForest)
Accuracy = (38 + 73) / sum(table(Test$Reverse,PredictForest))
Accuracy

#Quick Question
set.seed(100)
StevenForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
	data = Train, nodesize=25, ntree = 200)
PredictForest = predict(StevenForest, newdata=Test)
table(Test$Reverse,PredictForest)
Accuracy = (38 + 73) / sum(table(Test$Reverse,PredictForest))
Accuracy
set.seed(200)
StevenForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
	data = Train, nodesize=25, ntree = 200)
PredictForest = predict(StevenForest, newdata=Test)
table(Test$Reverse,PredictForest)
Accuracy = (38 + 73) / sum(table(Test$Reverse,PredictForest))
Accuracy
#Independent of set.seed(n), randomForest's randomness is platform-dependent

##Video 6 CROSS-VALIDATION
#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)

numFolds = trainControl(method="cv", number=10)	#CROSS-VALIDATION, 10 numFolds
seq(0.01,0.5,0.01)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))	#range to test
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
	data=Train, method="rpart", 
	trControl=numFolds, 
	tuneGrid=cpGrid)

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
				data=Train, method="class", cp=0.19)
PredictCV = predict(StevensTreeCV, newdata=Test, type = "class")
table(Test$Reverse, PredictCV)
Accuracy = (59+64) / sum(table(Test$Reverse, PredictCV))
Accuracy	#~0.724, much better than original ~0.65

#Quick Question

prp(StevensTreeCV)	#only one split for Justice Stevens!! Liberal or Conservative decision by the lower court

str(stevens)
summary(stevens$LowerCourt)

#Lecture Sequence 2 - Keeping an Eye on Healthcare Costs: D2Hawkeye

setwd("/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 04 Data Files")
getwd()

## Video 6 

claims = read.csv("ClaimsData.csv")
str(claims)	# random sample of 1% of medicare beneficiaries still alive at end of 2008
			# using 2008 data, predicting 2009 cost bucket

table(claims$bucket2009)/nrow(claims)
library(caTools)
set.seed(88)
spl = sample.split(claims$bucket2009, 0.6)
claimsTrain = subset(claims, spl == TRUE)
claimsTest = subset(claims, spl == FALSE)
message("Row check")
ifelse( 0 == nrow(claimsTest) + nrow(claimsTrain) - nrow(claims), "OK", "Something went wrong.")
#qq
mean(claimsTrain$age)
str(claimsTrain)
mean(claimsTrain$diabetes)

## Video 7 BASELINE METHOD AND PENALTY MATRIX

