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

baselineModel = table(claimsTest$bucket2009, claimsTest$bucket2008)		#just make same prediction as prior year
correct = 0
for(x in seq(1,5)){
	correct = correct + baselineModel[x,x]
}
Accuracy = correct/sum(baselineModel)
Accuracy 	#0.68
# Simple baseline model accuracy is 0.6838135
#But we want to add a penalty matrix to this, 
#  since it is worse to predict a low cost bucket but patient is actually high-cost

penaltyMatrix= matrix(c(0,1,2,3,4,
						2,0,1,2,3,
						4,2,0,1,2,
						6,4,2,0,1,
						8,6,4,2,0), byrow=TRUE, nrow=5)
penaltyMatrix

baselineModel = as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008))*penaltyMatrix			#multiply by penalty matrix

penaltyError = sum(baselineModel)/nrow(claimsTest)
penaltyError #0.74

#So our baseline model had an accuracy of 0.68 and penalty error of 0.74
#We want to create a CART model with a higher accuracy and a lower penalty error

#qq

# Hypothetical baseline - assume bucket 1 for everybody.
qAccuracy = length(subset(claimsTest$bucket2009, claimsTest$bucket2009 == 1)) /
			length(claimsTest$bucket2009)
qAccuracy	#0.67127

qModel1 = rep(1, length(claimsTest$bucket2009))	#rep(a,b) makes a vector of b instances of value a
length(qModel1)
summary(qModel1)

qModelTable = table(claimsTest$bucket2009, qModel1)
qModelTable
#another way to compute accuracy 
qModelTable[1,1]/sum(qModelTable)	#0.67127

penaltyErrorQModel = sum(as.matrix(qModelTable)*penaltyMatrix[,1])/nrow(claimsTest)
penaltyErrorQModel	# 1.044301

##Video 8 PREDICTING HEALTHCARE COSTS IN R


library(rpart)
library(rpart.plot)

claimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + 
					depression + diabetes + heart.failure + ihd + kidney + osteoporosis
					+stroke + bucket2008 + reimbursement2008,
					data=claimsTrain, method="class", cp = 0.00005)
prp(claimsTree)		#Tree is huge! Why? 5-class rather than binary, lots of variables

predictTest = predict(claimsTree, newdata=claimsTest, type="class")
treeModel = table(claimsTest$bucket2009, predictTest)
correct = 0
for(x in seq(1,5)){
	correct = correct + treeModel[x,x]
}
Accuracy = correct/sum(treeModel)
Accuracy 	#0.7126669

treeModel = as.matrix(treeModel)*penaltyMatrix
treeModel
sum(treeModel)/nrow(claimsTest)	#0.7578902 -- Penalty error

#Accuracy went up, but so did penalty error.
# This is because high-bucket individuals are few
# and our model does not take into account the different penalties

#So let's include the different penalties with the 'loss' parameter

claimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + 
					depression + diabetes + heart.failure + ihd + kidney + osteoporosis
					+stroke + bucket2008 + reimbursement2008,
					data=claimsTrain, method="class", cp = 0.00005,
					parms=list(loss=penaltyMatrix))

#With rpart taking into account the weight of different error types, we expect
#  the accuracy to decrease but the penalty error to decrease also.

prp(claimsTree)	
predictTest = predict(claimsTree, newdata=claimsTest, type="class")
treeModel = table(claimsTest$bucket2009, predictTest)
correct = 0
for(x in seq(1,5)){
	correct = correct + treeModel[x,x]
}
Accuracy = correct/sum(treeModel)
Accuracy 	#0.6472746

treeModel = as.matrix(treeModel)*penaltyMatrix
treeModel
sum(treeModel)/nrow(claimsTest)	#0.6418161 <-- Penalty error


#qq
p1 = table(claimsTest$bucket2009, predictTest)
p1[1,1]/sum(p1)

#End of Lecture
