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
require(rpart.plot) || install.packages("rpart.plot")
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

