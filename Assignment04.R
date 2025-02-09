### Assignment 4 : Trees

setwd("c:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 04 Data Files")
getwd()

## Part 1: UNDERSTANDING WHY PEOPLE VOTE
gerber = read.csv("gerber.csv")
str(gerber)

#Problem 1 -  EXPLORATION AND LOGISTIC REGRESSION

mean(gerber$voting)

gCivic = subset(gerber, gerber$civicduty == 1)
mean(gCivic$voting)
gHawth = subset(gerber, gerber$hawthorne == 1)
mean(gHawth$voting)
gSelf = subset(gerber, gerber$self == 1)
mean(gSelf$voting)
gNeigh = subset(gerber, gerber$neighbors == 1)
mean(gNeigh$voting)

gLog = glm(voting ~ civicduty + hawthorne + self  + neighbors, data=gerber, family="binomial")
summary(gLog)	#All significant (1.3)

predictTest = predict(gLog, type="response")
t = table(gerber$voting, predictTest > 0.3)
(t[1,1]+t[2,2])/sum(t)		#accuracy

t = table(gerber$voting, predictTest > 0.5)
t[1,1]/sum(t)		#accuracy


library(ROCR)
ROCRpred = prediction(predictTest, gerber$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc	#0.5308461

#Problem 2.1 - Trees
library(rpart)
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#2.4
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)	#In civic duty group, men (sex < 0.5 more likely to vote)

gControl = subset(gerber, gerber$control == 1)
mean(gControl$voting)
mean(subset(gControl, gControl$sex == 1)$voting) #women
mean(subset(gControl, gControl$sex == 0)$voting) #men

#Problem 3.1 - INTERACTION TERMS

# Control and Sex only

#gContLog = glm(voting ~ control, data=gerber, family="binomial")
CARTCont = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTCont, digits=6)
.34-.296638		# 0.043362 the absolute value of the difference in the predicted probability of voting between being in the control group versus being in a different group

CARTContSex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTContSex, digits = 6)

ControlMen = 0.302795
NotControlMen = 0.345818
dMen = abs(ControlMen - NotControlMen)
dMen
ControlWomen = .290456
NotControlWomen = .334176
dWomen = abs(ControlWomen - NotControlWomen)
dWomen
dMen-dWomen	#they are affected about the same

#3.4

LogModelSex = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(LogModelSex)
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
p = predict(LogModelSex, newdata=Possibilities, type="response")

# Interested in (Woman, Control) case ==>  sex == 1, control == 1
#  4th case as rendered, so: 0.2908065
# In our tree, we had ControlWomen == 0.290456
abs(0.290456 - 0.2908065)
as.numeric(abs(ControlWomen - p[4]))	# more generally
	# 0.0003504525 ==> 0.00035

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

p2 = predict(LogModel2, newdata=Possibilities, type="response")
as.numeric(abs(ControlWomen - p2[4])) # 2.207523e-07 == > 0.00000

## Part 2: LETTER RECOGNITION

setwd("c:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 04 Data Files")
getwd()

letters = read.csv("letters_ABPR.csv")

library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

letters$isB = as.factor(letters$letter == "B")
set.seed(1000)

spl = sample.split(letters$isB, SplitRatio = 0.5)
Train = subset(letters, spl == TRUE) #Remember that TRUE values from sample.split should go in the training set.
Test = subset(letters, spl == FALSE)

#Baseline model: Assume NOT-B. Computer accuracy of this baseline on Test set
predBase = rep(FALSE, nrow(Test))
summary(predBase)	#OK

p = table(Test$isB, predBase); p
p[1,1]/sum(p)

CARTb = rpart(isB ~ . - letter, data=Train, method="class")
prp(CARTb)

predCARTb = predict(CARTb, newdata=Test, type="class")
p = table(Test$isB, predCARTb); p
(p[1,1]+p[2,2])/sum(p)

set.seed(1000)
bForest = randomForest(isB ~ . - letter, data = Train)
predForest = predict(bForest, newdata=Test, type="class")
p = table(Test$isB, predForest); p
(p[1,1]+p[2,2])/sum(p)

#2.1

letters$letter = as.factor(letters$letter)
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
Train = subset(letters, spl == TRUE) #Remember that TRUE values from sample.split should go in the training set.
Test = subset(letters, spl == FALSE)

summary(letters$letter)	#Shows P is the most common outcome
predBase = rep("P", nrow(Test))
summary(predBase)	#OK
p = table(Test$letter, predBase); p
p['P','P']/sum(p)	#baseline accuracy 0.2573813

CARTletter = rpart(letter ~ . - isB, data=Train, method="class")
prp(CARTletter)
predCARTletter = predict(CARTletter, newdata=Test, type="class")
p = table(Test$letter, predCARTletter); p
sum(p['A','A'],p['B','B'],p['P','P'],p['R','R'])/sum(p)

set.seed(1000)
letterForest = randomForest(letter ~ . - isB, data = Train)
predForest = predict(letterForest, newdata=Test, type="class")
p = table(Test$letter, predForest); p
sum(p['A','A'],p['B','B'],p['P','P'],p['R','R'])/sum(p)


## Part 3: PREDICTING EARNINGS FROM CENSUS DATA

library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

setwd("c:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 04 Data Files")
getwd()

census = read.csv('census.csv')
str(census)
summary(census)

set.seed(2000)

spl = sample.split(census$over50k, SplitRatio = 0.6)
Train = subset(census, spl == TRUE)	# should be 60%
Test = subset(census, spl == FALSE)
nrow(Train)/nrow(census) #should be 60%
nrow(Test)/nrow(census) #should be 40%

Log50k = glm(over50k ~ ., data=Train, family="binomial")
# "You might see a warning message here - you can ignore it and proceed."
summary(Log50k)

predictTest = predict(Log50k, newdata=Test, type="response")
t = table(Test$over50k, predictTest > 0.5)	; t
(t[1,1]+t[2,2])/sum(t)		#accuracy 0.8552107

summary(Train$over50k)		#Shows FALSE is the most common outcome
#Baseline is just predict NOT-over50k
predBase = rep(FALSE, nrow(Test))
summary(predBase)	#OK
p = table(Test$over50k, predBase); p
p[1,1]/sum(p)	#baseline accuracy 0.7593621

library(ROCR)
ROCRpred = prediction(predictTest, Test$over50k)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
accuracy # 0.9061598

#2.1
CART50k = rpart(over50k ~ ., data=Train, method="class")
prp(CART50k)

predCART50k = predict(CART50k, newdata=Test, type="class")
t = table(Test$over50k, predCART50k); p
(t[1,1]+t[2,2])/sum(t)		#accuracy  0.8473927

#ROC for CART model:
PredictROC = predict(CART50k, newdata=Test)	
PredictROC		#gives probability of each outcome for each record in Test
pred = prediction(PredictROC[,2], Test$over50k)	#2nd col of PredictROC is predicted odds of over50k
perf = performance(pred, "tpr","fpr")
auc = as.numeric(performance(pred, "auc")@y.values)
auc 	# 0.8470256
plot(perf)


#ROC for logistic model:
predictTest = predict(Log50k, newdata=Test, type="response")
predLog = prediction(predictTest, Test$over50k)
perfLog = performance(predLog, "tpr","fpr")
aucLog = as.numeric(performance(predLog, "auc")@y.values)
aucLog 	# 0.9061598		higher auc than CART model
plot(perfLog)	#much smoother than for CART model

#3.1

set.seed(1)
trainSmall = Train[sample(nrow(Train), 2000), ]
set.seed(1)
forest50k = randomForest(over50k ~ . , data = trainSmall)
predForest = predict(forest50k, newdata=Test, type="class")
p = table(Test$over50k, predForest); p
(p[1,1]+p[2,2])/sum(p)

predForest = predict(forest50k, newdata=Test)
p = table(Test$over50k, predForest); p
(p[1,1]+p[2,2])/sum(p)

vu = varUsed(forest50k, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest50k$forest$xlevels[vusorted$ix]))

varImpPlot(forest50k)

#4.1
library(caret)
library(e1071)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
numFolds = trainControl(method="cv", number=10)	#CROSS-VALIDATION, 10 numFolds
set.seed(2)
train(over50k ~ ., data=Train, method="rpart",
	trControl = numFolds, 
	tuneGrid=cartGrid)
newCP = 0.002 #recommended by the train() fn
CART50kNew = rpart(over50k ~ ., data=Train, method="class", cp=newCP)
prp(CART50kNew)

predCART50kNew = predict(CART50kNew, newdata=Test, type="class")
t = table(Test$over50k, predCART50kNew); p
(t[1,1]+t[2,2])/sum(t)		#accuracy  


summary(CART50kNew)	#output is MASSIVE
s = summary(CART50kNew)
s['cptable']
as.data.frame(s['cptable'])
as.data.frame(s['cptable'])$cptable.nsplit
max(as.data.frame(s['cptable'])$cptable.nsplit)
numberofsplits = max(as.data.frame(s['cptable'])$cptable.nsplit)	#Booooooooooom


#Output of summary is:
# Call:
# rpart(formula = over50k ~ ., data = Train, method = "class", 
#     cp = newCP)
#   n= 19187 

#             CP nsplit rel error    xerror       xstd
# 1  0.121832359      0 1.0000000 1.0000000 0.01282467
# 2  0.065627031      2 0.7563353 0.7680312 0.01164497
# 3  0.037470219      3 0.6907083 0.7158328 0.01132857
# 4  0.007580680      4 0.6532380 0.6545376 0.01092878
# 5  0.005956249      8 0.6229153 0.6413255 0.01083831
# 6  0.004331817     10 0.6110028 0.6255144 0.01072792
# 7  0.004223522     11 0.6066710 0.6164176 0.01066334
# 8  0.003465454     13 0.5982240 0.6086203 0.01060735
# 9  0.003248863     16 0.5869612 0.6053715 0.01058384
# 10 0.002165909     17 0.5837124 0.6012562 0.01055392
# 11 0.002000000     18 0.5815465 0.6001733 0.01054602
#
#
# [... truncated, there was lots more]
#but nsplit tells the story without having to resort to that. See above for how to get straight to answer

