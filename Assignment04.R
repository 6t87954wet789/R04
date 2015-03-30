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

