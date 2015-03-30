### Assignment 4 : Trees

setwd("c:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 04 Data Files")
getwd()

## Part 1: UNDERSTANDING WHY PEOPLE VOTE
gerber = read.csv("gerber.csv")
str(gerber)

mean(gerber$voting)

gCivic = subset(gerber, gerber$civicduty == 1)
mean(gCivic$voting)
gHawth = subset(gerber, gerber$hawthorne == 1)
mean(gHawth$voting)
gSelf = subset(gerber, gerber$self == 1)
mean(gSelf$voting)
gNeigh = subset(gerber, gerber$neighbors == 1)
mean(gNeigh$voting)

