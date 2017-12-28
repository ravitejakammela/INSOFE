library(lubridate)
library(eeptools)
library(DMwR)
library(vegan)
library(stats)
library(xgboost)
library(caret)
library(randomForest)
#READING THE TRAIN AND TEST DATA---------------------------------------------------------------
train<-read.csv("train_mith.csv",sep = ",",header=T)
test<-read.csv("test_mith.csv",sep=",",header=T)

#CHECHING THE NAMES OF FEATURES PRESENT IN TRAIN AND TEST-----------------------------------------------------------------
colnames(train)
colnames(test)

#FOUND TARGET FEATURE IS "Pay_in_INR", EXTRACTING THAT FROM TRAIN DATA AND REMOVING IT FROM TRAIN DATA-------------------------------------------------
Pay_in_INR<-train$Pay_in_INR
train$Pay_in_INR<-NULL

#BINDING TRAIN AND TEST DATA FOR EASY PRE-PROCESSING------------
combi<-rbind(train,test)

#CHECKINHG FOR ANY MISSING DATA----------------
apply(combi,2,function(x) {sum(is.na(x))})
#GIVEN THAT MISSING DATA IS MARKED AS 0'S AND -1'S IN SOME ATTRIBUTES---------
#------------------------------------------------------------------------------
#DEALING WITH MISSING DATA-------------
combi$Date.Of.Birth<-as.character(combi$Date.Of.Birth)
combi$Date.Of.Birth<-ifelse(combi$Date.Of.Birth=="NA 00:00",NA,combi$Date.Of.Birth)
combi$Date.Of.Birth<-as.factor(combi$Date.Of.Birth)
sum(is.na(combi$Date.Of.Birth))
combi$Date.Of.Birth<-format(as.Date(combi$Date.Of.Birth),"%Y")
combi$Date.Of.Birth<-as.numeric(combi$Date.Of.Birth)
combi$age<-2017-combi$Date.Of.Birth
combi$Date.Of.Birth<-NULL
class(combi$age)

#----------------------------------------------------------
combi$School.Board.in.Tenth
str(combi$School.Board.in.Tenth)
sum(is.na(combi$School.Board.in.Tenth))
table(combi$School.Board.in.Tenth)
unique(combi$School.Board.in.Tenth)
combi$School.Board.in.Tenth<-as.character(combi$School.Board.in.Tenth)
combi$School.Board.in.Tenth<-ifelse(combi$School.Board.in.Tenth=="0",NA,combi$School.Board.in.Tenth)
sum(is.na(combi$School.Board.in.Tenth))
combi$School.Board.in.Tenth<-as.factor(combi$School.Board.in.Tenth)
combi$School.Board.in.Tenth<-as.factor(as.character(combi$School.Board.in.Tenth))
#----------------------------------------------------------------------------------------------------------------------------------------

combi$Board.in.Twelth
str(combi$Board.in.Twelth)
sum(is.na(combi$Board.in.Twelth))
table(combi$Board.in.Twelth)
unique(combi$Board.in.Twelth)
combi$Board.in.Twelth<-as.character(combi$Board.in.Twelth)
combi$Board.in.Twelth<-ifelse(combi$Board.in.Twelth=="0",NA,combi$Board.in.Twelth)
sum(is.na(combi$Board.in.Twelth))
combi$Board.in.Twelth<-as.factor(combi$Board.in.Twelth)
combi$Board.in.Twelth<-as.factor(as.character(combi$Board.in.Twelth))

#----------------------------------------------------------------------------
str(combi$Score.in.Domain)
combi$Score.in.Domain<-as.numeric(combi$Score.in.Domain)
combi$Score.in.Domain[combi$Score.in.Domain==-1]<-NA
sum(is.na(combi$Score.in.Domain))
#------------------------------------------------------------------
#IMPUTING USING CENTRAL IMPUTATION----------------------
combi<-centralImputation(combi)
combi$School.Board.in.Tenth<-NULL
combi$Board.in.Twelth<-NULL
combi$Candidate.ID<-NULL
str(combi$Year.Of.Twelth.Completion)
library(dplyr)
attach(combi)
colnames(combi)
Data_num<-subset(combi,select=c(Score.in.Tenth,Score.in.Twelth,GPA.Score.in.Graduation,Score.in.English.language,
                                Score.in.Logical.skill,Score.in.Quantitative.ability,
                                Score.in.Domain,Score.in.ComputerProgramming,Score.in.ElectronicsAndSemicon,
                                Score.in.ComputerScience,Score.in.MechanicalEngg,Score.in.ElectricalEngg,
                                Score.in.TelecomEngg,Score.in.CivilEngg,Score.in.conscientiousness,Score.in.agreeableness,
                                Score.in.extraversion,Score.in.nueroticism,Score.in.openess_to_experience,
                                age))

Data_cat<-subset(combi,select=-c(Score.in.Tenth,Score.in.Twelth,GPA.Score.in.Graduation,Score.in.English.language,
                                Score.in.Logical.skill,Score.in.Quantitative.ability,
                                Score.in.Domain,Score.in.ComputerProgramming,Score.in.ElectronicsAndSemicon,
                                Score.in.ComputerScience,Score.in.MechanicalEngg,Score.in.ElectricalEngg,
                                Score.in.TelecomEngg,Score.in.CivilEngg,Score.in.conscientiousness,Score.in.agreeableness,
                                Score.in.extraversion,Score.in.nueroticism,Score.in.openess_to_experience,
                                age))
#-------------------------------------------------------------------------------------------------------------------------
new_train_num<-Data_num[1:26415,]
new_test_num<-Data_num[26416:37290,]
new_train_num<-cbind(new_train_num,Pay_in_INR)
#------------------------------------------------------------------------------------------------------
model1<-lm(log(Pay_in_INR)~.,data=new_train_num)
summary(model1)
pred1<-predict(model1,newdata = new_test_num)
regr.eval(tr$Pay_in_INR, pred1)
library(MASS)
stepAIC(model1)
summary(model1)
new_train_num<-cbind(new_train_num,Pay_in_INR)


                      
#---------------XGbosssssssssssst---------------------------------

xgb.ctrl <- trainControl(method = "cv", number = 5,
                         search='random')

set.seed(123)
xgb.tune <-train(Pay_in_INR~.,
                 data = new_train_num,
                 method="xgbLinear",
                 trControl=xgb.ctrl,
                 tuneLength=7)
Salary<-predict(xgb.tune,new_test_num)
predictions<-cbind(ID=test$Candidate.ID,Salary)
predictions<-data.frame(predictions)
write.csv(predictions,file="predictions.csv",row.names=F)

#-------------------RONDOM FOREST----------------------------------------------------

modelforest1<-randomForest(Pay_in_INR~.,new_train_num)
Salary<-predict(modelforest1,new_test_num)
predictions<-cbind(ID=test$Candidate.ID,Salary)
predictions<-data.frame(predictions)
write.csv(predictions,file="predictions.csv",row.names=F)

#-----------------------------------------------------------------------