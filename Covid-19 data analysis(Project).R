rm(list=ls())
install.packages("Hmisc")
library(Hmisc)
data<-read.csv("C:/Users/priya/Downloads/COVID19_line_list_data.csv")
describe(data) #Hmisc command
data$death_dummy<-as.integer(data$death!=0)#cleaned up death colunm
#death rate
sum(data$death_dummy)/nrow(data)
#AGE
#claim:people who die are older
dead=subset(data,death_dummy==1)
alive=subset(data,death_dummy==0)
mean(dead$age,na.rm=TRUE)
mean(alive$age,na.rm=TRUE)
#is this statistically significant,check this out by a two sample t test
t.test(alive$age,dead$age,alternative="two.sided",conf.level = 0.95)
#we reject the null hypothesis at level 5,since p-value<0.05
#i.e. old people are likely to die than young ones

#GENDER
#claim:gender has no effect
men=subset(data,gender=="male")
women=subset(data,gender=="female")
mean(men$death_dummy,na.rm=TRUE) #8.46%
mean(women$death_dummy,na.rm=TRUE) #3.67%
#is this statistically significant,check this out by a two sample t test
t.test(men$death_dummy,women$death_dummy,alternative="greater",conf.level = 0.95)
#this is statistically significant ,p-value<0.05
#Men are likely to die than women

#fit a logistic regression
recover<-as.integer(data$recovered!=0)
y<-data$death_dummy
x1<-data$gender
x2<-recover
#model
model<-glm(y~x1+x2,family=binomial,data)
summary(model)
#checking multicollinearity
install.packages("car")
library(car)
vif(model)#no multicollinearity
#accuracy
install.packages("caret")
predicted_prob<-predict(model,data,type="response")
predicted_class<-ifelse(predicted_prob>0.5,1,0)
library(caret)
confusionMatrix(as.factor(predicted_class),as.factor(data$death_dummy))
#Accuracy:0.9357
#removing insignificant feature
full_model<-glm(y~x1,data,family=binomial)
step_model<-step(full_model,direction ="backward" )
summary(step_model)
#from p-value we can say x2 i.e. recovery is a insignificant feature
