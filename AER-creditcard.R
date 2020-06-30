#Library loading
#install.packages("AER")
#install.packages("ggthemes") #install if not present
library(ggthemes)
library(AER) #To load the dataset
library(dplyr)
library(tidyverse) #To do piping and tidy operations


#Load Data in environment
data("CreditCard")

#12 variables, 1319 observations in Dataset

#Exploratory Data Analysis
#generate a quick summary of variables in Dataset
summary(CreditCard)
#No missing Values in the dataset
#Variable Names: "card", "reports", "age", "income", "share", "expenditure", "owner", "selfamp", "dependents", "months","majorcards","active"
#Variables "card","owner" & "selfemp" are categorical
#While 9 variables are numerical, variable "majorcards" has only Binary Values (0/1)
#Variable "card" has a no:yes ratio of 296:1023 (~3:10)
#Variable "reports","dependents" have quiet a lot of 0 values
#Variable "age": The minimum age is implausibly low
#Variable "share" =monthly credit card expenditure/yearly income. While the minimum expenditure is zero, the minimum share is not (indicating either an error in the formula description or with the calculation of share)

#Plot variables comparing impact on "card"
#Plot Categorical Variables
CreditCard %>%
  mutate_if(is.factor,as.character) %>%
  mutate(majorcards = as.character(majorcards)) %>%
  select_if(is.character) %>%
  select(card, everything()) %>%
  gather(x, y, owner:majorcards) %>%
  count(card, x, y) %>%
  ggplot(aes(x = y, y = n, fill = card, color = card)) +
  facet_wrap(~ x, ncol = 4, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()
  
#its a easy visulisation of ratio of acceptance and non acceptance of creditcard in each category in all three categorical variables.

#Plot Numerical Variables
CreditCard %>%
  select(card, reports, age, income, share, expenditure, dependents, months,active) %>%
  gather(x, y, reports:active) %>%
  ggplot(aes(x = y, fill = card, color = card)) +
  facet_wrap(~ x, ncol = 3, scales = "free") +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()
#"reports", "Expenditure" and "share" looks to be good potential differentiators 
#"Expenditure" and "share" seem to be related
plot(CreditCard$share,CreditCard$expenditure)#here we are getting a linear pattern means they are highly correlated
#note two variables should not be correlated for a good model


#Train - Test Split
CreditCard$card<-ifelse(CreditCard$card=="yes",1,0)
set.seed(200)
index<-sample(nrow(CreditCard),0.70*nrow(CreditCard),replace = F)
train<-CreditCard[index,]
test<-CreditCard[-index,]
#model will be builded with train data and it will be validated with test data
 
#model building

#logit model
#As we have seen share and expenditure are highly correlated so we drop one variable (in this case we dropped expenditure)
logitmod1<-glm(card~.,data = train[,-6],family =binomial(link = "logit") )
summary(logitmod1)
#variables are available with coefficient and p values,we need to check sign of coefficient and significant value.
#variables with positive coefficient are positively related with acceptance of credit card and vise-versa.
#variables with low p value or * sign are significant and only significant variables are required.

step(logitmod1,direction = "both")
#step functin is used to get formula with significant variables 
logitmod2<-glm(formula = card ~ reports + income + share + dependents + 
      months + active, family = binomial(link = "logit"), data = train[,-6]) #this formula is gotten 
summary(logitmod2)#but summary shows variable "months" is still not significant so in next iteration it is removed manually
#final logit model
logitmod3<-glm(formula = card ~ reports + income + share + dependents + 
                  active, family = binomial(link = "logit"), data = train[,-6])
summary(logitmod3) #now all variables are significant as all have star(*) sign



#similarly probit model
probitmod1<-glm(card~.,data = train[,-6],family =binomial(link = "probit") )
summary(probitmod1)
step(probitmod1,direction = "both")
probitmod2<-glm(formula = card ~ reports + income + share + dependents + 
       active, family = binomial(link = "probit"), data = train[,-6])
summary(probitmod2)

#now all variables are significant with * sign

#*---------
#next step validation and comparision
library(gains)
library(irr)
library(caret)#install package if not availble
library(e1071)

#first both models are validated using kappa than confusion matrix


#logit with kappa
prediction<-predict(logitmod3,type="response",newdata = test) #logitmod3 is tested using new data "test"
head(prediction) #this is predicted values on test data with logitmod3
table(CreditCard$card)/nrow(CreditCard)#table gives percentage of 1 and 0 in total data
prediction<-ifelse(prediction>=.78,1,0)#because from above .7788 are 1
kappa2(data.frame(test$card,prediction))

#kappa =0.952 means the model is working very good

#logit with confusion matrix
prediction<-as.factor(prediction)
test$card<-as.factor(test$card)
confusionMatrix(prediction,test$card)

#from confusionmatrix our model showing 92 zeros as true zeros and only 1 zero as false one and showing 297 ones as true ones and 6 ones as false ones
#good sign
 

#similarly kappa test of probit model
prediction2<-predict(probitmod2,type="response",newdata = test)
head(prediction2)
table(CreditCard$card)/nrow(CreditCard)
prediction2<-ifelse(prediction2>=.78,1,0)#because from above .7788 are 1
kappa2(data.frame(test$card,prediction2))

#kappa is 0.95 ....good sign

#confusion matrix of probit model
prediction2<-as.factor(prediction2)
test$card<-as.factor(test$card)
confusionMatrix(prediction2,test$card)

##from confusionmatrix our model showing 92 zeros as true zeros and only 1 zero as false zero and showing 297 ones as true ones and 6 ones as false ones

#COMPARISION
#(1)AIC
AIC(logitmod3)#AIC OF logitmod3=100.34
AIC(probitmod2)#AIC OF probitmod2=99.935
#The Akaike information criterion (AIC) is an estimator of the relative quality of statistical models for a given set of data. Given a collection of models for the data.
#AIC estimates the quality of each model, relative to each of the other models. 
#Thus, AIC provides a means for model selection.
#Low AIC model should be selected
#probit model has less value of deviance

#(2)Residual deviance
#Deviance is a measure of goodness of fit of a model. Higher numbers always indicates bad fit.
deviance(logitimod3)#=88.3441
deviance(probitmod2)#=87.93509
#probit model has less value of deviance

#CONCLUSION
#Therefore AIC and Deviance both are showing probit model is performing better than logit.

#VARIABLE AFFECTING EXPENDITURE
#here we use linear regression because expenditure is a continous variable
mod<-lm(expenditure~.,data=CreditCard[,-5])
summary(mod)

#income and card  are coming as significant variables that are affecting expenditure (*sign ,low p values)




                             
