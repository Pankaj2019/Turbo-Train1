getwd()
setwd("E:/R")
list.files("E:/R")
train<-read.csv("bank-full_train (3).csv",stringsAsFactors = FALSE,header = T)
test<-read.csv("bank-full_test (1).csv",stringsAsFactors = FALSE,header = T) 

# STEP-1 (Finding and removing the NA values)

# imputing NA values in the datasets

apply(train,2,function(x)sum(is.na(x)))
# conclusion- There is no any NA in train Data 

apply(test,2,function(x)sum(is.na(x)))
# conclusion- There is no any NA in test data

#STEP-2 ( Data Preparation)

# combining both train and test data prior to data preparation

test$y=NA
train$data='train'
test$data='test'
all_data=rbind(train,test)
apply(all_data,2,function(x)sum(is.na(x)))


library(dplyr)
glimpse(all_data)

# Creating dummy variables by combining similar categories for variables job
t=table(all_data$job)
sort(t)

final=round(prop.table(table(all_data$job,all_data$y),1)*100,1)
final

s=addmargins(final,2)
sort(s[,1])
sort(s[,2])

View(s)

all_data<-all_data %>%
  mutate(job_1=as.numeric(job %in% c("self-employed","unknown","technician")),
         job_2=as.numeric(job %in% c("services","housemaid","enterpreneur")),
          job_3=as.numeric(job %in% c("management","admin")),
           job_4=as.numeric(job=="student"),
           job_5=as.numeric(job=="retired"),
         job_6=as.numeric(job=="unemployed")) %>%
  select(-job)

library(dplyr)
glimpse(all_data)

# Making dummies for variable marital

t=table(all_data$marital)
sort(t)

all_data=all_data %>%
  mutate(divorced=as.numeric(marital %in% c("divorced")),
         single=as.numeric(marital %in% c("single"))) %>%
  select(-marital)
library(dplyr)
glimpse(all_data)

# Making dummies for variable education

t= table(all_data$education)
t
sort(t)

all_data=all_data %>%
  mutate(edu_primary=as.numeric(education %in% c("primary")),
         edu_sec=as.numeric(education %in% c("secondary")),
         edu_tert=as.numeric(education %in% c("tertiary"))) %>%
  select(-education)
library(dplyr)
glimpse(all_data)

# Making dummies for variable default
table(all_data$default)

all_data$default=as.numeric(all_data$default=="yes")
all_data$default

# Making dummies for variable housing

table(all_data$housing)
all_data$housing=as.numeric(all_data$housing=="yes")

library(dplyr)
glimpse(all_data)

# Making dummies for variable loan

table(all_data$loan)
all_data$loan=as.numeric(all_data$loan=="yes")

library(dplyr)
glimpse(all_data)

# Making dummies for variable contact

t=table(all_data$contact)
sort(t)

all_data=all_data %>%
  mutate(co_cellular=as.numeric(contact %in% c("cellular")),
         co_tel=as.numeric(contact %in% c("telephone"))) %>%
  
  select(-contact)
library(dplyr)
glimpse(all_data)

# Making dummies for variable month

table(all_data$month)

# convering into percentage across months
finalmonth<-round(prop.table(table(all_data$month,all_data$y),1)*100,1)
finalmonth
S2=addmargins(finalmonth,2)
S2
sort(S2[,1])

# may has taken as base variable

all_data=all_data %>% 
  mutate(month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")) %>% 
  select(-month)

library(dplyr)
glimpse(all_data)

# Making dummies for varibale outcome
t=table(all_data$poutcome)
sort(t)
# Unknown is base variable

all_data=all_data %>% 
  mutate(poc_success=as.numeric(poutcome %in% c("success")),
         poc_failure=as.numeric(poutcome %in% c("failure")),
         poc_other=as.numeric(poutcome %in% c("other"))
  )%>% 
  select(-poutcome)

library(dplyr)
glimpse(all_data)

#separating the test and train Data

library(dplyr)
glimpse(all_data)

table(all_data$y)
table(train$y)
all_data$y=as.numeric(all_data$y=="yes")
table(all_data$y)

library(dplyr)
glimpse(all_data)


#separating train and test data

train=all_data %>%
  filter(data=='train') %>%
  select(-data)

library(dplyr)
glimpse(train)

test=all_data %>%
  filter(data=='test') %>%
  select(-data,-y)

library(dplyr)
glimpse(test)


# Will devide the train dataset in the 75:25

set.seed(5)
s=sample(1:nrow(train),0.75*nrow(train))
train_75=train[s,]
test_25=train[-s,]

# library(car)

for_vif<-lm(y~.,data=train)
summary(for_vif)

# in order to take care of multi collinearity, we remove variable whose VIF>5

t=vif(for_vif)
sort(t,decreasing=T)[1:5]

# Removing variable edu_sec

for_vif=lm(y~.-edu_sec,data=train)
t=vif(for_vif)
sort(t,decreasing = T)[1:5]

summary(for_vif)

# will remove edu_sec from train dataset

colnames(train)

fit_train=train %>% 
  select(-edu_sec)
colnames(fit_train)

# BUilding model on fit_train dataset

fit=glm(y~.,family = "binomial",data=fit_train)
summary(fit)


# now removing all variables whose p value is > 0.05 using step function

fit=step(fit)

# will check the remaining significant variables

names(fit$coefficients)

#  build final logistic model on significant variables on dataset fit_train

fit_final=glm(y~balance + housing + loan + duration + campaign + ID + 
                job_3 + job_5 + divorced + single + edu_primary + 
                co_cellular + co_tel + month_1 + month_2 + month_3 + month_4 + 
                month_5 + month_6 + poc_success + poc_failure + poc_other ,data=fit_train,family="binomial")

summary(fit_final)
names(fit_final$coefficients)

#logistic regression model is successfully built
# will predict score ( Pi)

train$score=predict(fit_final,newdata = train,type="response")

train$score

# Library ggplot2

ggplot(train,aes(y=y,x=score,color=factor(y)))+
  geom_point()+geom_jitter()

# Finding Cutoff value and measurement of model

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)

for (i in cutoffs){
  predicted=as.numeric(train$score>i)
  
  TP=sum(predicted==1 & train$y==1)
  FP=sum(predicted==1 & train$y==0)
  FN=sum(predicted==0 & train$y==1)
  TN=sum(predicted==0 & train$y==0)
  cutoff_data=rbind(cutoff_data,c(i,TP,FP,FN,TN))
}
cutoff_data=cutoff_data[-1,]

# calculate the performance measure:sencificity,accuracy,sensitivity,KS, precision

cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP, #total positives and negatives
         Sn=TP/P, #sensitivity
         Sp=TN/N, #specificity
         KS=abs((TP/P)-(FP/N)),
         Accuracy=(TP+TN)/(P+N),
         Lift=(TP/P)/((TP+FP)/(P+N)),
         Precision=TP/(TP+FP),
         Recall=TP/P
  ) %>% 
  select(-P,-N)

# cutoff vale based on ks maximum

KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

KS_cutoff

# predicting if the client has subscribed or not in final test data

test$score=predict(fit_final,newdata =test,type = "response")
test$score
test$left=as.numeric(test$score>KS_cutoff)
test$left
table(test$left)

# Final prediction

test$leftfinal=factor(test$left,levels = c(0,1),labels=c("no","yes"))
table(test$leftfinal)

##### THUS 3396 CUSTOMERS OUT OF 13564 SUBSCRIBE TO TERM DEPOSIT ACCORDING TO THE PREPARED MODEL

# Creating confusion matrix and find how good our model is ( by predicting on test_25 dataset)

test_25$score=predict(fit_final,newdata =test_25,type = "response")
test_25$score
table(test_25$y,as.numeric(test_25$score>KS_cutoff))
table(test_25$y)

# Here TP = 770, TN = 5888 

# Accuracy = (TP+TN)/(P+N)

A=(770+5888)/7912
A

Error=1-A

E=1-A
E

###Library(pROC)

roccurve=roc(test_25$y,test_25$score)
plot(roccurve)
auc(roccurve)

###Conclusion:
 ### Thus the target no. of customers to be focused
##upon for term deposits by the bank are predicted 
##successfully using logistic regression model
##with an accuracy of 84.15% using KS method. 
##The KS score examined came out to
##be: 0.72/1.00[our model and predictions wer very good]


