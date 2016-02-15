library(magrittr)
library(caret)
library(rpart)
library(rattle)
library(stringr)
train=read.csv("data/train_users_2.csv")
train_raw=train
test=read.csv("data/test_users.csv")
test_raw=test

#prop.table(table(train$country_destination))

levels(train$signup_method)=levels(test$signup_method)=unique(c(levels(test$signup_method), levels(train$signup_method)))
train$timestamp_first_active = strptime(as.character(train$timestamp_first_active), "%Y%m%d%H%M%S")
train$date_first_booking=NULL      #remove column as not present in test data

attach(train)

#unique(train$gender)
#table(age)
#train[age==2014  &! is.na(age),]

fit=rpart(country_destination ~ gender + age + signup_method, data=train)
fancyRpartPlot(fit)
pred=predict(fit, type = "class")

confusionMatrix(pred, train$country_destination)
table(pred==train$country_destination)
prop.table(table(pred))


pred.test= predict(fit, test, type="class")

#fit=rpart(country_destination ~ gender + age + signup_method + language, data=train)



######################################################
#generate test ranks using age gender brackets age_gender_bkts.R

attach(test_ranks)
test_ranks = data.frame(id=test$id, gender=test$gender, age=test$age, rank1=pred.test)
test_ranks$age_from=floor(test_ranks$age/5)*5
test_ranks[is.na(test_ranks$age_from) | (test_ranks$age_from>120),]$age_from=-1
test_ranks[test_ranks$age_from>100,]$age_from=100
test_ranks$gender=str_to_lower(test_ranks$gender)
test_ranks[!test_ranks$gender %in% c("male", "female"),]$gender="both"

test_ranks$rank2=NA
test_ranks$rank2=as.factor(test_ranks$rank2)
levels(test_ranks$rank2)=levels(test_ranks$rank1)
test_ranks[test_ranks$rank1=="NDF",]$rank2="US"
test_ranks[test_ranks$rank1=="US",]$rank2="NDF"
#since rank1 in all demographic profiles is US, assigning rank2 fro profile
#test_ranks_bk=test_ranks
test_ranks2=merge(x=test_ranks, y=ag.profile.mf, all.x=T, by= c('gender', 'age_from'))


levels(test_ranks2$rank2.y)=levels(test_ranks2$rank4)=levels(test_ranks2$rank3)=levels(test_ranks2$rank1.x)
table(is.na(unlist(list(test_ranks2$rank1.x,test_ranks2$rank2.x, test_ranks2$rank2.y, test_ranks2$rank3, test_ranks2$rank4))))
output=data.frame(id=rep(test_ranks2$id,5),country=unlist(list(test_ranks2$rank1.x,test_ranks2$rank2.x, test_ranks2$rank2.y, test_ranks2$rank3, test_ranks2$rank4)))
head(output)
write.table(output, file="output1.csv", row.names=F, quote=F, sep=",")


#####################################################





