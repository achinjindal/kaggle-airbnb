setwd("airbnb")
library(dplyr)
library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(reshape2)
library(xgboost)
#sessions = read_csv("data/sessions.csv")
#save(sessions,file="sessions.Rda")
load("sessions.Rda")

#convert columns to factors
sessions[names(sessions) %in% c("action", "action_type", "action_detail", "device_type")] = 
        lapply(sessions[names(sessions) %in% c("action", "action_type", "action_detail", "device_type")], as.factor)

#read train, test
train=read_csv("data/train_users_2.csv")
train$src="train"
test = read_csv("data/test_users.csv")
test$country_destination=NA
test$src="test"

#combine train and test
combined = rbind(train, test)

#extract session rows corresponding to train
sessions_combined = inner_join(sessions, combined, by = c("user_id"="id"))
sessions_reduced = sessions_combined %>%
        group_by(user_id, action, action_type, action_detail, device_type) %>%
        summarise(count=n(), total_time = sum(secs_elapsed, na.rm=T), 
                  country_destination = unique(country_destination), src=unique(src))



#create data frame with a row for every user
users = sessions_reduced %>%
        group_by(user_id) %>%
        summarise(country_destination = unique(country_destination), src = unique(src))
#save(users, file="objects/sessUsers.Rdata")
load("objects/sessUsers.Rdata")



#add OHE feature corresposiding for each action detail with frequency > 2000
table_a=table(sessions$action_detail)
action_details = names(table_a[table_a>2000])
u=length(users)
a = sessions_reduced %>%
        group_by(user_id, action_detail) %>%
        summarise(temp = (sum(total_time, na.rm=T))^0.25) %>%
        dcast(user_id ~ action_detail, value.var="temp") %>%
        select(-user_id)
a[is.na(a)]=-1       
users = cbind(users,a)                



#add OHE feature corresposiding to each action with frequency > 5000
table_a=table(sessions$action)
actions = names(table_a[table_a>5000])
u=length(users)
a = sessions_reduced %>%
        group_by(user_id, action) %>%
        summarise(temp = (sum(total_time, na.rm=T))^0.25) %>%
        dcast(user_id ~ action, value.var="temp") %>%
        select(-user_id)
a[is.na(a)]=-1       
users = cbind(users,a)       



#add OHE feature corresposiding to each action type
table_a=table(sessions$action_type)
action_types = names(table_a)
u=length(users)
a = sessions_reduced %>%
        group_by(user_id, action_type) %>%
        summarise(temp = (sum(total_time, na.rm=T))^0.25) %>%
        dcast(user_id ~ action_type, value.var="temp") %>%
        select(-user_id)
a[is.na(a)]=-1       
users = cbind(users,a)



#add OHE feature corresposiding to each device type
table_a=table(sessions$device_type)
device_types = names(table_a)
u=length(users)
a = sessions_reduced %>%
        group_by(user_id, device_type) %>%
        summarise(temp = (sum(total_time, na.rm=T))^0.25) %>%     ###
        dcast(user_id ~ device_type, value.var="temp") %>%
        select(-user_id)
a[is.na(a)]=-1
users = cbind(users,a)

save(users, file="objects/sessUsers_final_time_1202.Rdata")
load("objects/sessUsers_final_time_1202.Rdata")

load("objects/sets.train.Rdata")

usss=users

traindata=rbind(trainSet.train, testSet.train)

users = left_join(users, traindata, by = c("user_id" = "id"))
#users$country_destination = users$country_destination.x
#users$country_destination.x = NULL


trainSet = users[users$src=="train",]
outcomeSet = trainSet$country_destination
outcomeSet = as.numeric(as.factor(outcomeSet))-1
trainSet = trainSet[,!names(trainSet) %in% c("country_destination", "user_id", "src")]

testSet = users[users$src=="test",]
testSet = testSet[,!names(testSet) %in% c("country_destination", "user_id", "src")]

#run model
model.xgb.sess=xgboost(data=as.matrix(trainSet), 
                       label=as.matrix(outcomeSet), 
                       nrounds=25, 
                       verbose=1,
                       objective="multi:softprob",
                       num_class=12, 
                       eval_metric=ndcg5,
                       eta=0.08,
                       subsample = 0.7,
                       max_depth=8,
                       colsample_bytree = 0.6
)




save(model.xgb.sess, file="objects/xgb_model_sessions_time_1202_0430.RData")
#load("objects/xgb_model.RData")

#retrieve importance of features in model
#xgb_importance=xgb.importance(feature_names=colnames(trainSet), model=model.xgb.sess)
#save(xgb_importance,file="objects/xgb_imp_sessions1.RData")
#load("objects/xgb_imp.RData")
#xgb.plot.importance(xgb_importance)

#predict, convert to 2d matrix
pred=predict(model.xgb.sess, as.matrix(testSet))
#save()
pred.test.prob = matrix(pred, nrow=12, ncol=length(pred)/12)
pred.test.prob=t(pred.test.prob)
pred.test.prob=as.data.frame(pred.test.prob)
country_factors = c("AU","CA","DE","ES","FR","GB","IT","NDF","NL","other","PT","US") 
colnames(pred.test.prob)=country_factors



#write to file for ensembling   
 pred.test.prob.sessions = data.frame(id=users[users$src=="test",]$user_id, pred.test.prob)
 save(pred.test.prob.sessions , file="pred.test.prob.sessions.train.Rdata")



pred.test.submit = apply(pred.test.prob, 1, function(x) names(sort(x, decreasing=T)[1:5]))
pred.test.submit = t(pred.test.submit)
pred.test.submit = cbind(id=users[users$src=="test",]$user_id, data.frame(pred.test.submit))

pred.test.submit.sessions=pred.test.submit

load("objects/pred.test.submit.train.Rdata")


usersNotInSession = anti_join(pred.test.submit.train, pred.test.submit, by="id")
pred.test.submit.full = rbind(pred.test.submit, usersNotInSession)

submit=melt(pred.test.submit.full, id.vars="id")
submit = submit[,!names(submit) %in% "variable"]
submit = rename(submit, country=value)

write.table(submit, file="submit/output1202_time_5_sessions_train.csv", row.names=F, quote=F, sep=",")





