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
save(users, file="sessUsers.Rdata")
load("sessUsers.Rdata")



#add OHE feature corresposiding for each action detail with frequency > 2000
table_a=table(sessions$action_detail)
action_details = names(table_a[table_a>2000])
u=length(users)
users[,(u+1):(u+length(action_details))]=NA
i=1
for (a in action_details){
        users[,u+i] = sessions_reduced %>%
                group_by(user_id) %>%
                summarise(temp = as.numeric(a %in% action_detail)) %>%
                select(temp) %>%
                unlist()
        i=i+1
}



#add OHE feature corresposiding to each action with frequency > 5000
table_a=table(sessions$action)
actions = names(table_a[table_a>5000])
u=length(users)
users[,(u+1):(u+length(actions))]=NA
i=1
for (a in actions){
        users[,u+i] = sessions_reduced %>%
                group_by(user_id) %>%
                summarise(temp = as.numeric(a %in% action)) %>%
                select(temp) %>%
                unlist()
        
        i=i+1
}



#add OHE feature corresposiding to each action type
table_a=table(sessions$action_type)
action_types = names(table_a)
u=length(users)
users[,(u+1):(u+length(action_types))]=NA
i=1
for (a in action_types){
        users[,u+i] = sessions_reduced %>%
                group_by(user_id) %>%
                summarise(temp = as.numeric(a %in% action_type)) %>%
                select(temp) %>%
                unlist()
        
        i=i+1
}


save(users, file="sessUsers_final.Rdata")
load("sessUsers_final.Rdata")



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
                  eta=0.1,
                  subsample = 0.7,
                  max_depth=8,
                  colsample_bytree = 0.6
)



save(model.xgb.sess, file="objects/xgb_model_sessions_1.RData")
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
save(pred.test.prob.sessions , file="pred.test.prob.sessions.Rdata")



pred.test.submit = apply(pred.test, 1, function(x) names(sort(x, decreasing=T)[1:5]))
pred.test.submit = t(pred.test.submit)
pred.test.submit = cbind(id=users[users$src=="test",]$user_id, data.frame(pred.test.submit))

pred.test.submit.sessions=pred.test.submit

load("pred.test.submit.train.Rdata")


usersNotInSession = anti_join(pred.test.submit.train, pred.test.submit, by="id")
pred.test.submit.full = rbind(pred.test.submit, usersNotInSession)

submit=melt(pred.test.submit.full, id.vars="id")
submit = submit[,!names(submit) %in% "variable"]
submit = rename(submit, country=value)

write.table(submit, file="submit/output0902_1_sessions.csv", row.names=F, quote=F, sep=",")





