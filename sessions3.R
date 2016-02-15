setwd("airbnb")
library(dplyr)
library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(reshape2)

#sessions = read_csv("data/sessions.csv")
#save(sessions,file="sessions.Rda")
load("sessions.Rda")
sessions[names(sessions) %in% c("action", "action_type", "action_detail", "device_type")] = 
        lapply(sessions[names(sessions) %in% c("action", "action_type", "action_detail", "device_type")], as.factor)

#read train, clean age 
train=read_csv("data/train_users_2.csv")
train$date_account_created = strptime(as.character(train$date_account_created), "%Y-%m-%d")
train$age[is.na(train$age)]=-1
whenYearisDOB = which(year(train$date_account_created)-train$age<=100 & year(train$date_account_created)-train$age>=14)
train[whenYearisDOB,]$age=year(train[whenYearisDOB,]$date_account_created)-train[whenYearisDOB,]$age
train[train$age>100 | train$age<14,]$age=-1
train=train[names(train) %in% c("id", "gender", "age", "country_destination")]

#extract session rows corresponding to train
sessions_train = inner_join(sessions, train, by = c("user_id"="id"))

trSessions = sessions_train %>%
        group_by(user_id, action, action_type, action_detail, device_type) %>%
        summarise(count=n(), total_time = sum(secs_elapsed, na.rm=T)) 

#add train features to reduced sessions
trSessions = inner_join(trSessions, train, by = c("user_id"="id"))


#create data frame with a row for every user
trUsers = trSessions %>% group_by(user_id, gender, age, country_destination) %>%
        summarise()

trUsers$wishlist = trSessions %>%
        group_by(user_id) %>%
        summarise(wishlist = as.numeric("wishlist_content_update" %in% action_detail)) %>%
        select(wishlist) %>%
        unlist()

ggplot(data=trUsers,aes(age, colour=wishlist)) + geom_histogram(aes(fill=wishlist)) #+ facet_grid(wishlist ~ .)


ggplot(trUsers, aes(age, fill = as.factor(wishlist))) + geom_histogram(binwidth = 1)
ggplot(trUsers, aes(wishlist, fill = as.factor(country_destination))) + geom_bar(binwidth = 1)



wishlist_pc = trUsers %>%
        group_by(wishlist, country_destination) %>%
        summarize(n=n()) %>%
        mutate(per=round(n*100/sum(n),4))
ggplot(wishlist_pc, aes(as.factor(wishlist), fill = per)) + geom_bar(binwidth = 1)

#                   mcBrowser=names(summary(device_type)[1]), 
#                   mcBrowserCount=summary(sessions_train$device_type)[1])

#test=read_csv("data/test_users.csv")
#sessions_test = inner_join(sessions, test, by = c("user_id"="id"))
#rm(sessions_train, sessions_test)
        
# trUsers$wishlistAll = trSessions %>%
#         group_by(user_id) %>%
#         summarise(wishlist = max(c(0,length(grep("wishlist", action_detail))))) %>%
#         select(wishlist) %>%
#         unlist()
# 
# trUsers$countryNDF = as.numeric(trUsers$country_destination=="NDF")
# 
# confusionMatrix(as.numeric(trUsers$wishlistAll=0), trUsers$countryNDF)
# 
# ggplot(trUsers, aes(as.factor(wishlistAll))) + geom_bar(binwidth = 1) +facet_grid(countryNDF~.)
# 
# wishlistAll_pc = trUsers %>%
#         group_by(wishlistAll, country_destination) %>%
#         summarize(n=n()) %>%
#         mutate(per=round(n*100/sum(n),4))



trUsers$wishlist = trSessions %>%
        group_by(user_id) %>%
        summarise(wishlist = as.numeric("wishlist_content_update" %in% action_detail)) %>%
        select(wishlist) %>%
        unlist()


paymentActions=unique(sessions$action)[grep("payment", unique(sessions$action))]

trUsers$payment = trSessions %>%
        group_by(user_id) %>%
        summarise(payment = max(as.numeric(paymentActions %in% action_detail))) %>%
        select(payment) %>%
        unlist()

payment_pc = trUsers %>%
        group_by(payment, country_destination) %>%
        summarize(n=n()) %>%
        mutate(per=round(n*100/sum(n),4)) %>%
        dcast(country_destination ~ payment)

topAD = c("view_search_results", "p3", "-unknown-", "wishlist_content_update", "user_profile", 
          "change_trip_characteristics", "similar_listings", "user_social_connections", 
          "update_listing", "listing_reviews")

trUsers[,7:16]=NA
PCs = list()


i=1
for (ad in topAD){
        trUsers[,i+6] = trSessions %>%
                group_by(user_id) %>%
                summarise(temp = as.numeric(topAD[i] %in% action_detail)) %>%
                select(temp) %>%
                unlist()

        i=i+1
}        

listing_reviews = trUsers %>%
                group_by(V16, country_destination) %>%
                summarize(n=n()) %>%
                mutate(per=round(n*100/sum(n),4)) %>%
                dcast(country_destination ~ V16)
        
        
        
        
        
        
        
        
        
        
        
        
        
        
}






















