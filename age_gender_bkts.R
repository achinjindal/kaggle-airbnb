library(dplyr)
library(magrittr)
library(reshape2)

ag.df=read.csv("data/age_gender_bkts.csv")
#head(ag.df)
#unique(ag.df$age_bucket)
#sum(ag.df$population_in_thousands)       #732116


#add age from, age to columns in data frame
ag.df$age_from = sub("-.*", "", ag.df$age_bucket)
ag.df$age_to = sub(".*-", "", ag.df$age_bucket)
ag.df[ag.df$age_from=="100+",]$age_from=100
ag.df[ag.df$age_to=="100+",]$age_to=200
ag.df$age_from = as.numeric(ag.df$age_from)
ag.df$age_to = as.numeric(ag.df$age_to)

#get population in each bracket
populations = ag.df %>%
        group_by(age_bucket, gender) %>%
        summarise(total = sum(population_in_thousands))


ag.df=merge(x=populations, y=ag.df, all.y = TRUE)
ag.df$fraction = ag.df$population_in_thousands/ag.df$total
ag.df.noGender = ag.df %>% 
        group_by(age_bucket, country_destination) %>%
        summarise(gender = "both", total=sum(total), population_in_thousands=sum(population_in_thousands), year=2015, 
                  age_from=mean(age_from), age_to=mean(age_to), fraction=NA)
ag.df.noGender$fraction=ag.df.noGender$population_in_thousands/ag.df.noGender$total
ag.df.noAge = ag.df %>% 
        group_by(gender, country_destination) %>%
        summarise(age_bucket= "-1", total=sum(total), population_in_thousands=sum(population_in_thousands), year=2015, 
                  age_from=-1, age_to=-1, fraction=NA)
ag.df.noAge$fraction = ag.df.noAge$population_in_thousands/ag.df.noAge$total
ag.df.noAgenoGender = ag.df %>% 
        group_by(country_destination) %>%
        summarise(age_bucket= "-1", gender="both", total=sum(total), population_in_thousands=sum(population_in_thousands), year=2015, 
                  age_from=-1, age_to=-1, fraction=NA)
ag.df.noAgenoGender$fraction = ag.df.noAgenoGender$population_in_thousands/ag.df.noAgenoGender$total


ag.df = rbind(ag.df, ag.df.noGender, ag.df.noAge, ag.df.noAgenoGender)
rm(ag.df.noGender, ag.df.noAge, ag.df.noAgenoGender, populations)

ag.df = ag.df[,c(7,8,2,5,3,4,9)]
ag.df = dcast(ag.df, age_from+age_to+gender~country_destination, mean, value.var="fraction")

trainAG=read.csv("data/train_users_2.csv")
testAG=read.csv("data/test_users.csv")
combinedAG = rbind(trainAG[names(trainAG) %in% c("id", "gender", "age")], 
                   testAG[names(testAG) %in% c("id", "gender", "age")])
rm(trainAG, testAG)

combinedAG$age_from=floor(combinedAG$age/5)*5
combinedAG[is.na(combinedAG$age_from) | (combinedAG$age_from>120),]$age_from=-1
combinedAG[combinedAG$age_from>100,]$age_from=100
combinedAG$gender=str_to_lower(combinedAG$gender)
combinedAG=combinedAG[,!(names(combinedAG) %in% "age")]
#note: try both - predicting and not predicting for unknown gender
combinedAG[!combinedAG$gender %in% c("male", "female"),]$gender="both"
#test_scores=merge(x=test, y=ag.df, all.x=T, by= c('gender', 'age_from'))
combinedAG_scores=left_join(x=combinedAG, y=ag.df, match = c('gender', 'age_from'))
combinedAG_scores = combinedAG_scores[,!names(combinedAG_scores) %in% c("id", "gender", "age_from", "age_to")]
names(combinedAG_scores) = paste0("prob_",names(combinedAG_scores))

