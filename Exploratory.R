setwd("airbnb")

library(magrittr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gplots)

table(combined$affiliate_channel)
table(combined$affiliate_provider)   #explore minor providers to club into one
table(combined$first_affiliate_tracked) #explore NA's
table(combined$signup_app)
table(combined$first_device_type)
table(combined$first_browser) #club into a few

table(combined$signup_flow)    # 0  3  2  1 24  8  6  5 10 25 12  4 16 15 20 21 23 14
hist(combined$age)



#get country proportions by gender
gender_pc = train %>%
        group_by(gender, country_destination) %>%
        summarize(n=n()) %>%
        mutate(per=round(n/sum(n),4))
gender_grid = dcast(gender_pc, country_destination ~ gender, mean)
rownames(gender_grid) = gender_grid[,1]
gender_grid=gender_grid[,-1]
gender_grid2=gender_grid[!rownames(!gender_grid) %in% c("NDF", "US"),]
dev.off()
heatmap.2(x = as.matrix(gender_grid2), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = as.matrix(gender_grid2), notecol = "black", notecex = 2,
          trace = "none", key = FALSE, margins = c(7, 11))


gender_pcNoNDF = train %>%
        filter(country_destination!="NDF") %>%
        group_by(gender, country_destination) %>%
        summarize(n=n()) %>%
        mutate(per=round(n/sum(n),4))
gender_gridNoNDF = dcast(gender_pcNoNDF, country_destination ~ gender, mean)
gender_gridNoNDF = rbind(gender_gridNoNDF, c("NDF", rep("NA", 4)))
names(gender_gridNoNDF)[2:5] = lapply(names(gender_gridNoNDF)[2:5], function(x) {paste0(x,"_noNDF") } )

gender_grid_merge = merge(gender_grid, gender_gridNoNDF, by='country_destination')


# 
# x=train %>%
#         group_by(gender) %>%
#         mutate(countT= n()) %>%
#         group_by(country_destination, add=TRUE) %>%
#         mutate(n=n(), per=paste0(round(100*n()/countT,2),'%'))

#signup flow grid
# signupflow_pc = train %>%
#         group_by(signup_flow, country_destination) %>%
#         summarize(n=n()) %>%
#         mutate(per=round(n/sum(n),4))

signupflow_groups = train %>%
        group_by(signup_flow, country_destination) %>%
        summarize(n=n())







dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
options("contrasts")
model.matrix(~ a + b, dd)
