rm(list = ls())
test <- readRDS("data/y1_test_dat.rds")
View(test)

######################################################################################
####EDA
library(dplyr)
library(ggplot2)
library(tidyverse)
library(table1)

##All actigraph data(6)
test2<- test %>%
  select(StudyId, BirthYear, BirthSex, Race, light_raw_time_pw, mod_raw_time_pw, vig_raw_time_pw, vervig_raw_time_pw,light_time_pw_sasaki, mvpa_time_pw_sasaki)%>%
  na.omit()
test2["Race"][test2["Race"] == ''] <- NA
head(test2)
test2_long<- reshape2::melt(test2, id.vars=c('StudyId','BirthYear', 'BirthSex', 'Race'), variable.name = "type_variable", value.name = "time_value")

#Table1 for Race and BirthSex
table1(~  light_raw_time_pw+mod_raw_time_pw+vig_raw_time_pw+vervig_raw_time_pw+light_time_pw_sasaki+mvpa_time_pw_sasaki| BirthSex  , data=test2,
       overall=c(left="Total"))
table1(~  light_raw_time_pw+mod_raw_time_pw+vig_raw_time_pw+vervig_raw_time_pw+light_time_pw_sasaki+mvpa_time_pw_sasaki| Race  , data=test2,
       overall=c(left="Total"))

#Histgram
ggplot(test2_long, aes(x=time_value)) + 
  geom_histogram(bins = 30)+
  facet_wrap(~type_variable,scales="free_x") + scale_x_log10()

# Boxplot
ggplot(test2_long, aes(x = type_variable, y = time_value, fill = type_variable)) + 
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", color = "white")

######################################################################################
## Non-bouted(4)
test3<- test %>%
  select(StudyId, BirthYear, BirthSex, Race, light_raw_time_pw, mod_raw_time_pw, vig_raw_time_pw, vervig_raw_time_pw)%>%
  na.omit()
test3["Race"][test3["Race"] == ''] <- NA
head(test3)
test3_long<- reshape2::melt(test3, id.vars=c('StudyId','BirthYear', 'BirthSex', 'Race'), variable.name = "type_variable", value.name = "time_value")

#Histgram
ggplot(test3_long, aes(x=time_value)) + 
  geom_histogram(bins = 30)+
  facet_wrap(~type_variable,scales="free_x") + scale_x_log10()

#Boxplot
ggplot(test3_long, aes(x = type_variable, y = time_value, fill = type_variable)) + 
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", color = "white")

######################################################################################
##Bouted(2)
test4<- test %>%
  select(StudyId, BirthYear, BirthSex, Race, light_time_pw_sasaki, mvpa_time_pw_sasaki)%>%
  na.omit()
test4["Race"][test4["Race"] == ''] <- NA
head(test4)
test4_long<- reshape2::melt(test4, id.vars=c('StudyId','BirthYear', 'BirthSex', 'Race'), variable.name = "type_variable", value.name = "time_value")

#Histgram
ggplot(test4_long, aes(x=time_value)) + 
  geom_histogram(bins = 30)+
  facet_wrap(~type_variable,scales="free_x") + scale_x_log10()

#Boxplot
ggplot(test4_long, aes(x = type_variable, y = time_value, fill = type_variable)) + 
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", color = "white")


######################################################################################
## LRCQ
### match on `record_id` in test with LipidData 1st column
### column variable described:
###   Study ID: Column A
###   Substudy Class: column B (which “year” of the PROSPER study did the visit occur in)
###   Ss1: column C (1='Much more active', 2='Somewhat more active', 3='About the same', 4='Somewhat less active', 5='Much less active', 6='Not Applicable';)
###   Ss2: D (1='Much more active', 2='Somewhat more active', 3='About the same', 4='Somewhat less active', 5='Much less active', 6='Not Applicable';)
###   Ss3: E (1='Yes' 0='No')
###   Ss4: F (1='Yes' 0='No') 6
###   Ss5: G (1='Vigorous' 2='Moderate' 3='Low') 7
###   Completed substudy: H (0='Incomplete' 1='Unverified' 2='Complete') 8
###   Valid result: I (1='Yes' 0='No') 9
###   Test completed date: J (mm/dd/yyyy) 10
lrcq <- readr::read_csv("./data/PROSPERHIV-LipidData_DATA_NOHDRS_2023-01-11_1310.csv", col_names = F)
colname_all <- colnames(test)

apply(test[,colname_all[grep("raw", colname_all)]], 2,function(x) mean(is.na(x)))

## matched on `record_id`
lrcq_ind <- which(lrcq$X1 %in% test$record_id[which(!is.na(test$light_raw_time))])
test_ind <- which(test$record_id[which(!is.na(test$light_raw_time))]  %in% lrcq$X1)

library(tidyverse)
lrcq_matched <- lrcq[lrcq_ind, ] %>% as.data.frame
mean(lrcq_matched$X5) #0.49, two points cutoff (strenuous/not)
rownames(lrcq_matched) <- lrcq_matched$X1

test_matched <- test[test_ind, ]
rownames(test_matched) <- test_matched$record_id

comb_df <- merge(lrcq_matched, test_matched, by = 'row.names')
rownames(comb_df) <- comb_df$Row.names
comb_df <- comb_df[,-1]
