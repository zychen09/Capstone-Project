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


