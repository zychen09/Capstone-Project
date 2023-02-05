test<-y1_test_dset_4uw_v2
match<- test %>%
  select( mod_raw_time_pw, vig_raw_time_pw, vervig_raw_time_pw,mvpa_raw_time_pw,ss5,ss3)%>%
  na.omit()

match$mvpa2<- ifelse( match$mvpa_raw_time_pw >= 150,1,0)
match$mvpa3<- ifelse( match$mvpa_raw_time_pw < 150, ifelse(match$mvpa_raw_time_pw >=75, 2,1),3)
match$bi_ss3<- ifelse( match$ss3=='Yes',1,0)
match$bi_ss5<- ifelse( match$ss5=='Low',1,ifelse(match$ss5=='Moderate',2,3))

#Table 1_2
library (table1)
table1(~factor(mvpa2)+mvpa_raw_time_pw| factor(match$ss3), data=match)
table1(~factor(mvpa3)+mvpa_raw_time_pw| factor(match$ss5), data=match)

#EDA
#All actigraph data(6)
test2<- test %>%
  select(StudyId, BirthYear, BirthSex, Race, mvpa_raw_time_pw, mod_raw_time_pw, vig_raw_time_pw, vervig_raw_time_pw,ss5,ss3)%>%
  na.omit()
test2["Race"][test2["Race"] == ''] <- NA
head(test2)
test2_long<- reshape2::melt(test2, id.vars=c('StudyId','BirthYear', 'BirthSex', 'Race'), variable.name = "type_variable", value.name = "time_value")


#Histgram
ggplot(test2, aes(x=mvpa_raw_time_pw)) + 
  geom_histogram(bins = 40,color="#E69F00", fill="orange", alpha=0.2)+  
  labs(title="Actigraphy Histgram of mvpa",x="Actigraphy_mvpa")

# Change line colors by groups
ggplot(test2, aes(x=mvpa_raw_time_pw, color=ss3, fill=ss3)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title=" Actigraphy Histogram plot ",x="Actigraphy_mvpa", y = "Density")+
  theme_classic()

ggplot(test2, aes(x=mvpa_raw_time_pw, color=BirthSex, fill=BirthSex)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title=" Actigraphy Histogram plot group by birthsex",x="Actigraphy_mvpa", y = "Density")+
  theme_classic()

#Boxplot
ggplot(match, aes(x = ss3, y = mvpa_raw_time_pw)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  stat_summary(fun = "mean", geom = "point")+
  labs(title="Plot of two categories",x="Self_report_ss3", y = "Actigraphy_mvpa")

ggplot(match, aes(x = ss5, y = mvpa_raw_time_pw)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  stat_summary(fun = "mean", geom = "point")+
  labs(title="Plot of three categories",x="Self_report_ss5", y = "Actigraphy_mvpa")


#Weighted Kappa1
library(psych)
match$mvpa2<-as.factor(match$mvpa2)
match$mvpa3<-as.factor(match$mvpa3)
match$bi_ss3<-as.factor(match$bi_ss3)
match$bi_ss5<-as.factor(match$bi_ss5)
cohen.kappa(x=cbind(match$bi_ss3,match$mvpa2))
cohen.kappa(x=cbind(match$bi_ss5,match$mvpa3))

#Weighted Kappa2
library(irr)
kappa2(cbind(match$bi_ss3,match$mvpa2), "squared")
kappa2(cbind(match$bi_ss3,match$mvpa2), "equal")

kappa2(cbind(match$bi_ss5,match$mvpa3), "squared")
kappa2(cbind(match$bi_ss5,match$mvpa3), "equal")


