
### A four-point scoring system for LRCQ

library(tidyverse)
ba_df <- comb_df %>%
  select(`Study ID`, Ss1, Ss2, Ss3, Ss4, Ss5, 
         light_raw_time_pw, mod_raw_time_pw, vig_raw_time_pw) %>%
  mutate(score4 = case_when(
    Ss4 == 1 & Ss2 <= 2 ~ 4,
    Ss4 == 1 & Ss2 == 3 ~ 3,
    Ss4 == 0 & Ss2 <= 2 ~ 2,
    Ss4 == 0 & Ss2 %in% c(5,6) ~ 1
  ),
  score3 = case_when(
    Ss5 == 1 ~ 3,
    Ss5 == 2 ~ 2,
    Ss5 == 3 ~ 1,
  ))


# create new column for average measurement
## compare light activity under 3-point scoring

ba_light3 <- ba_df %>% select(score3, light_raw_time_pw)
ba_light3$avg <- rowMeans(ba_light3, na.rm = TRUE)
ba_light3$diff <- with(ba_light3, light_raw_time_pw - score3)

#find average difference
mean_diff <- mean(ba_light3$diff, na.rm = TRUE)

#find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(ba_light3$diff, na.rm = TRUE)
upper <- mean_diff + 1.96*sd(ba_light3$diff, na.rm = TRUE)


library(ggplot2)
#create Bland-Altman plot
ggplot(ba_light3, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement")


