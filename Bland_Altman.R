
### A four-point scoring system for LRCQ

library(tidyverse)
ba_df <- comb_df %>%
  select(`Study ID`, Ss1, Ss2, Ss3, Ss4, Ss5, 
         light_raw_time_pw, mod_raw_time_pw, vig_raw_time_pw, vervig_raw_time_pw) %>%
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
  ),
  score_mod = case_when(
    mod_raw_time_pw <=  75 ~ 1,
    mod_raw_time_pw >=150 ~ 3,
    TRUE ~ 2
  ),
  score_vig = case_when(
    vig_raw_time_pw <=  75 ~ 1,
    vig_raw_time_pw >=150 ~ 3,
    TRUE ~ 2
  ),
  score_vervig = case_when(
    vervig_raw_time_pw <=  75 ~ 1,
    vervig_raw_time_pw >=150 ~ 3,
    TRUE ~ 2
  ),
  score_light = case_when(
    light_raw_time_pw <=  75 ~ 1,
    light_raw_time_pw >=150 ~ 3,
    TRUE ~ 2
  ))


# create new column for average measurement
## compare light activity under 3-point scoring

ba_light3 <- ba_df %>% select(score3, score_light)
ba_light3$avg <- rowMeans(ba_light3, na.rm = TRUE)
ba_light3$diff <- with(ba_light3, score_light - score3)

#find average difference
mean_diff <- mean(ba_light3$diff, na.rm = TRUE)

#find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(ba_light3$diff, na.rm = TRUE)
upper <- mean_diff + 1.96*sd(ba_light3$diff, na.rm = TRUE)


library(ggplot2)
#create Bland-Altman plot
g_light <- ggplot(ba_light3, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot - Light Activity") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement") + 
  theme(plot.title = element_text(size = 12))



## compare mod activity under 3-point scoring

ba_mod3 <- ba_df %>% select(score3, score_mod)
ba_mod3$avg <- rowMeans(ba_mod3, na.rm = TRUE)
ba_mod3$diff <- with(ba_mod3, score_mod - score3)

#find average difference
mean_diff <- mean(ba_mod3$diff, na.rm = TRUE)

#find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(ba_mod3$diff, na.rm = TRUE)
upper <- mean_diff + 1.96*sd(ba_mod3$diff, na.rm = TRUE)


#create Bland-Altman plot
g_mod <- ggplot(ba_mod3, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot - Moderate Activity") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement") + 
  theme(plot.title = element_text(size = 12))



## compare vig activity under 3-point scoring

ba_vig3 <- ba_df %>% select(score3, score_vig)
ba_vig3$avg <- rowMeans(ba_vig3, na.rm = TRUE)
ba_vig3$diff <- with(ba_vig3, score_vig - score3)

#find average difference
mean_diff <- mean(ba_vig3$diff, na.rm = TRUE)

#find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(ba_vig3$diff, na.rm = TRUE)
upper <- mean_diff + 1.96*sd(ba_vig3$diff, na.rm = TRUE)

#create Bland-Altman plot
g_vig <- ggplot(ba_vig3, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot - Vigorous Activity") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement") + 
  theme(plot.title = element_text(size = 12))



## compare very vig activity under 3-point scoring

ba_vervig3 <- ba_df %>% select(score3, score_vervig)
ba_vervig3$avg <- rowMeans(ba_vervig3, na.rm = TRUE)
ba_vervig3$diff <- with(ba_vervig3, score_vervig - score3)

#find average difference
mean_diff <- mean(ba_vervig3$diff, na.rm = TRUE)

#find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(ba_vervig3$diff, na.rm = TRUE)
upper <- mean_diff + 1.96*sd(ba_vervig3$diff, na.rm = TRUE)

#create Bland-Altman plot
g_vervig <- ggplot(ba_vervig3, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot - Very Vigorous Activity") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement") + 
  theme(plot.title = element_text(size = 12))


pdf("ba_plots.pdf", onefile = TRUE, width = 8, height = 8)
gridExtra::grid.arrange(g_light, g_mod, g_vig, g_vervig)
dev.off()



################################# scale the actigraph

ba_df_s <- comb_df %>%
  select(`Study ID`, Ss1, Ss2, Ss3, Ss4, Ss5, 
         light_raw_time_pw, mod_raw_time_pw, vig_raw_time_pw, vervig_raw_time_pw) %>%
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
  ),
  s_mod = scale(mod_raw_time_pw, center = FALSE),
  s_vig = scale(vig_raw_time_pw, center = FALSE),
  s_vervig = scale(vervig_raw_time_pw, center = FALSE),
  s_light = scale(light_raw_time_pw, center = FALSE))


# create new column for average measurement
## compare mod activity under 3-point scoring

ba_mod3_s <- ba_df_s %>% select(score3, s_mod)
ba_mod3_s$avg <- rowMeans(ba_mod3_s, na.rm = TRUE)
ba_mod3_s$diff <- with(ba_mod3_s, s_mod - score3)

#find average difference
mean_diff <- mean(ba_mod3_s$diff, na.rm = TRUE)

#find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(ba_mod3_s$diff, na.rm = TRUE)
upper <- mean_diff + 1.96*sd(ba_mod3_s$diff, na.rm = TRUE)


#create Bland-Altman plot
g_mod_s <- ggplot(ba_mod3_s, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot - Moderate Activity(Scaled)") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement") + 
  theme(plot.title = element_text(size = 12))

## compare light activity under 3-point scoring

ba_light3_s <- ba_df_s %>% select(score3, s_light)
ba_light3_s$avg <- rowMeans(ba_light3_s, na.rm = TRUE)
ba_light3_s$diff <- with(ba_light3_s, s_light - score3)

#find average difference
mean_diff <- mean(ba_light3_s$diff, na.rm = TRUE)

#find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(ba_light3_s$diff, na.rm = TRUE)
upper <- mean_diff + 1.96*sd(ba_light3_s$diff, na.rm = TRUE)

#create Bland-Altman plot
g_light_s <- ggplot(ba_light3_s, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot - Light Activity(Scaled)") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement") + 
  theme(plot.title = element_text(size = 12))



## compare vig activity under 3-point scoring

ba_vig3_s <- ba_df_s %>% select(score3, s_vig)
ba_vig3_s$avg <- rowMeans(ba_vig3_s, na.rm = TRUE)
ba_vig3_s$diff <- with(ba_vig3_s, s_vig - score3)

#find average difference
mean_diff <- mean(ba_vig3_s$diff, na.rm = TRUE)

#find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(ba_vig3_s$diff, na.rm = TRUE)
upper <- mean_diff + 1.96*sd(ba_vig3_s$diff, na.rm = TRUE)

#create Bland-Altman plot
g_vig_s <- ggplot(ba_vig3_s, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot - Vigorous Activity(Scaled)") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement") + 
  theme(plot.title = element_text(size = 12))



## compare very vig activity under 3-point scoring

ba_vervig3_s <- ba_df_s %>% select(score3, s_vervig)
ba_vervig3_s$avg <- rowMeans(ba_vervig3_s, na.rm = TRUE)
ba_vervig3_s$diff <- with(ba_vervig3_s, s_vervig - score3)

#find average difference
mean_diff <- mean(ba_vervig3_s$diff, na.rm = TRUE)

#find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(ba_vervig3_s$diff, na.rm = TRUE)
upper <- mean_diff + 1.96*sd(ba_vervig3_s$diff, na.rm = TRUE)

#create Bland-Altman plot
g_vervig_s <- ggplot(ba_vervig3_s, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot - Very Vigorous Activity(Scaled)") +
  ylab("Difference Between Measurements") + xlab("Average Measurement") + 
  theme(plot.title = element_text(size = 11))


pdf("ba_plots_scaled.pdf", onefile = TRUE, width = 8, height = 8)
gridExtra::grid.arrange(g_light_s, g_mod_s, g_vig_s, g_vervig_s)
dev.off()

