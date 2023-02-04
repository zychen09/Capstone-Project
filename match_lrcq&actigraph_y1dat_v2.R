rm(list = ls())
library(tidyverse)
library(readxl)
y1dat <- read_excel('./data/y1_test_dset_4uw_v2.xlsx', col_names = T)
lrcq <- readr::read_csv("./data/PROSPERHIV-LipidData_DATA_NOHDRS_2023-01-11_1310.csv", col_names = F)
colnames(lrcq)[1:10] <- c("Study ID", "Substudy Class", "Ss1","Ss2","Ss3","Ss4","Ss5",
                             "Completed substudy", "Valid result", "Test completed date")

View(y1dat[ ,c("lipid_research_clini_v_0", "light_raw_time_pw")])
View(y1dat[ ,])
sum(is.na(y1dat$lipid_research_clini_v_0)) # missing lrcq entry: 492/623
sum(is.na(y1dat$mvpa_raw_time_pw)) # missing actigraph entry: 182/623

## matched on `record_id`
y1dat_v2_acti_id <- y1dat[which(!is.na(y1dat$mvpa_raw_time_pw)), "record_id"]
lrcq_v2_id <- y1dat[which(!is.na(y1dat$lipid_research_clini_v_0)), "record_id"]
all_lrcq_id <- unique(c(lrcq_v2_id$record_id, lrcq$`Study ID`))

matched_id <- lrcq_v2_id$record_id[lrcq_v2_id$record_id %in% y1dat_v2_acti_id$record_id]
setdiff(all_lrcq_id[all_lrcq_id %in% y1dat_v2_acti_id$record_id], matched_id)# based on this check, lrcq information is duplicated

## output the matched set

sub_y1dat <- y1dat[y1dat$record_id %in% matched_id, ]
View(sub_y1dat[ ,c("lipid_research_clini_v_0", "mvpa_raw_time_pw")])

