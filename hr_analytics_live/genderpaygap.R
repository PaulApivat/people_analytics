# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# Case Study: Gender Pay Gap Audit
# source: "How to Audit Your Gender Pay Gap: An Employers Guide Using R" (Andrew Chamberlain, Glassdoor)
# site: https://hranalyticslive.netlify.app/13-pay-gap.html
# site maintainer: Hendrik Feddersen

# load libraries
library(tidyverse)
library(tidymodels)
library(devtools)

# warning
install.packages('HRAnalytics')  #package ‘HRAnalytics’ is not available (for R version 3.6.3)

# turn off scientific notation
options(scipen = 999)

# load data
gd_data = read_csv("https://glassdoor.box.com/shared/static/beukjzgrsu35fqe59f7502hruribd5tt.csv")

###### DATA CLEANING AND PREP ######
# NOTE: new way to create age-brackets as factors

gd_data %>%
    mutate(age_bin = cut(age, 
                    breaks = c(0, 25, 35, 45, 55, Inf), 
                    right = FALSE)) %>%
    # create new column: total compensation
    mutate(total_pay = basePay + bonus) %>%
    # create log of compensation 
    mutate(log_base = log(basePay, base = exp(1)), 
             log_total = log(total_pay, base = exp(1)), 
             # Add +1 to allow for log of 0 bonus values
             log_bonus = log(bonus + 1, base = exp(1))) %>%
    # create flags
    mutate_if(is_character, fct_infreq) %>% 
    mutate(age_bin = fct_infreq(age_bin)) -> gd_data_clean

gd_data_clean

## quickly find missing data in any column
sapply(gd_data_clean, function(x) sum(is.na(x)))

# get summary statistics for BASEPAY by gender
gd_data_clean %>%
    filter(!is.na(basePay)) %>%
    group_by(gender) %>%
    summarize(mean_base = mean(basePay), 
              median_base = median(basePay), 
              count = n()) -> gd_summary_gender_base

gd_summary_gender_base

# get summary statistic for TOTAL pay by gender
gd_data_clean %>% 
    filter(!is.na(total_pay)) %>% 
    group_by(gender) %>% 
    summarize(total_mean = mean(total_pay), 
              total_median = median(total_pay), 
              count = n()) -> gd_summary_gender_total

gd_summary_gender_total


# get summary statistic for BONUS pay by gender
gd_data_clean %>% 
    filter(!is.na(total_pay)) %>% 
    group_by(gender) %>% 
    summarize(bonus_mean = mean(bonus), 
              bonus_median = median(bonus), 
              count = n()) -> gd_summary_gender_bonus

gd_summary_gender_bonus

