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

