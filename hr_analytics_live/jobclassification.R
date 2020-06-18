# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# Case Study: Job Classification
# source: https://hranalyticslive.netlify.app/19-job-classification.html

# library
library(tidyverse)
library(caret)
library(rattle)
library(rpart)
library(randomForest)
library(kernlab)
library(nnet)
library(car)
library(rpart.plot)
library(pROC)
library(ada)

# load multiple libaries
multiple_libraries <- c('tidyverse', 'caret', 'rattle', 'rpart', 'randomForest', 'kernlab', 'nnet', 'car', 'rpart.plot', 'pROC', 'ada')

lapply(multiple_libraries, require, character.only = TRUE)

# NOT FOUND
MYdataset <- read_csv("https://hranalytics.netlify.com/data/jobclassinfo2.csv")

## downloaded jobclassinfo2.csv from Archive HR Datasets
## source: https://hranalyticslive.netlify.app/f-appendixf

df <- read_csv("jobclassinfo2.csv")

## at-a-glance view of data
## note: jobclassinfo2.csv contains (subjective?) eval data that was made prior (ie., 'OrgImpact', 'ProblemSolving')

str(df)
summary(df)

######### Job Classification Objective: predict PG category (representation of numeric paygrade) ##########

#### Organize the data
#### note: narrow down info to just data used in the model
MYdataset <- df

# number of observations (rows)
MYnobs <- nrow(df)

# 70% of observation forms training dataset (n = 46)
MYsample <- df_train <- sample(nrow(df), 0.7*df_obs)

# 14% of observations forms validation dataset (n = 9)
MYvalidate <- sample(setdiff(seq_len(nrow(df)), df_train), 0.14*df_obs)

# remaining observations forms test dataset (n = 11)
MYtest <- setdiff(setdiff(seq_len(nrow(df)), df_train), df_validate)


# The following variable selections have been noted.
MYinput <- c("EducationLevel", "Experience", "OrgImpact", "ProblemSolving",
     "Supervision", "ContactLevel", "FinancialBudget")

MYnumeric <- c("EducationLevel", "Experience", "OrgImpact", "ProblemSolving",
     "Supervision", "ContactLevel", "FinancialBudget")

MYcategoric <- NULL

MYtarget  <- "PG"
MYrisk    <- NULL
MYident   <- "ID"
MYignore  <- c("JobFamily", "JobFamilyDescription", "JobClass", "JobClassDescription", "PayGrade")
MYweights <- NULL

