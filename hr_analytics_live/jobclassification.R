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


