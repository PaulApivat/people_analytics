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

### Initial Exploratory Data Analysis

# NOTE: geom_bar(stat = 'count') can substitute group_by

# Number of Job Classifications per PG category
MYdataset %>% 
    ggplot() 
    + aes(x = factor(PG)) 
    + geom_bar(stat = 'count', width = 0.7, fill = 'steelblue') 
    + theme_minimal() 
    + coord_flip() 
    + labs(title = 'Number of job classifications per PG category')

# Number of Job Classifications per Job Family
MYdataset %>% 
    ggplot() 
    + aes(x = factor(JobFamilyDescription)) 
    + geom_bar(stat = 'count', width = 0.7, fill = 'steelblue') 
    + theme_minimal() 
    + coord_flip() 
    + labs(title = 'Number of job classifications per job family')

# group variable before plotting (alternative to above)
MYdataset %>% 
    group_by(JobFamilyDescription) %>% 
    tally(sort = TRUE) %>% 
    ggplot(aes(x=JobFamilyDescription, y=n)) 
    + geom_bar(stat = 'identity', fill = 'steelblue', width = 0.7) 
    + theme_minimal() 
    + coord_flip() 
    + labs(title = 'Number of job classification per job family')

# Number of job classifications per Education Level
MYdataset %>% 
    ggplot() 
    + aes(EducationLevel) 
    + geom_bar(stat = 'count', width = 0.7, fill = 'steelblue') 
    + ggtitle('Number of job classifications per Education level')

MYdataset %>% 
    group_by(EducationLevel) %>% 
    tally(sort = TRUE) %>% 
    ggplot(aes(x=reorder(EducationLevel, n), y=n)) 
    + geom_bar(stat = 'identity', width = 0.7, fill = 'steelblue') 
    + labs(title = 'Number of job classifications per Education Level')

# Number of job classifications per Experience level
MYdataset %>% 
    ggplot() 
    + aes(Experience) 
    + geom_bar(stat = 'count', width = 0.7, fill = 'steelblue') 
    + labs(title = 'Number of job classifications per experience')

# Number of job classification per Problem Solving
MYdataset %>% 
    ggplot() 
    + aes(ProblemSolving) 
    + geom_bar(stat = 'count', width = 0.7, fill = 'steelblue') 
    + labs(title = 'Number of job classifications per problem solving')

# Number of job classification per Supervision
MYdataset %>% 
    ggplot() 
    + aes(Supervision) 
    + geom_bar(stat = 'count', width = 0.7, fill = 'steelblue') 
    + labs(title = 'Number of job classifications per supervision')

# Number of job classification per Contact level
MYdataset %>% 
    ggplot() 
    + aes(ContactLevel) 
    + geom_bar(stat = 'count', width = 0.7, fill = 'steelblue') 
    + labs(title = 'Number of job classifications per contact level')

### Use Caret package for graphical representation of Predictors of Pay Grade (PG)
library(caret)

MYdataset$PG <- as.factor(MYdataset$PG)

# distribution of predictors of PG
featurePlot(x = MYdataset[,7:13], 
            y = MYdataset$PG, 
            plot = 'density', 
            auto.key = list(columns = 2))

# range of values of predictors by PG
# PG ordered in ascending order PG1 to PG10
# we expect increasing levels as we move up paygrades from left to right
featurePlot(x = MYdataset[,7:13], 
            y = MYdataset$PG, 
            plot = 'box', 
            auto.key = list(columns = 2))

##### ------------- Build the MODEL -------------- #####

#### Four classification algorithms:
# decision tree
# random forest
# support vector machines
# linear regression model

### Decision Tree Algorithm

library(rattle)
library(rpart, quietly=TRUE)

# crv = current rattle variables
# reset random number seed to obtain same results each time
crv$seed <- 42
set.seed(crv$seed)

# Build decision tree model
# note: need to interpret classification tree
MYrpart <- rpart(PG ~ ., 
                data = MYdataset[, c(MYinput, MYtarget)], 
                method = 'class', 
                parms = list(split='information'), 
                control = rpart.control(minsplit = 10, 
                minbucket = 2, 
                maxdepth = 10, 
                usesurrogate = 0, 
                maxsurrogate = 0))

print(MYrpart)

printcp(MYrpart)

### Random Forest Algorithm
library(randomForest, quietly=TRUE)

set.seed(crv$seed)

MYrf <- randomForest::randomForest(PG ~ ., 
                        data=MYdataset[,c(MYinput, MYtarget)], 
                        ntree=500, 
                        mtry=2, 
                        importance=TRUE, 
                        na.action=randomForest::na.roughfix, replace=FALSE)

rn <- round(randomForest::importance(MYrf), 2)
rn[order(rn[,3], decreasing = TRUE),]