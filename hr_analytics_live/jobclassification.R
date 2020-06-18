# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# Case Study: Job Classification
# source: https://hranalyticslive.netlify.app/19-job-classification.html

# load
load(file = "jobclassification.RData")

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

### Support Vector Machine
library(kernlab, quietly=TRUE)

# build support vector machine model
set.seed(crv$seed)

MYksvm <- ksvm(as.factor(PG) ~., 
               data=MYdataset[,c(MYinput, MYtarget)], 
               kernel='rbfdot', 
               prob.model=TRUE)

MYksvm

### Linear Regression Model
library(nnet, quietly=TRUE)
library(car, quietly=TRUE)

MYglm <- multinom(PG ~ ., 
        data = MYdataset[,c(MYinput, MYtarget)], 
        trace=FALSE, maxit=1000)

rattle.print.summary.multinom(summary(MYglm, Wald.ratios = TRUE))

# Log Likelihood: -6.545 (72 df)
cat(sprintf("Log Likelihood: %.3f (%d df)", logLik(MYglm)[1], attr(logLik(MYglm), "df")))

# Pseudo R-Square: 0.99516038
if (is.null(MYglm$na.action)) omitted <- TRUE else omitted <- -MYglm$na.action
cat(sprintf("Pseudo R-Square: %.8f",cor(apply(MYglm$fitted.values, 1, function(x) which(x == max(x))),
as.integer(MYdataset[omitted,]$PG))))

cat('==== ANOVA ====')

print(Anova(MYglm))

##### PLOT DECISION TREE

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(MYrpart, main="Decision Tree MYdataset $ PG")


##### Evaluation of Best Fitting Model

##### Predict new job classsifications utilising the Decision Tree model.

MYpr <- predict(MYrpart, newdata=MYdataset[,c(MYinput, MYtarget)], type="class")

# Generate the confusion matrix showing counts.

table(MYdataset[,c(MYinput, MYtarget)]$PG, MYpr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions and misclassification error in the last column. Misclassification error, represents how often is the prediction wrong,

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}

per <- pcme(MYdataset[,c(MYinput, MYtarget)]$PG, MYpr)
round(per, 2)

# First we calculate the overall miscalculation rate (also known as error rate or percentage error).
#Please note that diag(per) extracts the diagonal of confusion matrix.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2)) # 23%

# Calculate the averaged miscalculation rate for each job classification. 
# per[,"Error"] extracts the last column, which represents the miscalculation rate per.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))  # 28%

##### RANDOM FOREST ######
# Generate the Confusion Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

MYpr <- predict(MYrf, newdata=na.omit(MYdataset[,c(MYinput, MYtarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(MYdataset[,c(MYinput, MYtarget)])$PG, MYpr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}

per <- pcme(na.omit(MYdataset[,c(MYinput, MYtarget)])$PG, MYpr)
round(per, 2)

# Calculate the overall error percentage. (5%)

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage. (9%)

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

####### Support Vector Machine

# Generate the Confusion Matrix for the SVM model.

# Obtain the response from the SVM model.

MYpr <- kernlab::predict(MYksvm, newdata=na.omit(MYdataset[,c(MYinput, MYtarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(MYdataset[,c(MYinput, MYtarget)])$PG, MYpr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}

per <- pcme(na.omit(MYdataset[,c(MYinput, MYtarget)])$PG, MYpr)
round(per, 2)

# Calculate the overall error percentage (18%)

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage (29%)

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

########## LINEAR REGRESSION

# Generate the confusion matrix for the linear regression model.

# Obtain the response from the Linear model.

MYpr <- predict(MYglm, newdata=MYdataset[,c(MYinput, MYtarget)])

# Generate the confusion matrix showing counts.

table(MYdataset[,c(MYinput, MYtarget)]$PG, MYpr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}

per <- pcme(MYdataset[,c(MYinput, MYtarget)]$PG, MYpr)
round(per, 2)

# Calculate the overall error percentage. (6%)

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage (11%)

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

########### DEPLOY THE MODEL #############

DeployDataset <- readxl::read_xlsx("Deploydata.xlsx")

# predicted job grade is PG05
PredictedJobGrade <- predict(MYrf, newdata=DeployDataset)
PredictedJobGrade

