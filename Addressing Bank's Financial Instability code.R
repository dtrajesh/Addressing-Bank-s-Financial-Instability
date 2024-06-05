#---------------------------------Importing Libraries------------------------------------------
library(tidyverse)
library(dplyr)
library(caret)
library(fastDummies)
library(ggplot2)
library(gridExtra)
library(GGally)
library(MASS)
library(C50)
require(randomForest)
library(e1071)
library(rpart.plot)
library(psych)
library(plotly)
library(randomForest)
library(gains)
library(RWeka)
library(gplots)
library(plotly)
library(VIM)
library(class)
library(e1071)
library(rvest)	
library(matrixStats)
require(cluster)
library(caret)
#--------------------------------Step 1 Collecting data-----------------------------------------------
#loading the data
mortgage.df <- read.csv("mortgage.csv")

head(mortgage.df)

#Aggregating the data
mortgage.df.agg <- mortgage.df %>%
  group_by(id) %>%
  summarise(time = last(time),
            mat_time = last(mat_time),
            balance_time= last(balance_time), 
            LTV_time = last(LTV_time), 
            interest_rate_time = last(interest_rate_time), 
            hpi_time = last(hpi_time), 
            gdp_time = last(gdp_time), 
            uer_time = last(uer_time), 
            default_time = last(default_time), 
            payoff_time = last(payoff_time),  
            status_time = last(status_time),
            orig_time = as.numeric(names(sort(table(orig_time), decreasing = TRUE))[1]),
            first_time = first(first_time),
            REtype_CO_orig_time = names(sort(table(REtype_CO_orig_time), decreasing = TRUE))[1],
            REtype_PU_orig_time = names(sort(table(REtype_PU_orig_time), decreasing = TRUE))[1],
            REtype_SF_orig_time = names(sort(table(REtype_SF_orig_time), decreasing = TRUE))[1],
            investor_orig_time = as.numeric(names(sort(table(investor_orig_time), decreasing = TRUE))[1]),
            balance_orig_time = as.numeric(names(sort(table(balance_orig_time), decreasing = TRUE))[1]),
            FICO_orig_time = as.numeric(names(sort(table(FICO_orig_time), decreasing = TRUE))[1]),
            LTV_orig_time = as.numeric(names(sort(table(LTV_orig_time), decreasing = TRUE))[1]),
            Interest_Rate_orig_time = as.numeric(names(sort(table(Interest_Rate_orig_time), decreasing = TRUE))[1]),
            hpi_orig_time = as.numeric(names(sort(table(hpi_orig_time), decreasing = TRUE))[1])) %>%
              ungroup()
mortgage.df.agg <- as.data.frame(mortgage.df.agg)

head(mortgage.df.agg)


#Check for NA values in each column
colSums(is.na(mortgage.df.agg))

#replacing the missing values using knn
set.seed(1)
mortgage.df.agg <- kNN(mortgage.df.agg,imp_var = FALSE)

#check for zero values
colSums(mortgage.df.agg==0,na.rm = TRUE)

mortgage.df.agg <- mortgage.df.agg %>%
  mutate(Interest_Rate_orig_time = ifelse(Interest_Rate_orig_time == 0, mean(Interest_Rate_orig_time, na.rm = TRUE), Interest_Rate_orig_time)) %>%
  mutate(interest_rate_time = ifelse(interest_rate_time == 0, mean(interest_rate_time, na.rm = TRUE), Interest_Rate_orig_time)) %>%
  mutate(balance_time = ifelse(balance_time == 0, balance_orig_time, balance_time)) %>%
  mutate(balance_time = ifelse(balance_time == 0, balance_orig_time, balance_time)) %>%
  mutate(LTV_time = ifelse(LTV_time == 0, LTV_orig_time, LTV_time))

#Removing rows that has balance_orig_time zero
mortgage.df.agg <- mortgage.df.agg %>%
  filter(balance_orig_time  != 0)

#Structure of the data
str(mortgage.df.agg)

#Adding new columns
mortgage.df.agg <- mortgage.df.agg %>%
  mutate(loan_term = mortgage.df.agg$mat_time - mortgage.df.agg$orig_time) %>%
  mutate(months_paid = mortgage.df.agg$time - mortgage.df.agg$first_time)



str(mortgage.df.agg)

#Removing rows that has status_time 0
mortgage.df.agg <- mortgage.df.agg %>%
  filter(status_time  != 0)


#Factoring the columns
mortgage.df.agg = mortgage.df.agg %>% mutate(status_time=factor(if_else(status_time == 2, "payoff", "default"), 
                                                                levels=c("payoff","default"),
                                                                labels=c("payoff","default")),
                                             REtype_CO_orig_time = as.factor(REtype_CO_orig_time),
                                             REtype_PU_orig_time = as.factor(REtype_PU_orig_time),
                                             REtype_SF_orig_time  = as.factor(REtype_SF_orig_time))

#Summary of the columns
summary(mortgage.df.agg[,c(-1,-2,-10,-11,-13)])

#Mat_time column
#mat_time vs status_time
plot_ly(mortgage.df.agg, x = ~mat_time, color = ~status_time , type = "box")

#mat_time vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~mat_time, y = ~balance_orig_time)


#balance_time
#balance_time vs status_time
plot_ly(mortgage.df.agg, x = ~balance_time, color = ~status_time, type = "box")

#LTV_time 
#LTV_time  vs status_time
plot_ly(mortgage.df.agg, x = ~LTV_time, color = ~status_time, type = "box")

#LTV_time vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~LTV_time, y = ~balance_orig_time)

#interest_rate_time 
#interest_rate_time  vs status_time
plot_ly(mortgage.df.agg, x = ~interest_rate_time, color = ~status_time, type = "box")

#interest_rate_time vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~interest_rate_time, y = ~balance_orig_time)

#gdp_time 
#gdp_time  vs status_time
plot_ly(mortgage.df.agg, x = ~gdp_time, color = ~status_time, type = "box")

#gdp_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~gdp_time, y=~balance_orig_time)

#uer_time
#uer_time  vs status_time
plot_ly(mortgage.df.agg, x = ~uer_time, color = ~status_time, type = "box")
#uer_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~uer_time, y=~balance_orig_time)

#first_time
#first_time  vs status_time
plot_ly(mortgage.df.agg, x = ~first_time, color = ~status_time, type = "box")
#first_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~first_time, y=~balance_orig_time)

#REtype_CO_orig_time
#REtype_CO_orig_time  vs status_time
barplot(table(mortgage.df.agg$REtype_CO_orig_time,mortgage.df.agg$status_time),
        legend.text = TRUE,
        beside = TRUE,
        main = "status_time vs REtype_CO_orig_time",
        xlab = "status_time",
        ylab = "REtype_CO_orig_time")
#REtype_CO_orig_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~balance_orig_time, color = ~REtype_CO_orig_time, type = "box")


#REtype_PU_orig_time
#REtype_PU_orig_time  vs status_time
barplot(table(mortgage.df.agg$REtype_PU_orig_time,mortgage.df.agg$status_time),
        legend.text = TRUE,
        beside = TRUE,
        main = "status_time vs REtype_PU_orig_time",
        xlab = "status_time",
        ylab = "REtype_PU_orig_time")
#REtype_PU_orig_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~balance_orig_time, color = ~REtype_PU_orig_time, type = "box")


#REtype_SF_orig_time
#REtype_SF_orig_time  vs status_time
barplot(table(mortgage.df.agg$REtype_SF_orig_time,mortgage.df.agg$status_time),
        legend.text = TRUE,
        beside = TRUE,
        main = "status_time vs REtype_SF_orig_time",
        xlab = "status_time",
        ylab = "REtype_SF_orig_time")
#REtype_SF_orig_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~balance_orig_time, color = ~REtype_SF_orig_time, type = "box")

#FICO_orig_time
#FICO_orig_time  vs status_time
plot_ly(mortgage.df.agg, x = ~FICO_orig_time, color = ~status_time, type = "box")
#FICO_orig_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~FICO_orig_time, y = ~balance_orig_time)


#LTV_orig_time
#LTV_orig_time  vs status_time
plot_ly(mortgage.df.agg, x = ~LTV_orig_time, color = ~status_time, type = "box")
#LTV_orig_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~LTV_orig_time, y = ~balance_orig_time)


#interest_rate_orig_time
#Interest_Rate_orig_time  vs status_time
plot_ly(mortgage.df.agg, x = ~Interest_Rate_orig_time, color = ~status_time, type = "box")
#Interest_Rate_orig_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~Interest_Rate_orig_time, y = ~balance_orig_time)

#hpi_orig_time
#hpi_orig_time  vs status_time
plot_ly(mortgage.df.agg, x = ~hpi_orig_time, color = ~status_time, type = "box")
#hpi_orig_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~hpi_orig_time, y = ~balance_orig_time)

#hpi_time
#hpi_time  vs status_time
plot_ly(mortgage.df.agg, x = ~hpi_time, color = ~status_time, type = "box")
#hpi_time  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~hpi_time, y = ~balance_orig_time)

#loan_term
#loan_term  vs status_time
plot_ly(mortgage.df.agg, x = ~loan_term, color = ~status_time, type = "box")
#loan_term  vs balance_orig_time
plot_ly(mortgage.df.agg, x = ~loan_term, y = ~balance_orig_time)

#months_paid
#months_paid  vs status_time
plot_ly(mortgage.df.agg, x = ~months_paid, color = ~status_time, type = "box")



#Correlation analysis for all numerical columns
cor(mortgage.df.agg[,c(3:9,18:25)])


head(mortgage.df.agg[,c(-1,-2,-10,-11,-13)])

#setting seed value for repetition
set.seed(1)

# partitioning into training (60%), validation (20%) and test (20%)

train.rows <- sample(rownames(mortgage.df.agg),size = nrow(mortgage.df.agg)*0.6)
valid.rows <- sample(setdiff(rownames(mortgage.df.agg), train.rows),
                     size =nrow(mortgage.df.agg)*0.20)
test.rows <- setdiff(rownames(mortgage.df.agg), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.df <- mortgage.df.agg[train.rows, ]
valid.df <- mortgage.df.agg[valid.rows, ]
test.df <- mortgage.df.agg[test.rows, ]

#Getting the dimensions of the partition data

dim(train.df)
dim(valid.df)
dim(test.df)


#Decison tree
decision.tree.model <- C5.0(train.df[,c(8,9,15:18,20,21,23)], train.df$status_time)
summary(decision.tree.model)

# ploting the tree
plot(decision.tree.model,type='simple')

#Evaluating model performance 
#predicting on valid data
decision.tree.model.valid.pred <- predict(decision.tree.model, valid.df[,c(8,9,15:18,20,21,23)])
#confusion matrix
confusionMatrix(decision.tree.model.valid.pred,valid.df$status_time,positive = "payoff")

#Decison tree with trail 6
decision.tree.model.trail <- C5.0(train.df[,c(8,9,15:18,20,21,23)], train.df$status_time,trials=6)
summary(decision.tree.model.trail)

# ploting the tree
plot(decision.tree.model.trail)

#Evaluating model performance 
#predicting on valid data
decision.tree.trail.model.valid.pred <- predict(decision.tree.model.trail, valid.df[,c(8,9,15:18,20,21,23)])
#confusion matrix
confusionMatrix(decision.tree.trail.model.valid.pred,valid.df$status_time,positive = "payoff")

# Logistic regression
#Converting yes to 1 and no to 0
train.df <- train.df %>% mutate( status_time_num= ifelse(status_time == 'payoff', 1, 0))
valid.df <- valid.df %>% mutate( status_time_num= ifelse(status_time == 'payoff', 1, 0))
test.df <- test.df %>% mutate( status_time_num= ifelse(status_time == 'payoff', 1, 0))

logis.model <- glm(status_time_num ~ .,data=train.df[,c(8,9,15:18,20,21,23,26)],family = "binomial")
#summary of the model
summary(logis.model)

#Predicting the validation data
model.logistic.valid.pred <- predict(logis.model, valid.df[,c(8,9,15:18,20,21,23,26)])

confusionMatrix(factor(ifelse(model.logistic.valid.pred > 0.5,'payoff','default'),labels = c('payoff','default'),levels = c('payoff','default')
                       ),factor(valid.df$status_time,labels = c('payoff','default'),levels = c('payoff','default')),positive = 'payoff')

#backward selection model

#Setting full and null model
fit.null <- glm(status_time_num ~ 1, data = train.df[,c(8,9,15:18,20,21,23,26)],family = "binomial")
fit.full <- glm(status_time_num ~ . ,data = train.df[,c(8,9,15:18,20,21,23,26)], family = "binomial")

#Step wise logistic regression backward
model.logistic.backward <- step(fit.full, direction = "backward")

#Summary of the backward model
summary(model.logistic.backward)

#Predicting the validation data
model.logistic.backward.validation.pred <- predict(model.logistic.backward, valid.df[,c(8,9,15:18,20,21,23,26)], type = "response")

confusionMatrix(factor(ifelse(model.logistic.backward.validation.pred > 0.5,'payoff','default'),labels = c('payoff','default'),levels = c('payoff','default')
),factor(valid.df$status_time,labels = c('payoff','default'),levels = c('payoff','default')),positive = 'payoff')


#Modelling for Regression Task
###############################################################################################################
#Filtering the data with only purchased customers

train.payoff.df <- train.df[train.df$status_time == "payoff", ]
valid.payoff.df <- valid.df[valid.df$status_time == "payoff", ]

dim(train.payoff.df)
dim(valid.payoff.df)


#Fitting linear model
model.linear <- lm(balance_orig_time ~ ., data = train.payoff.df[,c(8,9,15:18,20,21,23,19)])

#Summary of the model
summary(model.linear)
#Predicting 
linear.pred.valid <- predict(model.linear, valid.payoff.df[,c(8,9,15:18,20,21,23,19)])

#Rmse
caret::RMSE(linear.pred.valid, valid.payoff.df$balance_orig_time)

#Improving the model performance 
#Setting full and null model
linear.fit.null <- lm(balance_orig_time ~ 1, data = train.payoff.df[,c(8,9,15:18,20,21,23,19)])
linear.fit.full <- lm(balance_orig_time ~ . ,data = train.payoff.df[,c(8,9,15:18,20,21,23,19)])

#Step wise logistic regression backward
model.linear.backward <- step(linear.fit.full, direction = "backward")

#Summary of the backward model
summary(model.linear.backward)

#Predicting the validation data 
linear.backward.pred.valid <- predict(model.linear.backward, valid.payoff.df[,c(8,9,15:18,20,21,23,19)])
#Rmse
caret::RMSE(linear.backward.pred.valid, valid.payoff.df$balance_orig_time)

#Decision Tree for regression 
model.regression.rpart <-rpart(balance_orig_time~ ., data=train.payoff.df[,c(8,9,15:18,20,21,23,19)])
#summary of the tree
summary(model.regression.rpart)

#plotting the tree
rpart.plot(model.regression.rpart)

#Predicting the validation data
model.rpart.valid.pred <- predict(model.regression.rpart, valid.payoff.df[,c(8,9,15:18,20,21,23,19)])

#Rmse
caret::RMSE(model.rpart.valid.pred, valid.payoff.df$balance_orig_time)

#Improving Performance 
Rmse_list <- list()
cp_values_list <- list()
cp_values <- seq(0.02, 0.1, by = 0.01)
for (cp in cp_values) {
  model.regression.rpart.cp <-rpart(balance_orig_time~ ., train.payoff.df[,c(8,9,15:18,20,21,23,19)],cp = cp)
  #Predicting the validation data
  model.rpart.valid.pred <- predict(model.regression.rpart.cp, valid.payoff.df[,c(8,9,15:18,20,21,23,19)])
  #Rmse
  rmse <-caret::RMSE(model.rpart.valid.pred, valid.payoff.df$balance_orig_time)
  Rmse_list <- c(Rmse_list, rmse)}

#Getting the minimum Rmse value
Rmse_list

#Training the model with best cp 

model.regression.rpart.cp <-rpart(balance_orig_time~ ., data=train.payoff.df[,c(8,9,15:18,20,21,23,19)],cp =0.02 )

#plotting the tree
rpart.plot(model.regression.rpart.cp)


#Predicting the validation data
model.rpart.valid.pred <- predict(model.regression.rpart.cp, valid.payoff.df[,c(8,9,15:18,20,21,23,19)])
#Rmse
caret::RMSE(model.rpart.valid.pred, valid.payoff.df$balance_orig_time)



#Classification Modelling for ongoing customers

#Decison tree
decision.tree.model.ongoing <- C5.0(train.df[,c(4:9,14:25)], train.df$status_time)
summary(decision.tree.model.ongoing)


# ploting the tree
plot(decision.tree.model.ongoing,type='simple')

#Evaluating model performance 
#predicting on valid data
decision.tree.model.ongoing.valid.pred <- predict(decision.tree.model.ongoing, valid.df[,c(4:9,14:25)])
#confusion matrix
confusionMatrix(decision.tree.model.ongoing.valid.pred,valid.df$status_time,positive = "payoff")

#Decison tree with trail 10
decision.tree.model.ongoing.trail <- C5.0(train.df[,c(4:9,14:25)], train.df$status_time,trials=6)
summary(decision.tree.model.ongoing.trail)

# ploting the tree
plot(decision.tree.model.ongoing.trail)

#Evaluating model performance 
#predicting on valid data
decision.tree.ongoing.trail.model.valid.pred <- predict(decision.tree.model.ongoing.trail, valid.df[,c(4:9,14:25)])
#confusion matrix
confusionMatrix(decision.tree.ongoing.trail.model.valid.pred,valid.df$status_time,positive = "payoff")


#backward selection model

#Setting full and null model
fit.null.ongoing <- glm(status_time_num ~ 1, data = train.df[,c(4:9,14:26)],family = "binomial")
fit.full.ongoing <- glm(status_time_num ~ . ,data = train.df[,c(4:9,14:26)], family = "binomial")


#Step wise logistic regression backward
model.logistic.backward.ongoing <- step(fit.full.ongoing, direction = "backward")

#Summary of the backward model
summary(model.logistic.backward.ongoing)

#Predicting the validation data
model.logistic.backward.validation.pred.ongoing <- predict(model.logistic.backward.ongoing, valid.df[,c(3:9,14:26)], type = "response")

confusionMatrix(factor(ifelse(model.logistic.backward.validation.pred.ongoing > 0.5,'payoff','default'),labels = c('payoff','default'),levels = c('payoff','default')
),factor(valid.df$status_time,labels = c('payoff','default'),levels = c('payoff','default')),positive = 'payoff')


#Models Applied to test data
test.df$new_customer_status_time <- predict(model.logistic.backward, test.df[,c(8,9,15:18,20,21,23)],type = "response")
test.df$new_customer_balance <- predict(model.linear.backward, test.df[,c(8,9,15:18,20,21,23,19)])
test.df$ongoing_customer <- predict(model.logistic.backward.ongoing, test.df[,c(4:9,14:26)], type = "response")
#Converting probability to class
test.df <- test.df %>% mutate( new_customer_status_time= ifelse(new_customer_status_time > 0.5,'payoff','default'))
test.df<- test.df %>% mutate( ongoing_customer= ifelse(ongoing_customer > 0.5,'payoff','default'))

head(test.df,6)
