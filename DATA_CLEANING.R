setwd("C:/Users/HARIKRISHNA/Desktop/Fall Sem/Da")
library(readr)
train <- read_csv("train.csv")

library("dplyr") 

library(stringr) 

class(train)

dim(train)

names(train)

head(train)

train$date <- as.character(train$date) 
train$store <- as.character(train$store) 
train$item <- as.character(train$item) 
train$sales <- as.character(train$sales) 



train %>% 
  + mutate_if(is.character, str_trim) -> train

train$date <- as.factor(train$date) 
train$store <- as.factor(train$store) 
train$item <- as.factor(train$item) 
train$sales <- as.factor(train$sales) 

is.na(train)
sum(is.na(train))

summary(train)

mycol <- c("date", "store", "item", "sales") 
train[mycol]

train[1, 2:3]

train[1:10, 2]

mysample <- train[sample(1:nrow(train), 20, replace=FALSE),] 
sample(train)

freq.table <- table(train$item)

###second dataset ###

library(readr)
test <- read_csv("test.csv")

class(test)

dim(test)

names(test)

head(test)

test$id <- as.character(test$id) 

test$date <- as.character(test$date) 

test$store <- as.character(test$store) 

test$item <- as.character(test$item) 



library("dplyr") 

library(stringr) 

test %>% 
  mutate_if(is.character, str_trim) -> test 

test$id <- as.factor(test$id) 

test$date <- as.factor(test$date) 

test$store <- as.factor(test$store) 

test$item <- as.factor(test$item) 

is.na(date)

is.na(test)

summary(test)

mysample <- train[sample(1:nrow(test), 20, replace=FALSE),] 
sample(test)

freq.table <- table(test$store)

mycol <- c("id", "date", "store","item" ) 
test[mycol]

test[1, 2:3] 

test[1:10, 2] 

test[5,10][1] 