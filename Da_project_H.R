library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(data.table)
library(TTR)
library(forecast)
library(lubridate)



train$date = as.Date(train$date,format = '%d-%m-%Y')
train$date

#Date Features

train$year = lubridate::year(train$date)
train$yday = yday(train$date)
train$yday = yday(train$date)
train$quarter = quarter(train$date)
train$month = lubridate::month(train$date)
train$day = lubridate::day(train$date)
train$weekdays = weekdays(train$date)
glimpse(train)

#weekend inserting
train = as.data.table(train)
train$month = as.factor(train$month)
train$weekdays = factor(train$weekdays,levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday",'Sunday'))
train[weekdays %in% c("Saturday",'Sunday'),weekend:=1]
train[!(weekdays %in% c("Saturday",'Sunday')),weekend:=0]
train$weekend = as.factor(train$weekend)
train$year = as.factor(train$year)
train$quarter = as.factor(train$quarter)
train = as.data.frame(train)
glimpse(train)


#data Partitioning

set.seed(100) 
 
   train1 = train[train$store == 'Train1',]
 
   test1 = train[train$store == 'Test1',]

   dim(train1)

   dim(test1)

#Model Evaluation Metrics
   mape <- function(store,sales){
     mape <- mean(abs((store - sales)/train))*100
     return (mape)
   }
    
# Rain forest Modelling
   head = train[2:11,]
    set.seed(1000)
    
     library(randomForest)
   
      rf = randomForest(sales ~ store + year + yday + quarter + month + day + weekdays + weekend , data = head)
      
      print(rf)
      
# Prediction
    
      predictions = predict(rf, newdata = head)
       mape(head$sales, predictions) 
      
       
       predictions = predict(rf, newdata = head)
       mape(head$store, predictions) 
       
#Using Regression Classification
       View(train)
        
       attach(train)
       x1 <- lm(store~.,train)
       x1
       predection <- predict(x1,train)
       predection

#Decision Tree Classification
       library(tree)
       library(ISLR)
       a1 <- subset(train,select = c("store","item","sales","year","quarter","month"))
       Salecat <- ifelse(a1$sales<=8,"no","yes")
       data <-  data.frame(a1,Salecat)
       sale.tree <- tree(sales~store+item+year+quarter+month,data = data)
       summary(sale.tree)
       plot(sale.tree)
       text(sale.tree, pretty=0)
       predict(sale.tree)
       
       
#Plotting
       hist(train$sales,main = "Sales Analysis")
       
       barplot(train$sales,main = 'SALES BARPLOT')
       
       a1 <- ggplot(train,aes(x=store,y=item,z=sales,color=cut))+geom_jitter()
       a1
       
       #store and item
       a1 <- ggplot(head, aes(x=store, y=item,color=cut)) + geom_jitter()
       a1
       
       #histogram for Total & Day
       colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0","red","blue","yellow","green","pink")
       head(train)
       summary(train)
       
       hour_data <- train %>%
              group_by(day) %>%
              dplyr::summarize(Total = n())
       data.table(hour_data)
       
       ggplot(month_hour, aes(day, Total, fill = month)) +
               geom_bar( stat = "identity") +
               ggtitle("Plot by Day and Month") +
               scale_y_continuous(labels = waiver())
       
      #including day
        day_group <- train %>%
            group_by(day) %>%
               dplyr::summarize(Total = n())
       
       data.table(day_group)
       
       ggplot(day_group, aes(day, Total)) +
               geom_bar( stat = "identity", fill = "red") +
               ggtitle("Trips Every Day") +
               theme(legend.position = "none") +
               scale_y_continuous(labels = waiver())
       
       #grouping Month
       month_group <- train %>%
               group_by(month) %>%
              dplyr::summarize(Total = n())
       data.table(month_group)
       
       #barplot
       ggplot(month_group, aes(month, Total, fill = month)) +
          +     geom_bar( stat = "identity") +
          +     ggtitle("Trips by Month") +
          +     theme(legend.position = "none") +
          +     scale_y_continuous(labels = waiver()) +
          +     scale_fill_manual(values = colors)
       
       #Trips for day and month
       ggplot(month_weekday, aes(month, Total, fill = weekdays)) +
               geom_bar( stat = "identity") +
               ggtitle("Trips by Day and Month") +
               scale_y_continuous(labels = waiver()) +
              scale_fill_manual(values = colors)
       
       #Heatmap for sales and year
       salea_year <- train %>%
               group_by(sales,year ) %>%
               dplyr::summarize(Total = n())
       
       ggplot(salea_year, aes(sales, year, fill = Total)) +
               geom_tile(color = "white") +
               ggtitle("Heat Map by Sales And Year")
       
       ggplot(day_month_group, aes(day, month, fill = Total)) +
               geom_tile(color = cut) +
               ggtitle("Heat Map by Month and Day")
       
       #Sales and Store
       Sales_and_store <- train %>%
               group_by(sales,store) %>%
               dplyr::summarize(Total = n())
       data.table(Sales_and_store)
       
       ggplot(Sales_and_store, aes(sales,store, fill = Total)) +
               geom_tile(color = "white") +
               ggtitle("Heat Map by Sales And Store")
       
       #Binwidth histogram
       ggplot(train)+
         geom_histogram(mapping = aes(x = sales),binwidth = 1,color='red')
       
       
       #geompoint
       ggplot(train, aes(x=sales, y=store, fill='yellow')) + geom_point()
       
       #Visvalization using correation
       
       library("ggpubr")
       
      
        ggscatter(train, x = "store", y = "sales", 
                              add = "reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method = "pearson",
                              xlab = "Store", ylab = "Sales")
   
        a1<-train[1:100,]
        view(a1)
        shapiro.test(a1$sales)
        
        #Pearson Method Of Correlation
        
        res <- cor.test(a1$sales, a1$store, 
                        method = "pearson")
        res
     
       
   