library(scales)
library(ggplot2)
library(reshape2)
library(dplyr)
library(rmarkdown)
library(caret)
library(lubridate)
library(futile.logger)
library(ggrepel)
library(tidyr)
library(gridExtra)
library(optimization)
library(RPresto)
library(smotefamily)
library(zoo)
library(prophet)
library(tibble)
library(broom)
library(randomForest)
library(tseries)
library(forecast)
library(grid)


library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(xgboost)

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}



hotels.data <- read.csv("~/Downloads/hotel_bookings.csv")


#hotels.data %>% group_by(hotel) %>% summarise(n())
#apply(hotels.data,MARGIN=2,min)

hotels.data$reservation_status_date <- as.Date(hotels.data$reservation_status_date)
hotels.data <- hotels.data %>% filter(adults>0)
#apply(hotels.data,MARGIN=2,FUN=function(x) sum(is.na(x)))



hotels.data$children[is.na(hotels.data$children)] <- 0


hotels.data <- hotels.data %>% filter(reservation_status_date>='2015-06-01')

hotels.data.summ <- hotels.data %>% group_by(reservation_status_date) %>% summarise(num_orders=n(),num_cancelled_orders=sum(is_canceled==1),num_non_cancelled_orders=sum(is_canceled==0))
hotels.data.summ <- hotels.data.summ[order(hotels.data.summ$reservation_status_date),]
hotels.data.summ <- hotels.data.summ  %>% mutate(running_avg=rollapply(num_orders,width=10,mean,partial=T),
                                                 running_avg_cancel=rollapply(num_cancelled_orders,width=10,mean,partial=T),
                                                 running_avg_non_cancel=rollapply(num_non_cancelled_orders,width=10,mean,partial=T)
)

ggplot(data=hotels.data.summ,aes(x=reservation_status_date,y=num_orders)) + geom_line() + ylab('Daily Bookings') + ggtitle('Daily Bookings')


hotels.data.summ <- hotels.data %>% group_by(hotel,reservation_status_date) %>% summarise(num_orders=n(),num_cancelled_orders=sum(is_canceled==1),num_non_cancelled_orders=sum(is_canceled==0))
hotels.data.summ <- hotels.data.summ[order(hotels.data.summ$hotel,hotels.data.summ$reservation_status_date),]
hotels.data.summ <- hotels.data.summ %>% group_by(hotel) %>% mutate(running_avg=rollapply(num_orders,width=10,mean,partial=T),
                                                                    running_avg_cancel=rollapply(num_cancelled_orders,width=10,mean,partial=T),
                                                                    running_avg_non_cancel=rollapply(num_non_cancelled_orders,width=10,mean,partial=T))

ggplot(data=hotels.data.summ,aes(x=reservation_status_date,y=running_avg,col=hotel)) + geom_line() + ylab('Daily Bookings') + ggtitle('Daily Bookings (10d Running Avg)')


ggplot(data=hotels.data.summ,aes(x=reservation_status_date)) + geom_line(aes(y=running_avg_cancel,col='Cancelled')) + geom_line(aes(y=running_avg_non_cancel,col='Non-Cancelled')) + facet_wrap(~hotel) + ylab('Bookings') + ggtitle('Cancelled and Non-Cancelled Bookings')


#######

hotels.data$season <- getSeason(hotels.data$reservation_status_date)

hotels.data.summ <- hotels.data %>% group_by(hotel,reservation_status_date,season) %>% summarise(num_cancelled_orders=sum(is_canceled==1),num_non_cancelled_orders=sum(is_canceled==0))

hotels.data.summ$cancellation_ratio <- hotels.data.summ$num_cancelled_orders / (hotels.data.summ$num_cancelled_orders + hotels.data.summ$num_non_cancelled_orders)

ggplot(data=hotels.data.summ,aes(x=reservation_status_date,y=cancellation_ratio,col=season,group=hotel)) + facet_wrap(~hotel,ncol=1) + geom_line() + ggtitle('Cancellation Ratios by Season')


#######

p0 <- ggplot(data=hotels.data,aes(x=lead_time)) + geom_histogram() + ggtitle('Lead Time Histogram')
p1 <- ggplot(data=hotels.data %>% filter(lead_time<=200),aes(x=lead_time)) + geom_histogram() + ggtitle('Lead Time Histogram (Zoom-In)')


kmeans.output <- kmeans(hotels.data$lead_time,centers = 30)

centers <- sort(kmeans.output$centers)
centers <- sort(centers)

hotels.data$lead_time_buket <- kmeans(hotels.data$lead_time, centers = centers)$cluster

hotels.data.summ <- hotels.data %>% group_by(hotel,lead_time_buket) %>% summarise(cancellation_ratio=mean(is_canceled),freq=n()) %>% filter(freq>=100)

ggplot(data=hotels.data.summ,aes(x=lead_time_buket,y=cancellation_ratio,col=hotel)) + geom_point() + geom_line() + ggtitle('Probability to Cancel by Lead Time Bucket')


#######

hotels.data$family_comb <- paste0(hotels.data$adults,'_',hotels.data$children,'_',hotels.data$babies)

hotels.data.summ <- hotels.data %>% group_by(hotel,family_comb) %>% mutate(freq=n()) %>% filter(freq>=100) %>% summarise(cancellation_ratio=mean(is_canceled))

ggplot(data=hotels.data.summ,aes(x=family_comb,y=cancellation_ratio)) + geom_bar(stat='identity') + facet_wrap(~hotel,ncol=1) + ggtitle('Probability to Cancel by Family Combination')


#######

hotels.data$total_length <- hotels.data$stays_in_weekend_nights + hotels.data$stays_in_week_nights
hotels.data <- hotels.data %>% filter(total_length>0)

ggplot(data=hotels.data,aes(x=total_length)) + geom_histogram() + ggtitle('Lead Time Histogram')

ggplot(data=hotels.data %>% filter(total_length<=15),aes(x=total_length)) + geom_histogram() + ggtitle('Lead Time Histogram')

hotels.data.filt <- hotels.data %>% filter(total_length<=7) %>% group_by(hotel,total_length) %>% summarise(cancellation_ratio=mean(is_canceled))

ggplot(data=hotels.data.filt,aes(x=total_length,y=cancellation_ratio)) + geom_bar(stat='identity') + facet_wrap(~hotel,ncol=1) + ggtitle('Probability to Cancel by Vacation Length')


hotels.data.filt <- hotels.data %>% filter(stays_in_weekend_nights<=4) %>% group_by(hotel,stays_in_weekend_nights) %>% summarise(cancellation_ratio=mean(is_canceled),freq=n())

ggplot(data=hotels.data.filt,aes(x=stays_in_weekend_nights,y=cancellation_ratio)) + geom_bar(stat='identity') + facet_wrap(~hotel,ncol=1) + ggtitle('Probability to Cancel by Vacation Length')


######

hotels.data$hotel_country <- 'Non-Portugal'
hotels.data$hotel_country[hotels.data$country=='PRT'] <- 'Portugal'

hotels.data.summ <- hotels.data %>% group_by(hotel,hotel_country) %>% summarise(cancellation_ratio=mean(is_canceled),freq=n())


ggplot(data=hotels.data.summ,aes(x=hotel_country,y=cancellation_ratio,fill=hotel_country)) + geom_bar(stat='identity') + facet_wrap(~hotel) + ylim(c(0,1)) + ggtitle('Probability to Cancel by Country')


#######

hotels.data.summ <- hotels.data %>% group_by(hotel,deposit_type) %>% summarise(cancellation_ratio=mean(is_canceled),freq=n())


ggplot(data=hotels.data.summ,aes(x=deposit_type,y=cancellation_ratio,fill=deposit_type)) + geom_bar(stat='identity') + facet_wrap(~hotel) + ylim(c(0,1)) + ggtitle('Probability to Cancel by Deposit Type')



#######
hotels.data$adr <- as.numeric(hotels.data$adr)

hotels.data.summ <- hotels.data %>% group_by(hotel,reservation_status_date) %>% summarise(avg_price=mean(adr))

ggplot(data=hotels.data.summ,aes(x=reservation_status_date,y=avg_price,col=hotel)) + geom_line() + ggtitle('Average Price by Date')



kmeans.output <- kmeans(hotels.data$adr,centers = 30)

centers <- sort(kmeans.output$centers)
centers <- sort(centers)

hotels.data$price_bucket <- kmeans(hotels.data$adr, centers = centers)$cluster

hotels.data.summ <- hotels.data %>% group_by(hotel,price_bucket) %>% summarise(cancellation_ratio=mean(is_canceled),freq=n()) %>% filter(freq>=100)

ggplot(data=hotels.data.summ,aes(x=price_bucket,y=cancellation_ratio,col=hotel)) + geom_point() + geom_line() + ggtitle('Probability to Cancel by Price Bucket')

########


corr_data <- hotels.data %>% select('is_canceled','lead_time','total_of_special_requests','required_car_parking_spaces','booking_changes','previous_cancellations',
                                    'is_repeated_guest','adults','previous_bookings_not_canceled','days_in_waiting_list','adr','babies','stays_in_week_nights',
                                    'arrival_date_year','arrival_date_week_number','arrival_date_day_of_month','children','stays_in_weekend_nights')

corr_data_numeric <- corr_data %>% apply(MARGIN=2,as.numeric)

hotels_cor <- cor(corr_data_numeric)

corrplot(hotels_cor, method="number")

View(hotels_cor[,1])



#################### Models Section #################### 

### Train Test Validate Split

set.seed(123)
rnd.index <- sample(c(1:3),size=nrow(hotels.data),replace = TRUE,prob = c(0.7,0.15,0.15))

train.set <- hotels.data[rnd.index==1,]
validate.set <- hotels.data[rnd.index==2,]
test.set <- hotels.data[rnd.index==3,]


## Decision Tree

fit <- rpart(is_canceled ~ hotel + lead_time + arrival_date_week_number + arrival_date_day_of_month + stays_in_weekend_nights + stays_in_week_nights + adults + children + babies +
               is_repeated_guest + previous_cancellations + previous_bookings_not_canceled + required_car_parking_spaces + total_of_special_requests + adr +
               meal + market_segment + hotel_country, data = train.set, method = 'class')
rpart.plot(fit)


tree.predictions <- predict(fit, validate.set, type = 'class')


tree.results <- confusionMatrix(tree.predictions,as.factor(validate.set$is_canceled))

tree.predictions <- predict(fit, validate.set, type = 'prob')
tree.predictions <- pmin(tree.predictions,0.999)
tree.predictions <- pmax(tree.predictions,0.001)

- mean(validate.set$is_canceled * log(tree.predictions) + (1-validate.set$is_canceled ) * log(1-tree.predictions) )


tree.predictions <- predict(fit, test.set, type = 'class')


tree.results <- confusionMatrix(tree.predictions,as.factor(test.set$is_canceled))

tree.predictions <- predict(fit, test.set, type = 'prob')
tree.predictions <- pmin(tree.predictions,0.999)
tree.predictions <- pmax(tree.predictions,0.001)

- mean(test.set$is_canceled * log(tree.predictions) + (1-test.set$is_canceled ) * log(1-tree.predictions) )

tree.results$byClass
tree.results$overall
## Random Forest

ntree.vec <- c(100,200,300,400,500)
mtry.vec <- 4 #seq(3,13)
K <- 5
set.seed(123)
cv.index <- sample(c(1:K),size=nrow(train.set),replace=TRUE)



cv.mat <- matrix(data=0,nrow=length(ntree.vec),ncol=(K+1))

i <- 1


for (ntree_iter in ntree.vec){
  for (mtry_iter in mtry.vec){
    cv.mat[i,1] <- ntree_iter
    
    for(k in 1:K){
      cv.train <- train.set[cv.index!=k,]
      cv.test <- train.set[cv.index==k,]
      rf.iter <- randomForest(as.factor(is_canceled) ~ hotel + lead_time + arrival_date_week_number + arrival_date_day_of_month + stays_in_weekend_nights + stays_in_week_nights + adults + children + babies +
                                is_repeated_guest + previous_cancellations + previous_bookings_not_canceled + required_car_parking_spaces + total_of_special_requests + adr +
                                meal + market_segment + hotel_country ,data=cv.train,ntree=ntree_iter,mtry=mtry_iter,type='classification')
      
      rf.iter.predictions <- predict(rf.iter,newdata=cv.test ,type='class')
      
      rf.iter.results <- confusionMatrix(rf.iter.predictions,as.factor(cv.test$is_canceled))
      
      cv.mat[i,1+k] <-  as.numeric(rf.iter.results$byClass['F1'])
    }
  }
  i <- i + 1
}

which.min(apply(cv.mat[,c(2:6)],MARGIN=1,mean))



rf.model <- randomForest(as.factor(is_canceled) ~ hotel + lead_time + arrival_date_week_number + arrival_date_day_of_month + stays_in_weekend_nights + stays_in_week_nights + adults + children + babies +
                           is_repeated_guest + previous_cancellations + previous_bookings_not_canceled + required_car_parking_spaces + total_of_special_requests + adr +
                           meal + market_segment,data=train.set,ntree=200,mtry=4,type='classification',importance=TRUE)

varImpPlot(rf.model)

rf.predictions <- predict(rf.model,newdata=validate.set ,type='class')

rf.results <- confusionMatrix(rf.predictions,as.factor(validate.set$is_canceled))

rf.results$byClass

rf.predictions <- predict(rf.model, validate.set, type = 'prob')[,2]
rf.predictions <- pmin(rf.predictions,0.999)
rf.predictions <- pmax(rf.predictions,0.001)

- mean(validate.set$is_canceled * log(rf.predictions) + (1-validate.set$is_canceled ) * log(1-rf.predictions) )



rf.predictions <- predict(rf.model,newdata=test.set ,type='class')

rf.results <- confusionMatrix(rf.predictions,as.factor(test.set$is_canceled))

rf.results$byClass

rf.predictions <- predict(rf.model, test.set, type = 'prob')[,2]
rf.predictions <- pmin(rf.predictions,0.999)
rf.predictions <- pmax(rf.predictions,0.001)

- mean(test.set$is_canceled * log(rf.predictions) + (1-test.set$is_canceled ) * log(1-rf.predictions) )

rf.results$overall
### Logistic Regression

rf.importance <- as.data.frame(importance(rf.model))

rf.importance <- rf.importance[order(rf.importance$MeanDecreaseAccuracy,decreasing = T),]
selected_vars <- rownames(rf.importance)[1:5]


logisitc.model <- glm(as.factor(is_canceled) ~ hotel + lead_time + arrival_date_week_number + arrival_date_day_of_month + stays_in_weekend_nights + stays_in_week_nights + adults + children + babies +
                        is_repeated_guest + previous_cancellations + previous_bookings_not_canceled + required_car_parking_spaces + total_of_special_requests + adr +
                        meal,family=binomial(link='logit'),data=train.set)


backwards = step(logisitc.model) 




logistic.predictions <- predict(backwards,newdata=validate.set ,type='response')
logistic.predictions <- as.factor(ifelse(logistic.predictions > 0.5,1,0))


logistic.results <- confusionMatrix(logistic.predictions,as.factor(validate.set$is_canceled))

logistic.results$byClass

logistic.predictions <- predict(backwards,newdata=validate.set ,type='response')
- mean(validate.set$is_canceled * log(logistic.predictions) + (1-validate.set$is_canceled ) * log(1-logistic.predictions) )


logistic.predictions <- predict(backwards,newdata=test.set ,type='response')
logistic.predictions <- as.factor(ifelse(logistic.predictions > 0.5,1,0))


logistic.results <- confusionMatrix(logistic.predictions,as.factor(test.set$is_canceled))

logistic.results$byClass
logistic.results$overall
logistic.predictions <- predict(backwards,newdata=test.set ,type='response')
- mean(test.set$is_canceled * log(logistic.predictions) + (1-test.set$is_canceled ) * log(1-logistic.predictions) )


### XGBoost


train.set.xgboost <- train.set %>% select('hotel','lead_time','arrival_date_week_number','arrival_date_day_of_month','stays_in_weekend_nights','stays_in_week_nights','adults',
                                          'children','babies','is_repeated_guest','previous_cancellations','previous_bookings_not_canceled','required_car_parking_spaces','total_of_special_requests','adr','is_canceled')

train.set.xgboost$hotel_type <- as.numeric(train.set.xgboost$hotel=='Resort Hotel')

train.set.xgboost <- train.set.xgboost %>% select(-c('hotel'))
train.set.xgboost <- as.data.frame(apply(train.set.xgboost,MARGIN=2,FUN=as.numeric))

validate.set.xgboost <- validate.set %>% select('hotel','lead_time','arrival_date_week_number','arrival_date_day_of_month','stays_in_weekend_nights','stays_in_week_nights','adults',
                                                'children','babies','is_repeated_guest','previous_cancellations','previous_bookings_not_canceled','required_car_parking_spaces','total_of_special_requests','adr','is_canceled')

validate.set.xgboost$hotel_type <- as.numeric(validate.set.xgboost$hotel=='Resort Hotel')

validate.set.xgboost <- validate.set.xgboost %>% select(-c('hotel'))
validate.set.xgboost <- as.data.frame(apply(validate.set.xgboost,MARGIN=2,FUN=as.numeric))


dtrain <- xgb.DMatrix(as.matrix(train.set.xgboost[ , !(names(train.set.xgboost) %in% c('is_canceled'))]),label = train.set.xgboost[['is_canceled']])
dtest <- xgb.DMatrix(as.matrix(validate.set.xgboost[ , !(names(train.set.xgboost) %in% c('is_canceled'))]),label = validate.set.xgboost[['is_canceled']])


param <- list(max_depth=5, eta=0.1, verbosity=0)
watchlist <- list(train=dtrain, eval = dtest)

depth_vec <- c(1:10)
res_vec <- c()


for (dep in depth_vec){
  
  param <- list(max_depth=dep, eta=0.1, verbosity=0)
  
  watchlist <- list(eval = dtest)
  
  bst_model <- xgb.train(param, dtrain, 250, watchlist,early_stopping_rounds = 10,maximize = FALSE,objective= "binary:logistic")
  
  res_vec <- c( res_vec, as.numeric(bst_model$best_score) )
  
}


param <- list(max_depth=10, eta=0.1, verbosity=0)

bst.model <- xgb.train(param,nrounds=250,watchlist=watchlist,objective= "binary:logistic", data = dtrain,maximize = FALSE,early_stopping_rounds = 10)


xgboost.predictions <- predict(bst.model,newdata=dtest)
xgboost.predictions <- as.factor(ifelse(xgboost.predictions > 0.5,1,0))

xgboost.results <- confusionMatrix(xgboost.predictions,as.factor(validate.set$is_canceled))
xgboost.results$byClass

xgboost.predictions <- predict(bst.model,newdata=dtest)

- mean(validate.set$is_canceled * log(xgboost.predictions) + (1-validate.set$is_canceled ) * log(1-xgboost.predictions) )




test.set.xgboost <- test.set %>% select('hotel','lead_time','arrival_date_week_number','arrival_date_day_of_month','stays_in_weekend_nights','stays_in_week_nights','adults',
                                        'children','babies','is_repeated_guest','previous_cancellations','previous_bookings_not_canceled','required_car_parking_spaces','total_of_special_requests','adr','is_canceled')

test.set.xgboost$hotel_type <- as.numeric(test.set.xgboost$hotel=='Resort Hotel')

test.set.xgboost <- test.set.xgboost %>% select(-c('hotel'))
test.set.xgboost <- as.data.frame(apply(test.set.xgboost,MARGIN=2,FUN=as.numeric))


dtest <- xgb.DMatrix(as.matrix(test.set.xgboost[ , !(names(train.set.xgboost) %in% c('is_canceled'))]),label = test.set.xgboost[['is_canceled']])



xgboost.predictions <- predict(bst.model,newdata=dtest)
xgboost.predictions <- as.factor(ifelse(xgboost.predictions > 0.5,1,0))

xgboost.results <- confusionMatrix(xgboost.predictions,as.factor(test.set$is_canceled))
xgboost.results$byClass
xgboost.results$overall
xgboost.predictions <- predict(bst.model,newdata=dtest)

- mean(test.set$is_canceled * log(xgboost.predictions) + (1-test.set$is_canceled ) * log(1-xgboost.predictions) )







