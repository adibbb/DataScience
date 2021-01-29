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
apply(hotels.data,MARGIN=2,FUN=function(x) sum(is.na(x)))



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







