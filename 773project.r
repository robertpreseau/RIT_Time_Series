## STANDARD SETUP #########################################################################

#Create list of required packages
list.of.packages <- c('purrr', 'dplyr', 'rvest', 'stringr', 'aTSA', 'forecast')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#Install missing packages
if(length(new.packages)) install.packages(new.packages)

#Load packages
library('dplyr')
library('purrr')
library('rvest')
library('stringr')
library('aTSA')
library('forecast')
library('ggplot2')



#Clear out environment variables
rm(list=ls())

#Set random seed
set.seed(1738)

#Set working directory
setwd('C:\\Users\\rober\\OneDrive\\School\\STAT773\\Project')

#clear Plots pane
if(!is.null(dev.list())) dev.off() 

## END STANDARD SETUP #########################################################################


##### Start working #####

 #Year to start retrieving data
 year <- 2000

 #Initialize dataframe for downloaded data
 results_final <- c(NA, NA, NA)

 while (year < 2020) {

   #Specifying the url for desired website to be scraped
   url <- paste0('https://www.baseball-reference.com/teams/DET/', year,'-schedule-scores.shtml')

   #Reading the HTML code from the website
   webpage <- read_html(url)

   #Using this tutorial https://www.freecodecamp.org/news/an-introduction-to-web-scraping-using-r-40284110c848/
   table <- html_table(html_nodes(webpage,'#team_schedule'))

   #Pull out useful columns
   game_no <- as.integer(data.frame(table[1])[,1])
   home_away <- as.character(data.frame(table[1])[,5])
   attendance <- data.frame(table[1])[,19]
   attendance <- str_replace(attendance, ',', '')


   #Create dataframe from intermediate columns
   results <- data.frame(game_no, home_away, attendance)

   #Data Load cleanup
   rm(game_no, attendance, home_away)

   #Remove repeated header rows (these will have a blank game #)
   results <- results[!is.na(results$game_no), ]

   #Remove away games
   results <- results %>% select(everything()) %>% filter(home_away=="")

   #Renumber game_no
   results$game_no <- 1:nrow(results)

   #Assign Year
   results$Year <- year

   #Convert attendance to numeric
   results$attendance <- as.numeric(as.character(results$attendance))

   results_final <- rbind(results_final, results)

 year = year + 1
 }

#Remove NA - Pass 1
results_final <- results_final[!is.na(results_final$game_no), ]


#Check to see if all years have same # of games
results_final_grouped <- results_final %>% group_by(Year) %>% tally()
as.data.frame(results_final_grouped)

#A couple years are missing a game.

#Fill in missing games with average from the year
results_final %>% filter(Year==2002) %>% summarize(mean_size=mean(attendance))

de<-data.frame(81, "", 18792, 2002)
names(de)<-c("game_no", "home_away", "attendance", "Year")
results_final <- rbind(results_final, de)

results_final %>% filter(Year==2016) %>% summarize(mean_size=mean(attendance))
 
de<-data.frame(81, "", 31173, 2016)
names(de)<-c("game_no", "home_away", "attendance", "Year")
results_final <- rbind(results_final, de)

#Check to see if all years have same # of games
results_final_grouped <- results_final %>% group_by(Year) %>% tally()
as.data.frame(results_final_grouped)

#Are any attendance values missing? Will fix this later.
results_final[is.na(results_final$attendance)==TRUE,]
results_final <- results_final %>% arrange(Year, game_no)
 
bound_lower <- 0
bound_upper <- 46000

#Convert to timeseries
data <- ts(results_final$attendance, start=1, frequency=81)

#Fill in NA values
data <- na.interp(data)


#Create training and test sets
data.train <- window(data, start=c(1,1), end=c(19,81), frequency=81)
data.test <- window(data, start=c(20,1), frequency=81)

#Data load cleanup
rm(results_final_grouped, table, webpage, results, de, url, year)



## ANALYSIS ##

#Seasonal Naive
naive <- snaive(data.train, 81, level=95)
ts.plot(data, ylab='Attendance', xlab='Year', type='p', ylim=c(0,50000))
lines(naive$lower,lwd=2.5,lty=1,col='red')
lines(naive$upper,lwd=2.5,lty=1,col='red')
lines(naive$model$fitted,lwd=.5,lty=1,col='black')
title('Detroit Tigers Attendance with 2019 Fits & 95% PIs (Seasonal Naive)')

ts.plot(data.test, ylab='Attendance', xlab='Year', type='p', ylim=c(0,50000))
lines(naive$lower,lwd=2.5,lty=1,col='red')
lines(naive$upper,lwd=2.5,lty=1,col='red')
lines(naive$model$future,lwd=.5,lty=1,col='black')
title('Detroit Tigers Attendance with 2019 Fits & 95% PIs (Seasonal Naive)')

ts.plot(data.test - naive$model$future, ylab='Residual', xlab='Year', type='p')
title('Residuals (Seasonal Naive)')

forecast::Acf(data.test - naive$model$future)
forecast::Pacf(data.test - naive$model$future)

naive$residuals %>% ggtsdisplay() 

accuracy(naive, data.test)






## MULTIPLICATIVE DECOMPOSITION ##

decomp_mult <- data.train %>% decompose(type="multiplicative") 
decomp_mult %>%
   autoplot() + xlab("Year") +
   ggtitle("Classical multiplicative decomposition of attendance data")

decomp_add <- data.train %>% decompose(type="additive") 
decomp_add %>%
   autoplot() + xlab("Year") +
   ggtitle("Classical additive decomposition of attendance data")



## FIRST ORDER EXPONENTIAL SMOOTHING (AKA SES)
plot(data.train, xlab='Year', ylab='Attendance', main='Detroit Tigers Attendance (2000-2018) (First Order Smoother)')
data.holt1 <- HoltWinters(data.train, seasonal='multiplicative')
pred.int1 <- predict(data.holt1, 81, prediction.interval=TRUE)

ts.plot(data.train, ylab='Attendance', xlab='Year', type='p')
lines(data.holt1$fitted[,1],lty=1,lwd=1)
lines(pred.int1[,3],lwd=1,lty=1,col='red')
lines(pred.int1[,2],lwd=1,lty=1,col='red')
lines(pred.int1[,1],lwd=1,lty=1,col='black')
title('Detroit Tigers Attendance (2000-2018) with 2019 Fits & 95% PIs (First Order Smooth)')

ts.plot(data.test, ylab='Attendance', xlab='Year', type='p')
lines(data.holt1$fitted[,1],lty=1,lwd=2)
lines(pred.int1[,3],lwd=2.5,lty=1,col='red')
lines(pred.int1[,2],lwd=2.5,lty=1,col='red')
lines(pred.int1[,1],lwd=.5,lty=1,col='black')
title('Detroit Tigers Attendance with 2019 Fits & 95% PIs (First Order Smooth)')

pred.int1.df <- as.data.frame(pred.int1)
pred.int1.df$diff <- as.integer(data.test) - as.integer(pred.int1.df$fit)

pred.int1.df$diff  %>% ggtsdisplay(main ='Residuals (Holt-Winters - First Order Smooth)') 
plot(pred.int1.df$diff) #Residuals look like they have a curve to them.

accuracy(pred.int1, data.test)




## ARIMA
#Check for stationarity
data.train %>% ggtsdisplay()

#https://otexts.com/fpp2/autocorrelation.html
#The slow decrease in the ACF as the lags increase is due to the trend, while 
#the “scalloped” shape is due the seasonality.

#Remove seasonality
data.train %>% diff(lag=81) %>% ggtsdisplay()
forecast::Acf(diff(data.train, lag=81))

#Remove Trend
data.train %>% diff() %>% ggtsdisplay()

#Remove Seasonality + Trend
data.train %>% diff(lag=81) %>% diff() %>% ggtsdisplay() 
data_mod <- data.train %>% diff(lag=81) %>% diff() 

forecast::Acf(data_mod)
forecast::Pacf(data_mod)

adf.test(data_mod)
kpss.test(data_mod)

fit1 <- data.train %>% Arima(order=c(0,1,1))
checkresiduals(fit1)
fit1$aic

fit1 %>% forecast(h=81, level=95) %>% autoplot()
pred <- fit1 %>% forecast(h=81, level=95)
pred_df <- as.data.frame(pred)
pred_df$actual <- as.data.frame(data.test)
pred_df$diff <- pred_df$`Point Forecast` - pred_df$actual
names(pred_df) <- c('forecast', 'low_est', 'hi_est', 'actual' ,'diff')
pred_df$diff <- pred_df$forecast - pred_df$actual

accuracy(pred, data.test)




## Auto.ARIMA
fit.auto <- auto.arima(data.train, lambda=0)
accuracy(fit.auto, data.test)




## ARIMA - Log-bound

## THIS IS A TRICK TO KEEP THE FORECASTS BETWEEN UPPER AND LOWER BOUNDS
#log((eggs-a)/(b-eggs))
#results_final$log_attendance <- log((results_final$attendance-bound_lower)/(bound_upper - results_final$attendance))

data.logmod <- log((data-bound_lower)/(bound_upper - data))

data.train.logmod <- window(data.logmod, start=c(1,1), end=c(19,81), frequency=81)
data.test.logmod <- window(data.logmod, start=c(20,1), frequency=81)

plot(data.train.logmod)



#Check for stationarity
data.train.logmod %>% ggtsdisplay()

#https://otexts.com/fpp2/autocorrelation.html
#The slow decrease in the ACF as the lags increase is due to the trend, while 
#the “scalloped” shape is due the seasonality.

#Remove seasonality
data.train.logmod %>% diff(lag=81) %>% ggtsdisplay()
forecast::Acf(diff(data.train.logmod, lag=81))

#Remove Trend
data.train.logmod %>% diff() %>% ggtsdisplay()

#Remove Seasonality + Trend
data.train.logmod %>% diff(lag=81) %>% diff() %>% ggtsdisplay() 
data_mod.logmod <- data.train.logmod %>% diff(lag=81) %>% diff() 

forecast::Acf(data_mod.logmod)
forecast::Pacf(data_mod.logmod)

adf.test(data_mod)
kpss.test(data_mod)

fit1.logmod <- data.train.logmod %>% Arima(order=c(0,1,1))
checkresiduals(fit1.logmod)
fit1.logmod$aic

#Create forecast then convert the numbers back
fit1.logmod %>% forecast(h=81, level=95) %>% autoplot()
pred.logmod <- fit1.logmod %>% forecast(h=81, level=95)

pred.logmod <- as.data.frame(pred.logmod)
names(pred.logmod) <- c('forecast', 'low_est', 'hi_est')

pred.logmod$mean <- (bound_upper-bound_lower)*exp(pred.logmod$mean)/(1+exp(pred.logmod$mean)) + bound_lower
pred.logmod$lower <- (bound_upper-bound_lower)*exp(pred.logmod$lower)/(1+exp(pred.logmod$lower)) + bound_lower
pred.logmod$upper <- (bound_upper-bound_lower)*exp(pred.logmod$upper)/(1+exp(pred.logmod$upper)) + bound_lower

pred.logmod$actual <- as.data.frame(data.test)
pred.logmod$diff <- pred.logmod$mean - data.test

accuracy(pred.logmod, data.test)

pred.logmod %>% autoplot()


auto.arima(data.train, d=81)
