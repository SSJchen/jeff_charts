rm(list = ls())

library(readr)
library(highcharter)

####set_wd
setwd("C:/Users/jeffery_chen/Desktop/ActivationPrediction_Data/Data_Jeffery")  

####TW_ZE520KL####
moving <- read_csv('./ResultData/ZF3/googletrend/TW_ZE552KL_v1_google_trend_moving.csv')
shift_4week <- read_csv('./ResultData/ZF3/googletrend/TW_ZE520KL_v1_google_trend_shift_4week.csv')
shift_5week <- read_csv('./ResultData/ZF3/googletrend/TW_ZE520KL_v1_google_trend_shift_5week.csv')
shift_6week <- read_csv('./ResultData/ZF3/googletrend/TW_ZE520KL_v1_google_trend_shift_6week.csv')
shift <- read_csv('./ResultData/ZF3/googletrend/TW_ZE520KL_v1_google_trend_shift.csv')

result <- read_csv('./ResultData/ZF3/v1_ship_days_repair_weather_life_sepcscore_201703_predict_fitted(TW).csv')
result <- result[result$l0name == 'TW'&
                   result$model == 'ZE520KL',]
###
moving <- moving[moving$date >='2017-03-01',]
shift_4week <- shift_4week[shift_4week$date >='2017-03-01',]
shift_5week <- shift_5week[shift_5week$date >='2017-03-01',]
shift_6week <- shift_6week[shift_6week$date >='2017-03-01',]
shift <- shift[shift$date >='2017-03-01',]
result <- result[result$date >='2017-03-01',]

hc <- highchart() %>%
  hc_title(text = 'TW_ZE520KL') %>%
  hc_add_series(name = 'Real', type = "line", color = "#004B8D",
                data = moving$volume) %>%
  hc_add_series(name = 'shift', type = "line", color = "#88B04B",
                data = shift$predict_rf) %>%
  hc_add_series(name = 'shift_4week', type = "line", color = "#60774F",
                data = shift_4week$predict_rf) %>%
  hc_add_series(name = 'shift_5week', type = "line", color = "#955251",
                data = shift_5week$predict_rf) %>%
  hc_add_series(name = 'shift_6week', type = "line", color = "#AD5E99",
                data = shift_6week$predict_rf) %>%
  hc_add_series(name = 'last', type = "line", color = "#F7CAC9",
                data = result$predict_rf) %>%
  hc_add_series(name = 'moving', type = "line", color = "#F3CF55",
                data = moving$predict_rf) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_xAxis(categories = moving$date) %>%
  hc_yAxis(title = list(text = "activation"))
print(hc) 



####TW_ZE552KL####
moving <- read_csv('./ResultData/ZF3/googletrend/TW_ZE552KL_v1_google_trend_moving.csv')
shift <- read_csv('./ResultData/ZF3/googletrend/TW_ZE552KL_v1_google_trend_shift.csv')

result <- read_csv('./ResultData/ZF3/v1_ship_days_repair_weather_life_sepcscore_201703_predict_fitted(TW).csv')
result <- result[result$l0name == 'TW'&
                   result$model == 'ZE552KL',]
###
moving <- moving[moving$date >='2017-03-01',]
shift <- shift[shift$date >='2017-03-01',]
result <- result[result$date >='2017-03-01',]

hc <- highchart() %>%
  hc_title(text = 'TW_ZE552KL') %>%
  hc_add_series(name = 'Real', type = "line", color = "#004B8D",
                data = moving$volume) %>%
  hc_add_series(name = 'shift', type = "line", color = "#88B04B",
                data = shift$predict_rf) %>%
  hc_add_series(name = 'last', type = "line", color = "#F7CAC9",
                data = result$predict_rf) %>%
  hc_add_series(name = 'moving', type = "line", color = "#F3CF55",
                data = moving$predict_rf) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_xAxis(categories = moving$date) %>%
  hc_yAxis(title = list(text = "activation"))
print(hc) 



####add_asus####
shift1 <- read_csv('./ResultData/ZF3/googletrend/v1_google_trend_shift_all.csv')
shift2 <- read_csv('./ResultData/ZF3/googletrend/v1_google_trend_asus_shift_all.csv')

#rmse
shift1$se <- (shift1$predict_rf - shift1$volume)^2
shift2$se <- (shift2$predict_rf - shift2$volume)^2

rmse1 <- ddply(shift1,c('dist_country','model'),summarise,rmse = round(sqrt(mean(se,na.rm = T))))
rmse2 <- ddply(shift2,c('l0name','model'),summarise,rmse = round(sqrt(mean(se,na.rm = T))))

#TW_ZE552KL
plot1 <- shift1[shift1$dist_country == 'TW'&
                   shift1$model == 'ZE552KL',]
plot2 <- shift2[shift2$l0name == 'TW'&
                  shift2$model == 'ZE552KL',]

hc <- highchart() %>%
  hc_title(text = 'TW_ZE552KL') %>%
  hc_add_series(name = 'Real', type = "line", color = "#004B8D",
                data = plot1$volume) %>%
  hc_add_series(name = 'fitted1', type = "line", color = "#88B04B",
                data = plot1$fitted_rf) %>%
  hc_add_series(name = 'predict1', type = "line", color = "#88B04B",
                data = plot1$predict_rf) %>%
  hc_add_series(name = 'fitted2', type = "line", color = "#CE3175",
                data = plot2$fitted_rf) %>%
  hc_add_series(name = 'predict2', type = "line", color = "#CE3175",
                data = plot2$predict_rf) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_xAxis(categories = plot1$date) %>%
  hc_yAxis(title = list(text = "activation"))
print(hc) 

#TW_ZE552KL
plot1 <- shift1[shift1$dist_country == 'TW'&
                  shift1$model == 'ZE520KL',]
plot2 <- shift2[shift2$l0name == 'TW'&
                  shift2$model == 'ZE520KL',]

hc <- highchart() %>%
  hc_title(text = 'TW_ZE520KL') %>%
  hc_add_series(name = 'Real', type = "line", color = "#004B8D",
                data = plot1$volume) %>%
  hc_add_series(name = 'fitted1', type = "line", color = "#88B04B",
                data = plot1$fitted_rf) %>%
  hc_add_series(name = 'predict1', type = "line", color = "#88B04B",
                data = plot1$predict_rf) %>%
  hc_add_series(name = 'fitted2', type = "line", color = "#CE3175",
                data = plot2$fitted_rf) %>%
  hc_add_series(name = 'predict2', type = "line", color = "#CE3175",
                data = plot2$predict_rf) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_xAxis(categories = plot1$date) %>%
  hc_yAxis(title = list(text = "activation"))
print(hc) 

#plot_shift_平均位移週次
avg_shift1 <- shift1[shift1$date =='2017-03-01',]
counts <- ddply(avg_shift1,c('dist_country'),summarise,avg_shift_week = round(mean(shiftweek)))
counts1 <- ddply(avg_shift1,c('dist_country'),nrow)
counts$models <- counts1$V1

hc <- highchart() %>%
  hc_title(text = '平均位移週次') %>%
  hc_add_series(name = 'models', type = "column", color = "#F7CAC9",
                data = counts$models) %>%
  hc_add_series(name = 'avg_shift_week', type = "line", color = "#91A8D0",
                data = counts$avg_shift_week) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_xAxis(categories = counts$dist_country) %>%
  hc_yAxis(title = list(text = "week(s)"))
print(hc) 