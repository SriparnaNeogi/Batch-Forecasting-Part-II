library(dplyr)
library(padr)
library(forecast)
library(nnfor)
library(tsfknn)
library(ggplot2)
library(smooth)
library(MAPA)
library(tsfeatures)
library(thief)
library(nnfor)
library(lubridate)

df =read.csv("PL_G0.csv")
#CONVERT ALL NAs to ZEROES
df= df %>% fill_by_value(value=0)
n1 = ncol(df)
#REMOVING ROWS WITH ALL NA AND REMOVING ROWS WITH ALL NAs EXCEPT 10 (I.E., 10 DATAPOINTs)
df = df[rowSums(is.na(df[,2:n1])) < n1-12, ]
#Converting it into a matrix
df_m = as.matrix(df)
df_mt = t(df_m)
df2mt= df_mt
c = ncol(df2mt)
h_out=21
h=5


f_ets = matrix(nrow = h_out, ncol = c)
f_ar = matrix(nrow = h_out, ncol = c)
f_stlf = matrix(nrow = h_out, ncol = c)
f_tbats = matrix(nrow = h_out, ncol = c)
f_knn = matrix(nrow = h_out, ncol = c)
f_mlp = matrix(nrow = h_out, ncol = c)
f_theta = matrix(nrow = h_out, ncol = c)
f_nn= matrix(nrow = h_out, ncol = c)


  f_ets = round(forecast(ets(ts(df2mt, frequency=9),model='ZZZ'), h=h_out)$mean)
  f_ar = forecast(auto.arima(ts(df2mt,frequency=9)), h=h_out)$mean
  f_theta = round(thetaf(ts(df2mt, frequency=9),h=h_out)$mean)
  f_stlf= round(forecast(stlf(ts(df2mt, frequency=9),biasadj=TRUE), h=h_out)$mean)
  f_tbats= round(forecast(tbats(ts(df2mt, frequency=9),biasadj=TRUE), h=h_out)$mean)
  f_knn = knn_forecasting(ts(df2mt, frequency=9), h=h_out, k=9, msas = "recursive",cf="mean")$prediction
  f_mlp= round(forecast(mlp(ts(df2mt,frequency=9)),h=h_out)$mean)
  f_nn= round(forecast(nnetar(ts(df2mt,frequency=9)),h=h_out)$mean)
   
  ets(ts(df2mt, frequency=9),model='ZZZ')
  
  round(forecast(ets(ts(df2mt, frequency=9),model='ZZZ'), h=h_out)$mean)
f_ar_t = t(f_ar)
f_ets_t = t(f_ets)
f_stlf_t  = t(f_stlf)
f_tbats_t = t(f_tbats)
f_theta_t = t(f_theta)
f_knn_t = t(f_knn)
f_mlp_t = t(f_mlp)



#write.table(f_stlf_t,file="2021_FEB_G0_21M_STLF.csv",sep=",")
#write.table(f_mlp_t,file="2021_FEB_G0_21M_ANN.csv",sep=",")
#write.table(f_theta_t,file="2021_FEB_G0_21M_THETA.csv",sep=",")


f_combo1 = matrix(nrow=h_out, ncol = c)
f_combo2 = matrix(nrow=h_out, ncol = c)
f_combo3 = matrix(nrow=h_out, ncol = c)
f_combo4 = matrix(nrow=h_out, ncol = c)
f_combo4 = matrix(nrow=h_out, ncol = c)
f_combo5 = matrix(nrow=h_out, ncol = c)



    f_combo1 = round((f_mlp + 3*f_theta)/3.85);
    f_combo2 = round((f_knn + 2*f_stlf)/3);
    f_combo3 = round((f_mlp + 2*f_knn)/3);
    f_combo4 = round((f_mlp + 2*f_stlf)/3);
    f_combo5 = round((f_ar + 5*f_mlp)/5.5);


f_combo1_t = t(f_combo1)
f_combo2_t = t(f_combo2)
f_combo3_t = t(f_combo3)
f_combo4_t = t(f_combo4)
f_combo5_t = t(f_combo5)


write.table(f_combo1_t,file="2021_FEB_G0_21M_MLP_THETA.csv",sep=",")
write.table(f_combo2_t,file="2021_FEB_G0_21M_KNN_STLF.csv",sep=",")
write.table(f_combo3_t,file="2021_FEB_G0_21M_KNN_MLP.csv",sep=",")
write.table(f_combo4_t,file="2021_FEB_G0_21M_STLF_MLP.csv",sep=",")
write.table(f_combo5_t,file="2021_FEB_G0_21M_ARIMA_MLP.csv",sep=",")





###############################################     IN SAMPLE ACCURACY GRAPHS      ################################################

  t_ar = forecast(auto.arima(ts(df2mt[1:(l-h)], frequency=9)),h=h)$mean
  t_ets= forecast(ets(ts(df2mt[1:(l-h)], frequency=9),model='ZZZ'),h=h)$mean
  t_theta= thetaf(ts(df2mt[1:(l-h)], frequency=9),h=h)$mean
  t_stl = forecast(stlf(ts(df2mt[1:(l-h)], frequency=9)),h=h)$mean
  t_tbats= forecast(tbats(ts(df2mt[1:(l-h)], frequency=9)),h=h)$mean
  t_nn = forecast(nnetar(ts(df2mt[1:(l-h)],frequency=9)),h=h)$mean
  t_mlp= forecast(mlp(ts(df2mt[1:(l-h)], frequency=9)),h=h)$mean
  t_knn = knn_forecasting(ts(df2mt[1:(l-h)], frequency=9),h=h,k=9, msas = "recursive",cf="mean")$prediction
  t_combo5 <- (t_ar +5*t_mlp)/5.5

l=length(df2mt)

autoplot(ts(df2mt,frequency=8)) +
  autolayer(forecast(auto.arima(ts(df2mt[1:(l-h)],frequency=8)),h=h), series = "Auto-ARIMA", PI=FALSE) +
  autolayer(forecast(ets(ts(df2mt[1:(l-h)],frequency=8), model='ZZZ'),h=h), series = "Auto-ETS", PI=FALSE) +
  autolayer(thetaf(ts(df2mt[1:(l-h)],frequency=8),h=h), series = "THETA", PI=FALSE)+
  autolayer(forecast(stlf(ts(df2mt[1:(l-h)],frequency=8)),h=h), series = "STL", PI=FALSE)+
  autolayer(forecast(tbats(ts(df2mt[1:(l-h)],frequency=8),seasonal.periods=c(4,8)),h=h), series = "TBATS", PI=FALSE)+
ylab("Monthly Sales") +
  ggtitle("Forecast Comparison (Classical+SeasDecomp) PL G0")

autoplot(ts(df2mt,frequency=9)) +   
  autolayer(forecast(mlp(ts(df2mt[1:(l-h)],frequency=9)),h=h),series = "MLP")+
  autolayer(forecast(nnetar(ts(df2mt[1:(l-h)],frequency=9)),h=h), series = "NN", PI=FALSE)+ 
  autolayer(combo5, series= "WE2")+
  ylab("Monthly Sales") +
    ggtitle("Forecast Comparison (NN+WE) PL G0")
  

pred <- knn_forecasting(ts(df2mt[1:(l-h)],frequency=9), h = 12, lags = 1:12, k = 2, msas = "MIMO")
autoplot(pred, highlight = "neighbors", faceting = FALSE)



#####################################################################  Accuracy ###################
acc_ar = matrix(nrow = c, ncol = 5)
acc_ets = matrix(nrow = c, ncol = 5)
acc_thetaf = matrix(nrow = c, ncol = 5)
acc_stlf = matrix(nrow = c, ncol = 5)
acc_tbats = matrix(nrow = c, ncol = 5)
acc_nn =  matrix(nrow = c,ncol = 5)
acc_mlp =  matrix(nrow = c,ncol = 5)
acc_knn =  matrix(nrow = c,ncol = 5)
acc_combo5 =  matrix(nrow = c,ncol = 5)

  acc_ar = accuracy(ts(t_ar,frequency=9),ts(df2mt[(n-(h+1)):n,], frequency=9))										
  acc_ets  = accuracy (ts(t_ets, frequency=9),ts(df2mt[(n-(h+1)):n,], frequency=9))
  acc_thetaf = accuracy (ts(t_theta, frequency=9),ts(df2mt[(n-(h+1)):n,], frequency=9))
  acc_tbats = accuracy(ts(t_tbats, frequency=9),ts(df2mt[(n-(h+1)):n,], frequency=9))
  acc_stlf = accuracy(ts(t_stl,frequency=9),ts(df2mt[(n-(h+1)):n,], frequency=9))
  acc_mlp = accuracy(ts(t_mlp,frequency=9),ts(df2mt[(n-(h+1)):n,], frequency=9))
  acc_nn = accuracy(ts(t_nn,frequency=9),ts(df2mt[(n-(h+1)):n,], frequency=9))
  acc_knn = accuracy(ts(t_knn,frequency=9),ts(df2mt[(n-(h+1)):n,], frequency=9))
  acc_combo5 = accuracy(ts(t_combo5,frequency=9),ts(df2mt[(n-(h+1)):n,], frequency=9))

  
  acc_all=rbind(acc_ar,acc_ets, acc_thetaf,acc_tbats, acc_stlf, acc_mlp, acc_nn,  acc_knn,acc_combo5)
  rownames(acc_all) <- c("Auto-ARIMA","Auto-ETS","Theta","TBATS", "STL", "MLP","NN","KNN","WE2")
  View(acc_all)
  

  #################### LM ######################
  h=5
  data = t(as.matrix(read.csv("PL_G0.csv", header = FALSE)))
  colnames(data) <- c("date","sales")
  data = data.frame(data)
  data$date = as.Date(data$date, "%d/%m/%Y")
  data$sales= as.numeric (as.character(data$sales))
  data= arrange(data,date)
  extended_data = data %>%
    dplyr::mutate(years = lubridate::year(date), 
                  months = lubridate::month(date) 
    )
  train <- extended_data[1:36, ] # initial data
  pred <- extended_data[37:41,]
  train_df = as.data.frame(train[,2:4])
  pred_df=(as.data.frame(pred %>% 
                           dplyr::select(months, years)))
  lm(sales ~ factor(months) + factor(years),data = train_df)
  p=predict(lm(sales ~ factor(months) + factor(years),data = train_df), newdata = pred_df)
  View(p)
  
  ################# PROPHET #############
  library(prophet)
  library(tidyverse)
  str(data)
  ds = data
  colnames (ds) = c("ds","y")
  #Fit model
  m = prophet(ds[1:36,])
  # Make future periods
  future <- make_future_dataframe(m, periods = 5)
  future
  forecast <- predict(m, future)
  tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  plot(m, forecast)+ggtitle("FB Prophet ")
  prophet_plot_components(m, forecast)
  class(forecast)
  View(forecast$yhat)
  
  
  knn_forecasting(ts(df2mt, frequency=9), h=h_out, k=9, msas = "recursive",cf="mean")
  
  nnar=nnetar(ts(df2mt[1:(l-h)],frequency=9))
