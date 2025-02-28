library(quantmod)
library(dplyr)
library(lubridate)
library(xts)

##用爬蟲抓下需要的yahoo finance資料
#樣本資料從2024-01-01到2024-05-31
start_date <- "2024-01-01"
end_date <- "2024-05-31"

# 股票代碼
tickers <- c("2308.TW", "2327.TW", "2330.TW", "2345.TW", "2368.TW",
             "2376.TW", "2383.TW", "2404.TW", "2454.TW", "3017.TW",
             "3023.TW", "3034.TW", "3035.TW", "3406.TW", "3443.TW",
             "3653.TW", "4919.TW", "4961.TW", "5388.TW", "6196.TW",
             "6409.TW", "8210.TW", "2317.TW", "2382.TW", "2412.TW",
             "3711.TW", "2303.TW", "6669.TW", "3231.TW")

combined_data_df <- data.frame(Date = seq(from = as.Date(start_date), to = as.Date(end_date), by = "day"))
# 抓每天的資料
for (ticker in tickers) {
  stock_data <- tryCatch({
    getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  }, error = function(e) {
    message(paste("Error downloading data for", ticker))
    return(NULL)
  })
  
  if (!is.null(stock_data)) {
    adjusted_prices <- Ad(stock_data) # 取出調整後收盤價
    daily_prices_df <- data.frame(Date = index(adjusted_prices), coredata(adjusted_prices))
    colnames(daily_prices_df)[2] <- ticker  #命名為股票代碼
    combined_data_df <- merge(combined_data_df, daily_prices_df, by = "Date", all.x = TRUE)
  }
}

combined_data_df <- na.omit(combined_data_df)  ##刪掉包含na的列
head(combined_data_df)   
#write.csv(combined_data_df, "outsample_stock_price.csv", row.names = FALSE)

###個別資產樣本外報酬
#個別資產的每日return
#combined_data_df <- read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/期末報告/outsample_stock_price.csv",header = TRUE, sep=",", na.strings = "null") 
combined_data_df[,1]<-as.Date(combined_data_df[,1])
class(combined_data_df[,1]) #Date
dim(combined_data_df )
which(is.na(combined_data_df))  #0

##return for all 29 stocks
ret_outsample <- data.frame(Date = combined_data_df[,1])

retx <- function(x) {
  x[-1] / x[-length(x)] - 1
}


for (i in 2:ncol(combined_data_df)) {  
  col_name <- colnames(combined_data_df)[i]  
  returns <- retx(combined_data_df[, i])  
  ret_outsample[[col_name]] <- c(NA,returns) 
}

head(ret_outsample)
ret_outsample[is.na(ret_outsample)==1]<-0   ##第一列NA補成0

#cumulative return for all 29 stocks
cumuret_outsample <- data.frame(Date = combined_data_df[,1])
for (i in 2:ncol(combined_data_df)) {  
  col_name <- colnames(combined_data_df)[i]  
  returns <- retx(combined_data_df[, i])  
  cumu_ret <- cumprod(1+returns)
  cumuret_outsample[[col_name]] <- c(1,cumu_ret)
}
head(cumuret_outsample)

##plot return
y.range = range(ret_outsample[,2:ncol(ret_outsample)])
windows(width=10, height=8)
par(mfrow = c(3, 2))  
#股票代碼對應的股票名稱
ticker_names <- c("台達電", "國巨", "台積電", "智邦", "金像電",
                  "技嘉", "台光電", "漢唐", "聯發科", "奇鋐",
                  "信邦", "聯詠", "智原", "玉晶光", "創意",
                  "健策", "新唐", "天鈺", "中磊", "帆宣",
                  "旭隼", "勤誠", "鴻海", "廣達", "中華電",
                  "日月光控投", "聯電", "緯穎", "緯創")
start_col = 2
end_col = 7
for (i in start_col:end_col) {
  y.range <- range(ret_outsample[, i], na.rm = TRUE)*100  # 第i+1列是股票数据，第1列是Date
  
  plot(x = ret_outsample$Date, y = ret_outsample[, i]*100, type = "l", 
       ylim = y.range, 
       xlab = "Date", ylab = paste(ticker_names[i-1], "daily return"),
       main = paste("Daily returns  of", ticker_names[i-1],
                    ", 2024-01-02 ~ 2024-05-31"), col = i)
}

windows(width=10, height=8)
par(mfrow = c(3, 2)) 
start_col = 8
end_col = 13
for (i in start_col:end_col) {
  y.range <- range(ret_outsample[, i], na.rm = TRUE)*100  # 第i+1列是股票数据，第1列是Date
  
  plot(x = ret_outsample$Date, y = ret_outsample[, i]*100, type = "l", 
       ylim = y.range, 
       xlab = "Date", ylab = paste(ticker_names[i-1], "daily return"),
       main = paste("Daily returns  of", ticker_names[i-1],
                    ", 2024-01-02 ~ 2024-05-31"), col = i)
}


windows(width=10, height=8)
par(mfrow = c(3, 2)) 
start_col = 14
end_col = 19
for (i in start_col:end_col) {
  y.range <- range(ret_outsample[, i], na.rm = TRUE)*100  # 第i+1列是股票数据，第1列是Date
  
  plot(x = ret_outsample$Date, y = ret_outsample[, i]*100, type = "l", 
       ylim = y.range, 
       xlab = "Date", ylab = paste(ticker_names[i-1], "daily return"),
       main = paste("Daily returns  of", ticker_names[i-1],
                    ", 2024-01-02 ~ 2024-05-31"), col = i)
}

windows(width=10, height=8)
par(mfrow = c(3, 2)) 
start_col = 20
end_col = 25
for (i in start_col:end_col) {
  y.range <- range(ret_outsample[, i], na.rm = TRUE)*100  # 第i+1列是股票数据，第1列是Date
  
  plot(x = ret_outsample$Date, y = ret_outsample[, i]*100, type = "l", 
       ylim = y.range, 
       xlab = "Date", ylab = paste(ticker_names[i-1], "daily return"),
       main = paste("Daily returns  of", ticker_names[i-1],
                    ", 2024-01-02 ~ 2024-05-31"), col = i)
}

windows(width=10, height=8)
par(mfrow = c(3, 2)) 
start_col = 26
end_col = 30
for (i in start_col:end_col) {
  y.range <- range(ret_outsample[, i], na.rm = TRUE)*100  # 第i+1列是股票数据，第1列是Date
  
  plot(x = ret_outsample$Date, y = ret_outsample[, i]*100, type = "l", 
       ylim = y.range, 
       xlab = "Date", ylab = paste(ticker_names[i-1], "daily return"),
       main = paste("Daily returns  of", ticker_names[i-1],
                    ", 2024-01-02 ~ 2024-05-31"), col = i)
}

##plot cumulative return for all 29 stocks
y.range<-range(cumuret_outsample[,2:ncol(cumuret_outsample)])*100

windows(width=20, height=18)

plot(x = cumuret_outsample$Date, y = cumuret_outsample[, 2]*100, type = "l", 
     ylim = y.range, 
     xlab = "Date", ylab = "Cumulative gross return",
     main = "Cumulative returns of 29 stocks, 2024-01-02 ~ 2024-05-31")


last_values <- numeric(ncol(cumuret_outsample) - 1)
abline(h=100)
for (i in 2:ncol(cumuret_outsample)) {
  lines(x = cumuret_outsample$Date, y = cumuret_outsample[, i]*100, col = i, lty = 1, lwd = 2)
  last_values[i - 1] <- tail(cumuret_outsample[, i], 1)  # 存储最后一个数据点的值
}

colors = seq(2,30,1)
sorted_indices <- order(last_values, decreasing = TRUE)
sorted_colors <- colors[sorted_indices]
sorted_legends <- ticker_names[sorted_indices]

legend("topleft", legend = sorted_legends, col = sorted_colors, lty = 1, lwd = 2, cex = 0.7)

# Descriptive statistics of returns
##functions for calculating skewness and kurtosis
my_skewness<-function(x){
  T<-length(x)
  y<-x-mean(x)
  T*sqrt(T-1)/(T-2)*sum(y^3)/(sum(y^2)^(3/2))
}

my_kurtosis<-function(x){
  T<-length(x)
  y<-x-mean(x)
  f1<-T*(T+1)*(T-1)/((T-2)*(T-3))
  f2<-3*((T-1)^2)/((T-2)*(T-3))
  f1*sum(y^4)/(sum(y^2)^2)-f2
}

##a function for extracting ACF(1)
my_acf1<-function(x){            
  acf(x, plot=F)[[1]][2] 
}           

summary_data<-rbind(apply(ret_outsample[,2:ncol(ret_outsample)],2, length), 
                    apply(ret_outsample[,2:ncol(ret_outsample)]*100,2, summary)[c(1,3,6,4),],
                    apply(ret_outsample[,2:ncol(ret_outsample)]*100,2, sd)*sqrt(252),
                    apply(ret_outsample[,2:ncol(ret_outsample)]*100,2, my_skewness)*sqrt(252),
                    apply(ret_outsample[,2:ncol(ret_outsample)]*100,2, my_kurtosis)/252,
                    apply(ret_outsample[,2:ncol(ret_outsample)],2, my_acf1))

## Names of the statistics
rownames(summary_data)[1:nrow(summary_data)]<-c("Num of Obs", "Min","Median", "Max", "Mean", "Std","Skewness","Kurtosis","ACF1")

## Round decimals                              
summary_data<-round(summary_data,3)
colnames(summary_data) = ticker_names
t(summary_data)
#write.csv(t(summary_data), "summary.csv", row.names = FALSE)


##計算不同策略下的Net Return
library(quadprog)  
##gmvp using quadratic programming
gmvp_wx_quad<-function(r){              ## r: return data, mu_targ: required target expected return 
  
  n<-dim(r)[2]                          ## number of assets               
  covx<-cov(r, use = "complete.obs" )   ## sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1,n),n,1)            ## vector of ones
  
  A<-onex                               ## A matrix
  b0<-matrix(1, 1, 1)                   ## b vector
  d0<-matrix(rep(0,n), n, 1)            ## d vector
  
  wx<-solve.QP(Dmat = covx, dvec = d0, 
               Amat = A, bvec = b0, meq = 1)$solution  ##meq有幾條限制式
  return(wx)
  
}
## no-shortsale gmvp
nsgmvp_wx_quad<-function(r){
  
  n<-dim(r)[2]                          ## number of assets               
  covx<-cov(r, use = "complete.obs" )   ## sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1, n), n, 1)         ## vector of ones
  
  A<-onex                               ## A matrix
  Ix<-diag(1, n)                        ## An identity matrix for no-shortsale constraints
  A<-cbind(A, Ix)                       ##At=[11111][100000...][0100...]
  b0<-matrix(c(1, rep(0, n)), 1+n, 1)   ## b_0
  d0<-matrix(rep(0, n), n, 1)           ## d_0  
  
  wx<-solve.QP(Dmat = covx, dvec = d0, Amat = A, 
               bvec = b0, meq = 1)$solution
  return(wx)
  
}  
## portfolio weights of the tangency portfolio
tan_wx<-function(r, rf){                     ##r: return data, rf: risk-free return              
  
  n<-dim(r)[2]                               ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)          ##mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )        ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  inv_covx<-solve(covx)                      ##inverse covariance matrix
  onex<-matrix(rep(1,n), n, 1)               ##vector of ones
  
  ##other vector and constant
  rpx<-mux-rf*onex                           ##risk-premium vector 
  Dx<-t(onex)%*%inv_covx%*%rpx
  Dx<-as.numeric(Dx)
  
  ##optimal weight vector
  
  wx<-1/Dx*inv_covx%*%rpx
  
  return(wx)  
  
}
## no-shortsale mvp
mu_targ<-0.003
nsmvp_wx_quad<-function(r, mu_targ){
  
  n<-dim(r)[2]                          ## number of assets               
  mux<-apply(r, 2, mean, na.rm = T)     ## (in-sample) mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )   ## sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1, n), n,1)          ## vector of ones
  
  A<-cbind(mux, onex)                   ## A matrix
  Ix<-diag(1, n)                        ## An identity matrix for no-shortsale constraints
  A<-cbind(A, Ix)
  b0<-matrix(c(mu_targ, 1, rep(0, n)), 2+n, 1)      ##b_0
  d0<-matrix(rep(0, n), n, 1)                       ##d_0  
  
  wx<-solve.QP(Dmat = covx, dvec = d0, Amat = A, 
               bvec = b0, meq = 2)$solution
  return(wx)
  
}

#Net return of EW/GMVP/GMVP_nss/Tangency/MSRP_nss
kx<-40                                   ## window length
hx<-nrow(ret_outsample)-kx                       ## length of out-of-sample period

## portfolio weights, starting from period t-1
wx_mat_EW<-matrix(0, hx+1, ncol(ret_outsample)-1)
wx_mat_GMVP<-matrix(0, hx+1, ncol(ret_outsample)-1)
wx_mat_GMVPnss<-matrix(0, hx+1, ncol(ret_outsample)-1)
wx_mat_MSRP<-matrix(0, hx+1, ncol(ret_outsample)-1)
wx_mat_MVPnss<-matrix(0, hx+1, ncol(ret_outsample)-1)

## portfolio net return
por_netrx_EW<-numeric(hx)
por_netrx_GMVP<-numeric(hx)
por_netrx_GMVPnss<-numeric(hx)
por_netrx_MSRP<-numeric(hx)
por_netrx_MVPnss<-numeric(hx)

## turn over rate
tor_EW<-numeric(hx)
tor_GMVP<-numeric(hx)
tor_GMVPnss<-numeric(hx)
tor_MSRP<-numeric(hx)
tor_MVPnss<-numeric(hx)

## HHI
hhi_EW<-numeric(hx)
hhi_GMVP<-numeric(hx)
hhi_GMVPnss<-numeric(hx)
hhi_MSRP<-numeric(hx)
hhi_MVPnss<-numeric(hx)

## SLR
slr_EW<-numeric(hx)
slr_GMVP<-numeric(hx)
slr_GMVPnss<-numeric(hx)
slr_MSRP<-numeric(hx)
slr_MVPnss<-numeric(hx)

epx<-0.005                                  ##transcation cost

rfx = 1.5/100    ##台灣10年期公債殖利率1.5%

for(i in 2:hx){
  
  datax<-ret_outsample[i:(i+kx-1),2:ncol(ret_outsample)]                      ## data in the window (rolling window)
  
  wx<-1/29                                 ##EW
  wx1<-gmvp_wx_quad(datax)                 ## gmvp
  wx2<-nsgmvp_wx_quad(datax)               ##gmvp without short selling
  wx3<-tan_wx(datax,rfx)                   ##tangency portfolio
  wx4<-nsmvp_wx_quad(datax,mu_targ)        ##mvp without short selling
  
  rx<-ret_outsample[i+kx,2:ncol(ret_outsample)]  ## return at period i+kx (period t+1)
  rx_lag<-datax[nrow(datax),]                   ## return at period i+kx-1 (period t)
  
  ## individual assets' turnover over rate
  tor_ind<-wx-wx_mat_EW[i,]*(1+rx_lag)/(1+sum(wx_mat_EW[i,]*rx_lag))
  tor1_ind<-wx1-wx_mat_GMVP[i,]*(1+rx_lag)/(1+sum(wx_mat_GMVP[i,]*rx_lag))
  tor2_ind<-wx2-wx_mat_GMVPnss[i,]*(1+rx_lag)/(1+sum(wx_mat_GMVPnss[i,]*rx_lag))
  tor3_ind<-wx3-wx_mat_MSRP[i,]*(1+rx_lag)/(1+sum(wx_mat_MSRP[i,]*rx_lag))
  tor4_ind<-wx4-wx_mat_MVPnss[i,]*(1+rx_lag)/(1+sum(wx_mat_MVPnss[i,]*rx_lag))
  
  ## portfolio turn over rate
  tor_EW[i]<-sum(abs(tor_ind))
  tor_GMVP[i]<-sum(abs(tor1_ind))
  tor_GMVPnss[i]<-sum(abs(tor2_ind))
  tor_MSRP[i]<-sum(abs(tor3_ind))
  tor_MVPnss[i]<-sum(abs(tor4_ind))
  
  ## portfolio net return
  por_netrx_EW[i]<-(1+sum(wx*rx))*(1-epx*tor_EW[i])-1
  por_netrx_GMVP[i]<-(1+sum(wx1*rx))*(1-epx*tor_GMVP[i])-1
  por_netrx_GMVPnss[i]<-(1+sum(wx2*rx))*(1-epx*tor_GMVPnss[i])-1
  por_netrx_MSRP[i]<-(1+sum(wx3*rx))*(1-epx*tor_MSRP[i])-1
  por_netrx_MVPnss[i]<-(1+sum(wx4*rx))*(1-epx*tor_MVPnss[i])-1
  
  ## HHI
  hhi_EW[i]<-sum(wx^2)/(sum(abs(wx))^2)
  hhi_GMVP[i]<-sum(wx1^2)/(sum(abs(wx1))^2)
  hhi_GMVPnss[i]<-sum(wx2^2)/(sum(abs(wx2))^2)
  hhi_MSRP[i]<-sum(wx3^2)/(sum(abs(wx3))^2)
  hhi_MVPnss[i]<-sum(wx4^2)/(sum(abs(wx4))^2)
  
  ## SLR
  slr_EW[i]<-sum(abs(wx[wx<0]))/sum(abs(wx[wx>0]))
  slr_GMVP[i]<-sum(abs(wx1[wx1<0]))/sum(abs(wx1[wx1>0]))
  slr_GMVPnss[i]<-sum(abs(wx2[wx2<0]))/sum(abs(wx2[wx2>0]))
  slr_MSRP[i]<-sum(abs(wx3[wx3<0]))/sum(abs(wx3[wx3>0]))
  slr_MVPnss[i]<-sum(abs(wx4[wx4<0]))/sum(abs(wx4[wx4>0]))
  
  ## store portfolio weight vector at this period
  wx_mat_EW[i+1,]<-wx
  wx_mat_GMVP[i+1,]<-wx1
  wx_mat_GMVPnss[i+1,]<-wx2
  wx_mat_MSRP[i+1,]<-wx3
  wx_mat_MVPnss[i+1,]<-wx4

}  

por_netret_outsample<-data.frame(Date = combined_data_df[(kx+1):nrow(combined_data_df),]$Date)  
cumunetret_outsample<-data.frame(Date = combined_data_df[(kx+1):nrow(combined_data_df),]$Date)  
por_netret_outsample$EW = por_netrx_EW
por_netret_outsample$GMVP = por_netrx_GMVP
por_netret_outsample$GMVPnss = por_netrx_GMVPnss
por_netret_outsample$Tangency = por_netrx_MSRP
por_netret_outsample$MVPnss = por_netrx_MVPnss

##Price Weight
rolldata<-combined_data_df[(kx+1):nrow(combined_data_df),]
weights <- rolldata[, -1] / rowSums(rolldata[, -1])
weights <- cbind(Date = rolldata[, 1], weights)
weights<-weights[-nrow(weights),2:ncol(weights)]

netret_temp <- ret_outsample[(kx+1):nrow(combined_data_df),-1]
por_retpw = numeric(nrow(netret_temp)-1)
for(i in 1:(nrow(netret_temp )-1)){
  por_retpw[i]<-as.numeric(netret_temp[i,1:ncol(netret_temp )])%*%as.numeric(weights[i,]) 
}

por_netret_outsample$PW<- c(0,por_retpw)
pw_cumr<-cumprod(1+por_retpw)
cumunetret_outsample$PW <-c(1,pw_cumr)


## Buy-and-hold portfolio (initial weight 1/N)
rx<-rbind(0, netret_temp[-1,2:ncol(netret_temp)])   ##add initial period (return=0) 
rx<-1+rx                                             ##gross return(累積反推各期報酬)
head(rx)
bh_cumr<-apply(rx, 2, cumprod)                       ##cumulative return of each asset
bh_cumr<-apply(bh_cumr, 1, mean)                     ##average of these cumulative returns
por_retbh<-bh_cumr[-1]/bh_cumr[-length(bh_cumr)]-1   ##portfolio return at each period(GRt+1/GRt-1)
por_netret_outsample$BH <- c(0,por_retbh)
head(por_netret_outsample)

##Cumulative Return of every strategy
for (i in 2:ncol(por_netret_outsample)) {  
  col_name <- colnames(por_netret_outsample)[i]  
  cumu_ret <- cumprod(1+por_netret_outsample[,i])
  cumunetret_outsample[[col_name]] <- cumu_ret
}

head(cumunetret_outsample)

##plot of the out-of-sample (oos) portfolio returns
y.range = range(por_netret_outsample[,2:ncol(por_netret_outsample)])*100
windows(width=10, height=8)
par(mfrow = c(2, 2))  
plot(x = por_netret_outsample$Date, y = por_netret_outsample$EW*100, type = "l", 
       ylim = y.range, 
       xlab = "Date", ylab = "daily return of Equal Weight(%)",
       main = paste("Equal Weight Portfolios",
                    ", 2024-03-08 ~ 2024-05-31"), col = 2)
plot(x = por_netret_outsample$Date, y = por_netret_outsample$PW*100, type = "l", 
     ylim = y.range, 
     xlab = "Date", ylab = "daily return of Price Weight(%)",
     main = paste("Price Weight Portfolios",
                  ", 2024-03-08 ~ 2024-05-31"), col = 3)
plot(x = por_netret_outsample$Date, y = por_netret_outsample$BH*100, type = "l", 
     ylim = y.range, 
     xlab = "Date", ylab = "daily return of Buy and Hold(%)",
     main = paste("Buy and Hold",
                  ", 2024-03-08 ~ 2024-05-31"), col = 4)

##plot of the out-of-sample (oos) portfolio returns
y.range = range(por_netret_outsample[,2:ncol(por_netret_outsample)])*100
windows(width=12, height=8)
par(mfrow = c(2, 2))  
plot(x = por_netret_outsample$Date, y = por_netret_outsample$GMVP*100, type = "l", 
     ylim = y.range, 
     xlab = "Date", ylab = "daily return of GMVP(%)",
     main = paste("Daily  Returns  of GMVP Portfolios",
                  ", 2024-03-08~2024-05-31"), col = 5)
plot(x = por_netret_outsample$Date, y = por_netret_outsample$GMVPnss*100, type = "l", 
     ylim = y.range, 
     xlab = "Date", ylab = "daily return of GMVP without short sale(%)",
     main = paste("Daily  Returns  of No Short GMVP",
                  ", 2024-03-08~2024-05-31"), col = 6)
plot(x = por_netret_outsample$Date, y = por_netret_outsample$Tangency*100, type = "l", 
     ylim = y.range, 
     xlab = "Date", ylab = "daily return of Tangency Portfolios(%)",
     main = paste("Daily  Returns  of Tangency Portfolios",
                  ", 2024-03-08~2024-05-31"), col = 7)
plot(x = por_netret_outsample$Date, y = por_netret_outsample$MVPnss*100, type = "l", 
     ylim = y.range, 
     xlab = "Date", ylab = "daily return of MVP without short sales(%)",
     main = paste("Daily  Returns  of No Short MVP",
                  ", 2024-03-08~2024-05-31"), col = 8)


##plot out of sample cumulative gross returns of the portfolios
y.range<-range(cumunetret_outsample[,2:ncol(cumunetret_outsample)])*100
windows(width=20, height=18)
plot(x = cumunetret_outsample$Date, y = cumunetret_outsample[, 2]*100, type = "l", 
     ylim = y.range, 
     xlab = "Date", ylab = "Cumulative gross return",
     main = "Cumulative Returns of  Portfolios, 2024-03-08 ~ 2024-05-31")

abline(h = 100)
last_values <- numeric(ncol(cumunetret_outsample) - 1)
for (i in 2:ncol(cumunetret_outsample)) {
  lines(x = cumunetret_outsample$Date, y = cumunetret_outsample[, i]*100, col = i, lty = 1, lwd = 2)
  last_values[i - 1] <- tail(cumunetret_outsample[, i], 1)  
}

colors = seq(2,30,1)
sorted_indices <- order(last_values, decreasing = TRUE)
sorted_colors <- colors[sorted_indices]
sorted_legends <- colnames(cumunetret_outsample[,-1])[sorted_indices]

legend("topleft", legend = sorted_legends, col = sorted_colors, lty = 1, lwd = 2, cex = 0.7)

# Descriptive statistics of the oos portfolio returns
summary_data<-rbind(apply(por_netret_outsample[,2:ncol(por_netret_outsample)],2, length), 
                    apply(por_netret_outsample[,2:ncol(por_netret_outsample)]*100,2, summary)[c(1,3,6,4),],
                    apply(por_netret_outsample[,2:ncol(por_netret_outsample)]*100,2, sd)*sqrt(252),
                    apply(por_netret_outsample[,2:ncol(por_netret_outsample)]*100,2, my_skewness)*sqrt(252),
                    apply(por_netret_outsample[,2:ncol(por_netret_outsample)]*100,2, my_kurtosis)/252,
                    apply(por_netret_outsample[,2:ncol(por_netret_outsample)],2, my_acf1))

rownames(summary_data)[1:nrow(summary_data)]<-c("Num of Obs", "Min","Median", "Max", "Mean", "Std","Skewness","Kurtosis","ACF1")
                            
summary_data<-round(summary_data,3)
colnames(summary_data) = colnames(por_netret_outsample[,-1])
t(summary_data)
#write.csv(t(summary_data), "summary_data2.csv", row.names = FALSE)

## annualized Sharpe ratio
rfx<-1.5/100/252

Sharpe_ratio <- data.frame(matrix(ncol = ncol(por_netret_outsample) - 1, nrow = 1))
colnames(Sharpe_ratio) <- colnames(por_netret_outsample)[2:ncol(por_netret_outsample)]
for (i in 2:ncol(por_netret_outsample)) {  
  col_name <- colnames(por_netret_outsample)[i]
  por_sharpe_ratio <- round(((mean(por_netret_outsample[,i]) - rfx) / sd(por_netret_outsample[,i]) * sqrt(252)), 3)
  Sharpe_ratio[1, col_name] <- por_sharpe_ratio
}

Sharpe_ratio

#Turnover Rate
Turnover <- cbind(tor_EW,tor_GMVP,tor_GMVPnss,tor_MSRP,tor_MVPnss)
round(rbind(apply(Turnover*100,2, summary)[c(1,3,6,4),],
      apply(Turnover*100,2, sd)),3)
#HHI
HHI <- cbind(hhi_EW,hhi_GMVP,hhi_GMVPnss,hhi_MSRP,hhi_MVPnss)
round(rbind(apply(HHI*100,2, summary)[c(1,3,6,4),],
            apply(HHI*100,2, sd)),3)
#SLR
SLR<- cbind(slr_GMVP,slr_MSRP)
round(rbind(apply(SLR*100,2, summary)[c(1,3,6,4),],
            apply(SLR*100,2, sd)),3)

#VaR
VaR_samplex<-function(x, amountx, alphax){     ##xx: return vector, amountx: money
  qx<-as.numeric(quantile(x, alphax))    ##quantile(data, 指定分位數(0到1之間數字))
  qx*amountx
}

#ES
ES_samplex<-function(x, amountx, alphax){   ##xx: return vector, amountx: money
  qx<-as.numeric(quantile(x, alphax))
  indx<-x<=qx
  mean(x*indx)/alphax*amountx
  
}
##LPSD
LPSDx<-function(x, rfx){                ##x = asset's return, rfx = risk-free return
  
  erx<-x-rfx
  indx<-erx<0
  
  sqrt(sum((erx)^2*indx)/length(x))
  
}

alpha <- 0.05
risk_outsample <- data.frame(matrix(0,3,7))
colnames(risk_outsample) <- colnames(por_netret_outsample[,-1])
rownames(risk_outsample) <- c("VaR", "ES", "LPSD")

for (i in 2:ncol(por_netret_outsample)) {  
  col_name <- colnames(por_netret_outsample)[i]
  risk_outsample[1,i-1] <- round(VaR_samplex(por_netret_outsample[,i], 100, alpha),3)
  risk_outsample[2,i-1] <- round(ES_samplex(por_netret_outsample[,i], 100, alpha),3)
  risk_outsample[3,i-1] <- round(LPSDx(por_netret_outsample[,i], rfx)*100,3)

}
risk_outsample