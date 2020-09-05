ichimoku                                  <- function(HLC, nFast = 16, nMed = 33, nSlow = 66) {
  #browser()
  HLC2 <- copy(HLC)
  HLC2[,`:=`(turningLine = (frollapply(Najwyzszy, nFast, max) + frollapply(Najnizszy, nFast, min))/2,
             baseLine    = (frollapply(Najwyzszy, nMed, max)  + frollapply(Najnizszy, nMed, min))/2),
       by = Spolka]
  HLC2[, spanA       := shift((turningLine + baseLine)/2, nMed), by = Spolka]
  HLC2[, spanB       := shift((frollapply(Najwyzszy, nSlow, max) + frollapply(Najnizszy, nSlow, min))/2, nMed), by = Spolka]
  HLC2[, plotSpan    := shift(Zamkniecie, -nMed), by = Spolka]   
  HLC2[, laggingSpan := shift(Zamkniecie, nMed), by = Spolka]  
  HLC2[, lagSpanA    := shift(spanA, nMed), by = Spolka]  
  HLC2[, lagSpanB    := shift(spanB, nMed), by = Spolka] 
  
  colnames(HLC2) <- c(colnames(HLC2)[1:10],"Tenkan", "Kijun", "SenkouA", "SenkouB", "Chikou", "laggingSpan", "lagSpanA","lagSpanB")
  return(HLC2)
}
transform_ichimocu                        <- function(data, Calibration_Period = 'Month'){
  
  hhh <- data %>%
    lazy_dt() %>% 
    mutate(Condition_0 = lagSpanA > lagSpanB,
           Condition_1 = Zamkniecie > Kijun,
           Condition_2 = Zamkniecie > lagSpanA,
           Condition_3 = Zamkniecie > SenkouA,
           Condition_4 = Tenkan > lagSpanA & Kijun > lagSpanA,
           Condition_5 = Tenkan > Kijun,
           Condition_6 = Zamkniecie > Tenkan) %>%
    filter(Condition_0 == TRUE) %>%
    filter(Condition_1 == TRUE) %>%
    filter(Condition_2 == TRUE) %>%
    filter(Condition_3 == TRUE) %>%
    filter(Condition_4 == TRUE) %>%
    filter(Condition_5 == TRUE) %>%
    filter(Condition_6 == TRUE) %>%
    select(Spolka, Data, Year, Quarter, Month, Day, Zamkniecie) %>%
    group_by(Spolka, Year, get(Calibration_Period))  %>%
    filter(seq_len(n()) == n()) %>%
    as.data.table() 
  
  DT <- data.table()
  for(i in unique(hhh$Year)){
    for(j in unique(hhh$`get(Calibration_Period)`)){
      dec <- as.character(hhh[Year == i & `get(Calibration_Period)` == j]$Spolka)
      dt  <-  data.table(Year = i, 'Calibration_Period' = j, t(dec))
      DT  <- rbind(DT, dt, fill = T)
    }
  }
  setkeyv(DT,c('Year', 'Calibration_Period'))
  return(DT)
}
pick_stocks_MACD                          <- function(Data, year, quarter = NULL, month = NULL, top = 13){
  #browser()
  Data <-   Data[Statistics == 'Cena', .(Data, Year, Quarter, Month, Day, Spolka, Cena = PnL)] %>%
    group_by(Spolka) %>%
    mutate(EMA_22 = EMA(Cena, 22),
           EMA_66 = EMA(Cena, 66),
           MACD = EMA_22 - EMA_66,
           Signal_line = EMA(MACD, 14),
           Direction_EMA = 
             if_else(Cena < EMA_22 & Cena < EMA_66, 'Spadkowy', if_else(Cena > EMA_22 & Cena > EMA_66, 'Rosnacy', 'Consolidacja')),
           Direction_MACD = if_else(MACD < 0 & Signal_line < 0, 'Spadkowy', if_else(MACD > 0 & Signal_line > 0, 'Rosnacy', 'Consolidacja')),
           Trend = if_else(Direction_EMA == 'Spadkowy' & Direction_MACD == 'Spadkowy', 'Spadkowy', if_else(Direction_EMA == 'Rosnacy' & Direction_MACD == 'Rosnacy', 'Rosnacy', 'Brak'))) %>%
    gather(key = 'Statistics', value = "PnL", -c(Data, Year, Quarter, Month,  Day, Spolka)) %>%
    as.data.table
  
  DT <-  data.table()
  
  if(is.null(quarter)){
    Tickers <- unique(Data[Statistics == 'Trend' & Year == year & Month == month]$Spolka)
    
    Data[Statistics == 'Trend' & Year == year & Month == month] %>% 
      lazy_dt() %>%
      group_by(Spolka) %>%        
      mutate(Scalar = 1:n()/sum(1:n()),
             Trend = if_else(PnL == 'Spadkowy', -1, if_else(PnL == 'Rosnacy', 1, 0)),
             Weight = Scalar*Trend) %>%
      summarise(Points = sum(Weight)) %>%
      as.data.table() -> DT
    
    DT <- DT[order(Points)][Points > .9]#[(.N-(top-1)):(.N)]#[order(Spolka)]
    DT <- data.table(Year = year, Calibration_Period = month, t(DT$Spolka))
    
  } else {
    Tickers <- unique(Data[Statistics == 'Trend' & Year == year & Quarter == quarter]$Spolka)
    Data[Statistics == 'Trend' & Year == year & Quarter == quarter] %>% 
      lazy_dt() %>%
      group_by(Spolka) %>%        
      mutate(Scalar = 1:n()/sum(1:n()),
             Trend = if_else(PnL == 'Spadkowy', -1, if_else(PnL == 'Rosnacy', 1, 0)),
             Weight = Scalar*Trend) %>%
      summarise(Points = sum(Weight)) %>%
      as.data.table() -> DT
    
    DT <- DT[order(Points)][Points > .9]#[(.N-(top-1)):(.N)]#[order(Spolka)]
    DT <- data.table(Year = year, Calibration_Period = quarter, t(DT$Spolka))
  }
  
  #browser()
  
  return(DT)
}
ADX                                       <- function (TS, n = 14, maType = "ema", ...){
  data <- copy(TS)
  data[,dH       :=  c(rep(NA,n), diff(Najwyzszy, n)), by = Spolka]
  data[,dL       := c(rep(NA,n), -diff(Najnizszy, n)), by = Spolka]
  data[,DMIp     := fifelse(dH == dL | (dH < 0 & dL < 0), 0, fifelse(dH > dL, dH, 0)), by = Spolka]
  data[,DMIn     := fifelse(dH == dL | (dH < 0 & dL < 0), 0, fifelse(dH < dL, dL, 0)), by = Spolka]
  data[,closeLag := shift(Zamkniecie,1), by = Spolka]
  data[,trueHigh := pmax(Najwyzszy, closeLag, na.rm = FALSE), by = Spolka]
  data[,trueLow  := pmin(Najnizszy, closeLag, na.rm = FALSE), by = Spolka]
  data[,tr       := trueHigh - trueLow, by = Spolka]
  data[,TRsum    := .Call("wilderSum", tr, n, PACKAGE = "TTR"), by = Spolka]
  data[,DIp      := 100 * .Call("wilderSum", DMIp, n, PACKAGE = "TTR")/TRsum, by = Spolka]
  data[,DIn      := 100 * .Call("wilderSum", DMIn, n, PACKAGE = "TTR")/TRsum, by = Spolka]
  data[,DX       := 100 * (abs(DIp - DIn)/(DIp + DIn)), by = Spolka]
  data[is.na(DX)]$DX <- 0
  data[,ADX      := .Call(maType, DX, n, ratio = NULL, wilder = FALSE, PACKAGE = "TTR") , by = Spolka]
  return(data[,.(Data, Year, Quarter, Month, Day, Spolka, Otwarcie, Najwyzszy, Najnizszy, Zamkniecie, ADX)])
}
Estimate_portfolio                        <- function(data, portfolio, Year_start = 2015){
  data <- data[Year >= Year_start]
  portfolio <- portfolio[Year >= Year_start]
  
  Returns <- data[Statistics == 'Cena' & Rodzaj == 'Zamkniecie', -c('Day', 'Rodzaj', 'Statistics')] %>%
    .[, .SD[seq_len(.N) == 1 | seq_len(.N) == .N], keyby = .(Spolka,Year, Month)] %>%
    group_by(Spolka, Year, Month) %>%
    summarise(Return = diff(log(PnL)))
  
  Y <- unique(data$Year)
  M <- unique(data$Month)
  # browser()
  DT <- data.table()
  for(i in Y){
    for(j in M){
      dt <- data.table(Returns)[Year == i & Month == j & Spolka %in% unlist(unique(portfolio[Year == i & Calibration_Period == j, -c('Year', 'Month')]))] %>% 
        mutate(Return2 = Return*1/n()) %>%
        group_by(Year, Month) %>% 
        summarise(sum(Return2), n()) %>%
        set_names(c('Year', 'Month', 'Return', 'Number_of_Stocks')) %>%
        as.data.table()
      DT <- rbind(DT, dt)
    }
  }
  #  browser()
  setkeyv(DT,c('Year','Month'))
  return(DT)
}
Pick_Stocks                               <- function(ichimoku_data = temp, adx_data = temp, lower = 20, upper = 40){
  #browser()
  adx_data <- adx_data[,.SD[seq_len(.N) %in% (.N:(.N-1))], keyby = .(Spolka, Year, Month, Quarter)]
  #adx_data <- na.omit(adx_data[,.(ADX2 = (ADX - lag(ADX, 1)), ADX), keyby = .(Spolka, Year, Month, Quarter)])[ADX2 > 0]
  hhh <- adx_data[ADX >= lower & ADX <= upper, .(Spolka, Year, Month, Quarter)]
  DT <- data.table()
  
  for(i in unique(hhh$Year)){
    for(j in unique(hhh$Month)){
      dec <- as.character(hhh[Year == i & Month == j]$Spolka)
      dt <-  data.table(Year = i, Month = j, t(dec))
      DT <- rbind(DT, dt, fill = T)
    }
  }
  #  browser()
  Stocks <- data.table()
  for(i in unique(hhh$Year)){
    for(j in unique(hhh$Month)){
      dec <- as.character(na.omit(unlist(DT[Year == i & Month == j, -c('Year', 'Month')])))
      sec <- as.character(na.omit(unlist(ichimoku_data[Year == i & Calibration_Period == j, -c('Year', 'Month')])))
      
      picked <- intersect(as.character(na.omit(as.character(dec))), as.character(na.omit(as.character(sec))))
      dt <-  data.table(Year = i, Month = j, t(picked))
      Stocks <- rbind(Stocks, dt, fill = T)
    }
  }
  return(Stocks)
}
download_stooq_index_component_data       <- function(index){
  
  URL <- paste0("https://stooq.pl/q/i/?s=", index)
  data <- readHTMLTable(getURL(URL))
  
  return(data$fth1)
}
download_stooq_data                       <- function(ticker){
  URL <- paste0("https://stooq.pl/q/d/l/?s=", ticker, "&i=d")
  data <- as_tibble(fread(getURL(URL), fill = TRUE)) 
  Sys.sleep(10)
  message(paste0('Loading ', ticker, ' data ended up with success.'))
  return(data)
}
transform_ts                              <- function(timeSeries, filter_date = '2001-01-01'){
  rbindlist(lapply(lapply(timeSeries, as.data.table),
                   melt.data.table,
                   variable.name = "Rodzaj",
                   value.name = "Cena",
                   id.vars = 'Data', 
                   measure.vars = c('Otwarcie', 'Najwyzszy', 'Najnizszy', 'Zamkniecie')),
            idcol = 'Spolka') %>%
    dcast.data.table(Data + Rodzaj ~ Spolka, value.var = 'Cena') %>%
    lazy_dt() %>%
    mutate(Data = as.Date(Data)) %>%
    mutate(Year = year(Data), Month = month(Data), Day = yday(Data), Quarter = quarter(Data)) %>% 
    select(Data, Year, Quarter, Month, Day, everything()) %>%
    arrange(Data) %>%
    group_by(Rodzaj) %>%
    mutate_at(vars(-c('Data', 'Year', 'Quarter', 'Month', 'Day', 'Rodzaj')),
              .funs = function(x){zoo::na.locf(x, na.rm = F, fromLast	= T)}) %>%
    filter(Data >= as.Date(filter_date)) %>%
    as.data.table %>% 
    melt.data.table(variable.name = "Spolka",
                    value.name = "Cena",
                    id.vars = c('Data', 'Year', 'Quarter', 'Month', 'Day', 'Rodzaj'))
}
Create_time_series_info                   <- function(temp){
  data <- copy(temp)
  ts_1 <- data %>% 
    group_by(Spolka, Rodzaj) %>%
    mutate(Daily_Return = c(NA,diff(Cena)), 
           Daily_Log_Return = c(NA,diff(log(Cena)))) %>%
    ungroup() %>%
    gather(key = 'Statistics', value = "PnL", -c(Data, Year, Quarter, Month,  Day, Rodzaj, Spolka)) %>%
    filter(Data < as.Date('2020-01-01')) %>%
    as.data.table() 
  
  ts_2 <-  data[, .SD[seq_len(.N) == 1 | seq_len(.N) == .N], by = c('Year', 'Month', 'Spolka', 'Rodzaj')] 
  ts_2 <- ts_2[,Monthly_return := c(NA,diff(Cena)), by =  c('Year', 'Month', 'Spolka', 'Rodzaj')][,-c('Cena')] %>%
    melt.data.table(value.name = 'PnL', 
                    variable.name = 'Statistics', 
                    id.vars = c('Data', 'Year', 'Quarter', 'Month', 'Day', 'Rodzaj', 'Spolka'))
  
  ts_3 <-  data[, .SD[seq_len(.N) == 1 | seq_len(.N) == .N], by = c('Year', 'Quarter', 'Spolka', 'Rodzaj')] 
  ts_3 <- ts_3[,Quarterly_return := c(NA,diff(Cena)), by =  c('Year', 'Quarter', 'Spolka', 'Rodzaj')][,-c('Cena')] %>%
    melt.data.table(value.name = 'PnL', 
                    variable.name = 'Statistics', 
                    id.vars = c('Data', 'Year', 'Quarter', 'Month', 'Day', 'Rodzaj', 'Spolka'))
  
  return(rbindlist(list(ts_1, ts_2, ts_3)))
  
}
Yearly_plot                               <- function(data, Fun = function(x){x}){
  data %>%
    ggplot() +
    geom_line(aes(x = as.Date(yday(Data), "1970-01-01"), y = Fun(PnL), 
                  color = factor(year(Data)))) +
    scale_x_date(date_breaks="months", date_labels="%b") +
    labs(x="Month",colour="") +
    theme_hc()
}
Line_plot                                 <- function(data, Fun = function(x){as.numeric(x)}){
  data %>%
    ggplot() +
    geom_line(aes(x = Data, y = Fun(PnL),  color = Statistics)) +
    theme_hc()
}
Filter_list                               <- function(data, year, data.cap = 2020){
  data %>%
    lazy_dt() %>%
    filter(Data >= as.Date(as.character(paste(year,"01","01",sep='-')))) %>%
    filter(Data < data.cap) %>%
    as.data.table()
} 
Estimate_Maximal_Return_Portfolio_Weights <- function(Grouped_Time_Series, BY = 'Month', Periods = periods, return = 'Cena', shorts = F, seed = 123){
  Weights <- data.table()
  
  for(i in Periods$Year){
    
    Returns_By_Period <- lapply(Grouped_Time_Series[[as.character(i)]], Portfolio_Returns, Stat = return)
    Names_By_Period   <- lapply(Returns_By_Period, names)
    Length_By_Period  <- sapply(Returns_By_Period, length)
    
    B <- list()
    for(j in 1:length(Returns_By_Period)){
      set.seed(seed)
      M <- matrix(runif(1e5 * Length_By_Period[j]), ncol = Length_By_Period[j])
      M <- M/rowSums(M)
      
      TEMP <- apply(M, 1, thresh, threshold = 0.5)
      M <- M[TEMP,]
      
      B[[j]] <- as.data.table(t(calc_expected_return(Returns_By_Period[[j]], M)))
      names(B[[j]]) <- Names_By_Period[[j]]
    }
    
    B <- rbindlist(B, fill = TRUE)
    B[,Year:=i]
    B[,Calibration_Period:=1:.N]
    Weights <- rbind(Weights, B, fill = TRUE)
  }
  
  Replace_NA(Weights)
}
calc_expected_return                      <- function(Returns,Weigth){
  #browser()
  #Weigth = Weigth/sum(Weigth)
  opt <- c()
  
  for(i in 1:nrow(Weigth)){
    opt[i] <- -mean(as.matrix(Returns) %*% Weigth[i,])
  }
  Weigth[which.max(opt),]
  # max(opt)
}
Filter_year                               <- function(DT, year){DT[Year == year]}
Filter_quarter                            <- function(DT, quarter){DT[Quarter == quarter]}
Filter_month                              <- function(DT, month){DT[Month == month]}
Pick_Type                                 <- function(Data, type = 'Zamkniecie'){
  lapply(Data, function(x){x[Rodzaj == 'Zamkniecie'][,-c('Rodzaj')]})
}
Transform_Grouped_Portfolio               <- function(Data, Ret = return){
  lapply(Data, Portfolio_Returns, Stat = Ret)
}
calc_portfolio_stat                       <- function(data){
  VaR <- quantile(data$Return, c(0.001, 0.01, .05))
  ES  <- apply(matrix(VaR),1,function(x){mean(data$Return[data$Return < x])})
  Mean_Return <- mean(data$Return)
  Mean_Capital <- mean(data$Capital)
  SD_Return <- sd(data$Return)
  SD_Capital <- sd(data$Capital)
  Max_Return = max(data$Return)
  Min_Return = min(data$Return)
  Max_Capital = max(data$Capital)
  Min_Capital = min(data$Capital)
  Last_Capital = data[.N,]$Capital
  DT <- data.frame(as.data.table(c(Last_Capital = Last_Capital, Max_Capital, Min_Capital, Mean_Capital, SD_Capital, Max_Return, Min_Return, Mean_Return, SD_Return, VaR, ES)))
  rownames(DT) <- c('Last_Capital', 'Max_Capital', 'Min_Capital', 'Mean_Capital', 'SD_Capital', 'Max_Return', 'Min_Return', 'Mean_Return', 'SD_Return', 'VaR_99.9%', 'VaR_99%', 'VaR_95%', 'ES_99.9%', 'ES_99%', 'ES_95%')
  DT
}
Portfolio_Returns                         <- function(ts, Stat){
  if(nrow(ts) == 0){
    return(data.table())
  }
  na.omit(
    dcast.data.table(
      ts[Statistics == Stat , .(Data, Spolka, PnL)],
      Data ~ Spolka,
      value.var = 'PnL'
    )[,-c('Data')]
  )
}
cov_mean                                  <- function(Ret){
  Covariance <- tryCatch({nearPD(cov(Ret))$mat}, 
                         error   = function(cond){return(NULL)}, 
                         warning = function(cond){return(NULL)})
  
  Means <- tryCatch({colMeans(Ret)},
                    error   = function(cond){return(NULL)},
                    warning = function(cond){return(NULL)})
  
  list(Covariance = Covariance, Means = Means)
}
Mean_VaR_Portfolio                        <- function(data, short = F, opt.meq.par = 2, precision = 0.00000001, max_port_weighth = 0.2){
  #browser()
  condition = nrow(data$Covariance) > 0
  if(isTRUE(condition)){
    if(short == TRUE){
      opt.constraints <- matrix(rep(1, length(data$Means)),
                                nrow = 1,
                                byrow=TRUE)
      opt.rhs <- matrix(c(1))
    } else {
      opt.constraints <- matrix(c(rep(1, length(data$Means)), diag(length(data$Means)), -diag(length(data$Means))),
                                ncol = length(data$Means) ,
                                byrow=TRUE)
      opt.rhs <- matrix(c(1, rep(precision, length(data$Means)), rep(-max_port_weighth, length(data$Means))))
    }
    
    opt.meq <- opt.meq.par  # first constraint is '=', rest are '>='
    solution.minvol <- tryCatch({
      
      #solve.QP(data$Covariance, dvec = data$Means, t(opt.constraints), opt.rhs, meq = opt.meq)$solution
      
      w0 <- rep(1/length(data$Means), length(data$Means))
      fn_SR <- function(w) {
        return(-as.numeric(t(w) %*% data$Means + 0.5 * (t(w) %*% data$Covariance %*% w)))
      }
      
      alabama::constrOptim.nl(w0, fn_SR,
                              hin = function(w) return(w),  # w >= 0
                              heq = function(w) return(sum(w) - 1),    # sum(w) = 1
                              control.outer = list(trace = FALSE))$par
      
      },
      error   = function(cond){return(NULL)},
      warning = function(cond){return(NULL)})
      
    solution.minvol <- solution.minvol/sum(solution.minvol)
    
    solution.minvol <- if(length(solution.minvol) == 0){
      rep(0, length(names(data$Means)))
    } else {
      solution.minvol
    }
    names(solution.minvol) <- names(data$Means)
    return(as.data.table(t(solution.minvol)))
  } else {
    return(data.table())
  }

}
Portfolio_Grouping                        <- function(Time_Series, BY = 'Month', Periods = periods){
  
  By_Year <- lapply(Periods$Year, Filter_year, DT = Time_Series) %>% set_names(Periods$Year)
  Portfolio_By_Year <- lapply(Periods$Year, Filter_year, DT = Time_Series) %>% set_names(Periods$Year)
  By_Period <- list()
  if(BY == 'Quarter'){
    for(i in Periods$Year){
      By_Period[[as.character(i)]] <- lapply(Periods$Quarter, Filter_quarter, DT = By_Year[[as.character(i)]])
    }
  } else {
    if(BY == 'Month'){
      for(i in Periods$Year){
        By_Period[[as.character(i)]] <- lapply(Periods$Month, Filter_month, DT = By_Year[[as.character(i)]])
      }
    } else {
      message('Pick Correct Calibration Frequency')
      stop()
    }
  }
  return(By_Period)
}
Replace_NA                                <- function(data){
  data %>% 
    lazy_dt() %>%
    mutate_at(vars(-c('Year', 'Calibration_Period')), .funs = function(x){ifelse(is.na(x),0,x)}) %>% 
    select(Year, Calibration_Period, everything()) %>%
    as.data.table()
}
Estimate_Markowitz_Weights                <- function(Grouped_Time_Series, BY = 'Month', Periods = periods, shorts = T, return = 'Daily_Return', max_port_weighth = max_w){
  Markowitz_alocation <- function(data){
      lapply(lapply(data, cov_mean), Mean_VaR_Portfolio, shorts, max_port_weighth = max_w) %>%
          rbindlist(fill = T, idcol = 'Calibration_Period') %>%
      lazy_dt() %>%
      mutate_at(vars(-c('Calibration_Period')), .funs = function(x){fifelse(is.na(x), 0, x)}) %>%
      as.data.table()
  }
  #browser()
  Weights      <- rbindlist(lapply(Grouped_Time_Series, Markowitz_alocation), fill = T, idcol = 'Year')
  Weights$Year <- as.numeric(Weights$Year)
  Weights <- merge(Weights, Full_table, all = T)[,-c('TEST')]
  Replace_NA(Weights)
}
Estimate_EW_Weights                       <- function(Grouped_Time_Series, BY = 'Month', Periods = periods, return = 'Cena', shorts = T){
  EW_alocation <- function(data){
    lapply(lapply(lapply(data, colMeans, na.rm = T), t), data.table) %>%
      rbindlist(fill = T, idcol = 'Calibration_Period') %>%
      lazy_dt() %>%
      mutate_at(vars(-c('Calibration_Period')), .funs = function(x){fifelse(is.na(x), 0, 1)}) %>%
      group_by(Calibration_Period) %>%
      mutate(N = rowSums(.)) %>%
      as.data.table() %>%
      .[,lapply(.SD, function(x){x/N}), by = 'Calibration_Period'] %>%
      .[,N:=NULL]
  }
  Weights      <- rbindlist(mclapply(Grouped_Time_Series, EW_alocation), fill = T, idcol = 'Year')
  Weights$Year <- as.numeric(Weights$Year)
  Weights <- merge(Weights, Full_table, all = T)[,-c('TEST')]
  Replace_NA(Weights)
}
Estimate_Exp_Geom_Growth_Weights          <- function(Grouped_Time_Series, BY = 'Month', Periods = periods, return = 'Cena', shorts = F){
  Weights <- data.table()
  for(j in Periods$Year){
    B <- Grouped_Time_Series[[as.character(j)]] 
    for(i in 1:length(B)){
      if(nrow(B[[i]]) > 0){
        temp <- apply(B[[i]], 1, which.max)
        mat <- matrix(0, ncol = ncol(B[[i]]), nrow(B[[i]]))
        for(k in 1:nrow(B[[i]])){
          mat[k,temp[k]] <- 1
        }
        colnames(mat) <- colnames(B[[i]])
        B[[i]] <- data.table(Year = j, Calibration_Period = i, t(colMeans(mat)))
      }
    }
    B <- rbindlist(B, fill = TRUE)
    # B[,Year:=j]  
    # B[,Calibration_Period:=1:.N]
    Weights <- rbind(Weights, B, fill = TRUE)
  }
  Weights <- merge(Weights, Full_table, all = T)[,-c('TEST')]
  Replace_NA(Weights)
}
Estimate_Weight_Wrapper                   <- function(fub, portfolio, ret_type, perio, sh, ...){
  fub(Grouped_Time_Series = portfolio, return = ret_type, Periods = perio, shorts = sh)
}
Adjust_Weights                            <- function(Weight, BY = 'Month'){
  Weight2 <- copy(Weight)
  Weight2[,Calibration_Period:=Calibration_Period+1]
  
  if(BY == 'Month'){
    Weight2[,Year:= ifelse(Calibration_Period == 13, Year + 1, Year)]
    Weight2[,Calibration_Period:= ifelse(Calibration_Period == 13, 1, Calibration_Period)]
  }
  
  if(BY == 'Quarter'){
    Weight2[,Year:= ifelse(Calibration_Period == 5, Year + 1, Year)]
    Weight2[,Calibration_Period:= ifelse(Calibration_Period == 5, 1, Calibration_Period)]
  }
  
  return(Weight2)
}
selection                                 <- function(data, region){
  unlist(data[Year == region$Year& Calibration_Period == region$Calibration_Period, -c('Year', 'Calibration_Period')])
}
Portfolio_Evolution                       <- function(Cena_Akcji_kup, Cena_Akcji_sprzedaj, initial_Capital = 10000, Weights,
                                                      marge = 0.0012, tax_rate = 0.019, marge_floor = 10,
                                                      percent_for_tax = 0.085, PODATKI,...){
  #browser()
  Capital <- c()
  Capital[1] <- initial_Capital
  Left <- c()
  
  col = ncol(Weights) - 2
  row = nrow(Weights) - 1
  
  Sold_Shares <- matrix(0, ncol = col, nrow = row + 1)
  Shs_Adj     <- matrix(0, ncol = col, nrow = row + 1)
  No_Shares   <- matrix(0, ncol = col, nrow = row + 1)
  Cash_Allo   <- matrix(0, ncol = col, nrow = row)
  Provision   <- matrix(0, ncol = col, nrow = row)
  New_Shares  <- matrix(0, ncol = col, nrow = row)
  Tax         <- matrix(0, ncol = col, nrow = row)
  
  colnames(Sold_Shares) <- colnames(Weights[,-c('Year', 'Calibration_Period')])
  colnames(Shs_Adj)     <- colnames(Weights[,-c('Year', 'Calibration_Period')])
  colnames(No_Shares)   <- colnames(Weights[,-c('Year', 'Calibration_Period')])
  colnames(Cash_Allo)   <- colnames(Weights[,-c('Year', 'Calibration_Period')])
  colnames(New_Shares)  <- colnames(Weights[,-c('Year', 'Calibration_Period')])
  colnames(Provision)   <- colnames(Weights[,-c('Year', 'Calibration_Period')])
  colnames(Tax)         <- colnames(Weights[,-c('Year', 'Calibration_Period')])
  
  Region <- Weights[,.(Year, Calibration_Period)]
  P1_S_pervious <-  0 
  
  for(i in 1:row){
    #browser()
    W1   <- selection(Weights, Region[i]);                #W1   <- W1[order(names(W1))]
    P1_S <- selection(Cena_Akcji_kup,         Region[i]); #P1_S <- P1_S[order(names(P1_S))]
    P1_E <- selection(Cena_Akcji_sprzedaj,    Region[i]); #P1_E <- P1_E[order(names(P1_E))]
    
    Cash_Allo[i,] <- W1*Capital[i]*(1 - percent_for_tax)
    
    No_Shares[i+1,] <- floor(Cash_Allo[i,]/P1_S)
    No_Shares[i+1,][!is.finite(No_Shares[i+1,])] <- 0
    Sold_Shares[i+1,] <- No_Shares[i+1,] - No_Shares[i,]
    Sold_Shares[i+1,][Sold_Shares[i+1,] > 0] <- 0
    
    Shs_Adj[i+1,] <- No_Shares[i+1,]
    Shs_Adj[i+1,][Shs_Adj[i+1,] > 0] <- 0
    Shs_Adj[i+1,] <- abs(Shs_Adj[i+1,])
    
    Sold_Shares[i+1,] = Sold_Shares[i+1,] + Shs_Adj[i+1,] 
    Sold_Shares[i+1,][Sold_Shares[i+1,] > 0] <- 0
    Tax[i,] <- ((Sold_Shares[i+1,]) * (P1_S-P1_S_pervious) - Provision[i,]) * tax_rate
    Tax[i,][Tax[i,] <0] <- 0
    
    New_Shares[i,] <- No_Shares[i+1,] - No_Shares[i,]
    
    Provision[i,] <- unlist(abs(New_Shares[i,])*P1_S*marge)
    Provision[i,][Provision[i,] < marge_floor & Provision[i,] != 0] <- marge_floor
    
    #Tax_2_not_apply_on <- which(New_Shares[i,] > 0)
    #Tax[i,] <- unlist((abs(New_Shares[i,]) * P1_S - Provision[i,]) * tax_rate)
    #Tax[i,][Tax_2_not_apply_on] <- 0
    
    #shs_temp <- No_Shares[i+1,]
    #names(shs_temp) <- names(W1)
    
    #shs_pos <- shs_temp
    #shs_neg <- shs_temp
    
    #names(shs_pos) <- names(shs_temp)
    #names(shs_neg) <- names(shs_temp)
    
    #shs_pos <- shs_temp[shs_temp > 0]
    #shs_neg <- shs_temp[shs_temp <= 0]
    if(PODATKI == TRUE){
      Left[i] <- Capital[i] + sum(-Tax[i,]) - sum(Provision[i,]) -  sum(No_Shares[i+1,]*P1_S)
    }
    if(PODATKI == FALSE){
      Left[i] <- Capital[i] -  sum(No_Shares[i+1,]*P1_S)
    }
    Capital[i+1] <- sum(No_Shares[i+1,]*P1_E) + Left[i]
    P1_S_pervious <- P1_S
  }
  
  Output <- data.table(Cena_Akcji_kup[,.(Year, Calibration_Period)], 
                       Provision = c(0, rowSums(Provision)), 
                       Tax = c(0, rowSums(Tax)), 
                       Unalocated_Capital = c(0,Left), 
                       Capital = Capital,
                       Return = c(0, diff(Capital)))[,Return_Percent := Return/Capital]
  
  colnames(Output)[2] <- calibration_period
  
  return(Output)
}
Run_Optimisation                          <- function(Data_TS, Return_type = return_type, Calibration = calibration_period,
                                                      tick = ticker, Shorts = F, y = year, Capital = 30000,
                                                      Weight_Functions, Stock_Selection = port, podatki, ...){
  
  periods           <- list(Year    = sort(unique(Data_TS[Year >= y & Statistics %in% c('Cena', Calibration)]$Year)),
                            Quarter = sort(unique(Data_TS[Year >= y & Statistics %in% c('Cena', Calibration)]$Quarter)),
                            Month   = sort(unique(Data_TS[Year >= y & Statistics %in% c('Cena', Calibration)]$Month)))
  
  Price             <- Data_TS[Year >= y & Statistics %in% c('Cena', Return_type)]
  
  Grouped_portfolio <- Portfolio_Grouping(Price, BY = Calibration, Periods = periods) 
  
  for(y in periods$Year){
    for(per in periods[[Calibration]]){
      Grouped_portfolio[[as.character(y)]][[per]] <- 
        Grouped_portfolio[[as.character(y)]][[per]][
          Spolka %in% unlist(Stock_Selection[Year == y & Calibration_Period  == per, -c('Year', 'Calibration_Period')])]
    }
  }
  
  Grouped_portfolio <- lapply(Grouped_portfolio, Pick_Type)
  Grouped_portfolio <- lapply(Grouped_portfolio, Transform_Grouped_Portfolio, Ret = Return_type)
  
  Price               <- Price[Statistics == 'Cena']
  Price_Z             <- Replace_NA(dcast.data.table(Price[Rodzaj == 'Zamkniecie',.(Data, Year, Calibration_Period  = get(Calibration), Day, Spolka, PnL)], Data + Year + Calibration_Period  ~ Spolka, value.var = 'PnL')[, .SD[seq_len(.N) == 1 | seq_len(.N) == .N], keyby = .(Year, Calibration_Period )][,-c('Data')])
  Price_O             <- Replace_NA(dcast.data.table(Price[Rodzaj == 'Otwarcie',.(Data, Year, Calibration_Period  = get(Calibration), Day, Spolka, PnL)], Data + Year + Calibration_Period  ~ Spolka, value.var = 'PnL')[, .SD[seq_len(.N) == 1 | seq_len(.N) == .N], keyby = .(Year, Calibration_Period )][,-c('Data')])
  
  Full_table <<- data.table(expand.grid(Year = periods[['Year']], Calibration_Period = periods[[Calibration]]), TEST = 'TEST')
  
  Weights <- list()
  for(i in 1:length(Weight_Functions)){
    Weights[[i]]    <- Adjust_Weights(Estimate_Weight_Wrapper(get(Weight_Functions[[i]]), portfolio = Grouped_portfolio, ret_type = Return_type, perio = periods, sh = Shorts, ...),  BY = Calibration)
  }
  
  Price_Z <- Price_Z %>% 
    select(names(Weights %>% '[['(1))) %>% data.table()
  
  Price_O <- Price_O %>% 
    select(names(Weights %>% '[['(1))) %>% data.table()
  names(Weights)    <- gsub('Estimate_','',gsub('_Weights','',unlist(weight_functions)))
  
  Output            <- lapply(Weights, 
                              FUN = Portfolio_Evolution , 
                              Cena_Akcji_kup = Price_O[, .SD[seq_len(.N) == 1], keyby = .(Year, Calibration_Period )],
                              Cena_Akcji_sprzedaj = Price_Z[, .SD[seq_len(.N) == .N], keyby = .(Year, Calibration_Period )],
                              initial_Capital = Capital,
                              marge = 0.0012,
                              tax_rate = 0.019,
                              marge_floor = 10,
                              percent_for_tax = 0.015,
                              PODATKI = podatki)
  
  return(list(Output = Output, Weights = Weights))
}
