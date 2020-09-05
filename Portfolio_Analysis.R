setwd("~/Pulpit")
source('Functions.R')
source('libraries.R')

Wig_20  <-   download_stooq_index_component_data('wig20')      %>% pull(Symbol) %>% as.character()
mWig_40 <-   download_stooq_index_component_data('mwig40')     %>% pull(Symbol) %>% as.character()
sWig_80 <- c(download_stooq_index_component_data('swig80')     %>% pull(Symbol) %>% as.character(),
             download_stooq_index_component_data('swig80&l=2') %>% pull(Symbol) %>% as.character())

Wig     <- c(download_stooq_index_component_data('wig')     %>% pull(Symbol) %>% as.character(),
             download_stooq_index_component_data('wig&l=2') %>% pull(Symbol) %>% as.character(),
             download_stooq_index_component_data('wig&l=3') %>% pull(Symbol) %>% as.character(),
             download_stooq_index_component_data('wig&l=4') %>% pull(Symbol) %>% as.character(),
             download_stooq_index_component_data('wig&l=5') %>% pull(Symbol) %>% as.character(),
             download_stooq_index_component_data('wig&l=6') %>% pull(Symbol) %>% as.character(),
             download_stooq_index_component_data('wig&l=7') %>% pull(Symbol) %>% as.character())

SP500     <- c(download_stooq_index_component_data('^spx')      %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=2')  %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=3')  %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=4')  %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=5')  %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=6')  %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=7')  %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=8')  %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=9')  %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=10') %>% pull(Symbol) %>% as.character(),
               download_stooq_index_component_data('^spx&l=11') %>% pull(Symbol) %>% as.character())

tickers <- c(Wig)


time_series <- tickers %>%
  lapply(download_stooq_data) %>%
  set_names(tickers)

long_time_series <- transform_ts(time_series)
wide_time_series <- dcast.data.table(long_time_series,
                                     Data + Year + Quarter + Month + Day + Spolka ~ Rodzaj,
                                     value.var = 'Cena')
setkey(wide_time_series, Spolka)

time_series_info <- Create_time_series_info(long_time_series)
setkey(time_series_info, Spolka)

time_series_info[Spolka %in% c('CCC') & Statistics %in% c('Signal_line', 'MACD') & Year > 2018]  %>%
  dcast.data.table(Data + Year + Quarter + Month + Day + Spolka ~ Statistics, value.var = 'PnL') %>%
  mutate(Trend = ifelse(Signal_line > MACD, 'Rosnacy', 'Spadkowy')) %>% 
  as.data.table()

# Plots
grid.arrange(
  time_series_info %>%
    filter(Statistics == "Cena", Spolka == "NRO", Year >= 2019) %>%
    Line_plot,
  time_series_info %>%
  filter(Statistics == "Cena", Spolka == "NEU", Year >= 2010) %>% 
  Yearly_plot
)

time_series_info %>%
  filter(Statistics == "Cena", Rodzaj == 'Zamkniecie', Spolka == "PKN", Year >= 2019) %>%
  Line_plot()

# Setup Strategy Run
return_type = 'Daily_Log_Return'
calibration_period <- 'Month'
weight_functions <- list('Estimate_EW_Weights', 'Estimate_Exp_Geom_Growth_Weights', 'Estimate_Markowitz_Weights')
max_w = .85#0.37
Capi = 30000
year = 2019
#time_series_info <- time_series_info[Year < 2020]
Results <- Run_Optimisation(Data_TS = time_series_info, Shorts = F, Weight_Functions = weight_functions, 
                            tick = ticker, Capital = Capi, max_port_weighth = max_w, Stock_Selection = port, podatki = T)

Results$Output %>%
  rbindlist(idcol = 'Strategy') %>%
  lazy_dt() %>%
  mutate(Period = as.Date(as.character(paste(Year, Month, '01',sep = '-')),'%Y-%m-%d'))%>%
  as.data.table() %>%
  ggplot(aes(x = Period, y= Capital, group = Strategy, color = Strategy)) +
  geom_line() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0.001)) +
  theme_hc() 

set_names(do.call(cbind, lapply(Results$Output, calc_portfolio_stat)),names(Results$Output))

######
year = 2012
return_type = 'Daily_Log_Return'
weight_functions <- list('Estimate_EW_Weights')
weight_functions <- list('Estimate_Markowitz_Weights')
weight_functions <- list('Estimate_Exp_Geom_Growth_Weights')
weight_functions <- list('Estimate_EW_Weights', 'Estimate_Exp_Geom_Growth_Weights')
weight_functions <- list('Estimate_Markowitz_Weights', 'Estimate_EW_Weights')
weight_functions <- list('Estimate_Markowitz_Weights', 'Estimate_Exp_Geom_Growth_Weights')
weight_functions <- list('Estimate_EW_Weights', 'Estimate_Markowitz_Weights', 'Estimate_Exp_Geom_Growth_Weights')

ichimoku(wide_time_series, nFast = 33, nMed = 66, nSlow = 88) %>%
  transform_ichimocu(Calibration_Period = 'Month') %>%
  Pick_Stocks(ichimoku_data = . ,adx_data = ADX(wide_time_series, n = 15), lower = 5, upper = 25) %>% 
  setnames(colnames(.),c('Year', 'Calibration_Period', colnames(.)[-c(1:2)])) %>%
  Adjust_Weights() %>% 
  Run_Optimisation(Data_TS = time_series_info, Shorts = F, Weight_Functions = weight_functions, 
                 tick = ticker, Capital = Capi, max_port_weighth = max_w, Stock_Selection = .,
                 podatki = T) %>% 
  '[['(1) %>%
  rbindlist(idcol = 'Strategy') %>%
  lazy_dt() %>%
  mutate(Period = as.Date(as.character(paste(Year, Month, '01',sep = '-')),'%Y-%m-%d')) %>%
  as.data.table() %>%
  ggplot(aes(x = Period, y = Capital, group = Strategy, color = Strategy)) +
  geom_line() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line        = element_line(colour = "black")) +
  theme(axis.text.x      = element_text(angle = -45, hjust = 0.001)) +
  theme_hc() 


###### Grid Calibration ######
Grid <- expand.grid(1:10, 2:15, 3:20, 1:10, 15:99, 20:100)
Grid <- as.data.table(Grid)
Grid <- Grid[Var5 < Var6]
Grid <- Grid[Var1 < Var2]
Grid <- Grid[Var2 < Var3]
Grid <- Grid[Var1 >= Var4]
colnames(Grid) <- c('NFAST', 'NMED', 'NSLOW', 'ADX_N', 'Lower', 'Upper')