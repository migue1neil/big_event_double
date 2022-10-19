library(data.table)
library(tidyverse)
library(lubridate)
options(scipen=999)

stock_price = fread("market_index.csv", encoding = "unknown" , header = T,sep = "," , colClasses = "numeric") #這是一張另外抓的表格資料時間從20040102~20221007，只有指數商品
stock_price = stock_price[,c(1:3,6,9)] 

#stock_price = fread("return_index.txt", encoding = "unknown" , header = T,sep = "\t" , colClasses = "numeric") #這是一張另外抓的表格資料時間從20040102~20221007，只有指數商品
stock_price = stock_price %>% dplyr::rename( "stock_code"= "證券代碼",
                                             #"TSE產業別" = "TSE 產業別" ,
                                             #"Date" = "年月日",
                                             "調整開盤價" = "開盤價(元)" ,
                                             "調整收盤價" = "收盤價(元)" )
                                             #"成交張數" = "成交量(千股)")

unique(stock_price$stock_code)

market_index = stock_price[stock_code == "Y9999 加權指數",][,c("stock_code","年月日","調整收盤價")] %>% 
                          mutate( daily_return = ((調整收盤價-lag(調整收盤價)) / lag(調整收盤價)) %>% nafill(.,fill = 0)) %>% 
                          mutate(cumprod = cumprod(daily_return +1)-1 )

#從2004年開始到20221007 -> 單日跌幅超過5%的有11天
table( market_index$daily_return < -0.05)
market_index[daily_return < -0.05,]
market_index[daily_return < -0.05,] 

#從2004年開始到20221007 -> 單日跌幅超過5%的有83天
table( market_index$daily_return < -0.03)
market_index[daily_return < -0.03,] 

#目標是希望進場間隔可以大於252交易日 #或自己定義

big_event_date = market_index[daily_return < -0.03,]  %>% select(年月日) %>%
                                                          mutate(interval_days = ( 年月日- lag(年月日) ) %>% as.numeric()) %>% 
                                                          mutate(interval_days = nafill(interval_days ,fill = 9999)) #%>%
                                                        #  filter(interval_days > 360) #小於360天不交易
                                    
big_event_date = big_event_date %>% select(年月日)


#####這邊是要看反彈的日期

###這邊連跌四天，反彈
# 連跌四天想一下
market_index = market_index %>% mutate(up_or_down = ifelse(daily_return > 0 , 1 , 0))
#這邊是在寫連漲幾天                                  
seq_up = c(market_index$up_or_down[1])
for (i in c(2:nrow(market_index))){
  if (market_index$up_or_down[i] > 0){
    n = seq_up[i-1] + 1
    seq_up = c(seq_up , n)
  }
  else( seq_up = c(seq_up , 0))
}
market_index = cbind(market_index ,seq_up)

#table( market_index[年月日>20220101,] %>% select(seq_up))

#這邊是連跌幾天
seq_down = c(　ifelse(market_index$up_or_down[1]==1 , 0 , 1) )
for (i in c(2:nrow(market_index))){
  if (market_index$up_or_down[i] == 0){
    n = seq_down[i-1] + 1
    seq_down = c(seq_down , n)
  }
  else( seq_down = c(seq_down , 0))
}
market_index = cbind(market_index ,seq_down)

#table(market_index[年月日>20220101,] %>% select(seq_down))

up_P = market_index %>% filter(年月日>ymd(20220101))
1-mean(up_P$up_or_down) #下跌機率


#算持有期間報酬
market_index = market_index %>%　mutate(持有1天報酬 = (( lead(調整收盤價 , n = 1 ) - 調整收盤價)/調整收盤價) %>% round(4) ,
                                        持有5天報酬 = (( lead(調整收盤價 , n = 5 ) - 調整收盤價)/調整收盤價) %>% round(4) ,
                                        持有10天報酬 = (( lead(調整收盤價 , n = 10 ) - 調整收盤價)/調整收盤價) %>% round(4) ,
                                        持有20天報酬 = (( lead(調整收盤價 , n = 20 ) - 調整收盤價)/調整收盤價) %>% round(4) ,
) 

plot(x = ymd(market_index$年月日) , y = market_index$調整收盤價)


#找出連跌n天，當天上漲的日期

tmp = market_index[年月日>20210101 & up_or_down == 1 & lag(seq_down) >= 4, ]

tmp$年月日
# data.table(
#   day1_long_win.rate = 
#   day1_short_win.rate =
#     
#     day1_long_win.rate
  

#)
long_win.rate = ifelse(tmp$持有5天報酬 > 0 , 1 ,0 ) %>% mean( na.rm =T ) %>% round(4)
short_win.rate = ifelse(tmp$持有5天報酬*(-1) > 0 , 1 ,0 ) %>% mean( na.rm =T ) %>% round(4)

long_win.rate = ifelse(tmp$持有1天報酬 > 0 , 1 ,0 ) %>% mean( na.rm =T ) %>% round(4)
short_win.rate = ifelse(tmp$持有1天報酬*(-1) > 0 , 1 ,0 ) %>% mean( na.rm =T ) %>% round(4)

#如果是連跌四天反彈的話，做空持有5天，勝率5成8



##### 來看看台指期，but要先確定夜盤日盤怎麼看，TEJ的只有日盤的

future = fread("台指期近月.csv" , encoding = "UTF-8")

future = future %>% select(證券代碼 , 年月日 , 開盤價 ) %>%
  mutate(
  daily_return = ((開盤價-lag(開盤價,1)) / lag(開盤價,1))  %>% shift(-1) %>% nafill(fill = 0)
  #今天開盤價買，明天開盤價賣的報酬
)

#算持有期間報酬
future = future %>%　mutate(持有1天報酬 = (( lead(開盤價 , n = 1 ) - 開盤價)/開盤價) %>% round(4) ,
                            持有5天報酬 = (( lead(開盤價 , n = 5 ) - 開盤價)/開盤價) %>% round(4) ,
                            持有10天報酬 = (( lead(開盤價 , n = 10 ) - 開盤價)/開盤價) %>% round(4) ,
                            持有20天報酬 = (( lead(開盤價 , n = 20 ) - 開盤價)/開盤價) %>% round(4) ,
) 
#選出符合大事件的日期
future_tmp = future[年月日 %in% tmp$年月日,]

# for (col in c(5:ncol(future_tmp))){
# 
# future_tmp[,col]

aa = future_tmp[ ,5:ncol(future_tmp)] %>% lapply(.,mean)
future_tmp[ ,5:ncol(future_tmp)] %>% lapply( function(x){ ifelse(x>0,1,0) %>% mean( na.rm =T )}   )

# inv.sheet = data.table(持有n天報酬 = colnames(x) , 做多勝率 =  , 做多平均報酬 , 做多期望值 ,
#                        做空勝率 = ,
#                        做空平均報酬 = ,
#                        做空期望值 = , 
#                                                 
#                        )
# }

long_win.rate = ifelse(future_tmp$持有5天報酬 > 0 , 1 ,0 ) %>% mean( na.rm =T ) %>% round(4)
short_win.rate = ifelse(future_tmp$持有5天報酬*(-1) > 0 , 1 ,0 ) %>% mean( na.rm =T ) %>% round(4)

mean(future_tmp$持有5天報酬)
#期望值
short_win.rate*mean(future_tmp$持有5天報酬)*-1

long_win.rate = ifelse(future_tmp$持有1天報酬 > 0 , 1 ,0 ) %>% mean( na.rm =T ) %>% round(4)
short_win.rate = ifelse(future_tmp$持有1天報酬*(-1) > 0 , 1 ,0 ) %>% mean( na.rm =T ) %>% round(4)

mean(future_tmp$持有1天報酬)
short_win.rate*mean(future_tmp$持有1天報酬)*-1

#看一下台積電

TSMC = fread("TSMC_to20221016.txt")[,1:3]
colnames(TSMC) = c("證券代碼","年月日","收盤價")
TSMC = TSMC %>% select(證券代碼 , 年月日 , 收盤價 ) %>%
  mutate(
  daily_return = ((收盤價-lag(收盤價,1)) / lag(收盤價,1))  %>% shift(-1) %>% nafill(fill = 0)
  #昨天收盤買，今天收盤賣的報酬
  )

#算持有期間報酬
TSMC = TSMC %>%　mutate(持有1天報酬 = (( lead(收盤價 , n = 1 ) - 收盤價)/收盤價) %>% round(4) ,
                           持有5天報酬 = (( lead(收盤價 , n = 5 ) - 收盤價)/收盤價) %>% round(4) ,
                           持有10天報酬 = (( lead(收盤價 , n = 10 ) - 收盤價)/收盤價) %>% round(4) ,
                           持有20天報酬 = (( lead(收盤價 , n = 20 ) - 收盤價)/收盤價) %>% round(4) ,
) 


#####市場雄市#####
#

market_index$DD = (( market_index$調整收盤價 - cummax(market_index$調整收盤價)) / cummax(market_index$調整收盤價)) %>% round(4)

aa = market_index[ DD < -0.2 ,]

#要怎麼定義熊市?
