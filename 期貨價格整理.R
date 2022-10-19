library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)
setwd("C:/Users/Neil/Documents/git-repos/大跌時加碼報酬/Data")

#載入2010年以來所有期貨資料
future_price = fread("future_price_to20221017.txt" , encoding = "unknown" , colClasses = list(character = 1 , numeric=2:25), header = T ,sep = ",")
future_price = future_price %>% rename("成交張數" ="成交張數(量)") 

col_name = colnames(future_price)
for (i in 2:25){
 future_price[,col_name[i]] = future_price[[i]] %>% as.numeric()
}

#篩出台積電期貨價格資訊
TSMC_future = future_price[grepl("台積電", 證券代碼, ignore.case=FALSE),]
#排除TEJ的近月差補
TSMC_future = TSMC_future[!grepl("近", 證券代碼, ignore.case=FALSE),]
#移除小台積電
TSMC_future = TSMC_future[!grepl("QFF", 證券代碼, ignore.case=FALSE),]

#small.TW_future = future_price[grepl("ZMTXA", 證券代碼, ignore.case=FALSE),]

# 我要把不同期數的價格整理成一個指數
# 方案一，直接取交易日當天成交量最大的，but轉倉當天會有點差，因為通常是在最後交易日才會轉倉
# 方案二，最後交易日前n天轉倉
# 方案三，連續兩天最大的話就拿他當計算

#先用方案一吧 #方案一無法 

# TSMC_future_tmp = TSMC_future[,c(1:10)]
# TSMC_future_tmp = TSMC_future %>% group_by(年月日) %>% filter(成交張數 == min(最後交易日-年月日)) %>%　select(證券代碼,年月日) %>% mutate(bug_one = 1)
# TSMC_future = left_join(TSMC_future , TSMC_future_tmp , by = c("證券代碼","年月日"))
# TSMC_future$bug_one = TSMC_future$bug_one %>% nafill(fill = 0)

#方案二
#我要提前1天換股，所以distance = 0的話就不選他，選最近的
TSMC_future_tmp = TSMC_future  %>% mutate(distance = 最後交易日-年月日) %>% filter(distance !=0 & 成交張數 != 0) %>%  #這邊是要刪除成交張數等於0
                  group_by(年月日) %>% filter( distance == min(distance)) %>% ungroup

#arrange('年月日','distance') %>% .[.,SD[1],by = 年月日]

#跟最後一天的股票
#檢查資料用的
TSMC_future_tmp$星期 = wday( ymd(TSMC_future_tmp$年月日) )-1
TSMC_future_tmp$星期 = ifelse( TSMC_future_tmp$星期  == 0 , 7 , TSMC_future_tmp$星期 )

#ㄎ杯，交易量太少的股票沒辦法這樣整理，因為如果整天都沒交易的話，價格為0會等於NA
#台積電的話在2010年有出現過兩次，把前面改成如果當期最近期為0的話，選有價格最近到期日的標的
test = TSMC_future_tmp %>% filter(is.na(收盤價) == T)

#改名稱
TSMC_future_tmp = TSMC_future_tmp %>% separate(證券代碼 , c("代號","證券代碼","到期日"),sep = " ") %>% select(-到期日)
TSMC_future_tmp$證券代碼 = "台積電期"

#算持有期間報酬
TSMC_future_tmp = TSMC_future_tmp %>%　mutate(持有1天報酬 = (( lead(收盤價 , n = 1 ) - 收盤價)/收盤價) %>% round(4) ,
                                               持有5天報酬 = (( lead(收盤價 , n = 5 ) - 收盤價)/收盤價) %>% round(4) ,
                                               持有10天報酬 = (( lead(收盤價 , n = 10 ) - 收盤價)/收盤價) %>% round(4) ,
                                               持有20天報酬 = (( lead(收盤價 , n = 20 ) - 收盤價)/收盤價) %>% round(4) ,
) 

attach(TSMC_future_tmp)
TSMC_future_tmp = TSMC_future_tmp[年月日 %in%  tmp$年月日,]

