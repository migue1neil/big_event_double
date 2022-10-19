library(data.table)
library(tidyverse)
library(lubridate)
options(scipen=999)

#####問題集#####
# 1,問題事件日疊到的話報酬會重複計算，
#   例如事件日發生在20080101、2080103，只差兩天，但程式會買兩次，但事實上沒有那麼多錢，報酬會高估
#
#


#####程式說明#####
#當事件日發生大跌的時候，哪一檔指數表現或比較好，或是哪一支股票表現會比較好處理。
#我們使用單日變動超過5%的日期當成大事件big_event
#我們將大跌後一天報酬率計算分成，1日,7日,20日,60日,120日,252日，看看大跌後要持有多長時間報酬最好。
#接著我們再加入加碼設計，嘗試看看是不是分批投入效果會更好
#PS:在測試時我們使用指數去做回測，有分散投資的概念在，因此在個股上需要多加注意，有些股票可能一去不復返。

#####載入資料、資料整理#####
#載入的資料來源為TEJ資料庫，謝謝學校。載入的資料有各種產業的報酬指數，嘗試從產業進行分析
stock_price = fread("return_index.txt", encoding = "unknown" , header = T,sep = "\t" , colClasses = "numeric") #這是一張另外抓的表格資料時間從20040102~20221007，只有指數商品
#修改一下格式
stock_price = stock_price %>% dplyr::rename( "stock_code"= "證券代碼",
                                             "TSE產業別" = "TSE 產業別" ,
                                             "調整開盤價" = "開盤價(元)" ,
                                             "調整收盤價" = "收盤價(元)",
                                             "成交張數" = "成交量(千股)")

stock_price = stock_price[,c("stock_code","年月日","調整收盤價")] #只選取這三個欄位

#生成一個以收盤價為基礎的日報酬return，不用開盤價的原因是因為，指數開盤價資料嚴重缺漏
stock_price = stock_price %>% group_by(stock_code) %>%
                              mutate( stock_code = str_trim(stock_code , side = "both"),
                                      closed_price_daily_change = (調整收盤價-lag(調整收盤價)) / lag(調整收盤價)) %>%
                              na.omit %>% mutate(cumprod = cumprod(closed_price_daily_change +1)-1) %>%
                              ungroup %>% arrange(stock_code , 年月日)
#市場對照組
market_index = stock_price %>% filter(stock_code == "Y9997 報酬指數") %>% 
                               select(stock_code,年月日,調整收盤價,closed_price_daily_change ,cumprod) %>%
                               mutate(brench_mark =  cumprod(closed_price_daily_change + 1)-1)
#畫市場報酬圖
plot(x = ymd(market_index$年月日) , y = market_index$brench_mark )

#####找大事件定義#####
#從2004年開始到20221007 -> 單日跌幅超過5%的有11天
#table( market_index$closed_price_daily_change < -0.05)
#market_index[closed_price_daily_change < -0.05,]
#從2004年開始到20221007 -> 單日跌幅超過5%的有83天
#table( market_index$closed_price_daily_change < -0.03)
#market_index[closed_price_daily_change < -0.03,] 

#目標是希望進場間隔可以大於(360日曆日)252交易日 #或自己定義
big_event_date = market_index %>% filter(closed_price_daily_change < -0.05) %>% select(年月日) %>% #選出大於5%的交易日
  mutate(interval_days = (年月日- lag(年月日) ) %>% as.numeric()) %>% 
  mutate(interval_days = nafill(interval_days ,fill = 9999)) %>%
  filter(interval_days > 360) %>% #小於360天不交易 
  select(年月日) #轉成一行
big_event_date = big_event_date[[1]]

#移除2009年的日期，因為產業編制指數時間太短了QQ
big_event_date = big_event_date[big_event_date > 20090101]

##### 加碼function #####
#根據上面的資訊，我們可以發現說，在持有期間中，如果以10%為基準，要可以加碼一次就要偷笑了。
#因此我們在測試的時候先以第一次放50%，達到條件之後再放50%為測試。
#加碼的話一樣要先抽取每個叫一日再把他並起來，但是中間要進行判斷，如果有出大事的話(跌幅超過設定門檻)，報酬全重要調整。
#我們要先取出第一次到達停損點的日期，再調整權重
#應該要設一個if else如果有停損的話，比重要先調低。然後要先檢查完日期再調比重
#要找幾個加碼點，就先看我要分成幾份資資料
#每5%加碼
#我要先累乘cumprod，然後看跌了多少%，算加碼日期，改變日報酬，在重新累乘一次cumprod
buy_more_func = function(return ,buy_more = F , n_part_money = 3 , buy_more_point = -0.05){ #return是一張data.table
  #施工使用
  #return = single.stock
  #n_times = 1
  #buy_more = T
  if (buy_more == T){
    add_money_time = n_part_money-1 #加碼幾次
    buy_more_time = c() 
    for (n_times in 1:add_money_time){ #求加碼時間點出來
      tmp.time = return %>% filter(cumprod < buy_more_point*n_times) %>% select(年月日) %>% 
                            filter(年月日 == min(年月日 , na.rm = T))
      tmp.time = tmp.time[[1]] #把她轉換成vector
      buy_more_time = c(buy_more_time , tmp.time )
    }
    buy_more_time = buy_more_time[!is.infinite(buy_more_time)]
    return$closed_price_daily_change = return$closed_price_daily_change*(1/n_part_money) #先將原本的報酬乘上1/n倍，先放一點點錢的意思
    n_len = length(buy_more_time) #計算次數
    return = return %>% mutate(加碼次數 = n_len)
    if (n_len == 0){return(return)} #如果等於零的話直接掰
    if (n_len > 0){ #如果有達到加碼點的話:分成加碼一次、加碼一次以上
      if(n_len == 1){ #這邊是再說，如果只加碼一次的話，就加碼後面的報酬全部乘上(2/n_part_money)，多一分比重的意思
        return$closed_price_daily_change = ifelse( return$年月日 > buy_more_time , return$closed_price_daily_change*( 2/n_part_money) , return$closed_price_daily_change )
      }
      if(n_len > 1){ #這邊是想要設計說，如果加碼大於1的話
        for (t in 2:n_len ){
          if (t < n_len){ #先將t,t-1格的報酬算好
            return$closed_price_daily_change = ifelse(return$年月日 > buy_more_time[t-1] & return$年月日 <= buy_more_time[t] , return$closed_price_daily_change*((t)/n_part_money) , return$closed_price_daily_change )
          }
          if (t == n_len){ #再將t以後的報酬算出來
            return$closed_price_daily_change = ifelse(return$年月日 > buy_more_time[t] , return$closed_price_daily_change*((t)/n_part_money) , return$closed_price_daily_change )
          }
        }
      }
    }
  }
return(return)
}
#buy_more_func(return , buy_more = T , n_part_money = 5 , buy_more_point = -0.1 )

#####這邊是根據買進要持有的日期，以及持有天數，進行投資報酬計算#####
#要進行回測的產業(或股票)
waited.stock_code = c("2412 中華電","0050 元大台灣50","2330 台積電","M11TR 水泥TR" , "M12TR 食品TR" , "M13TR 塑膠TR" , "M14TR 紡織TR" , "M15TR 電機機TR" 
               ,"M16TR 電器電TR" , "M17T1 化學TR" , "M17T2 生技TR" , "M17TR 化學生TR" , "M17T2 生技TR" , "M18TR 玻璃TR" , "M19TR 造紙TR" , "M20TR 鋼鐵TR" ,
               "M21TR 橡膠TR" , "M22TR 汽車TR" , "M23T1 半導體報酬指數" , "M23T2 電腦及週邊設備業TR" , "M23T3 光電類TR" , "M23T4 通信網路類TR" ,
               "M23T5 電子零組件TR", "M23T6 電子通路業TR" , "M23T7 資訊服務業TR" , "M23T8 其他電子業TR" , "M23TR 電子報酬" , "M25TR 營造TR",
               "M26TR 航運TR" , "M27TR 觀光TR" , "M28TR 金融報酬" , "M29TR 百貨TR" , "M97TR 油電燃TR" , "M99TR 其他TR")

event_day = big_event_date  #事件日日期
ndays = c(1,7,20,60,120,252) #買入持有天數

###測試使用
# ndays = c(1,7,20,60) 
# n = 120
# i = c("M23T1 半導體報酬指數" )
# date = 20200130
# waited.stock_code = c("M23T1 半導體報酬指數" ,"M23T5 電子零組件TR")
# ndays = c(20,60)
###測試使用

#篩選完Date之後，選出每檔前n筆資料
list = data.table()
period.report.sheet = data.table()
for (n in  ndays){ 
for (i in  waited.stock_code){  
for (date in event_day){  
single.stock = stock_price %>% filter(stock_code == i & 年月日 > date) %>% mutate(holding_days = n , #因為是用收盤價所以不是年月日 >= date，沒有等於代表後一天不包含當天
                                                                                   cumprod = cumprod(1+closed_price_daily_change)-1)
single.stock = single.stock[1:n,]
#加碼func >
buy_more = T
single.stock = buy_more_func(return = single.stock , buy_more = buy_more , n_part_money = 3 , buy_more_point = -0.05 )
#加碼func <

single.stock = single.stock %>% mutate(cumprod = cumprod(1+closed_price_daily_change)-1) #因為加碼func先把日報酬重算一遍了，所以要再重算一次複利

period.report = data.table( stock_code = last(single.stock$stock_code), #取最後一筆資料出來
                            #公司名稱 = last(single.stock$公司名稱),
                            #TSE產業別 = last(single.stock$TSE產業別),
                            買入日期 = first(single.stock$年月日),
                            買入持有天數 = n ,
                            標準差 = sd(single.stock$cumprod , na.rm = T) %>% round( digits = 4) ,
                            期間最大漲幅 = max(single.stock$cumprod) %>% round( digits = 4),
                            期間最大跌幅 = min(single.stock$cumprod) %>% round( digits = 4),
                            累積報酬率 = last(single.stock$cumprod) %>% round( digits = 4)
                    )

if (buy_more == T){ #設計表格的加碼欄位，有加碼的話再新增表格紀錄
  period.report = period.report %>% mutate(加碼次數 = last(single.stock$加碼次數))
}else{
  period.report = period.report %>% mutate(加碼次數 = "不加碼" )
}
period.report.sheet = rbind(period.report.sheet , period.report) #合併成每期間的報酬表
list = list %>% filter(!duplicated(list)) #移除重複值，怕重複
list = rbind(list ,  single.stock)
}}}

write.csv( period.report.sheet , "single_period_return_sheet.csv" , fileEncoding = "utf-8" ,row.names = F)

#####這邊是要計算所有期間結束之後，計算"(全部)"期間投組的報酬率，與風險指標#####
total.performance.sheet = data.table()
for (n in  ndays){ 
for (i in  waited.stock_code){ 
  
tmp = list %>% select(stock_code ,年月日 , closed_price_daily_change , holding_days) %>%
               filter(holding_days == n & stock_code == i ) %>% arrange(年月日) %>% 
               na.omit %>% 
               mutate(cumprod_return =  cumprod(closed_price_daily_change + 1 ) -1 , #前面有套用加碼func的話，是改變日報酬，所以這邊再累乘一次應該是對的
                      cummax = cummax(1+cumprod_return)    )                         

all.report = data.table( stock_code = first(tmp$stock_code),
                         #公司名稱 = first(tmp$公司名稱),
                         #TSE產業別= first(tmp$TSE產業別),
                         買入持有天數 = n ,
                         標準差 = sd(tmp$cumprod_return)  %>% round( digits = 4) ,
                         期間最大漲幅 = max(tmp$cumprod_return ,na.rm = TRUE) %>% round( digits = 4),
                         期間最大跌幅 = min(tmp$cumprod_return ,na.rm = TRUE) %>% round( digits = 4),
                         最大回落MDD = min( (1+tmp$cumprod_return - tmp$cummax) / tmp$cummax ,na.rm = TRUE)  %>% round( digits = 4 ),
                         累積報酬率 = last(tmp$cumprod_return) %>% round( digits = 4)
                        )
   
total.performance.sheet = rbind(total.performance.sheet , all.report ) %>% arrange(stock_code , 買入持有天數)
}}

write.csv( total.performance.sheet , "all_period_return_sheet.csv" , fileEncoding = "utf-8" ,row.names = F)

#####畫圖用的表格，取出前幾天的報酬，然後沒有交易的日期給他填0做計算，因為要跟brench_mark比較#####
#接下來要取買進並持有的日期
trade_date = data.table(market_index %>% select(年月日) %>% unique())  #方便實驗組比較使用，先提取出來

#我想要生成一張表格，是現有公司的資料跟時間日期合併，沒有交易的日期daily_change為0
daily_compound.sheet = data.table(年月日 = market_index$年月日 , brench_mark = market_index$brench_mark )

for (n in  ndays){ 
  for (i in  waited.stock_code){ 
    print(n)
    tmp = list %>% filter(holding_days == n & stock_code == i) %>% arrange(年月日)
    tmp = merge(tmp ,trade_date , all = T)
    tmp = tmp %>% mutate( stock_code = first(stock_code),
                          #公司名稱 = first(公司名稱),
                          #TSE產業別= first(TSE產業別),
                          holding_days = n ,
                          closed_price_daily_change = ifelse(is.na(closed_price_daily_change),
                                                         0,closed_price_daily_change )
)
tmp = tmp %>% mutate( cumprod_return =  cumprod(closed_price_daily_change + 1 ) -1 )
col.name = paste(i,"_holding_",n,"_compound_return" ,sep = "" )
col.name
tmp = tmp[,.(年月日,cumprod_return)]
colnames(tmp) = c("年月日", col.name)
#print(n)
daily_compound.sheet = merge(daily_compound.sheet , tmp ,by ="年月日" , all = T  )
}
}
#plot(y = daily_compound.sheet$`2412_holding_1_compound_return` , x = daily_compound.sheet$年月日)

#儲存檔案
write.csv( daily_compound.sheet , "graph_breach_portfolio_daily_cumprod.csv" , fileEncoding = "utf-8" ,row.names = F)


#儲存檔案
# file.name = paste(stock_code,n,"daily_change.csv" ,sep = "_" )
# file.name
# write.csv(tmp , file.name, fileEncoding = "utf-8" ,row.names = F)


#小節 選對產業比加碼重要，台積電真香

