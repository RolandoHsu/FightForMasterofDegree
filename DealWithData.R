library(tidyverse)
library(ggplot2)
library(stringr)
library(readxl)
library(lubridate)
library(PerformanceAnalytics)
library(ggrepel)
library(data.table)

##### 注意 #####
#ListFirm 包含上市上櫃下市下櫃

##### Import Data #####
IPOData <- read_xlsx("IPOinTaiwan_1106.xlsx") %>% as.data.table()
DelistData <- read_xlsx("DelistCompanies.xlsx") %>% as.data.table()
colnamesinIPOData <- names(IPOData) %>% as.data.table()
DelistFirms_Data <- read_xlsx("DelistFirms_undealwith.xlsx") %>% as.data.table
FullCashDeliveryStock_Data <- read_xlsx("FullCashDeliveryStock.xlsx") %>% as.data.table
Price_Vol_FCDS_Data <- read_xlsx("StockPrice_Vol_FCDS.xlsx") %>% as.data.table()
ListFirm_Data <- read_xlsx("上市公司_1111.xlsx") %>% as.data.table()
MandA_Data <- read_xlsx("併購data.xlsx") %>% as.data.table()
load("ROTCPriceData.RData") %>% as.data.table()
MarketReturn_Data <- read_xlsx("MarketReturn.xlsx") %>% 
  as.data.table() %>% 
  .[, .SD, .SDcols = c("年月日", "市場投組", "無風險利率")]

##### 產業特性 #####
# 共有641個樣本
ListFirm <- ListFirm_Data %>% 
  .[`首次掛牌日期` >= "2002-01-01 UTC" & `首次掛牌日期` <= "2014-12-31 UTC"] %>% 
  .[`首次掛牌市場` == "ROTC"] %>% 
  .[!(`TSE新產業名` %in% c("M2800 金融業"))] %>% 
  .[`前一次變更代碼日期` <= "2014-12-31 UTC" & `前一次變更代碼日期` >= "2002-06-30 UTC"] %>% 
  .[!(`前二次變更市場` == "OTC") | is.na(`前二次變更市場`) == T] %>% 
  .[, SurvivalYears := round((`下市日期` - `前一次變更代碼日期`)/365, 2)] %>% 
  .[, ROTC_Time := (`前一次變更代碼日期` - `首次掛牌日期`)/365] %>% 
  .[ROTC_Time >= 0.5] # 興櫃時間超過六個月
  

names(ListFirm)

# Get the Company's Code list 
write.csv(ListFirm$前一次變更代碼, file = "CompanyCodeList.csv")

##### 併購公司清單 ##### 
MandA <- MandA_Data %>% 
  .[`事件日` >= ymd("2002-01-01")] %>% 
  .[, `:=` (MajorComCode = str_split(主併公司碼, " ", simplify = T)[, 1])] %>% 
  .[, .SD, .SDcols = c(18,4, 2, 3, 5:17)] %>% 
  .[!(`主併公司產業名` %in% c("金融業", "証券", "金融控", "壽險"))]

test <- ListFirm %>% 
  .[前一次變更代碼 %in% MandA$MajorComCode] %>% 
  .[order(`前一次變更代碼`)]

test2 <- MandA %>% 
  .[MajorComCode %in% ListFirm$前一次變更代碼] %>% 
  .[order(MajorComCode)]

test3 <- ListFirm %>%
  .[前一次變更代碼 %in% MandA$被併公司碼] %>% 
  .[order(`前一次變更代碼`)]

test4 <- MandA %>% 
  .[`被併公司碼` %in% ListFirm$前一次變更代碼] %>% 
  .[order(`被併公司碼`)]

# 被併公司有無在那個時間下市
test5 <- DelistFirm %>% 
  .[`前一次變更代碼` %in% MandA$MajorComCode]
test6 <- DelistFirm %>% 
  .[`前一次變更代碼` %in% MandA$被併公司碼] %>% 
  .[, .SD, .SDcols = c("前一次變更代碼", "下市日期" , "前一次變更市場")] %>% 
  rename("Code" = "前一次變更代碼") %>% 
  left_join(., MandA %>% 
              .[, .SD, .SDcols = c("MajorComCode", "主併公司名", "事件日", "併購事件", "被併公司碼", "被併公司名")] %>% 
              rename("Code" = "被併公司碼"),
            by = "Code")

# 下市公司為48個 (有17家為併購)
DelistFirm <- ListFirm %>% 
  .[is.na(`下市日期`) == F ] #%>% 
  #.[!(`前一次變更代碼` %in% MandA$MajorComCode)]

# 下市公司中 平均的存活年數為7年：
DelistFirm %>% 
  pull(SurvivalYears) %>%
  as.numeric() %>% 
  mean() %>% 
  round(., 2)

# 五年內下市的公司共有20家
DelistWithin5Years <- DelistFirm %>% 
  .[SurvivalYears <= 5]

# 上市公司中 前三多的產業分別為 電子零件組(16.77%) 半導體 (13.45%) 光電業(12.66%)
Industryratio <- table(ListFirm$TSE新產業名) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  setDT() %>% 
  .[, `IndustryPre (%)` := round((Freq/sum(Freq))*100,2)]

png("上市櫃公司 產業分配.png", width = 11, height = 8, units = 'in', res = 300)
ggplot(data = Industryratio)+
  geom_bar(aes(x = Var1, y = `IndustryPre (%)`), stat = "identity", fill = "#007979")+
  geom_label(aes(x = Var1, y = `IndustryPre (%)`, label = `IndustryPre (%)`), data = Industryratio) +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        text = element_text(family="黑體-繁 中黑"),
        plot.background  = element_rect(fill = "aliceblue"), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue")) +
  xlab("產業別")+
  ggtitle("上市櫃公司 產業分配")+
  coord_flip()
dev.off()

png("每年上市櫃數量.png", width = 11, height = 8, units = 'in', res = 300)
ListFirm %>%
  .[, .SD, .SDcols = c("公司簡稱", "前一次變更代碼日期")] %>% 
  .[, `:=`(Year = as.factor(year(`前一次變更代碼日期`)),
           Count = 1)] %>%
  .[, .SD, .SDcols = c("Year", "Count")] %>% 
  .[, sum(Count), by = "Year"] %>% 
  ggplot(., aes(x = Year, y = V1)) +
  geom_bar(aes(x = Year, y = V1), stat = "identity", fill = "#007979")+
  geom_label(aes(x = Year, y = V1, label = V1)) +
  ggtitle("每年上市櫃數量")+
  ylab("Count")+
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        text = element_text(family="黑體-繁 中黑"),
        plot.background  = element_rect(fill = "aliceblue"), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue"))
dev.off()

# 全641家公司中 電子工業 326 非電子工業 315家 
ListFirm_Ele <- ListFirm %>% 
  .[, Ele := ifelse(`首次掛牌TSE產業` == "M2300 電子工業", 1, 0)] %>% 
  pull(Ele) %>% 
  table

# 下市櫃公司中 電子工業 34 非電子工業 14家 
DelistFirm %>% 
  .[, Ele := ifelse(`首次掛牌TSE產業` == "M2300 電子工業", 1, 0)] %>% 
  pull(Ele) %>% 
  table

# 五年內下市櫃公司中 電子工業 13 非電子工業 7家 
DelistWithin5Years %>% 
  .[, Ele := ifelse(`首次掛牌TSE產業` == "M2300 電子工業", 1, 0)] %>% 
  pull(Ele) %>% 
  table


#  沒下市的公司中 曾發生全額交割股 共71家
FCDFirms <- ListFirm %>% 
  .[is.na(`下市日期`) == T] %>% 
  .[is.na(`全額交割起日(一)`) == F] %>% 
  .[, FCD := round((`全額交割起日(一)` - `前一次變更代碼日期`)/365, 2)]

# 其中曾發生全額交割股 OTC 佔61家 TSE 10家
table(FCDFirms$前一次變更市場)

table(FCDFirms$TSE新產業名)

# 平均來說 8.57 年會有全額交割股的現象
FCDFirms %>% 
  pull(FCD) %>%
  as.numeric() %>% 
  mean %>% 
  round(., 2)

FCD_Industryratio <- table(FCDFirms$TSE新產業名) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  setDT() %>% 
  .[, `IndustryPre (%)` := round((Freq/sum(Freq))*100,2)]

png("全額交割股 產業分配.png", width = 11, height = 8, units = 'in', res = 300)
ggplot(data = FCD_Industryratio)+
  geom_bar(aes(x = Var1, y = `IndustryPre (%)`), stat = "identity", fill = "#007979")+
  geom_label(aes(x = Var1, y = `IndustryPre (%)`, label = `IndustryPre (%)`), data = FCD_Industryratio) +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        text = element_text(family="黑體-繁 中黑"),
        plot.background  = element_rect(fill = "aliceblue"), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue")) +
  xlab("產業別")+
  ggtitle("全額交割股 產業分配")+
  coord_flip()
dev.off()

# 在所有上市櫃公司(沒有下市的)中，共有 21 家在五年內曾經列為全額交割股
FCDFirms_within5Years <- FCDFirms %>% 
  .[FCD <= 5]

FCDFirms_within5Years %>% 
  .[, Ele := ifelse(`首次掛牌TSE產業` == "M2300 電子工業", 1, 0)] %>% 
  pull(Ele) %>% 
  table

# 五年內全額交割股中，OTC 18家 TSE 3家 
table(FCDFirms_within5Years$前一次變更市場)

# (Check) 下市日期一定大於全額交割起日
CheckDiff_DelistandFCD <- ListFirm %>% 
  .[is.na(`下市日期`) == F] %>% 
  .[is.na(`全額交割起日(一)`) == F] %>% 
  .[, check := ifelse((`下市日期` - `全額交割起日(一)`) > 0 , 1, 0)]


# 各產業上市櫃情況(年).png
QuantitiesofIPOforeachYear <- function(data, industry){
  Plot <- data %>%
    .[, .SD, .SDcols = c("公司簡稱", "前一次變更代碼日期", "TSE新產業名")] %>% 
    .[`TSE新產業名` %in% industry] %>% 
    .[, `:=`(Year = as.factor(year(`前一次變更代碼日期`)),
             Count = 1)] %>%
    .[, Count := sum(Count), by = list(Year, `TSE新產業名`)] %>% 
    ggplot(., aes(x = Year, y = Count)) +
    geom_bar(aes(x = Year, y = Count), stat = "identity", fill = "#007979")+
    geom_label(aes(x = Year, y = Count, label = Count)) +
    theme_classic() +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
          text = element_text(family="黑體-繁 中黑"),
          plot.background  = element_rect(fill = "aliceblue"), 
          panel.background = element_rect(fill = "aliceblue"),
          legend.background = element_rect(fill = "aliceblue")) +
    facet_wrap(~ `TSE新產業名`, scales = "free")
  
  return(Plot)
}

Industrysign <- Industryratio$Var1 %>%
  head(., 9)

png("各產業上市櫃情況(年).png", width = 15, height = 10, units = 'in', res = 300)
QuantitiesofIPOforeachYear(ListFirm, Industrysign)
dev.off()

##### 重大財務危機 ##### 
# 興櫃期間是否有發生重大危機事件
# Delist <- IPOinTaiwan %>% 
#   .[, .SD, .SDcols = c("公司中文簡稱", "下市日期")] %>% 
#   .[is.na(`下市日期`) != T] %>% 
#   arrange(`下市日期`)
CauseofCrisis_Incomplete <- ListFirm %>% 
  .[, .SD, .SDcols = c("公司中文簡稱", "危機事件大類別說明", "危機事件類別說明")] %>% 
  .[complete.cases(.), ] %>% 
  .[!(str_sub(危機事件類別說明, 1, 7) == "大虧,淨值低5" | str_sub(危機事件類別說明, 1, 4) == "全額下市" | str_sub(危機事件類別說明, 1, 5) == "紓困-財危")] %>% 
  .[, `:=` (CauseOfBigType = str_split(危機事件大類別說明, "[:punct:]", simplify = T)[, 1],
            Cause = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 1],
            Year = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 2],
            Month = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 3],
            Day = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 4])] %>% 
  .[, Time := ymd(str_c(Year, Month, Day))]

DealWithSpecialCause <- function(TypeofCrisis){
  SpecialCause <- ListFirm %>% 
    select(公司中文簡稱, 危機事件大類別說明, 危機事件類別說明) %>% 
    .[complete.cases(.), ] %>% 
    setDT() %>% 
    .[str_sub(危機事件類別說明, 1, str_length(TypeofCrisis)) == TypeofCrisis] %>% 
    .[, `:=` (CauseOfBigType = str_split(危機事件大類別說明, "[:punct:]", simplify = T)[, 1],
              Cause = str_c(str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 1], 
                            str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 2],
                            sep = "_"),
              Year = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 3],
              Month = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 4],
              Day = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 5])] %>% 
    .[, Time := ymd(str_c(Year, Month, Day))]
  return(SpecialCause)
}
SpecialCause1 <- DealWithSpecialCause("大虧,淨值低5")
SpecialCause2 <- DealWithSpecialCause("紓困-財危")
CauseofCrisis <- rbind(CauseofCrisis_Incomplete, SpecialCause1, SpecialCause2) %>% 
  arrange(Time) %>% 
  setDT() %>% 
  .[`公司中文簡稱` %in% ListFirm$公司中文簡稱] %>% 
  left_join(., 
            ListFirm %>% .[, .SD, .SDcols =c("公司中文簡稱", "首次掛牌日期", "前一次變更代碼日期",  "前一次變更市場" , "下市日期")], 
            by = "公司中文簡稱") 

CrisisTime <- setDT(CauseofCrisis) %>%
  .[, HowMuchMonth := round((Time - ymd(`前一次變更代碼日期`))/30)] %>% 
  .[, CrisisTime := case_when(
    Time <= `首次掛牌日期` ~ "未興櫃時",
    (Time > `首次掛牌日期` & Time <= `前一次變更代碼日期`) ~ "興櫃中",
    Time > `前一次變更代碼日期` ~ str_c("上市櫃後 ", round(as.numeric(Time - ymd(`前一次變更代碼日期`))/30), "個月")
  )]

# 以中位數來看，公司上市後104.5個月才會發生重大危機
AverageTimeToCrisis <- CrisisTime %>% 
  .[HowMuchMonth > 0] %>% 
  pull(HowMuchMonth) %>%
  as.numeric() %>% 
  median() %>% 
  round(., 2) %>% 
  paste0("平均 ", .," 個月")

# 重大危機大類別
CauseOfBigTypeRatio <- CauseofCrisis %>% 
  pull(CauseOfBigType) %>% 
  table() %>% 
  as.data.frame() %>% 
  rename("CauseOfBigType" = ".")

png("重大危機事件 類別頻率.png", width = 11, height = 8, units = 'in', res = 300)
ggplot(data = CauseOfBigTypeRatio)+
  geom_bar(aes(x = CauseOfBigType, y = Freq), stat = "identity", fill = "#007979")+
  geom_label(aes(x = CauseOfBigType, y = Freq, label = Freq), data = CauseOfBigTypeRatio) +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        text = element_text(family="黑體-繁 中黑"),
        plot.background  = element_rect(fill = "aliceblue"), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue")) +
  xlab("重大危機事件")+
  ggtitle("重大危機事件 類別頻率")
dev.off()

# 重大財務危機發生頻率
Crisisratio <- CauseofCrisis %>% 
  .[CauseOfBigType == "財務危機"] %>% 
  pull(Cause) %>% 
  table() %>% 
  as.data.frame() %>%
  rename("Cause" = ".") %>% 
  arrange(desc(Freq))

png("危機事件 類別分佈.png", width = 11, height = 8, units = 'in', res = 300)
ggplot(data = Crisisratio)+
  geom_bar(aes(x = Cause, y = Freq), stat = "identity", fill = "#007979")+
  geom_label(aes(x = Cause, y = Freq, label = Freq), data = Crisisratio) +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        text = element_text(family="黑體-繁 中黑"),
        plot.background  = element_rect(fill = "aliceblue"), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue")) +
  ylab("Cause")+
  ggtitle("危機事件 類別分佈")+
  coord_flip()
dev.off()

# 重大財務危機 而下市
CrisisToDelistratio <- CauseofCrisis %>% 
  .[, DelistFromCrisis := ifelse(Time < 下市日期, 1, 0)] %>% 
  pull(DelistFromCrisis)
CrisisToDelistratio[is.na(CrisisToDelistratio)] <- 0

CrisisToDelist <- CrisisToDelistratio %>% 
  table() %>% 
  as.data.frame() %>%
  rename("DelistorNot" = ".") %>% 
  arrange(desc(Freq))

png("是否因財務危機下市 類別分佈.png", width = 11, height = 8, units = 'in', res = 300)
ggplot(data = CrisisToDelist)+
  geom_bar(aes(x = DelistorNot, y = Freq), stat = "identity", fill = "#007979")+
  geom_label(aes(x = DelistorNot, y = Freq, label = Freq), data = CrisisToDelist) +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        text = element_text(family="黑體-繁 中黑"),
        plot.background  = element_rect(fill = "aliceblue"), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue")) +
  ggtitle("是否因財務危機下市")
dev.off()

##### 下市櫃公司 ##### 
# 產業來說以 電子工業佔據絕大多數（有8家電子工業沒有新產業名）
table(DelistWithin5Years$TSE新產業名)

# 可發現 2016 2018年的失敗公司最多
CountDelistInEachYear <- DelistFirm$下市日期 %>% 
  year() %>% 
  table() %>% 
  as.data.table %>% 
  rename("Year" = ".")

png("每年下市櫃比例.png", width = 11, height = 8, units = 'in', res = 300)
CountDelistInEachYear %>% 
  ggplot(., aes(x = Year, y = N)) +
  stat_summary(fun.y = sum, geom = "bar", fill = "#007979") +
  geom_label(aes(label= N)) +
  ggtitle("每年下市櫃比例")+
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        text = element_text(family="黑體-繁 中黑"),
        plot.background  = element_rect(fill = "aliceblue"), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue"))
dev.off()

# 判定為五年內下市櫃的公司中，2006 - 2008 為失敗比例最高 因此可能得考慮金融風暴
CountDelistInEachYear_filter5year <- DelistWithin5Years$下市日期 %>% 
  year() %>% 
  table() %>% 
  as.data.table %>% 
  rename("Year" = ".")

png("每年下市櫃比例_(五年內).png", width = 11, height = 8, units = 'in', res = 300)
CountDelistInEachYear_filter5year %>% 
  ggplot(., aes(x = Year, y = N)) +
  stat_summary(fun.y = sum, geom = "bar", fill = "#007979") +
  geom_label(aes(label= N)) +
  ggtitle("每年下市櫃比例_(五年內)")+
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        text = element_text(family="黑體-繁 中黑"),
        plot.background  = element_rect(fill = "aliceblue"), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue"))
dev.off()
  
##### 全額交割 與 管理股票 #####
# 列為全額交割股後一年內的股價表現與成交量表現如何
FullCashDeliveryStocks_FCDDate <- FCDFirms_within5Years %>% 
  .[, .SD, .SDcols = c("前一次變更代碼", "全額交割起日(一)")] %>% 
  .[, `公司簡稱` := as.character(`前一次變更代碼`)]

Price_Vol_FCDS <- Price_Vol_FCDS_Data %>% 
  .[證券代碼 %in% FullCashDeliveryStocks_FCDDate$公司簡稱]

GetFCDPlotForYear <- function(Firm_sign, Var){
  FCDDate <- FullCashDeliveryStocks_FCDDate %>% 
    .[`公司簡稱` == Firm_sign] %>% 
    pull(`全額交割起日(一)`) %>% 
    as.character() 
  
  Data <- Price_Vol_FCDS %>% 
    .[證券代碼 == Firm_sign] %>% 
    .[ymd(`年月日`) >= ymd(FCDDate) & ymd(`年月日`) <= ymd(FCDDate)+ 365]
  
  Plot <- Data %>% 
    ggplot(., aes(x = ymd(年月日), y = get(Var))) +
    geom_point() +
    geom_smooth() +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
          text = element_text(family="黑體-繁 中黑")) + # 解決中文亂碼問題
    labs(x = "Time",  y = Var, title = Firm_sign)+
    theme_classic() +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
          text = element_text(family="黑體-繁 中黑"),
          plot.background  = element_rect(fill = "aliceblue"), 
          panel.background = element_rect(fill = "aliceblue"),
          legend.background = element_rect(fill = "aliceblue"))
  
  return(Plot)
}

GetFCDPlotForYear("2724", "收盤價(元)")

test <- FullCashDeliveryStocks_FCDDate %>% 
  pull(公司簡稱) %>% 
  as.character() 
test[1:5]

map(test[1:5], function(x){GetFCDPlotForYear(x, "收盤價(元)")})


# 計算 全額交割股的 認列後累積一年報酬 發現21家公司中 15家為負 6家為正
GetFCDP_CAR_ForYear <- function(Firm_sign){
  FCDDate <- FullCashDeliveryStocks_FCDDate %>% 
    .[`公司簡稱` == Firm_sign] %>% 
    pull(`全額交割起日(一)`) %>% 
    as.character() 
  
  Data <- Price_Vol_FCDS %>% 
    .[證券代碼 == Firm_sign] %>% 
    .[ymd(`年月日`) >= ymd(FCDDate) & ymd(`年月日`) <= ymd(FCDDate)+ 365] %>% 
    .[, .SD, .SDcols = c("年月日", "收盤價(元)")] %>% 
    .[, `年月日` := as.Date(ymd(`年月日`))] %>% 
    as.data.frame()
  
  xts_Data <- xts(Data[, -1], Data[, 1])
  
  CAR <- Return.calculate(xts_Data) %>% 
    Return.cumulative() %>% 
    as.numeric()
  
  return(CAR)
}

FCDwithCAR <- FullCashDeliveryStocks_FCDDate %>% 
  .[, CAR := 0] %>% 
  .[, .SD, .SDcols = -c("公司簡稱")] %>% 
  na.omit()

for (i in 1:nrow(FullCashDeliveryStocks_FCDDate)) {
  FCDwithCAR[i, 3] <- GetFCDP_CAR_ForYear(as.character(FCDwithCAR[i, 1]))
  print(i)
}

setDT(FCDwithCAR) %>% 
  .[, Check := ifelse(CAR < 0, "Negitive", "Positive")] %>% 
  pull(Check) %>% 
  table()

# 負的公司中，平均：- 37 %，中位數：-29 % 
setDT(FCDwithCAR) %>% 
  .[CAR < 0] %>% 
  pull(CAR) %>% 
  summary()

setDT(FCDwithCAR) %>% 
  .[CAR > 0] %>% 
  pull(CAR) %>% 
  summary()

# 負報酬的全額交割股中，產業沒有特別的分佈
FCDwithCAR %>% 
  left_join(., ListFirm %>% .[, .SD, .SDcols = c("前一次變更代碼", "TSE新產業名")], by = "前一次變更代碼") %>% 
  setDT() %>% 
  .[CAR < 0] %>% 
  pull(`TSE新產業名`) %>% 
  table
  

##### 平均興櫃時間 #####
# 上市櫃公司的平均興櫃時間為1.85年 中位數為1.22年 最長可達10年
SummaryROTCTime <- ListFirm %>% 
  pull(ROTC_Time) %>% 
  as.numeric() %>% 
  summary()
# 所有下市櫃公司中，興櫃平均時間為1.86年 中位數1.24年
SummaryROTCTime_DelistFirm <- DelistFirm %>% 
  pull(ROTC_Time) %>% 
  as.numeric() %>% 
  summary()
# 五年內下市櫃的公司中，興櫃平均時間為1.89年 中位數1.24年
SummaryROTCTime_DelistWithin5Years <- DelistWithin5Years %>% 
  pull(ROTC_Time) %>% 
  as.numeric() %>% 
  summary()

# 下市櫃公司的電子工業中，平均興櫃時間為1.66年 非電子工業平均為2.28年
DelistFirm %>% 
  .[首次掛牌TSE產業 == "M2300 電子工業"] %>% 
  pull(ROTC_Time) %>% 
  as.numeric() %>% 
  summary()

DelistFirm %>% 
  .[首次掛牌TSE產業 != "M2300 電子工業"] %>% 
  pull(ROTC_Time) %>% 
  as.numeric() %>% 
  summary()

# 五年內即下市櫃的電子工業，平均興櫃時間為 2 年，非電子工業平均為1.73年
DelistFirm %>% 
  .[首次掛牌TSE產業 == "M2300 電子工業"] %>% 
  .[SurvivalYears <= 5] %>% 
  pull(ROTC_Time) %>% 
  as.numeric() %>% 
  summary()

DelistFirm %>% 
  .[首次掛牌TSE產業 != "M2300 電子工業"] %>% 
  .[SurvivalYears <= 5] %>% 
  pull(ROTC_Time) %>% 
  as.numeric() %>% 
  summary()

##### 找出所有公司的興櫃期間 #####
RotcTime <- ListFirm %>% 
  .[, .SD, .SDcols = c("前一次變更代碼", "首次掛牌日期", "前一次變更代碼日期")] %>% 
  .[, ROTC_Time := round((`前一次變更代碼日期` - `首次掛牌日期`)/365, 2)]

##### 興櫃的股價資料 #####
MarketReturn <- MarketReturn_Data %>% 
  .[, Date := ymd(`年月日`)] %>% 
  .[, .SD, .SDcols = c("Date", "市場投組", "無風險利率")]

names(MarketReturn)

ROTCPrice <- setDT(ROTCPriceData) %>% 
  .[, .SD, .SDcols = c("證券代碼", "年月日",  "報酬率％")] %>% 
  .[, Date := ymd(`年月日`)] %>% 
  left_join(., MarketReturn, by = "Date") %>% 
  setDT() %>% 
  .[, .SD, .SDcols = c("證券代碼", "年月日", "報酬率％", "市場投組", "無風險利率")] %>% 
  .[order(`證券代碼`, `年月日`)]

# 計算所有公司的Market Model 殘差
GetMarketModelResiduals <- function(Firm){
  CompanyData <- ROTCPrice %>% 
    .[`證券代碼` == Firm]
  MarketModel <- lm(`報酬率％` ~ `市場投組`, data = CompanyData)
  Residual <- MarketModel$residuals %>% as.data.table 
  return(Residual)
}
MarketModelResidual <- NULL
for (i in unique(ROTCPrice$證券代碼)) {
  Residual <- GetMarketModelResiduals(i)
  MarketModelResidual <- rbind(MarketModelResidual, Residual)
  print(i)
}
  
ROTCPrice <- cbind(ROTCPrice, MarketModelResidual) %>% 
  rename("MarketModelResidual" = ".")

SDForEachFirmin10Days_Data <- ROTCPrice %>% 
  .[, .SD, .SDcols = c("證券代碼", "MarketModelResidual")] %>% 
  setDT() %>% 
  .[, rollapply(`MarketModelResidual`, 10, sd, by = 10, align = "left", partial=T), by = `證券代碼`]

SDForEachFirmin10Days <- SDForEachFirmin10Days_Data %>% 
  .[, id := seq_len(.N), by = "證券代碼"] %>% 
  .[, Numbering := abs(id - max(id)) + 1, by = "證券代碼"] %>% 
  .[, .SD, .SDcols = c("證券代碼", "Numbering", "V1")] %>% 
  .[, AveSD := mean(V1, na.rm = T), by = "Numbering"] %>% 
  rename("ResidualSD" = "V1") %>% 
  as.data.table()

rm(SDForEachFirmin10Days)














