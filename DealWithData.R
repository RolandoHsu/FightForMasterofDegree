library(tidyverse)
library(ggplot2)
library(stringr)
library(data.table)
library(readxl)
library(lubridate)

IPOData <- read_xlsx("IPOinTaiwan_1106.xlsx")
colnamesinIPOData <- names(IPOData) %>% as.data.frame()

# date range : 2002-01-01 ~ 2014-12-31
# 金融控＋證券＋金融業＋壽險 = 22
IPOinTaiwan <- setDT(IPOData) %>% 
  .[`首次掛牌日期` >= "2002-01-01 UTC" & `首次掛牌日期` <= "2014-12-31 UTC"] %>% 
  .[!(`首次掛牌TSE產業` %in% c("M28HO 金融控", "M3000 証券", "M2800 金融業", "M2871 壽險"))] 

# Know that almost half of IPO companies belong to elastic industry. (Almost 50%)
Industryratio <- table(IPOinTaiwan$首次掛牌TSE產業) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  setDT() %>% 
  .[, `IndustryPre (%)` := round((Freq/sum(Freq))*100,2)]

# The quantities of Electronic industry have reduced year by year.
QuantitiesofIPOforeachYear <- function(data, industry){
  Plot <- setDT(data) %>%
    .[, .SD, .SDcols = c("公司簡稱", "首次掛牌日期", "首次掛牌TSE產業")] %>% 
    .[`首次掛牌TSE產業` %in% industry] %>% 
    .[, `:=`(Year = year(`首次掛牌日期`),
             Count = 1)] %>%
    .[, Count := sum(Count), by = list(Year, 首次掛牌TSE產業)] %>% 
    ggplot(., aes(x = Year, y = Count)) +
    stat_summary(fun.y = sum, geom = "bar", fill = "#007979")+
    facet_wrap(~ `首次掛牌TSE產業`, scales = "free") +
    geom_label(aes(label = Count), nudge_y = 1)
  
  return(Plot)
}

Industrysign <- Industryratio$Var1 %>%
  head(., 9)
map(Industrysign, function(x){QuantitiesofIPOforeachYear(IPOinTaiwan, x)})
QuantitiesofIPOforeachYear(IPOinTaiwan, "M2300 電子工業")
QuantitiesofIPOforeachYear(IPOinTaiwan, "M1722 生技醫療")
QuantitiesofIPOforeachYear(IPOinTaiwan, Industrysign)

##### Quantities of IPO failure #####
Delist <- setDT(IPOinTaiwan) %>% 
  .[, .SD, .SDcols = c("公司中文簡稱", "下市日期")] %>% 
  .[is.na(`下市日期`) != T] %>% 
  arrange(`下市日期`)

CauseofCrisis <- IPOinTaiwan %>% 
  select(公司中文簡稱, 危機事件大類別說明, 危機事件類別說明) %>% 
  .[complete.cases(.), ] %>% 
  setDT() %>% 
  .[!(str_sub(危機事件類別說明, 1, 7) == "大虧,淨值低5" | str_sub(危機事件類別說明, 1, 4) == "全額下市" | str_sub(危機事件類別說明, 1, 5) == "紓困-財危")] %>% 
  .[, `:=` (CauseOfBigType = str_split(危機事件大類別說明, "[:punct:]", simplify = T)[, 1],
            Cause = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 1],
            Year = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 2],
            Month = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 3],
            Day = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 4])]

DealWithSpecialCause <- function(TypeofCrisis){
  SpecialCause <- IPOinTaiwan %>% 
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
              Day = str_split(危機事件類別說明, "[:punct:]", simplify = T)[, 5])]
  return(SpecialCause)
}
SpecialCause1 <- DealWithSpecialCause("大虧,淨值低5")
SpecialCause2 <- DealWithSpecialCause("全額下市")
SpecialCause3 <- DealWithSpecialCause("紓困-財危")
CauseofCrisis <- rbind(CauseofCrisis, SpecialCause1, SpecialCause2, SpecialCause3) %>% 
  arrange(Year, Month, Day)

setDT(CauseofCrisis) %>% 
  .[Cause == "全額下市_非5"]

setDT(CauseofCrisis) %>% 
  .[CauseOfBigType == "財務危機"] %>% 
  pull(Cause) %>% 
  unique

setDT(CauseofCrisis) %>% 
  .[CauseOfBigType == "重大財務事件"] %>% 
  pull(Cause) %>% 
  unique

CrisiswithDelist <- left_join(CauseofCrisis, Delist, by = "公司中文簡稱")

# 在所有下市公司中，曾發生 倒閉破產*4 繼續經營疑慮*6 淨值為負*7 跳票擠兌*6
TabletheCauseWhichDelist <- setDT(CrisiswithDelist) %>% 
  .[is.na(`下市日期`) == F] %>% 
  .[, CrisisDate := ymd(str_c(Year, Month, Day))] %>% 
  .[CrisisDate <= `下市日期`] %>% 
  pull(Cause) %>% 
  table() %>% 
  as.data.frame()

SeriousCause <- c("倒閉破產", "繼續經營疑慮", "淨值為負", "跳票擠兌")

test <- setDT(CrisiswithDelist) %>% 
  .[Cause %in% SeriousCause] %>% 
  .[, .SD, .SDcols = c("Cause", "下市日期")] %>% 
  .[]


test <- setDT(IPOinTaiwan) %>% 
  .[, .SD, .SDcols = c("公司中文簡稱", "首次掛牌市場", "首次掛牌代碼", "首次掛牌日期")] %>% 
  .[year(`首次掛牌日期`) == 2003]











