library(tidyverse)
library(jsonlite)
library(rvest)
library(httr)

dtc = c(81:242) 
dtl = as.list(dtc) %>% `names<-`(dtc)

getDates = function(x){
  url = paste0('https://www.linestarapp.com/Projections/Sport/PGA/Site/DraftKings/PID/',x)
  id = x+1
  res = GET(url)$content %>% read_html() %>% html_nodes(xpath='//*[@id="dnn_ctr749_ModuleContent"]/div/div/div[2]/div[3]/a') %>% html_text(trim=TRUE)
  out = tibble(id = id, ds = res)
}

contestDatesList = lapply(dtl,getDates)
contestDatesDF = bind_rows(contestDatesList)

contests=contestDatesDF %>% 
  separate(ds,into=c('start_date','end_date'),sep="-") %>%
  separate(end_date, into = c('end_date','year'),sep=', ') %>%
  mutate(end_date   = ifelse(nchar(end_date)<=2,as.Date(paste0(substr(start_date,1,3)," ",end_date,"-",year),'%b %d-%Y'),as.Date(paste0(end_date,'-',year),'%b %d-%Y'))) %>% 
  mutate(end_date = as.Date(end_date, origin='1970-01-01')) %>% 
  mutate(start_date = as.Date(paste0(start_date,'-',year),'%b %d-%Y')) 


getProjections = function(x){
  jsonurl = paste0('https://www.linestarapp.com/DesktopModules/DailyFantasyApi/API/Fantasy/GetSalariesV5?sport=5&site=1&periodId=',x)
  content = jsonlite::fromJSON(jsonurl)
  names          = content$Ownership$Salaries %>% as_tibble %>% select(Id,Name)
  projections    = content$Ownership$Projected[[1]] %>% select(PlayerId,Owned,Id=SalaryId) %>% left_join(.,names)
  out = projections %>% select(name=Name,owned=Owned) %>% mutate(id=x)
  return(out)
}

project = lapply(dtl,getProjections)
projectDF = bind_rows(project) %>% left_join(.,contests) %>% filter(!is.na(start_date))

write_csv(projectDF,'linestarapp_ownership_projections.csv')