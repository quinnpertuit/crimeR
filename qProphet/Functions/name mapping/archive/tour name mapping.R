setwd("/Users/quinnx/Documents/GitHub/PGAscrapR/Forecasting")

source('./Functions/load libraries.R')

ownershipFiles = list.files(path='./Input/Ownership Files', pattern='.csv',full.names = T)
dkfileList = as.list(ownershipFiles) %>% `names<-`(as.Date(gsub("./Input/Ownership Files/","",gsub(".csv","",ownershipFiles)),"%Y-%m-%d"))
dkfileList = dkfileList[names(dkfileList) %>% sort] %>% lapply(.,read_csv_print) %>% map_df(., ~as.data.frame(.), .id="date") %>% bind_rows %>% as_tibble %>% mutate(date = as.Date(date)) %>% rename(end_date=date) #%>% mutate_at(vars(name),formatNames)

pga = schedules %>% select(ds=end_date,tour=tourname) %>% unique() %>% group_by(ds) %>% mutate(n=n()) %>% 
  filter(n>1) %>% ungroup() %>% group_by(tour,ds,name) %>% top_n(10, worldrank) %>% 
  select(-n) %>% filter(ds %in% unique(dkfileList$date))

pga %>% select(ds,tour) %>% unique() %>% group_by(ds) %>% summarise(tour = first(tour)) %>% ungroup()

xx = worldrank %>% 
  select(end_date,tour) %>% unique() %>% 
  group_by(end_date) %>% mutate(n=n_distinct(tour)) %>% filter(n>1) %>% ungroup() %>% select(-n) %>%
  group_by(end_date) %>% summarise(tour = first(tour)) %>%
  filter(end_date %in% unique(dkfileList$end_date))

x = schedules %>% select(end_date,tour=tourname) %>% unique() %>%
  filter(!(end_date %in% xx$end_date)) %>%
  filter(end_date %in% unique(dkfileList$end_date)) 

xxx = rbind(x,xx)

dkfileListDF = left_join(dkfileList,xxx) %>%
  rename(draftkings.name = name) %>% 
  select(end_date,tour,everything())

dbRemoveTable(conn, "draftkings_ownership")
dbCreateTable(conn, "draftkings_ownership", x)
dbWriteTable(conn, "draftkings_ownership", x, overwrite=TRUE)

library(clipr)
clipr::write_clip(pga)

x=dbGetQuery(conn, "select * from draftkings_ownership") %>% mutate(add_timestamp=as.POSIXct(format(Sys.time(),'%Y-%m-%d %H:%M:%S')))



worldrank %>% 
  select(ds=end_date,tour,name,worldrank) %>% 
  group_by(ds) %>% mutate(n=n_distinct(tour)) %>% filter(n>1) %>% ungroup() %>% select(-n) %>% 
  arrange(ds,tour,worldrank) %>% 
  group_by(ds,tour) %>% 
  top_n(.,5,wt=-worldrank) %>% select(-worldrank) %>% ungroup()
