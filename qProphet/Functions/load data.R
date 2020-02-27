
formatNames = function(x){
  x = stri_trans_general(x, "Latin-ASCII")
  x = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
  x = trimws(x)
  x = gsub("-","",x,fixed=TRUE)
  x = gsub("'","",x,fixed=TRUE)
  x = gsub("  "," ",x,fixed=TRUE)
  return(x)
}

mapNames = function(df){
  newNames = suppressMessages(read_csv('./Functions/name mapping/pga_name_mapper_20200226.csv')) %>% 
    rename(name=name.draftkings, new.name = name.pga) %>% unique()
  
  df = df %>%
    left_join(.,newNames, by = c('name')) %>%
    mutate(name = ifelse(is.na(new.name),name,new.name)) %>% select(-new.name)
  
  return(df)
}

schedules = read_csv('./Data/pga/schedulesExtract.csv') %>% mutate(ds=end_date) %>% filter(ds>=as.Date('2016-09-25')) %>% select(-scrape_timestamp)
worldrank = read_csv('./Data/pga/worldrankExtract.csv') %>% mutate(ds=end_date) %>% mutate(name = formatNames(name)) %>% select(-scrape_timestamp) %>% filter(ds>=as.Date('2016-09-25')) %>%
  select(ds,name,worldrank,avgpoints) %>% unique() %>%
  group_by(ds,name) %>% summarise(worldrank=mean(worldrank),avgpoints=mean(avgpoints)) %>% ungroup()
DKownershiphist = read_csv('./Data/pga/DKownershipExtract.csv') %>% rename(ds=end_date) %>% select(-add_timestamp) %>% rename(name=draftkings.name) %>% mutate(name = formatNames(name)) %>% mapNames() %>% filter(ds>=as.Date('2016-09-25'))
linestar  = read_csv('./Data/pga/linestarappExtract.csv') %>% select(-id,-year) %>% unique %>% rename(ds=end_date) %>% mutate(name = formatNames(name)) %>% mapNames() %>% select(ds,name,linestar=owned) %>% mutate(isline='Y') %>% filter(ds>=as.Date('2016-09-25'))


prepInput = function(){
  DKsalaryinputs = read_csv('./Data/pga/DKfuturesalariesExtract.csv') %>% rename(ds=end_date) %>% rename(name = draftkings.name) %>% mutate(name = formatNames(name)) %>% mapNames() %>% select(name,Salary=salary,ds) %>% filter(ds>=as.Date('2016-09-25')) %>%
    left_join(.,schedules)
  
    input = DKsalaryinputs %>% filter(ds==date_to_forecast) %>% select(ds,tour=tourname,name,salary=Salary) 
    mostprior = history %>%
      filter(!is.na(score) & !is.na(salary_p) & !is.na(ownership) & !is.na(worldrank_p)) %>%
      filter(name %in% input$name) %>%
      group_by(name) %>%
      filter(ds < date_to_forecast) %>%
      summarise(ds = max(ds)) %>% ungroup() %>%
      left_join(.,history) %>%
      select(name,salary_p = salary,score_p = score,
             scorerank_p = scorerank,
             ownership_p = ownership,
             worldrank_p = worldrank, avgpoints_p = avgpoints, linestar)
    
    out = mostprior %>% 
      left_join(input,.) %>% mutate(y=NA) %>%
      group_by(ds) %>% 
      mutate(nPlayers = n()) %>% 
      mutate(competition = mean(worldrank_p,na.rm = TRUE)) %>% ungroup() %>%
      select(name,salary,ds,y,everything()) 
    
    return(out)
}

getInput = function(){
  if(backtest == FALSE){
    input = prepInput()
  }else{
    input = prepBacktestInput()
  }
  return(input)
}

prepBacktestInput = function(){

  out = history %>% 
    filter(ds == date_to_forecast) %>% 
    select(ds,tour,name,salary,salary_p,score_p,scorerank_p,
           ownership_p,worldrank_p,avgpoints_p,linestar,competition) %>%
    mutate(y=NA) %>% 
    select(tour,name,salary,ds,y,everything()) 
    
  return(out)
}




## Format DraftKings Ownership Files
getHistory = function(){
  
  # Remove 2-Tour Players (24 records of the 19k) - one player name one one date in 2 tours
  remove2tourplayers = tibble(name = c("John Mallinger", "Robert Garrigus", 
                                       "John Mallinger", "Andrew Landry", "Alexander Noren", "Alexander Noren", 
                                       "Alexander Noren", "Michael LorenzoVera", "KyoungHoon Lee", "Alexander Noren", 
                                       "Alexander Noren", "KyoungHoon Lee"),
                              ds = structure(c(17447, 17615, 17615, 17685, 17734, 17748, 17755, 17755, 17825, 17832, 17951, 18189), class = "Date")) %>% rbind(.,
                              DKownershiphist %>% group_by(ds,name) %>% filter(n()>1) %>% ungroup() %>% select(ds,name) %>% unique()) %>%
                              mutate(remove='Remove') 
  
  out = DKownershiphist %>%
    left_join(.,worldrank, by = c('ds','name')) %>% 
    left_join(.,linestar, by = c('ds','name')) %>% 
    left_join(.,remove2tourplayers, by = c('ds','name')) %>% 
    filter(is.na(remove)) %>% 
    filter(isline=='Y') %>% select(-isline) %>% 
    arrange(name,ds) %>%
    group_by(name) %>%
    filter(!is.na(score)) %>%
    filter(!is.na(tour)) %>% 
    mutate(salary_p=lag(salary)) %>%
    mutate(score_p = lag(score)) %>%
    mutate(scorerank_p = lag(scorerank)) %>%
    mutate(ownership_p = lag(ownership)) %>%
    mutate(worldrank = ifelse(is.na(worldrank),na.locf(worldrank, fromLast = TRUE),worldrank)) %>% 
    mutate(avgpoints = ifelse(is.na(avgpoints),na.locf(avgpoints, fromLast = TRUE),avgpoints)) %>% 
    mutate(worldrank_p=lag(worldrank)) %>%
    mutate(avgpoints_p=lag(avgpoints)) %>% 
    ungroup() %>%
    group_by(ds) %>%
    mutate(nPlayers = n()) %>%
    mutate(competition = mean(worldrank_p,na.rm=TRUE)) %>% 
    ungroup() 
  
  rm(remove2tourplayers)
  
return(out)
}
