## Add known information at start of tournament to the input of the tour to forecast
## Prepare input of known salaries (adds in regressors known at current time for tour you want to forecast, e.g. score of prior week)

getModel = function(){
  trainDF = history %>%
    filter(!is.na(score) & !is.na(salary_p) & !is.na(ownership) & !is.na(worldrank_p) & !is.na(tour)) %>%
    rename(y = ownership) %>%
    mutate(tour = as.factor(tour)) %>%
    mutate(name = as.factor(name)) 
  
  ## Train the model
  model <- prophet(yearly.seasonality=FALSE, weekly.seasonality=FALSE, daily.seasonality=FALSE) %>%
    add_regressor('name') %>%
    add_regressor('tour') %>%
    add_regressor('nPlayers') %>%
    add_regressor('salary') %>%
    add_regressor('linestar') %>% 
    add_regressor('worldrank_p') %>%
    add_regressor('avgpoints_p') %>%
    add_regressor('competition') %>%
    add_regressor('salary_p') %>%
    add_regressor('score_p') %>%
    add_regressor('scorerank_p') %>%
    add_regressor('ownership_p') %>%
    fit.prophet(m = ., df = trainDF)
}

doProphet = function(df_history, df_input, model, nPlayers){

  tourname = unique(df_input$tour)
  # Input history and known data for forecast date (e.g. salary is known for forecasting week)
  df_player_name = unique(df_history$name) %>% as.character() %>% print()
  #df_history = df_history %>% filter(ds!=min(df_history$ds))
  df_input2   = df_input %>% filter(name==df_player_name) %>% mutate(y=NA,tour=tourname,nPlayers=nPlayers) 

  # Forecast
  forecast = predict(model, df_history %>% mutate(name = as.character(name)) %>% bind_rows(.,df_input2)  %>% mutate_at(vars(name,tour),as.factor))

  # Output
  output = forecast %>% select(ds,median=yhat,lower=yhat_lower,upper=yhat_upper) %>%
    gather(statistic,value,-c('ds')) %>% mutate(name = df_player_name, tour = tourname, nPlayers = nPlayers)

  return(output)
}

runModel = function(){
  history %>%
    filter(!is.na(score) & !is.na(salary_p) & !is.na(ownership) & !is.na(worldrank_p) & !is.na(tour)) %>%
    filter(name %in% input$name) %>% 
    group_by(name) %>%
    do(doProphet(df_history = ., df_input = input, model = model, nPlayers = length(input$name))) %>%
    select(name, ds, statistic, value) %>% ungroup()
}

formatFcst = function(){
  outFcst = modelResults %>% mutate(ds = as.Date(ds)) %>% select(name,ds,statistic,ownership=value) %>% mutate(ownership = ifelse(statistic %in% c('median','lower') & ownership<0,0,ownership)) %>% group_by(name,ds,statistic) %>% summarise(ownership = mean(ownership)) %>% ungroup() %>% unique() 
  outHist = history %>% filter(name %in% outFcst$name) %>% mutate(statistic='actual') %>% select(name,ds,statistic,ownership)
  outDF   = rbind(outHist,outFcst)
  return(outDF)
}
