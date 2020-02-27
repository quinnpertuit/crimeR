
ts = seq.Date(from = as.Date('2010-05-08'), to = as.Date('2020-02-22'), by = 'weeks') 
ts = as.list(format(ts,'%y%m%d')) %>% `names<-`(as.Date(ts))
d = lapply(ts,function(x){
  tryCatch(
  read.csv(
    paste0("http://web.mta.info/developers/data/nyct/turnstile/turnstile_",x,".txt"), stringsAsFactors = FALSE),error=function(err){print(x)})})
