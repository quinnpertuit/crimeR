library(tidyverse)
library(DBI)
library(RODBC)

# Database connection
con.text = paste0("driver=", "ODBC Driver 17 for SQL Server",
                  ";database=", "StatModel",
                  ";server=", "dreamlinefgb1.database.windows.net",
                  ";port=", 1433,
                  ";protocol=TCP",
                  ";UID=", "Support@dreamlinefgb1",
                  ";PWD=",rstudioapi::askForPassword("Password"))
conn = dbConnect(odbc::odbc(), .connection_string = con.text) 

# Get player names from DraftKings and PGA
draftkings = dbGetQuery(conn, "select distinct name as 'name.draftkings' from draftkings_ownership where date>='2016-09-11'")
pga        = dbGetQuery(conn, "select distinct name as 'name.pga' from world_rank where end_date>='2016-09-11'") 

source('~/Documents/GitHub/PGAscrapR/Forecasting/Functions/format functions.R')

Z=tibble(name.draftkings=c("Augusto Nunez","David Lawrence"),
       name.pga=c("Augusto Núñez","David Law"))
# Leverage prior mapping starter file

fixNamesNew = function(df){
  newNames = read_csv('./Functions/name mapping/2020-02-19 22:10:11 DraftKings PGA Name Matches to PGA.csv') %>% rename(name=name.draftkings)
  df = df %>%
    left_join(.,newNames) %>%
    mutate(name = ifelse(is.na(name.pga),name,name.pga))
  return(df)
}


# Find which draftkings names do not have a PGA match
draftkings_matching_pganames = draftkings %>% filter(name.draftkings %in% pga$name.pga) %>% mutate(name.pga = name.draftkings)
draftkings_missing_pganames  = draftkings %>% rename(name=name.draftkings) %>% fixNamesNew() %>% rename(name.draftkings=name) %>% 
  filter(!name.draftkings %in% pga$name.pga) %>% mutate(name.pga = NA) 

# Find closest PGA name for each draft kings name that doesn't have a PGA name
for(i in 1:nrow(draftkings_missing_pganames)) {
  x <- agrep(draftkings_missing_pganames$name.draftkings[i], pga$name.pga,
             ignore.case=TRUE, value=TRUE,
             max.distance = 0.4, useBytes = TRUE) %>% paste0(.,"")
  draftkings_missing_pganames$name.pga[i] <- x
} 
draftkings_new_matches = draftkings_missing_pganames %>% filter(name.pga!="")

# Final output: DraftKings names with corresponding PGA match
newnamesverified = draftkings_new_matches 
  newnamesverified2 = newnamesverified %>% 
  #mutate_at(vars(name.draftkings,name.pga),function(x){paste0("'",x,"'")}) %>% 
  arrange(desc(verify)) %>%
    filter(verify=='good') 
  write_csv(newnamesverified2,'new_name_matches_20200219.csv')

final = left_join(draftkings, rbind(draftkings_matching_pganames, draftkings_new_matches))
write_csv(final, paste(format(Sys.time(),'%Y-%m-%d %H:%M:%S'),'DraftKings PGA Name Matches to PGA.csv'))
