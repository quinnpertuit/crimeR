setwd("/Users/quinnx/Documents/GitHub/crimeR")
library(tidyverse)
library(lubridate)
library(jsonlite)
library(magrittr)
library(httr)
library(maps)

timeTibble = tibble(timename = c('Last 30 Days',c(2008:2019)),
                    timenum=c(8,32,33,34,35,11,10,9,27,26,38,0,1))

getCrimes = function(url){
  json = fromJSON(url, flatten = TRUE)
  restemp = json$features %>% as_tibble()
  return(restemp)
}

getCrimes_Fail = tibble(attributes.CCN = NA_character_, attributes.REPORT_DAT = 0, 
                        attributes.SHIFT = "NONE", attributes.METHOD = "NONE", 
                        attributes.OFFENSE = "NONE", attributes.BLOCK = "NONE", 
                        attributes.XBLOCK = 397228, attributes.YBLOCK = 137253, attributes.WARD = "2", 
                        attributes.ANC = "2F", attributes.DISTRICT = "2", attributes.PSA = "207", 
                        attributes.NEIGHBORHOOD_CLUSTER = "Cluster 8", attributes.BLOCK_GROUP = "010100 1", 
                        attributes.CENSUS_TRACT = "010100", attributes.VOTING_PRECINCT = "Precinct 129", 
                        attributes.LATITUDE = 38.9031278211765, attributes.LONGITUDE = -77.0319575524472, 
                        attributes.BID = "DOWNTOWN", attributes.START_DATE = 1571457004000, 
                        attributes.END_DATE = 1571460376000, attributes.OBJECTID = 360548027L, 
                        attributes.OCTO_RECORD_ID = "19186942-01", geometry.x = -77.0319598461276, 
                        geometry.y = 38.9031356087505)

outDF = tibble()
for (i in 1:nrow(timeTibble)){
  ids = fromJSON(paste0("http://maps2.dcgis.dc.gov/dcgis/rest/services/FEEDS/MPD/MapServer/",timeTibble$timenum[i],"/query?where=1%3D1&outFields=*&outSR=4326&f=json&returnIdsOnly=TRUE")) %>% .$objectIds %>% as.character()
  
  id_seq = tibble(
    start=seq(from=1,to=ceiling(length(ids)/100)*100,by=100),
    end=c(seq(from=100,to=length(ids),by=100),length(ids)))
  
  res = tibble()
  for (seq_i in 1:nrow(id_seq)){
  
    print(paste("Scraping:",timeTibble$timename[i],"- at",seq_i,"of",nrow(id_seq)))
    ids_string = str_c(ids[id_seq$start[seq_i]:id_seq$end[seq_i]], collapse = ",")
    #Sys.sleep(3)
    url = paste0("http://maps2.dcgis.dc.gov/dcgis/rest/services/FEEDS/MPD/MapServer/",timeTibble$timenum[i],"/query?where=1%3D1&objectIds=",ids_string,"&outFields=*&outSR=4326&f=json")
    
    restemp = tryCatch(getCrimes(url),
                       error=function(err){
                         print(paste0("Failed at: ",timeTibble$timename[i],"on: ",seq_i)) 
                         return(getCrimes_Fail %>% mutate(attributes.CCN = url))
                         }
                       )

    res = rbind(res,restemp)
  }
  
  resout = res %>% mutate(YEAR = timeTibble$timename[i])
  outDF = rbind(outDF,resout)
}

outDF_formatted = outDF %>%
  `colnames<-`(gsub("geometry.","geo_",(gsub("attributes.","",(tolower(colnames(.))))))) %>%
  rename(rpt_date = report_dat,neighborhood = neighborhood_cluster) %>% 
  mutate_at(vars(rpt_date,start_date,end_date),~as.Date(as.POSIXct(./1000, origin="1970-01-01")))

file1 = outDF_formatted[1:206406,]
file2 = outDF_formatted[206407:nrow(outDF_formatted),]

write_csv(file1,'file1.csv')
write_csv(file2,'file2.csv')



df = outDF_formatted %>% filter(offense=="SEX ABUSE") 
  
base_map = map_data('state')

ggplot(base_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")+
  geom_point(inherit.aes = FALSE, data = df, aes(x = longitude, y = latitude), fill = "red")+
  coord_fixed(xlim = c(-77.1,-76.9), ylim = c(38.8, 39.0))
  coord_sf(, expand = FALSE)
  



