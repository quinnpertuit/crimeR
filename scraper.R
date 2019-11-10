setwd("/Users/quinnx/Documents/GitHub/crimeR/crimeR")
library(tidyverse)
library(lubridate)
library(jsonlite)
library(magrittr)
library(httr)
library(maps)
library(ggmap)
library(geojsonio)
library(sf)
library(maptools)
library(rgdal)
library(tigris)

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

outDF_formatted = rbind(
  read.csv('file1.csv',stringsAsFactors = F),
  read.csv('file2.csv',stringsAsFactors = F)
)


sts <- c("DC", "MD", "VA") 
combined <- rbind_tigris(
  lapply(sts, function(x) {
    tracts(x, cb = FALSE) #TRUE)
  })
)

plot(combined %>% filter())+

dcroads = roads("DC", 1)
dctracts = tracts("DC", 1)
dcblocks = blocks("DC", 1)
dcplaces = places("DC", 1)
dccounties = counties("DC", 1)
dcblockgroups = block_groups("DC", 1)
dcpumas = pumas("DC", 1)
#dcschooldistricts = school_districts("DC", 1)
dcstates = states("DC", resolution = "500k")
dczctas = zctas("DC", 1)
#dcuas <- urban_areas("DC")
dcwater = area_water("DC",1)
dclandmarks = landmarks("DC")
divisions = divisions("DC", resolution = "500k")
dcmetro_divisions = metro_divisions("DC")
dcrails = rails("DC")
dcregions = regions("DC")
dcstatelegislativedistricts = state_legislative_districts("DC")
dcvotingdistricts = voting_districts("DC")

## DC Boundary
dcboundary = counties("DC", cb = TRUE) 
dcwards = state_legislative_districts("DC", cb = TRUE)
dcwater = area_water("DC", cb = TRUE)
dcroads = roads("DC", cb = TRUE)

#chi_joined = geo_join(chi_tracts, values, by = "GEOID")


plot(dcboundary)

ggplot()+
plot(dcwards, col=factor(dcwards$NAMELSAD), add = TRUE, alpha = 0.4)
plot(dcwater, col='blue', add = TRUE)
plot(dcroads, add = TRUE)

df = outDF_formatted %>% filter(offense=="SEX ABUSE") 
dc = get_map(location = 'DC', zoom = 12)


ggmap(dc)

dc_basemap <- ggplot() +
  ggmap(dcroads)
  geom_sf(data = dcroads, fill = "#d4dddc", color = NA, alpha = 0.2) +
  geom_sf(data = dc_roads_sf, color = street_yellow, alpha = 0.5) +
  geom_sf(data = dc_boundary_sf, fill = NA, color = "#909695") +
  geom_sf(data = dc_water_sf, fill = "#cbdeef", color = "#9bbddd") +
  map_theme + # the theme I created
  labs(title = "DC Basemap")


plot(dcroads)

ggplot() +
  geom_sf(data = dc, fill = "#d4dddc", color = NA, alpha = 0.2)
  
gg <- ggplot()
gg <- gg + geom_map(data=combined, map=combined,
                    aes(x=long, y=lat, map_id=id),
                    color="black", fill="white", size=0.25)
gg <- gg + coord_map()
gg <- gg + theme_map()
gg

######################################################
######################################################
######################################################

zip=readShapeSpatial( "tl_2010_11001_tract10.shp" )



df = outDF_formatted %>% filter(offense=="SEX ABUSE") 

dc = get_map(location = 'DC', zoom = 12)

dc_boundaries = readOGR("Washington_DC_Boundary.shp")

# dc wards from http://opendata.dc.gov/datasets/ward-from-2012
dc_wards_sf <- fromJSON("https://opendata.arcgis.com/datasets/0ef47379cbae44e88267c01eaec2ff6e_31.geojson")

# major roads from http://opendata.dc.gov/datasets/major-roads/geoservice
dc_roads_sf <- fromJSON("https://opendata.arcgis.com/datasets/3031b2c72a4942bb86356e921e73e8c3_38.geojson")

# national parks from http://opendata.dc.gov/datasets/national-parks
dc_nat_parks_sf <- fromJSON("https://opendata.arcgis.com/datasets/14eb1c6b576940c7b876ebafb227febe_10.geojson")

# dc boundary line from http://opendata.dc.gov/datasets/washington-dc-boundary
dc_boundary_sf <- Washington_DC_Boundary.shp

map_theme <-
  theme(
    panel.grid.major = element_line(colour = "transparent"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    legend.justification = c(0, 0),
    title  = element_text(family = "Bradley Hand ITC", size = 15),
    legend.title = element_text(family = "Century Gothic", size = 9),
    legend.text = element_text(family = "Century Gothic", size = 8),
    legend.background = element_blank(),
    legend.position="none", # legend is not really necessary
    plot.caption = element_text(family = "Century Gothic", size = 8, hjust = 0, color = "gray60"),
    plot.subtitle = element_text(family = "Century Gothic", size = 8, vjust = -2, color = "gray60"),
    plot.background = element_rect(color = "#d4dddc")
  )

dc_basemap <- ggplot() +
  geom_sf(data = dc_wards_sf, fill = "#d4dddc", color = NA, alpha = 0.2) +
  geom_sf(data = dc_roads_sf, color = street_yellow, alpha = 0.5) +
  geom_sf(data = dc_boundary_sf, fill = NA, color = "#909695") +
  geom_sf(data = dc_water_sf, fill = "#cbdeef", color = "#9bbddd") +
  map_theme + # the theme I created
  labs(title = "DC Basemap")