setwd("/Users/quinnx/Documents/GitHub/crimeR")
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
library(ggthemes)


timeTibble = tibble(timename = c('Last 30 Days',c(2008:2020)),
                    timenum=c(8,32,33,34,35,11,10,9,27,26,38,0,1,2))

timeTibble = timeTibble %>% filter(timename %in% c('Last 30 Days',2020))
getCrimes = function(url){
  json = fromJSON(url, flatten = TRUE)
  restemp = json$features %>% as_tibble()
  return(restemp)
}

outList = list()
for (i in 1:nrow(timeTibble)){
  print(i)
  ids = fromJSON(paste0("http://maps2.dcgis.dc.gov/dcgis/rest/services/FEEDS/MPD/MapServer/",timeTibble$timenum[i],"/query?where=1%3D1&outFields=*&outSR=4326&f=json&returnIdsOnly=TRUE")) %>% .$objectIds %>% as.character()
  
  id_seq = tibble(
    start=seq(from=1,to=ceiling(length(ids)/100)*100,by=100),
    end=c(seq(from=100,to=length(ids),by=100),length(ids)))
  
  resList = list()
  for (seq_i in 1:nrow(id_seq)){
  
    print(paste("Scraping:",timeTibble$timename[i],"- at",seq_i,"of",nrow(id_seq)))
    ids_string = str_c(ids[id_seq$start[seq_i]:id_seq$end[seq_i]], collapse = ",")
    Sys.sleep(3)
    url = paste0("http://maps2.dcgis.dc.gov/dcgis/rest/services/FEEDS/MPD/MapServer/",timeTibble$timenum[i],"/query?where=1%3D1&objectIds=",ids_string,"&outFields=*&outSR=4326&f=json")
    
    restemp = tryCatch(getCrimes(url) %>% mutate(timename=timeTibble$timename[i],timenum=timeTibble$timenum[i],seq_i=seq_i),error=function(err){tibble(timename=timeTibble$timename[i],timenum=timeTibble$timenum[i],seq_i=seq_i)})

    resList[[seq_i]]=restemp
  }
  
  outList[[i]] = resList %>% bind_rows() 
}
saveRDS(outList,paste0(format(Sys.time(),'%Y%m%d %H%M%S'),'crimes_outList_2020and30days_20200224.RData'))
outList1 = readRDS('20200126 092841crimes_outList_20200124.RData')
outList2 = readRDS('20200225 203945crimes_outList_2020and30days_20200224.RData')


outCrimeDF =  bind_rows(outList) %>%
  `colnames<-`(gsub("geometry.","geo_",(gsub("attributes.","",(tolower(colnames(.))))))) %>%
  rename(rpt_date = report_dat,neighborhood = neighborhood_cluster) %>% 
  mutate_at(vars(rpt_date,start_date,end_date),~as.Date(as.POSIXct(./1000, origin="1970-01-01")))


plot1 = outCrimeDF %>%
  filter(offense=="MOTOR VEHICLE THEFT") %>% 
  mutate(rpt_month = )
  group_by(rpt_date) %>% 
    as.month
  summarise(n=n())

ggplot(plot1) +
  geom_line(aes(x=rpt_date, y=n)) +
  theme(axis.title=element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%W")



write_csv(outCrimeDF,paste(format(Sys.time(),'%Y-%m-%d %H:%M:%S'),'outCrimeDF.csv'))

write_csv(outCrimeDF %>% mutate(rpt_year = year(rpt_date)) %>% filter(rpt_year %in% c(2008:2012)),paste(format(Sys.time(),'%Y-%m-%d %H:%M:%S'),'outCrimeDF 2008-2012.csv'))
write_csv(outCrimeDF %>% mutate(rpt_year = year(rpt_date)) %>% filter(rpt_year %in% c(2013:2017)),paste(format(Sys.time(),'%Y-%m-%d %H:%M:%S'),'outCrimeDF 2013-2017.csv'))
write_csv(outCrimeDF %>% mutate(rpt_year = year(rpt_date)) %>% filter(rpt_year %in% c(2018:2020)),paste(format(Sys.time(),'%Y-%m-%d %H:%M:%S'),'outCrimeDF 2018-2020.csv'))
write_csv(outCrimeDF %>% mutate(rpt_year = year(rpt_date)) %>% filter(is.na(rpt_year)),paste(format(Sys.time(),'%Y-%m-%d %H:%M:%S'),'outCrimeDF Year NA.csv'))

outDF =  lapply(outList %>% `names<-`(c('Last 30 Days',c(2008:2020))),bind_rows)

out201

outDF_formatted = outDF

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
dcroadsraw = roads("DC", "District of Columbia")
dcroads = primary_secondary_roads("DC")
roads_map <- fortify(dcroadsraw)
wards_map <- fortify(dcwards)
water_map <- fortify(dcwater)
boundary_map <- fortify(dcboundary)


gg <- ggplot()
gg <- gg + geom_map(data=wards_map, map=wards_map,
                    aes(x=long, y=lat, map_id=id), color=NA, fill=wards_map$group, size=0.25, alpha=0.3)+
  scale_color_manual(values = c("0.1"="red","1.1"="orange","2.1"="yellow","3.1"="green","4.1"="blue","5.1"="purple","6.1"="pink","7.1"="grey"))
gg <- gg + geom_map(data=water_map, map=water_map,
                    aes(x=long, y=lat, map_id=id),fill="darkblue",color=NA) 
gg <- gg + geom_map(data=roads_map, map=roads_map,
                    aes(x=long, y=lat, map_id=id),
                    color="black", fill=NA, size=0.25) 
gg <- gg + coord_map()
gg <- gg + theme_map()
gg
ggsave("dc map test v1 20191110.png",width=6,height=6,units="in")

###
mapdata = bind_rows(wards_map %>% mutate(m='wards'), water_map %>% mutate(m='water'), roads_map %>% mutate(m='roads')) 
crimes = outDF_formatted %>% filter(offense=='HOMICIDE') %>% 
  mutate(year = ifelse(year=='Last 30 Days','2019',year))


ggplot() + 
geom_map(data=mapdata %>% filter(m=='wards'), map=mapdata %>% filter(m=='wards'),
                    aes(x=long, y=lat, map_id=id, fill= group), color=NA, size=0.25, alpha=0.3)+
geom_map(data=mapdata %>% filter(m=='water'), map=mapdata %>% filter(m=='water'),
                    aes(x=long, y=lat, map_id=id),fill="darkblue",color=NA)+
# geom_map(data=mapdata %>% filter(m=='roads'), map=mapdata %>% filter(m=='roads'),
#                     aes(x=long, y=lat, map_id=id),
#                     color="black", fill=NA, size=0.25) +
  geom_point(data = crimes,
             mapping = aes(x = longitude, y = latitude),
             color="red", size=0.1)+ coord_map()+ theme_map() + facet_wrap(~year, ncol=4) +
  theme(legend.position="none") 


#
st_map <- mapdata

set.seed(2017-12-19)
data_frame(
  lat = sample(st_map$lat, 30),
  lng = sample(st_map$long, 30),
  level = rep(c("A", "B", "C"), 10)
) -> points_df

ggplot() +
  geom_path(data=st_map, aes(long, lat, group=group), size=0.25) + gg + # basemap
  geom_point(data=crimes, aes(longitude, latitude, color="red")) +         # add our points layer
  coord_map("polyconic") +                                         # for funsies
  facet_wrap(~year) +                                             # NEVER use a fully qualified column unless you know what you're doing
  labs(x=NULL, y=NULL) +
  theme(axis.text=element_blank()) +
  theme(legend.position="none")
###



plot(dcboundary)

ggplot()+
plot(dcwards, col=factor(dcwards$NAMELSAD), add = TRUE, alpha = 0.4)
plot(dcwater, col='blue', add = TRUE)
plot(dcroads, add = TRUE)

df = outDF_formatted %>% filter(offense=="SEX ABUSE") 
dc = get_map(location = 'DC', zoom = 12)

#################
#https://s3.amazonaws.com/assets.datacamp.com/production/course_6839/slides/chapter4.pdf

options(tigris_class = "sf")
dc_roads <- roads("DC"
                  ,
                  "District of Columbia") %>%
  filter(RTTYP %in% c("I"
                      ,
                      "S"
                      ,
                      "U"))
dc_water <- area_water("DC"
                       ,
                       "District of Columbia")
dc_boundary <- counties("DC"
                        , cb = TRUE)

plot(dc_water$geometry, col = "lightblue")

ggplot() +
  geom_sf(data = dc_boundary, color = NA, fill = "white") +
  geom_sf(data = dc_dots, aes(color = group, fill = group), size = 0.1) +
  geom_sf(data = dc_water, color = "lightblue"
          , fill = "lightblue") +
  geom_sf(data = dc_roads, color = "grey") +
  coord_sf(crs = 26918, datum = NA) +
  scale_color_brewer(palette = "Set1"
                     , guide = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The racial geography of Washington, DC"
       ,
       subtitle = "2010 decennial U.S. Census"
       ,
       fill = ""
       ,
       caption = "1 dot = approximately 100 people.\nData acquired with
the R tidycensus and tigris packages.")



################
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
  map_theme 
setwd("C:/Users/e321843/Documents/GitHub/crimeR")
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
library(raster)

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

outDF_formatted = rbind(read_csv('file1.csv'),read_csv('file2.csv'))

df = outDF_formatted %>% filter(offense=="SEX ABUSE") 

dc_crimelimits = c(xmin=min(outDF_formatted$latitude),
                   xmax=max(outDF_formatted$latitude),
                   ymin=min(outDF_formatted$longitude),
                   ymax=max(outDF_formatted$longitude))
  
###################################

# get MD boundaries - USA boundaries apckage
md.state<-us_states(resolution = 'high', states='washington dc')

###################################
map.dc <- get_map("Washington DC", zoom = 12)
ggmap(map.dc)+
  map_theme+
  geom_point(data = df,
             mapping = aes(x = longitude, y = latitude),
             color="black", size=1, alpha = 0.1)+
  geom_point(data = df,
             mapping = aes(x = longitude, y = latitude),
             fill='cyan', size=1)

ldn = readOGR(dsn='DC Census Tracts.shp', 'DC Census Tracts' )

####################################
usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "red") + 
  coord_fixed(1.3)

states <- map_data("state")

dmv <- subset(states, region %in% c("washington dc","maryland","virginia"))

ggplot(data = dmv) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  scale_x_continuous(limits = dc_crimelimits['xmin'], dc_crimelimits['xmax'])+
  scale_y_continuous(limits = dc_crimelimits['ymin'], dc_crimelimits['ymax'])

+
  guides(fill=FALSE)  # do this to leave off the color legend


dc_state <- subset(states, region == "washington dc")


ca_base <- ggplot(data = dc_state, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ca_base + theme_nothing()

#######################################
dc = get_map(location = 'DC', zoom = 12)

nhbds = read_csv("DC Census Tracts.csv")

ggmap(dc) + 
  geom_point(aes(x = longitude, y = latitude), 
               data = df,
               alpha = 0.8, 
               color = "black",
               size = 0.2)

# courtesy R Lovelace
ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}

dc <- get_map(location = 'DC', zoom = 12) 
dc.rast <- ggmap_rast(map = dc) # convert google map to raster object
nhbds <- readOGR("DC Census Tracts", "DC Census Tracts") # use rgdal to preserve projection
dc.only <- mask(dc.rast, nhbds) # clip to bounds of census tracts

# prep raster as a data frame for printing with ggplot
dc.df <- data.frame(rasterToPoints(dc.only))
ggplot(dc.df) + 
  geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255))) + 
  scale_color_identity()



###################################

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
  map_theme + 
  labs(title = "DC Basemap")
  
  
  
  
  library("ggspatial")
  ggplot(data = world) +
    geom_sf() +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))