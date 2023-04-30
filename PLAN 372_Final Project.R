library(tidyverse) #load requisite libraries
library(tidycensus) #load requisite libraries
library(sf) #load requisite libraries

building_permits = read_sf("buildingpermits/BuildingPermits.shp") #read in building permits data
blue_line_route = read_sf("LYNX_Blue_Line_Route/LYNX_Blue_Line_Route.shp") #read in blue line of light rail data
gold_line_route = read_sf("LYNX_Gold_Line_Route/LYNX_Gold_Line_Route.shp") #read in gold line of light rail data
blue_line_stops = read_sf("LYNX_Blue_Line_Stations/LYNX_Blue_Line_Stations.shp") #read in blue line stops of light rail data
gold_line_stops = read_sf("LYNX_Gold_Line_Stops/LYNX_Gold_Line_Stops.shp") #read in gold line stops of light rail data

View(building_permits[1:20,]) #view building permits data set
View(railroads[1:20,]) #view railroads data set

ggplot() +
  geom_sf(data=blue_line_route, color="darkblue") +
  geom_sf(data=blue_line_stops, color="blue") #generate initial visualization of blue line route and stops data

ggplot() +
  geom_sf(data=gold_line_route, color="gold") +
  geom_sf(data=gold_line_stops, color="yellow") #generate initial visualization of gold line route and stops data

unique(building_permits$permittype) #understand what unique kinds of building permit types are present in the data set

building_permits_filt = filter(building_permits, permittype == "One/Two Family") #filter building permits data set to only residential permits
building_permits_filt = filter(building_permits_filt, prmtfeetyp == "Construction") #filter building permits data set to only "Construction" projects, excluding "Demolition" projects (as they are irrelevant to this study)
building_permits_filt = filter(building_permits_filt, issue_year >= "2007") #filter building permits data set to only include permits issued 2007 or later because this was the year when construction of the first light rail line (The Lynx Blue Line) in Charlotte, NC was completed and opened

ggplot() +
  geom_sf(data=building_permits_filt) #generate visualization of filtered building permits data set

ggplot() +
  geom_sf(data=building_permits_filt) +
  geom_sf(data=blue_line_stops, color="blue", size=2) +
  geom_sf(data=blue_line_route, color="darkblue", size=2) +
  geom_sf(data=gold_line_stops, color="yellow", size=2) +
  geom_sf(data=gold_line_route, color="gold", size=2) #generate visualization of light rail lines and stops overlaid on the (filtered) residential permit data

silver_line_route = read_sf("LYNX_Silver_Line_Route_Proposed/LYNX_Silver_Line_Route_Proposed.shp") #read in proposed silver line route data
silver_line_stops = read_sf("LYNX_Silver_Line_Stations_Proposed/LYNX_Silver_Line_Stations_Proposed.shp") #read in proposed silver line stops data

ggplot() +
  geom_sf(data=building_permits_filt) +
  geom_sf(data=silver_line_route, color="orange") +
  geom_sf(data=silver_line_stops, color="red") #generate visualization of proposed silver line route and stops data overlaid on the (filtered) residential permit data (colors changed to make route and stops more easily visible)
  
ggplot() +
  geom_sf(data=building_permits_filt) +
  geom_sf(data=blue_line_stops, color="blue", size=2) +
  geom_sf(data=blue_line_route, color="darkblue", size=2) +
  geom_sf(data=gold_line_stops, color="yellow", size=2) +
  geom_sf(data=gold_line_route, color="gold", size=2) +
  geom_sf(data=silver_line_route, color="orange") +
  geom_sf(data=silver_line_stops, color="red") #generate visualization of light rail lines (including proposed silver line) and stops overlaid on the (filtered) residential permit data (colors changed to make route and stops more easily visible)

building_permits_filt = st_transform(building_permits_filt, 32119) #project filtered building permits data to North Carolina projection
blue_line_route = st_transform(blue_line_route, 32119) #project blue line route data to North Carolina projection
blue_line_stops = st_transform(blue_line_stops, 32119) #project blue line stops data to North Carolina projection

blue_line_stops_buffer = st_buffer(blue_line_stops, 1000) #build a 1km buffer around the blue line stops

ggplot() +
  geom_sf(data=blue_line_stops_buffer, color="red") +
  geom_sf(data=building_permits_filt) +
  geom_sf(data=blue_line_stops, color="blue") #map showing filtered residential building permits map with blue line stops and 1km buffers

intersects = st_intersects(building_permits_filt, blue_line_stops_buffer) #identify which residential building permits are within 1 kilometer of a blue rail stop

building_permits_filt$blue_intersects = apply(intersects, 1, any) #new column that returns TRUE if permit intersects any part of blue line buffer layer

ggplot() +
  geom_bar(data = building_permits_filt, aes(x = blue_intersects)) #plot that displays how many residential building permits are within 1km of a blue line stop
  
gold_line_route = st_transform(gold_line_route, 32119) #project gold line route data to North Carolina projection
gold_line_stops = st_transform(gold_line_stops, 32119) #project gold line stops data to North Carolina projection

gold_line_stops_buffer = st_buffer(gold_line_stops, 1000) #bulid a 1km buffer around the gold line stops

intersects_gold = st_intersects(building_permits_filt, gold_line_stops_buffer) #identify which residential building permits are within 1 kilometer of a gold line rail stop

building_permits_filt$gold_intersects = apply(intersects_gold, 1, any) #new column that returns TRUE if permit intersects any part of gold line buffer layer

ggplot() +
  geom_bar(data=building_permits_filt, aes(x=gold_intersects)) #plot that displays how many residential building permits are within 1km of a gold line stop
