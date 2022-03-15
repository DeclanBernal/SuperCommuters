##Final Project

#Load Packages
library(sf)
library(tidyverse)
library(tmap)
library(tidycensus)
library(tigris)
library(dplyr)
library(viridis)

#Setup
setwd("C:/Users/Declan Bernal/Desktop/SCU/Winter 22/ENVS 117/Final Project (1)")
Sys.getenv("CENSUS_API_KEY")
v19 <- load_variables(2019, "acs5", cache = TRUE)
TravelTime <- get_acs(geography = "tract",
                          year = 2019,
                          table = "B08303",
                          cache_table = TRUE,
                          state = "CA",
                          county = c("San Francisco", "San Mateo", "Santa Cruz", "Santa Clara", 
                                     "Merced", "Stanislaus", "San Joaquin", "Sacramento", "Solano", 
                                     "Sonoma", "Napa", "Marin", "Contra Costa", "Alameda"), 
                          output = "wide",
                          geometry = TRUE)
TravelTime <- st_transform(TravelTime, crs = 2227)
v09 <- load_variables(2009, "acs5", cache = TRUE)
TravelTimeOld <- get_acs(geography = "tract",
                      year = 2009,
                      table = "B08303",
                      cache_table = TRUE,
                      state = "CA",
                      county = c("San Francisco", "San Mateo", "Santa Cruz", "Santa Clara", 
                                 "Merced", "Stanislaus", "San Joaquin", "Sacramento", "Solano", 
                                 "Sonoma", "Napa", "Marin", "Contra Costa", "Alameda"), 
                      output = "wide",
                      geometry = TRUE)
TravelTimeOld <- st_transform(TravelTimeOld, crs = 2227)

#Data Management
Over90 <- TravelTime %>%
  dplyr::select(1,2,3,4,27,28,29) %>%
  rename(Total = B08303_001E,
         TotalMOE = B08303_001M,
         NinetyPlusMin = B08303_013E,
         NinetyPlusMinMOE = B08303_013M) %>%
  mutate(NinetyPlusPCT = (NinetyPlusMin/Total)*100)
Over90 <- Over90 %>% filter(!st_is_empty(.))

Over90Old <- TravelTimeOld %>%
  dplyr::select(1,2,3,4,5,28,29) %>%
  rename(Total = B08303_001E,
         TotalMOE = B08303_001M,
         NinetyPlusMin = B08303_013E,
         NinetyPlusMinMOE = B08303_013M) %>%
  mutate(NinetyPlusPCT = (NinetyPlusMin/Total)*100)
Over90Old <- Over90Old %>% filter(!st_is_empty(.))

Over90$Year <- 2019
Over90Old$Year <- 2009
Over90Total <- rbind(Over90, Over90Old) #Combining the 2019 and 2009 datasets to be able to overlay them on the histogram below

LongestCommutes <- dplyr::filter(Over90, NinetyPlusMin > NinetyPlusMinMOE)
LongestCommutes <- dplyr::filter(LongestCommutes, NinetyPlusPCT > 20)
LCB <- st_union(LongestCommutes)
LCB2 <- st_buffer(LCB, dist = 10000)
LC_Buffer <- st_difference(LCB2, LCB)
rm(LCB, LCB2)

LongestCommutesOld <- dplyr::filter(Over90Old, NinetyPlusMin > NinetyPlusMinMOE)
LongestCommutesOld <- dplyr::filter(LongestCommutesOld, NinetyPlusPCT > 20)
LCOB <- st_union(LongestCommutesOld)
LCOB2 <- st_buffer (LCOB, dist = 10000)
LCO_Buffer <- st_difference(LCOB2, LCOB)
rm(LCOB, LCOB2)

CountyCommutes <- st_read("polygon_2019co.shp") #LEHD Data obtained at https://onthemap.ces.census.gov/
sum(CountyCommutes$s000)
CountyCommutes <- CountyCommutes %>%
  dplyr::select(1,2,3,14) %>%
  rename(ID = id,
         County = label,
         Number = s000) %>%
  mutate(Pct = (Number/478163)*100)
CountyCommutes <- st_transform(CountyCommutes, crs = 2227)
CountyCommutes$County <- gsub(" County, CA","", CountyCommutes$County)
CountyCommutes$Pct <- format(round(CountyCommutes$Pct,2), nsmall=2)
CountyCommutes <- CountyCommutes[-c(9),] #LA County (only 2%) removed, otherwise the map is borderline unreadable. Will mention in final analysis
CountyCommutes$Pct <- as.numeric(CountyCommutes$Pct)

HighestCounty <- CountyCommutes %>%
  filter(County == "Contra Costa")

#Mapping
tmap_mode("view")
tm_shape(Over90Old) +
  tm_fill("NinetyPlusPCT", palette = "-inferno", style = "cont", showNA = F, popup.vars = c("% Super Commuters = "="NinetyPlusPCT")) +
  tm_borders(col = "gray5", lwd = 0.3) +
  tm_shape(LCO_Buffer) +
  tm_fill(col = "white", alpha = 0.75)

tm_shape(Over90) +
  tm_fill("NinetyPlusPCT", palette = "-inferno", style = "cont", showNA = F, popup.vars = c("% Super Commuters = "="NinetyPlusPCT")) +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_borders(col = "gray5", lwd = 0.3, alpha = 0.5) +
  tm_shape(LC_Buffer) +
  tm_fill(col = "white", alpha = 0.75)

ggplot(Over90Total, aes(NinetyPlusPCT)) +
  geom_histogram(data = subset(Over90Total, Year == 2019), fill = "red", alpha = 0.5, color = "gray5", lwd = 1) +
  geom_histogram(data = subset(Over90Total, Year == 2009), fill = "orange", alpha = 0.5, color= "gray5", lwd = 1) +
  theme_classic() +
  labs(title = "Bay Area Super Commuters By Census Tract, 2009 vs. 2019", x = "Grouping of Tracts by Percentage of Residents with Commute Times over 90 Min. (Orange = 2009, Red = 2019)", y = "Count") +
  theme(line = element_blank(), plot.title = element_text(hjust = 0.5))

palette1 <- c("#fcffa4", "#f9e46e", "#fbbe22", "#f3771a", "#d34743", "#63156e") #Custom color palette since inferno's scale doesn't work well with the map below

tm_shape(CountyCommutes) +
  tm_fill("Pct", palette = palette1, style = "fixed", breaks = c(0,2,5,10,15,25,50)) +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_borders(col = "gray5", lwd = 0.3) +
  tm_text("County", ymod = 1) +
  tm_shape(CountyCommutes) +
  tm_text("Pct", ymod = -1) +
  tm_shape(HighestCounty) +
  tm_borders(col = "gray5", lwd = 3)

ggplot(CountyCommutes, aes(x = reorder(County, -Pct), y = Pct)) + 
  geom_col(fill="#ff923f", alpha = 1, color = "gray5", lwd = 0.5) + 
  theme_classic() +
  labs(x="County of Work Destination", y="Percentage", title="Place of Work for Contra Costa County Residents") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,45)) +
  geom_text(aes(label = Pct), vjust = -0.5) +
  theme(line = element_blank(), plot.title = element_text(hjust = 0.5))