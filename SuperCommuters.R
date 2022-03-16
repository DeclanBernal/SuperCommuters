###Final Project


##Load Packages
library(sf)
library(tidyverse)
library(tmap)
library(tidycensus)
library(tigris)
library(dplyr)
library(viridis)


##Setup
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


##Data Management
#2019 ACS Tract Data
Over90 <- TravelTime %>%
  dplyr::select(1,2,3,4,27,28,29) %>%
  rename(Total = B08303_001E,
         TotalMOE = B08303_001M,
         NinetyPlusMin = B08303_013E,
         NinetyPlusMinMOE = B08303_013M) %>%
  mutate(NinetyPlusPCT = (NinetyPlusMin/Total)*100)
Over90 <- Over90 %>% filter(!st_is_empty(.))

#2009 ACS Tract Data
Over90Old <- TravelTimeOld %>%
  dplyr::select(1,2,3,4,5,28,29) %>%
  rename(Total = B08303_001E,
         TotalMOE = B08303_001M,
         NinetyPlusMin = B08303_013E,
         NinetyPlusMinMOE = B08303_013M) %>%
  mutate(NinetyPlusPCT = (NinetyPlusMin/Total)*100)
Over90Old <- Over90Old %>% filter(!st_is_empty(.))

#Combining the 2019 and 2009 datasets to be able to overlay them on the histogram below
Over90$Year <- 2019
Over90Old$Year <- 2009
Over90Total <- rbind(Over90, Over90Old)

#Mountain House Data
MHData <- TravelTime %>%
  dplyr::select(1,2,3,5,7,9,11,13,15,17,19,21,23,25,27,29) %>%
  rename(Total = B08303_001E,
         Under5 = B08303_002E,
         Fiveto9 = B08303_003E,
         Tento14 = B08303_004E,
         Fifteento19 = B08303_005E,
         Twentyto24 = B08303_006E,
         TwFiveto29 = B08303_007E,
         Thirtyto34 = B08303_008E,
         ThFiveto39 = B08303_009E,
         Fortyto44 = B08303_010E,
         FrFiveto59 = B08303_011E,
         Sixtyto89 = B08303_012E,
         NinetyPlus = B08303_013E) %>%
  mutate(">5" = (Under5/Total)*100,
         "05-09" = (Fiveto9/Total)*100,
         "10-14" = (Tento14/Total)*100,
         "15-19" = (Fifteento19/Total)*100,
         "20-24" = (Twentyto24/Total)*100,
         "25-29" = (TwFiveto29/Total)*100,
         "30-34" = (Thirtyto34/Total)*100,
         "35-39" = (ThFiveto39/Total)*100,
         "40-44" = (Fortyto44/Total)*100,
         "45-59" = (FrFiveto59/Total)*100,
         "60-89" = (Sixtyto89/Total)*100,
         "90+" = (NinetyPlus/Total)*100) %>%
  filter(NAME == "Census Tract 52.06, San Joaquin County, California")

MHData <- dplyr::select(MHData, 17:28)
MHDataTidy <- pivot_longer(MHData, cols=1:12, names_to = "Time", values_to = "Pct")
MHDataTidy$Pct <- format(round(MHDataTidy$Pct,2), nsmall=2)
MHDataTidy$Pct <- as.numeric(MHDataTidy$Pct)
rm(MHData)

#Identifying Tracts of Note
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

#LEHD County Data
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
CountyCommutes <- CountyCommutes[-c(9),] #LA County (only 2%) removed, otherwise the map is borderline unreadable (mentioned in final analysis)
CountyCommutes$Pct <- as.numeric(CountyCommutes$Pct)

#LEHD Tract Data
MHCommutes <- st_read("polygon_2019mh.shp")
sum(MHCommutes$s000)
MHCommutes <- MHCommutes %>%
  dplyr::select(1,2,3,14) %>%
  rename(ID = id,
         County = label,
         Number = s000) %>%
  mutate(Pct = (Number/4745)*100)
MHCommutes <- st_transform(MHCommutes, crs = 2227)
MHCommutes$County <- gsub(" County, CA","", MHCommutes$County)
MHCommutes$Pct <- format(round(MHCommutes$Pct,2), nsmall=2)
MHCommutes <- MHCommutes[-c(10),] #Again, removing LA County (1.7%)
MHCommutes$Pct <- as.numeric(MHCommutes$Pct)

#Cleanup and Prep for Visualization
TractGeo <- st_union(Over90)
CountyCommutesCrop <- st_intersection(CountyCommutes, TractGeo)

HighestCounty <- CountyCommutesCrop %>%
  filter(County == "Contra Costa")

MHCommutesCrop <- st_intersection(MHCommutes, TractGeo)

options(tigris_use_cache = TRUE)
CAPlaces <- places(state = 06)
CAPlaces <- st_transform(CAPlaces, crs = 2227)
KeyPlaces <- filter(CAPlaces, NAME == "San Jose" | NAME == "San Francisco" | NAME == "Santa Cruz" | NAME == "Merced" | 
                      NAME == "Modesto" | NAME == "Stockton" | NAME == "Sacramento" | NAME == "Vallejo" | NAME == "Santa Rosa" | 
                      NAME == "Gilroy" | NAME == "Vacaville" | NAME == "Berkeley" | NAME == "San Mateo")
MH <- filter(CAPlaces, NAME == "Mountain House")
BayAreaPlaces <- KeyPlaces %>%
  dplyr::select(4, 5, 17)
MountainHouse <- MH %>%
  dplyr::select(4, 5, 17)
rm(CAPlaces, KeyPlaces, MH)


##Mapping
#Tract Level
tmap_mode("view")
tm_shape(Over90Old) +
  tm_fill("NinetyPlusPCT", palette = "-inferno", alpha = 0.9, style = "cont", showNA = F, popup.vars = c("% Super Commuters = "="NinetyPlusPCT")) +
  tm_borders(col = "gray5", lwd = 0.3, alpha = 0.5) +
  tm_basemap(server = "CartoDB.PositronNoLabels") +
  tm_shape(BayAreaPlaces) +
  tm_text(text = "NAME", size = 1, auto.placement = FALSE, ymod = 1, shadow = TRUE, bg.color = "white", bg.alpha = 0.25) +
  tm_shape(LCO_Buffer) +
  tm_fill(col = "white", alpha = 0.75)

tm_shape(Over90) +
  tm_fill("NinetyPlusPCT", palette = "-inferno", alpha = 0.9, style = "cont", showNA = F, popup.vars = c("% Super Commuters = "="NinetyPlusPCT")) +
  tm_borders(col = "gray5", lwd = 0.3, alpha = 0.5) +
  tm_basemap(server = "CartoDB.PositronNoLabels") +
  tm_shape(BayAreaPlaces) +
  tm_text(text = "NAME", size = 1, auto.placement = FALSE, ymod = 1, shadow = TRUE, bg.color = "white", bg.alpha = 0.25) +
  tm_shape(LC_Buffer) +
  tm_fill(col = "white", alpha = 0.75)

ggplot(Over90Total, aes(NinetyPlusPCT)) +
  geom_histogram(data = subset(Over90Total, Year == 2019), fill = "red", alpha = 0.5, color = "gray5", lwd = 1) +
  geom_histogram(data = subset(Over90Total, Year == 2009), fill = "orange", alpha = 0.5, color= "gray5", lwd = 1) +
  theme_classic() +
  labs(title = "Bay Area Super Commuters By Census Tract, 2009 vs. 2019", x = "Grouping of Tracts by Percentage of Residents with Commute Times over 90 Min. (Orange = 2009, Red = 2019)", y = "Count") +
  theme(line = element_blank(), plot.title = element_text(hjust = 0.5))

#County Level
palette1 <- c("#fcffa4", "#f9e46e", "#fbbe22", "#f3771a", "#d34743", "#962765") #Custom color palette since inferno's scale doesn't work well with the map below

tmap_options(check.and.fix = T)
tm_shape(CountyCommutesCrop) +
  tm_fill("Pct", palette = palette1, alpha = 0.85, style = "fixed", breaks = c(0,2,5,10,15,25,50)) +
  tm_borders(col = "gray5", lwd = 0.3) +
  tm_text("County", ymod = 1) +
  tm_basemap(server = "CartoDB.PositronNoLabels") +
  tm_shape(CountyCommutes) +
  tm_text("Pct", ymod = -1) +
  tm_shape(HighestCounty) +
  tm_borders(col = "#63156e", lwd = 3)

ggplot(CountyCommutes, aes(x = reorder(County, -Pct), y = Pct)) + 
  geom_col(fill = "#ff923f", alpha = 1, color = "gray5", lwd = 0.5) + 
  theme_classic() +
  labs(x = "County of Work Destination", y = "Percentage", title = "County of Place of Work for Contra Costa  Residents") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,45)) +
  geom_text(aes(label = Pct), vjust = -0.5) +
  theme(line = element_blank(), plot.title = element_text(hjust = 0.5))

#Mountain House
tm_shape(MountainHouse) +
  tm_borders(col = "gray5", lwd = 2, alpha = 0.5) +
  tm_basemap(server = "CartoDB.Positron")

tm_shape(MHCommutesCrop) +
  tm_fill("Pct", palette = palette1, alpha = 0.85, style = "fixed", breaks = c(0,2,5,10,15,25,50)) +
  tm_borders(col = "gray5", lwd = 0.3) +
  tm_text("County", ymod = 1) +
  tm_basemap(server = "CartoDB.PositronNoLabels") +
  tm_shape(MHCommutes) +
  tm_text("Pct", ymod = -1) +
  tm_shape(MountainHouse) +
  tm_fill(col = "white") +
  tm_borders(col = "#63156e", lwd = 3) +
  tm_text("NAME", ymod = -1.5, col = "white")

ggplot(MHCommutes, aes(x = reorder(County, -Pct), y = Pct)) + 
  geom_col(fill = "#ff923f", alpha = 1, color = "gray5", lwd = 0.5) + 
  theme_classic() +
  labs(x = "County of Work Destination", y = "Percentage", title = "County of Place of Work for Mountain House Residents") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,45)) +
  geom_text(aes(label = Pct), vjust = -0.5) +
  theme(line = element_blank(), plot.title = element_text(hjust = 0.5))

ggplot(MHDataTidy, aes(x = Time, y = Pct)) + 
  geom_col(fill = "#ff923f", alpha = 1, color = "gray5", lwd = 0.5) + 
  theme_classic() +
  labs(x = "Average Commute Time (Minutes)", y = "Percentage", title = "Average Commute Times Among Mountain House Residents") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,30)) +
  geom_text(aes(label = Pct), vjust = -0.5) +
  theme(line = element_blank(), plot.title = element_text(hjust = 0.5))