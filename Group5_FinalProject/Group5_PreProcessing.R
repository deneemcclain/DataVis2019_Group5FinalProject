# Data Visualization
# Fall 2019
# Group 5 Final Project - Preprocessing Code

setwd("C:/Users/denee/Documents/University of Notre Dame Data Science/Fall 2019/Data Visualization/Assignments/Group5_FinalProject")
# Load libraries
library(dplyr)
library(rgdal)
library(stringr)
library(ggmap)
# Geocode register
register_google(key = "AIzaSyAe2Brb2eyF0ZeyEncpk_36JQkC-o9Xyvg")

## Street Lights & Abandoned Properties Tab ##
# load abandoned buildings data
abandoned <- readOGR(dsn="Abandoned_Property_Parcels", 
                     layer = "Abandoned_Property_Parcels", stringsAsFactors = FALSE)
buildings <- abandoned@data

######### CODE TO REVERSE GEOCODE STREET LIGHTS DATA ###########
# lights <- read.csv("Street_Lights.txt")
# lights$latlon <- paste(lights$Lon, lights$Lat, sep = " ")
# zips <- vector(mode = "character", length = nrow(lights))
# for (i in 1:nrow(lights)) {
#   light <- lights[i, ]
#   latlon_s <- as.numeric(str_split_fixed(light['latlon'], " ", n = 2))
#   zips[i] <- str_extract(revgeocode(latlon_s), "(?!^)([0-9]){5}")
# }
# lights$zipcodes <- zips
######### CODE TO REVERSE GEOCODE STREET LIGHTS DATA ###########

load("lights_revgeocoded.RData") #- necessary on reruns because above section runs long

# select necessary columns
lights <- lights %>%
  select(OBJECTID, Pole_Num_1, Bulb_Type, Wattage, Pole_Type, zipcodes) %>% 
  na.omit()
# clean data
lights$Bulb_Type <- str_trim(lights$Bulb_Type)
lights$Bulb_Type <- 
  str_replace_all(lights$Bulb_Type,
                  "Yellow-H.P. Sodium|Yellow-H.P Sodium|Yellow- H.P. Sodium|Yellow- H.P. Spdium|Yellow H.P Sodium",
                  "Yellow - H.P. Sodium")
lights$Bulb_Type <- 
  str_replace_all(lights$Bulb_Type,
                  "H.p. Sodium",
                  "H.P. Sodium")
lights$Bulb_Type <- 
  str_replace_all(lights$Bulb_Type,
                  "HPs",
                  "HPS")
lights$Wattage <- str_replace_all(lights$Wattage,
                                          "[^0-9\\-]",
                                          "")

save(lights, file = "lights_clean.RData")
save(buildings, file = "buildings_clean.RData")
## Street Lights & Abandoned Properties Tab ##

## Business Info Tab ##
# Read in initial Business Data file
Businesses <- read_csv("FinalProject Files/Business_Licenses.csv")

# Geocode Business Data

# Set up google API key:
register_google(key = "AIzaSyAe2Brb2eyF0ZeyEncpk_36JQkC-o9Xyvg")

# Create table of geocoded data
coded_data <- geocode(Businesses$Full_Address, output = "more")

# Add columns to data frame
Businesses$lon <- coded_data$lon
Businesses$lat <- coded_data$lat
Businesses$loctype <- coded_data$loctype
Businesses$type <- coded_data$type

# Save Data Frame
write_csv(Businesses, path = "FinalProject Files/Geocoded_Business_Licenses.csv")
# Read Geocoded Business Data from file
Businesses <- read_csv("Geocoded_Business_Licenses.csv")

BusinessesClean <- na.omit(Businesses) %>% filter(State == "IN")

# Read Class Ref Sheet
classref <- read_csv("ClassRef.csv")

# Join data onto df
BusinessesClean <- left_join(BusinessesClean,classref)

# Create Spatial Business Data
businesses.spatial <- SpatialPointsDataFrame(coords = BusinessesClean[,c("lon","lat")], 
                                             data = BusinessesClean,
                                             proj4string = CRS("+proj=longlat +datum=WGS84"))

# Create Popup Object
businesses.spatial$popup <- paste("<b>",businesses.spatial$Business_Name,"</b><br>",
                                  "Category:",businesses.spatial$Category,"<br>",
                                  "Type: ",businesses.spatial$Classification_Description,"<br>",
                                  "Phone: ",businesses.spatial$Business_Phone_Number,sep ="")

# Write to file
save(businesses.spatial,file = "CleanedBusinessed.Rdata")

# Get and Clean Census Data

# Set Up API key (one time run)
# tidycensus::census_api_key("cacaf1ff1d0fdf9bc06a72427eded4c164cd0d08", install = T)

# Select Variables

vars = c("total.population" = "B01001_001", 
         "housng.units" = "B25001_001",
         "median.age" = "B01002_001",
         "median.income" = "B07011_001")

# Get data by zip code for all US from ACS 2017 
zip_census <- get_acs(geography = "zcta", 
                      variables = vars, 
                      year = 2017, 
                      geometry = T, 
                      output = "wide")

# Filter to just the zip codes for South Bend
sb_index <- (zip_census$GEOID == 46628 | zip_census$GEOID == 46613 | zip_census$GEOID == 46601 | 
               zip_census$GEOID == 46635 | zip_census$GEOID == 46617 |zip_census$GEOID == 46619 | 
               zip_census$GEOID == 46615 | zip_census$GEOID == 46614 | zip_census$GEOID == 46637 |
               zip_census$GEOID == 46616 | zip_census$GEOID == 46680 | zip_census$GEOID == 46624 |
               zip_census$GEOID == 46634 | zip_census$GEOID == 46660)

# Transform for plotting w/ leaflet
sb_zip_data <- zip_census[sb_index,] %>% st_transform(crs = "+init=epsg:4326")

# Create Popup Object
sb_zip_data$popup <- paste("<b>",sb_zip_data$GEOID,"</b><br>",
                           "Population: ",sb_zip_data$total.populationE,"<br>")

# Save File
save(sb_zip_data, file = "SB_Zip_Data.Rdata")
## Business Info Tab ##

## Code Violations Tab ##
dat <- read.csv("Code_Enforcement_Cases.txt")

dat$Case_Year[dat$Case_Year == 8] <- 2008
dat$Case_Year[dat$Case_Year == 9] <- 2009
dat$Case_Year[dat$Case_Year == 10] <- 2010
dat$Case_Year[dat$Case_Year == 11] <- 2011
dat$Case_Year[dat$Case_Year == 12] <- 2012
dat$Case_Year[dat$Case_Year == 13] <- 2013
dat$Case_Year[dat$Case_Year == 14] <- 2014
dat$Zip_Code[dat$Zip_Code == 0] <- "Unknown"

codeViolations <- dat %>% select(-Case_Status_Code, -Date_Case_Reported___Calc, -State_Code, -City)
markers <- SpatialPointsDataFrame(coords = codeViolations[,c("Lon", "Lat")], data = codeViolations,
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))

save(codeViolations, file = "codeViolations_clean.RData")
save(markers, file = "markers_clean.RData")
## Code Violations Tab ##

