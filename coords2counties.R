#libraries
library(sf)
library(tigris)
library(cdlTools) #this is probably new to you; install it
library(ggplot2)
library(dplyr)
library(stringr)



#load coordinate data
coords<- read.csv("~/Desktop/Research/UNM/Fixing Herbarium County data/unmbatchrefissues.csv")
coords.sf <- as.data.frame(coords) %>% 
  filter(!decimalLongitude == "") %>%
  filter(!decimalLatitude == "") %>%
  st_as_sf(coords=c("decimalLongitude","decimalLatitude"), crs=4326, remove=FALSE) %>%
  st_transform(crs=4326)



#load county data from Tigris package
nm <- counties("New Mexico") 
nm.prj <- st_as_sf(nm) %>%
  st_transform(crs=4326)

#plot to make sure; looks okay to me
ggplot() +
  geom_sf(data = nm.prj, fill = "lightblue") +
  geom_sf(data = coords.sf, color = "red", size = 3, shape = 17) +
  theme_minimal()


#Join coords to layer of county boundaries. DUPLICATING RECORDS. 522-ish rows added
coords.sf <- coords.sf %>% 
  st_join(left = TRUE, nm.prj["NAME"])

# Now format the two relevant columsn with county data and check for mismatches
### 'county' is the original county string when first read
### 'county.orig' is the original county string minus the 'County' string
### 'NAME' is the extracted county string (from tigris)

coords.sf <- coords.sf %>%
  mutate(county.orig = str_remove_all(county, " County")) %>%
  mutate(county.match = mapply(grepl, county.orig, NAME))

coords.mismatch <- coords.sf %>%
  filter(county.match == "FALSE")

write.csv(coords.mismatch, "/Users/elizabethlombardi/Desktop/Research/UNM/Fixing Herbarium County data/coords.mismatch.csv")


##Try calculating minimum distance between each coordinate and the nearest county line
try <- coords.sf %>%
  sf::sf_nearest_feature(nm.prj)
  





#now convert the FIPS codes to county names using the cdlTools package in R
coords.sf <- coords.sf %>%
  mutate(countyName = fips(try$fips_codes, to = 'Name')) #I get some errors, but it generally seemed to work

#check
table(coords.sf$countyName)



### Try tidygeocoder reverse georeferencing; this is probably going to take WAY too long
library(tidygeocoder)

sub.coords <- 

reverse <- coords.sf %>% 
  reverse_geocode(lat = decimalLatitude, long = decimalLongitude, method = 'osm',
                  address = address_found, full_results = TRUE) %>%
  select(-addr, -licence)


##TRY creating a sf count map of new mexico with string values for names, then spatial joining with coords
