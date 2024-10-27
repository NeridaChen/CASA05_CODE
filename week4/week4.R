install.packages("countrycode")

library(sf)
library(here)
library(dplyr)
library(stringr)
library(countrycode)
library(tmap)
library(tmaptools)
library(janitor)
library(OpenStreetMap)


# read csv
Data <- st_read(here::here("week4","HDR23-24_Composite_indices_complete_time_series.csv"))

# create a new csv only contains global gender inequality index 
GenderData <- Data %>%
  dplyr::select(contains("iso3"), 
                contains("country"),
                contains("gii")) 

GenderData2 <- GenderData[-c(196:206),]
GenderData2 <- GenderData2 %>%
  select(-gii_rank_2022)

# str(GenderData_cleaned)
# change the column from char to numeric
GenderData2$gii_2019 <- as.numeric(as.character(GenderData2$gii_2019))
GenderData2$gii_2010 <- as.numeric(as.character(GenderData2$gii_2010))
str(GenderData2)

# new column with difference in inequality between 2010 and 2019
GenderDate_diff <- GenderData2 %>%
  mutate(difference = gii_2019 - gii_2010) %>%
  dplyr::select(iso3,
                country,
                gii_2010, 
                gii_2019,
                difference)
GenderDate_diff

GenderDate3 <- GenderData2 %>%
  mutate(difference = gii_2019 - gii_2010)

# read Geojson
WorldMap <- st_read("week4/World_Countries_(Generalized)_9029012925078512962.geojson")

names(WorldMap)

# change ISO2 to ISO3
WorldMap <- WorldMap %>%
  mutate(iso3 = countrycode(ISO, origin = "iso2c", destination = "iso3c"))

# check data
head(WorldMap)

# save geo data
st_write(WorldMap, "modified_file.geojson")

# merge data
WorldDataMap <- WorldMap %>%
  clean_names() %>% # change the column name to lowercase
  left_join(.,
            GenderDate_diff, by = "iso3")

# tmap mode set to interactive viewing
tmap_mode("view")

tm_shape(WorldDataMap) + 
  tm_polygons("difference", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants")