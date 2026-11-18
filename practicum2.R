##natalie daniels
##practicum 2 
##NY maps

###this R code depicts a series of maps of the State of New York, 
###highlighting particular identified features of the state.

# Load packages
library(sf)
library(ggplot2)
library(dplyr)

# Read in
counties <- st_read("cb_2024_us_county_500k.shp")
highpoints <- read.csv("nyshighpoints.csv")

#### MAP 1: map of the counties of New York State (ONLY)

# Filter to include only New York State
ny_counties <- counties %>%
  filter(STATEFP == "36")

# Create the map
ggplot(data = ny_counties) +
  geom_sf(fill = "lightblue", color = "black") +
  labs(
    title = "Counties of New York State",
    subtitle = "Map of all New York counties",
    caption = "Source: U.S. Census Bureau"
  ) +
  theme_minimal()

#### FIND smallest and largest counties

# Largest county
largest_county <- ny_counties %>%
  arrange(desc(ALAND)) %>%
  select(NAME, ALAND) %>%
  slice(1)
print(largest_county)

# Smallest county
smallest_county <- ny_counties %>%
  arrange(ALAND) %>%
  select(NAME, ALAND) %>%
  slice(1)
print(smallest_county)

## so, new york has the smallest county while st lawrence has the largest


#### Map 2. Plot county map with high points
highpoint_map <- ggplot() +
  geom_sf(data = ny_counties, fill = "lightblue", color = "black") +
  geom_point(data = highpoints,
             aes(x = Longitude, y = Latitude),
             color = "red", size = 1.0) +
  labs(title = "High Points of New York Counties",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
print(highpoint_map)


#### PLOT 3. color based on height
elevation_plot <- ggplot() +
  geom_sf(data = ny_counties, fill = "lightgray", color = "black") +
  geom_point(data = highpoints %>%
               group_by(County) %>%
               slice_max(Elevation_Feet, n = 1),
             aes(x = Longitude, y = Latitude, color = Elevation_Feet),
             size = 2) +
  scale_color_gradientn(
    colors = c("green", "yellow", "brown", "white", "red"),
    name = "Elevation (Feet)"
  ) +
  labs(title = "Highest Elevation Points in New York Counties") +
  theme_minimal()

print(elevation_plot)


#### PLOT 4. label highest and lowest point on map

# Find highest and lowest points
extreme_sf <- st_as_sf(extreme_points,
                       coords = c("Longitude", "Latitude"),
                       crs = 4326) %>%
  st_transform(st_crs(ny_counties))

high_low <- ggplot() +
  geom_sf(data = ny_counties, fill = "lightgray", color = "black") +
  geom_sf(data = extreme_sf, aes(color = Point_Type), size = 2) +
  scale_color_manual(
    values = c("Low Point, Battle Hill" = "red",
               "High Point, Mount Marcy" = "blue"),
    name = "Elevation"
  ) +
  labs(title = "Highest and Lowest High Points in New York") +
  theme_minimal()

high_low

max(highpoints$Elevation_Feet)   # highest
min(highpoints$Elevation_Feet)   # lowest

#### PLOT 5. load county population in New York
# Install and load packages
install.packages("tidycensus")

library(tidycensus)
library(tidyverse)

#my census key
census_api_key("a3efbf969b2632b21c00ee2b54be048567a46384", install = TRUE, overwrite=TRUE)

#use key now 
readRenviron("~/.Renviron")

# Get New York county population data
population <- get_acs(
  geography = "county",
  variables = "B01003_001",
  state = "NY",
  year = 2024,
  survey = "acs5"
)

# Join population data to county map
ny_population <- ny_counties %>%
  left_join(population, by = "GEOID")

# Create map
ggplot(ny_population) +
  geom_sf(aes(fill = estimate), color = "black") +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkorange",
    name = "Population"
  ) +
  labs(
    title = "Population by County in New York",
    subtitle = "2024 American Community Survey"
  ) +
  theme_minimal()

#### PLOT 6. pop density and land area

# Calculate population density
ny_population <- ny_population %>%
  mutate(Pop_Density = estimate / (ALAND / 2589988.11))
ny_population

# Create population density map
ggplot(ny_population) +
  geom_sf(aes(fill = Pop_Density), color = "black") +
  scale_fill_gradient(
    low = "lavender",
    high = "purple",
    trans = "log10",
    name = "People per Square Mile"
  ) +
  labs(
    title = "Population Density by County in New York",
    subtitle = "Logarithmic color scale"
  ) +
  theme_minimal()

#### CALC 
names(ny_population)

# Largest county by population
ny_population %>%
  arrange(desc(estimate)) %>%
  select(NAME.y, estimate) %>%
  slice(1)

###answer: kings county

# Smallest county by population
ny_population %>%
  arrange(estimate) %>%
  select(NAME.y, estimate) %>%
  slice(1)

###answer: Hamilton county

# Most dense county
ny_population %>%
  arrange(desc(Pop_Density)) %>%
  select(NAME.y, Pop_Density) %>%
  slice(1)

###answer: New york county

# Least dense county
ny_population %>%
  arrange(Pop_Density) %>%
  select(NAME.y, Pop_Density) %>%
  slice(1)

###answer: Hamilton county


#### BONUS 
# load data 

load("/Users/nataliedaniels/Downloads/dataverse_shareable_presidential_county_returns_1868_2020 (1).Rdata")
ls()

