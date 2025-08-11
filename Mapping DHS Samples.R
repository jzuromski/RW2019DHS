library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(rnaturalearth) 
library(rnaturalearthdata)
library(sf) #see ubuntu issues here: https://rtask.thinkr.fr/installation-of-r-4-0-on-ubuntu-20-04-lts-and-tips-for-spatial-packages/
library(ggspatial)
library(scatterpie)
library(sp)
library(rdhs)
library(ggplot2)
library(scales)
library(dplyr)

# get our related spatial data frame object
sp <- download_boundaries(surveyId = d$SurveyId[1], method = "sf")

m <- d$Value[match(sp$sdr_subnational_boundaries$REG_ID, d$RegionId)]
sp$sdr_subnational_boundaries2$Value <- m


# Download a shapefile for CORRECT administrative level of RW

admin10 <- ne_download(scale="large", type = "admin_1_states_provinces_lines",
                       category = "cultural", returnclass = "sf")
rivers10 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', 
                        category = 'physical', returnclass = "sf")
lakes10 <- ne_download(scale = "large", type = 'lakes', 
                       category = 'physical', returnclass = "sf")
sov110 <- ne_download(scale="medium", type = "sovereignty",
                      category = "cultural", returnclass = "sf")
admin110 <- ne_download(scale="large", type = "populated_places",
                        category = "cultural", returnclass = "sf")
roads10 <- ne_download(scale="large", type = "roads",
                       category = "cultural", returnclass = "sf")

# download mapping data (the geography file)
world <- ne_countries(scale = "medium", returnclass = "sf") 
rwanda <-  subset(world, admin == "Rwanda")





####-------------------------------------------------------------------------------####
#      Map malaria-positive DHS cases with number of samples in each cluster
####-------------------------------------------------------------------------------####

# Step 1: Make a df for malaria-tested cases with geometry, latitude, and longitude
map_malaria_positive_cases <- Malaria_positive_cases %>%
  select(hv001, LATNUM, LONGNUM, geometry)


# Step 2: Group by cluster and count the number of samples in each cluster (clusters = 100)
cluster_counts <- map_malaria_positive_cases %>%
  group_by(geometry) %>%
  summarise(sample_count = n())

# Step 3: Join the counts back to the original dataset
map_malaria_positive_cases_clustercounts <- map_malaria_positive_cases %>%
  left_join(cluster_counts, by = "geometry")

# Step 4: Plot the points with color based on the number of samples per cluster
ggplot(rwanda) +
  geom_sf(fill = "blanchedalmond") +
  geom_sf(data=admin10, color="grey10", size= 0.9) +
  geom_sf(data=rivers10, color="cyan4", size=0.9, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  geom_sf(data=roads10, color="grey70",fill= "ivory", size= 0.2) +
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
           color="grey60", size=2 , fontface="italic") +
  geom_point(data = map_malaria_positive_cases_clustercounts, 
             aes(x = LONGNUM, y = LATNUM, fill = sample_count), 
             size = 2, color = "black", shape = 21, stroke = 0.5) +
  scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(color = "Samples per Cluster", title = "Malaria-positive Cases by Cluster")
theme_void()


####--------------------------------------------------------------------------------------####
#  Map malaria-positive DHS cases with samples per cluster AND region circled and labeled
####--------------------------------------------------------------------------------------####


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(sf)

# Convert map_malaria_positive_cases_clustercounts to an sf object with geometry if it isn't already
map_malaria_positive_cases_clustercounts1 <- map_malaria_positive_cases_clustercounts %>%
  st_as_sf(coords = c("LONGNUM", "LATNUM"), crs = 4326) # Ensure LONGNUM and LATNUM are the coordinate columns

# Group by region (DHSREGNA) and calculate convex hulls around clusters in each region
region_hulls <- map_malaria_positive_cases_clustercounts1 %>%
  group_by(DHSREGNA) %>%
  summarize(geometry = st_convex_hull(st_union(geometry)))

# Calculate the centroids of each region to use for labeling
region_hulls <- region_hulls %>%
  mutate(centroid = st_centroid(geometry))

# Now create the map with grouped clusters by region and labels
ggplot(rwanda) +
  geom_sf(fill = "blanchedalmond") +
  geom_sf(data = admin10, color = "grey10", size = 0.9) +
  geom_sf(data = rivers10, color = "cyan4", size = 0.9, alpha = 0.5) +
  geom_sf(data = lakes10, color = "grey40", fill = "lightblue", size = 0.8) +
  geom_sf(data = sov110, color = 'black', size = 0.8, 
          fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  
  # Add convex hulls (polygons) for each region
  geom_sf(data = region_hulls, color = "red", fill = NA, linetype = "solid", size = 1) +
  
  # Add clusters as points
  geom_point(data = map_malaria_positive_cases_clustercounts, 
             aes(x = LONGNUM, y = LATNUM, fill = sample_count), 
             size = 1, color = "black", shape = 21, stroke = 0.5) +
  
  # Add region names at the centroid of each convex hull
  geom_sf_text(data = region_hulls, aes(label = DHSREGNA, geometry = centroid), 
               size = 2, color = "black") +
  
  # Color scale
  scale_fill_viridis_c(option = "plasma") +
  
  # Annotations for neighboring countries
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color = "grey60", size = 2, fontface = "italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color = "grey60", size = 2, fontface = "italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color = "grey60", size = 2, fontface = "italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
           color = "grey60", size = 2, fontface = "italic") +
  
  # Set coordinates and themes
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(color = "Samples per Cluster", title = "Malaria-positive Cases by Cluster") +
  theme_void()

####-------------------------------------------------------------------------------####
#                 Map malaria prevalence (by microscopy) in each cluster
####-------------------------------------------------------------------------------####

# Step 1: Take a df for malaria-tested cases with geometry, latitude, and longitude
#map_Malaria_prevalence_by_cluster

# Step 2: Plot the points with color based on the number of samples per cluster
ggplot(rwanda) +
  geom_sf(fill = "blanchedalmond") +
  geom_sf(data=admin10, color="grey10", size= 0.9) +
  geom_sf(data=rivers10, color="cyan4", size=0.9, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  geom_sf(data=roads10, color="grey70",fill= "ivory", size= 0.2) +
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
           color="grey60", size=2 , fontface="italic") +
  geom_point(data = Map_mic_mal_prev_cluster, 
             aes(x = LONGNUM, y = LATNUM, fill = Mic_Positive), 
             size = 2.1, color = "black", shape = 21, stroke = 0.5) +
  scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(color = "Percent Positivity", title = "Malaria Prevalence by Microscopy")
theme_void()

####-------------------------------------------------------------------------------####
#                 Map malaria prevalence (by RDT) in each cluster
####-------------------------------------------------------------------------------####

# Step 1: Take a df for malaria-tested cases with geometry, latitude, and longitude
#map_Malaria_prevalence_by_cluster

# Step 2: Plot the points with color based on the number of samples per cluster
ggplot(rwanda) +
  geom_sf(fill = "blanchedalmond") +
  geom_sf(data=admin10, color="grey10", size= 0.9) +
  geom_sf(data=rivers10, color="cyan4", size=0.9, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  geom_sf(data=roads10, color="grey70",fill= "ivory", size= 0.2) +
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
           color="grey60", size=2 , fontface="italic") +
  geom_point(data = Map_RDT_mal_prev_cluster, 
             aes(x = LONGNUM, y = LATNUM, fill = RDT_Positive), 
             size = 2.1, color = "black", shape = 21, stroke = 0.5) +
  scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(color = "Percent Positivity", title = "Malaria Prevalence by RDT ")
theme_void()
