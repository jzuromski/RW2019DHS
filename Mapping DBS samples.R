install.packages("rnaturalearthdata")
install.packages("rnaturalearth")
install.packages("tidyverse")
install.packages("sf")
install.packages("ggspatial")
install.packages("scatterpie")
install.packages("sp")
install.packages("ggplot2")
install.packages("scales")


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

# make request
d <- dhs_data(countryIds = "RW",
              surveyYearStart = 2018,
              breakdown = "subnational")


# make a df for only the unique_malaria_positive_DBS with IDs, RDT, microscopy, latitude, and longitude
map_mal_pos_DBS <- Malaria_positive_DBS %>%
  select(LATNUM, LONGNUM, barcode, hml32, hml35, geometry)

# change the column labels in the data frame from hml32 to microscopy and hml35 to mal_RDT
map_mal_pos_DBS <- map_mal_pos_DBS %>%
  rename(
    blood_smear = hml32,
    mal_RDT = hml35,
  )

#----------------------------------------------------------------------------------#

#                               Getting GPS and geography data
#----------------------------------------------------------------------------------#


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

#obtain survey datasets
surveys <- dhs_surveys(countryIds =  "RW", #rwanda
                       surveyType = "DHS",
                       surveyYearStart = 2018) #return the desired surveys

datasets <- dhs_datasets(surveyIds = surveys$SurveyId, fileFormat = "FL", fileType = ("GE")) #GPS
str(datasets)
downloads <- get_datasets(datasets$FileName, clear_cache = TRUE)

# read in our dataset
cdpr <- readRDS(downloads$RWGE81FL)

# let's look at the variable_names
head(get_variable_labels(cdpr))

# download mapping data (the geography file)
world <- ne_countries(scale = "medium", returnclass = "sf") 
rwanda <-  subset(world, admin == "Rwanda")


#----------------------------------------------------------------------------------------------------
#                                      Mapping malaria-positive DBS
#----------------------------------------------------------------------------------------------------

ggplot(map_mal_pos_DBS) +
  geom_sf(data=rivers10, color="cyan4", size=0.5, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=admin10, color="grey80", size= 0.4) +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanz -\nania.", 
           color="grey60", size=5 , fontface="italic") +
  geom_point(data = map_mal_pos_DBS, 
             aes(x = LONGNUM, y = LATNUM, 
                 fill = "lightblue", 
                 color = "darkblue", 
                 shape = 17),
             size = 2) +
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1,-2.9), expand = TRUE)+
  theme_void()


####---------------------------------------------------------------------####
#      Map malaria-positive DBS with number of samples in each cluster
####---------------------------------------------------------------------####

# Example dataset (replace 'map_mal_pos_DBS' and 'cluster_column' with your actual dataset and cluster column)
# map_mal_pos_DBS <- your_data
# Cluster column contains the geographic point ID or cluster ID
# Replace 'cluster_column' with your actual column name that identifies clusters

# Step 1: Group by cluster and count the number of samples in each cluster
cluster_counts <- map_mal_pos_DBS %>%
  group_by(geometry) %>%
  summarise(sample_count = n())

# Step 2: Join the counts back to the original dataset
map_mal_pos_DBS_clustercounts <- map_mal_pos_DBS %>%
  left_join(cluster_counts, by = "geometry")

# Step 3: Plot the points with color based on the number of samples per cluster
ggplot(rwanda) +
  geom_sf(fill = "blanchedalmond") +
  geom_sf(data=admin10, color="grey10", size= 0.9) +
  geom_sf(data=rivers10, color="cyan4", size=0.9, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  geom_sf(data=roads10, color="grey70",fill= "ivory", size= 0.2) +
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=4 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=4 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=4 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
           color="grey60", size=4 , fontface="italic") +
  geom_point(data = map_mal_pos_DBS_clustercounts, 
             aes(x = LONGNUM, y = LATNUM, fill = sample_count), 
             size = 2, color = "black", shape = 21, stroke = 0.5) +
  scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(color = "Samples per Cluster", title = "Malaria-Positive Samples by Cluster")
  theme_void()
  
  
  ####-------------------------------------------------------------------####
  #      Map all DBS with number of samples in each cluster
  ####-------------------------------------------------------------------####
  
  # Step 1: Make a df for all_DBS with barcodes, geometry, latitude, and longitude
  map_all_DBS <- DBS_samples %>%
    select(hv001, LATNUM, LONGNUM, barcode, geometry)
  
  
  # Step 2: Group by cluster and count the number of samples in each cluster
  cluster_counts <- DBS_samples %>%
    group_by(geometry) %>%
    summarise(sample_count = n())
  
  # Step 3: Join the counts back to the original dataset
  map_all_DBS_clustercounts <- map_all_DBS %>%
    left_join(cluster_counts, by = "geometry")
  
  # Step 3: Plot the points with color based on the number of samples per cluster
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
    geom_point(data = map_all_DBS_clustercounts, 
               aes(x = LONGNUM, y = LATNUM, fill = sample_count), 
               size = 1, color = "black", shape = 21, stroke = 0.5) +
    scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
    coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
    theme_void() +
    labs(color = "Samples per Cluster", title = "ALL DBS Samples by Cluster")
  theme_void()
  
  
  ####---------------------------------------------------------------------####
  #      Map malaria-positive DBS with number of samples in each cluster
  ####---------------------------------------------------------------------####

  
  # Step 1: Group by cluster and count the number of samples in each cluster
  cluster_counts <- map_mal_pos_DBS %>%
    group_by(geometry) %>%
    summarise(sample_count = n())
  
  # Step 2: Join the counts back to the original dataset
  map_mal_pos_DBS_clustercounts <- map_mal_pos_DBS %>%
    left_join(cluster_counts, by = "geometry")
  
  # Step 3: Plot the points with color based on the number of samples per cluster
  ggplot(rwanda) +
    geom_sf(fill = "blanchedalmond") +
    geom_sf(data=admin10, color="grey10", size= 0.9) +
    geom_sf(data=rivers10, color="cyan4", size=0.9, alpha=0.5) +
    geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +
    geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
    geom_sf(data=roads10, color="grey70",fill= "ivory", size= 0.2) +
    annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
             color="grey60", size=4 , fontface="italic") +
    annotate("text", x = 30, y = -1.1, label = "Uganda", 
             color="grey60", size=4 , fontface="italic") +
    annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
             color="grey60", size=4 , fontface="italic") +
    annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
             color="grey60", size=4 , fontface="italic") +
    geom_point(data = map_mal_pos_DBS_clustercounts, 
               aes(x = LONGNUM, y = LATNUM, fill = sample_count), 
               size = 2, color = "black", shape = 21, stroke = 0.5) +
    scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
    coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
    theme_void() +
    labs(color = "Samples per Cluster", title = "Malaria-Positive Samples by Cluster")
  theme_void()
  
  
  ####-------------------------------------------------------------------####
  #      Map DBS tested for malaria with number of samples in each cluster
  ####-------------------------------------------------------------------####
 
  # Step 1: Make a df for malaria-tested DBS with geometry, latitude, and longitude
  map_mal_tested_DBS <- mal_tested_DBS %>%
    select(hv001, LATNUM, LONGNUM, geometry)
  
  
  # Step 2: Group by cluster and count the number of samples in each cluster
  cluster_counts <- map_mal_tested_DBS %>%
    group_by(geometry) %>%
    summarise(sample_count = n())
  
  # Step 3: Join the counts back to the original dataset
  map_mal_tested_DBS_clustercounts <- map_mal_tested_DBS %>%
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
             color="grey60", size=4 , fontface="italic") +
    annotate("text", x = 30, y = -1.1, label = "Uganda", 
             color="grey60", size=4 , fontface="italic") +
    annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
             color="grey60", size=4 , fontface="italic") +
    annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
             color="grey60", size=4 , fontface="italic") +
    geom_point(data = map_mal_tested_DBS_clustercounts, 
               aes(x = LONGNUM, y = LATNUM, fill = sample_count), 
               size = 2, color = "black", shape = 21, stroke = 0.5) +
    scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
    coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
    theme_void() +
    labs(color = "Samples per Cluster", title = "Malaria-tested DBS Samples by Cluster")
  theme_void()
  
  #----------------------------------------------------------------------------------------##
  # Mapping samples based on household weighting using malaria-pos DBS data set as example                         
  #----------------------------------------------------------------------------------------##
  
  # Step 1:Make a new dataframe to test in
  Malaria_positive_DBS_byweight <- Malaria_positive_DBS
  
  # Ensure that 'hv005' is a character or factor before converting
  Malaria_positive_DBS_byweight$hv005 <- as.character(Malaria_positive_DBS_byweight$hv005)
  # Convert 'hv005' to numeric
  Malaria_positive_DBS_byweight$hv005 <- as.numeric(Malaria_positive_DBS_byweight$hv005)
  # Inspect the first few rows of the 'hv005' column
  print(head(Malaria_positive_DBS_byweight$hv005))
  
  # Create function to convert CDC to Date
  household_weights <- function(Malaria_positive_DBS_byweight) {
    Malaria_positive_DBS_byweight$hv005 <- Malaria_positive_DBS_byweight$hv005 / 1000000 
    return(Malaria_positive_DBS_byweight)
  }
  
  # Define the function to calculate the weights
  calculate_weight <- function(hv005) {
    hv005 / 1e6
  }
  
  # Apply the function to create a new column in your data frame
  Malaria_positive_DBS_byweight$household_weight <- calculate_weight(Malaria_positive_DBS_byweight$hv005)
  
  # Map based on household sample weights
  
  ggplot(rwanda) +
    geom_sf(fill = "blanchedalmond") +
    geom_sf(data=admin10, color="grey10", size= 0.9) +
    geom_sf(data=rivers10, color="cyan4", size=0.9, alpha=0.5) +
    geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +
    geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
    geom_sf(data=roads10, color="grey70",fill= "ivory", size= 0.2) +
    annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
             color="grey60", size=4 , fontface="italic") +
    annotate("text", x = 30, y = -1.1, label = "Uganda", 
             color="grey60", size=4 , fontface="italic") +
    annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
             color="grey60", size=4 , fontface="italic") +
    annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
             color="grey60", size=4 , fontface="italic") +
    geom_point(data = Malaria_positive_DBS_byweight, 
               aes(x = LONGNUM, y = LATNUM, fill = household_weight), 
               size = 3, color = "black", shape = 21, stroke = 0.5) +
    scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
    coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
    theme_void() +
    labs(color = "Weight of sample", title = "Malaria-Positive Samples by household sample weight")
  theme_void()

  #----------------------------------------------------------------------------------------##
  # Mapping samples based on household weighting using ALL DBS samples                       
  #----------------------------------------------------------------------------------------##
  
  # Step 1:Make a new dataframe to test in
  DBS_samples_byhouseholdweight <- DBS_samples
  
  # Ensure that 'hv005' is a character or factor before converting
  DBS_samples_byhouseholdweight$hv005 <- as.character(DBS_samples_byhouseholdweight$hv005)
  # Convert 'hv005' to numeric
  DBS_samples_byhouseholdweight$hv005 <- as.numeric(DBS_samples_byhouseholdweight$hv005)
  # Inspect the first few rows of the 'hv005' column
  print(head(DBS_samples_byhouseholdweight$hv005))
  
  # Step 2: Create function to convert CDC to Date
  household_weights <- function(DBS_samples_byhouseholdweight) {
    DBS_samples_byhouseholdweight$hv005 <- DBS_samples_byhouseholdweight$hv005 / 1000000 
    return(DBS_samples_byhouseholdweight)
  }
  
  # Define the function to calculate the weights
  calculate_weight <- function(hv005) {
    hv005 / 1e6
  }
  
  # Step 3: Apply the function to create a new column in your data frame
  DBS_samples_byhouseholdweight$household_weight <- calculate_weight(DBS_samples_byhouseholdweight$hv005)
  
  ### Step 4: Map based on household sample weights
  
  # Define your color scale with ranges and colors (number of colors is 6)
  colors <- colorRampPalette(viridis::plasma(5))(6)
  
  # Plot data points based on the sample weights, with each weight plotted as a color within a value range
  ggplot(rwanda) +
    geom_sf(fill = "blanchedalmond") +
    geom_sf(data=admin10, color="grey10", size= 0.9) +
    geom_sf(data=rivers10, color="cyan4", size=0.9, alpha=0.5) +
    geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
    geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +
    geom_sf(data=roads10, color="grey70",fill= "ivory", size= 0.2) +
    annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
             color="grey60", size=3 , fontface="italic") +
    annotate("text", x = 30, y = -1.1, label = "Uganda", 
             color="grey60", size=3 , fontface="italic") +
    annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
             color="grey60", size=3 , fontface="italic") +
    annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
             color="grey60", size=3 , fontface="italic") +
    geom_point(data = DBS_samples_byhouseholdweight, 
               aes(x = LONGNUM, y = LATNUM, fill = factor(cut(household_weight, breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), 
                                                              labels = c("0-0.5", "0.5-1", "1-1.5", "1.5-2", "2-2.5", "2.5-3", "3-3.5")))), 
               size = 1, color = "black", shape = 21, stroke = 0.5) +
    scale_fill_manual(values = colors) +
    coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
    labs(fill = "Household Weight", title = "DBS samples by household sample weight") +
    theme_void() +
    guides(fill = guide_legend(title = "Household Weight"))