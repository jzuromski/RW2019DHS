library(rdhs)
library(dplyr)
library(haven)
library(here)
library(labelled)
library(devtools)
library(microbenchmark)
library(survey)
library(readr)



# read in our dataset
rwanda_data_2 <- readRDS(downloads$RWPR81FL)

#------------------------------------Add barcode variable (combines male and female barcode variables) and make a DBS samples only dataframe----------------

# Ensure the column is of character type (if not already)
rwanda_data_2$ha62 <- as.character(rwanda_data_2$ha62)
rwanda_data_2$hb62 <- as.character(rwanda_data_2$hb62)

# Regular expression for a 5-character string containing both letters and numbers
pattern <- "^(?=.*[A-Za-z])(?=.*[0-9])[A-Za-z0-9]{5}$"

# Create new column "barcodes" in rwanda_data_2 to provide DBS barcode
rwanda_data_2 <- rwanda_data_2 %>%
  mutate(
    barcode = case_when(
      !is.na(ha62) & grepl(pattern, ha62, perl = TRUE) ~ ha62,
      !is.na(hb62) & grepl(pattern, hb62, perl = TRUE) ~ hb62,
      TRUE ~ NA_character_ ))

# Create new dataframe called DBS_samples, retaining only individuals in household member recode who have an HIV (DBS) sample barcode
DBS_samples <- rwanda_data_2 %>%
  filter(!is.na(barcode))


#-------------------------------------Summary tables of DHS data-----------------------

#-------------------------------------High prevalence clusters

# All samples malaria prev by cluster
cluster_malaria <- rwanda_data_2 %>%
  group_by(hv001) %>% 
  summarize(
    tested_for_malaria = sum(hml33 == 0, na.rm = TRUE), # total n= 11053
    n_pos_rdt = sum(hml35 == 1, na.rm = TRUE), # total n= 185
    n_pos_mic = sum(hml32 == 1, na.rm = TRUE), # total n= 70
    n_both_pos = sum(hml32 == 1 & hml35 == 1, na.rm = TRUE), # total n= 48
    n_malaria = sum(malaria == 1, na.rm = TRUE), # total n= 207
    prev_malaria_rdt = (n_pos_rdt / tested_for_malaria) * 100, # total n= 1.673754
    prev_malaria_mic = (n_pos_mic / tested_for_malaria) * 100, # total n= 0.6333122
    prev_malaria_total = (sum(malaria == 1, na.rm = TRUE) / tested_for_malaria) * 100, # total n= 1.872795
    long = mean(LONGNUM), 
    lat = mean(LATNUM), 
    observations = n() # n= 55920
  )

high_prev <- data.frame(filter(cluster_malaria, prev_malaria_total >= 15)) #select clusters with > 15% , n = 16 clusters



# DBS malaria prev by cluster
cluster_malaria_DBS <- DBS_samples %>%
  group_by(hv001) %>% 
  summarize(
    tested_for_malaria = sum(hml33 == 0, na.rm = TRUE), # total n= 7354
    n_pos_rdt = sum(hml35 == 1, na.rm = TRUE), # total n= 86
    n_pos_mic = sum(hml32 == 1, na.rm = TRUE), # total n= 36
    n_both_pos = sum(hml32 == 1 & hml35 == 1, na.rm = TRUE), # total n= 21
    n_malaria = sum(malaria == 1, na.rm = TRUE), # total n= 101
    prev_malaria_rdt = (n_pos_rdt / tested_for_malaria) * 100, # total n= 1.169432
    prev_malaria_mic = (n_pos_mic / tested_for_malaria) * 100, # total n= 0.4895295
    prev_malaria_total = (sum(malaria == 1, na.rm = TRUE) / tested_for_malaria) * 100, # total n= 1.373402
    long = mean(LONGNUM), 
    lat = mean(LATNUM), 
    observations = n() # total n= 13941
  )

high_prev_DBS <- data.frame(filter(cluster_malaria_DBS, prev_malaria_total >= 15)) #select clusters with > 15% , n = 13 clusters


# Make a summary table showing the following variables:
#cluster number (hv001), 
#region name (hv024), 
#number of cases per cluster, 
#number of men (hv104 =1)
#number of women (hv104 =2)
#Age breakdown (15-96)  (hv105)
#number of RDT-positive (hml35 =1)
#number of blood smear-positive (hml32 =1), 
#Pf (hml32a =1)
#Pm (hml32b =1)
#Po (hml32c =1)
#Pv (hml32d =1)

rwanda_data_2 <- unique(rwanda_data_2) #remove duplicates **n= 11053**

# Create summary for all DHS cases
summary_rwanda_data_2 <- rwanda_data_2 %>%
  summarize(
    #Sample number
    Total_samples = n(),
    # number of clusters
    unique_clusters = n_distinct(hv001),
    # hv104: Sex breakdown
    male = sum(hv104 == 1, na.rm = TRUE),
    female = sum(hv104 == 2, na.rm = TRUE),
    # hv105: Age breakdown (15-96)
    age_15_30 = sum(hv105 >= 15 & hv105 <= 30, na.rm = TRUE),
    age_31_50 = sum(hv105 >= 31 & hv105 <= 50, na.rm = TRUE),
    age_51_70 = sum(hv105 >= 51 & hv105 <= 70, na.rm = TRUE),
    age_71_96 = sum(hv105 >= 71 & hv105 <= 96, na.rm = TRUE),
    # hml33: Tested for malaria
    tested_for_malaria = sum(hml33 == 0, na.rm = TRUE),
    # hml32: Malaria blood smear positive
    malaria_blood_smear_positive = sum(hml32 == 1, na.rm = TRUE),
    # hml35: Malaria RDT positive
    malaria_RDT_positive = sum(hml35 == 1, na.rm = TRUE),
    #Pf (hml32a =1)
    Pf_positive = sum(hml32a == 1, na.rm = TRUE),
    #Pm (hml32b =1)
    Pm_positive = sum(hml32b == 1, na.rm = TRUE),
    #Po (hml32c =1)
    Po_positive = sum(hml32c == 1, na.rm = TRUE),
    #Pv (hml32d =1)
    #Pv_positive = sum(hml32d == 1, na.rm = TRUE),
  )



# Pull malaria-positive cases (RDT is hml35, microscopy is hml32 with positives = 1, negative = 0)

# Create summary for malaria-positive cases
summary_Malaria_positive_cases <- Malaria_positive_cases %>%
  summarize(
    #Sample number
    Total_samples = n(),
    # number of clusters
    unique_clusters = n_distinct(hv001),
    # hv104: Sex breakdown
    male = sum(hv104 == 1, na.rm = TRUE),
    female = sum(hv104 == 2, na.rm = TRUE),
    # hv105: Age breakdown (15-96)
    age_15_30 = sum(hv105 >= 15 & hv105 <= 30, na.rm = TRUE),
    age_31_50 = sum(hv105 >= 31 & hv105 <= 50, na.rm = TRUE),
    age_51_70 = sum(hv105 >= 51 & hv105 <= 70, na.rm = TRUE),
    age_71_96 = sum(hv105 >= 71 & hv105 <= 96, na.rm = TRUE),
    # hml33: Tested for malaria
    tested_for_malaria = sum(hml33 == 0, na.rm = TRUE),
    # hml32: Malaria blood smear positive
    malaria_blood_smear_positive = sum(hml32 == 1, na.rm = TRUE),
    # hml35: Malaria RDT positive
    malaria_RDT_positive = sum(hml35 == 1, na.rm = TRUE),
    #Pf (hml32a =1)
    Pf_positive = sum(hml32a == 1, na.rm = TRUE),
    #Pm (hml32b =1)
    Pm_positive = sum(hml32b == 1, na.rm = TRUE),
    #Po (hml32c =1)
    Po_positive = sum(hml32c == 1, na.rm = TRUE),
    #Pv (hml32d =1)
    #Pv_positive = sum(hml32d == 1, na.rm = TRUE),
  )


# Add source columns to identify the dataframe
summary_rwanda_data_2 <- summary_rwanda_data_2 %>%
  mutate(source = "Rwanda data")
summary_Malaria_positive_cases <- summary_Malaria_positive_cases %>%
  mutate(source = "Malaria-positive cases")

# Combine the two summary tables
combined_summary_cases <- bind_rows(summary_Malaria_positive_cases,
                                    summary_rwanda_data_2)


#--------------------------------------------------------------------
#   Make a summary table for malaria-positive cases by cluster

# Summarize statistics for each cluster
# Group data by cluster number (hv001) and region name (DHSREGNA), and summarize
malaria_pos_cases_summary_by_cluster <- mal_tested_cases %>%
  group_by(hv001, DHSREGNA.y) %>%
  summarize(
    # Total samples
    Total_mal_tested_samples = n(),
    # Sex breakdown: male = 1, female = 2
    male = sum(hv104 == 1, na.rm = TRUE),
    female = sum(hv104 == 2, na.rm = TRUE),
    # Age breakdown (15-96)
    age_15_30 = sum(hv105 >= 15 & hv105 <= 30, na.rm = TRUE),
    age_31_50 = sum(hv105 >= 31 & hv105 <= 50, na.rm = TRUE),
    age_51_70 = sum(hv105 >= 51 & hv105 <= 70, na.rm = TRUE),
    age_71_96 = sum(hv105 >= 71 & hv105 <= 96, na.rm = TRUE),
    # Tested for malaria (hml33 == 0 means tested)
    tested_for_malaria = sum(hml33 == 0, na.rm = TRUE),
    # Malaria blood smear positive (hml32 == 1)
    malaria_blood_smear_positive = sum(hml32 == 1, na.rm = TRUE),
    # Malaria RDT positive (hml35 == 1)
    malaria_RDT_positive = sum(hml35 == 1, na.rm = TRUE),
    # Pf (hml32a == 1)
    Pf_positive = sum(hml32a == 1, na.rm = TRUE),
    # Pm (hml32b == 1)
    Pm_positive = sum(hml32b == 1, na.rm = TRUE),
    # Po (hml32c == 1)
    Po_positive = sum(hml32c == 1, na.rm = TRUE),
    # Pv (hml32d == 1)
    #Pv_positive = sum(hml32d == 1, na.rm = TRUE)
  ) %>%
  # Ungroup the data (optional, if you want to work with a flat dataframe later)
  ungroup()



#-----------------------------------------------------------------------------------------------------------
# Calculate malaria prevalence by cluster (hv001). District = DHSREGNA. Geometry = geographic coordinates
#-----------------------------------------------------------------------------------------------------------

# Function to calculate malaria prevalence by cluster
calculate_percentages <- function(mal_tested_cases) {
  
  mal_prevalence_by_cluster <- mal_tested_cases %>%
    group_by(hv001, DHSREGNA, geometry) %>%
    summarize(
      # Total samples tested per cluster
      Total_samples = n(),
      
      # Percentage of blood smear positive (Mic_Positive)
      Mic_Positive = sum(hml32 == 1, na.rm = TRUE) / Total_samples * 100,
      
      # Percentage of RDT positive
      RDT_Positive = sum(hml35 == 1, na.rm = TRUE) / Total_samples * 100,
      
      # Percentage of Plasmodium falciparum positive (Pf_positive)
      Pf_Positive = sum(hml32a == 1, na.rm = TRUE) / Total_samples * 100,
      
      # Percentage of Plasmodium malariae positive (Pm_positive)
      Pm_Positive = sum(hml32b == 1, na.rm = TRUE) / Total_samples * 100,
      
      # Percentage of Plasmodium ovale positive (Po_Positive)
      Po_Positive = sum(hml32c == 1, na.rm = TRUE) / Total_samples * 100
      
      # Uncomment if you want to include Pv (Plasmodium vivax) positives
      # Pv_Positive = sum(hml32d == 1, na.rm = TRUE) / Total_samples * 100
    ) %>%
    # Ungroup the data (optional, if you want to work with a flat dataframe later)
    ungroup()
  
  return(mal_prevalence_by_cluster)
}

# Use the function to calculate percentages for malaria prevalence
mal_prevalence_by_cluster <- calculate_percentages(mal_tested_cases)

mal_prevalence_by_cluster <- unique(mal_prevalence_by_cluster)

#################################################################################################
#----------------------------- Stats on the entire DHS------------------------------------------#
#################################################################################################


# Breakdown of RDT and microscopy testing by sex for all participants
summary_mal_tested_participants <- rwanda_data_2 %>%
  group_by(sex) %>%
  summarize(
    # Total samples
    Total_individuals = n(),
    # total samples tested for malaria
    tested_for_malaria = sum(hml33 == 0, na.rm = TRUE),
    # RDT and microscopy tested (both hml32 and hml35)
    RDT_and_microscopy_tested = sum((hml32 == 0 | hml32 == 1) & (hml35 == 0 | hml35 == 1), na.rm = TRUE),
    # RDT tested (either 0 or 1)
    RDT_tested = sum(hml35 %in% c(0, 1), na.rm = TRUE),
    # Malaria RDT positive (hml35 == 1)
    RDT_pos = sum(hml35 == 1, na.rm = TRUE),
    # Malaria RDT negative (hml35 == 0)
    RDT_neg = sum(hml35 == 0, na.rm = TRUE),
    # Microscopy tested (hml32 == 0 or 1)
    microscopy_tested = sum(hml32 == 0 | hml32 == 1, na.rm = TRUE),
    # Microscopy positive
    microscopy_pos = sum(hml32 == 1, na.rm = TRUE),
    # Microscopy negative
    microscopy_neg = sum(hml32 == 0, na.rm = TRUE),
    RDTpos_micneg = sum(hml35 == 1 & hml32 == 0, na.rm = TRUE),
    # Microscopy positive, RDT negative
    micpos_RDTneg = sum(hml35 == 0 & hml32 == 1, na.rm = TRUE),
    #Pf (hml32a =1)
    Pf_positive = sum(hml32a == 1, na.rm = TRUE),
    #Pm (hml32b =1)
    Pm_positive = sum(hml32b == 1, na.rm = TRUE),
    #Po (hml32c =1)
    Po_positive = sum(hml32c == 1, na.rm = TRUE),
  ) %>%
  # Ungroup the data (optional, if you want to work with a flat dataframe later)
  ungroup()



# Breakdown of RDT and microscopy testing for DBS by sex
summary_mal_tested_DBS <- DBS_samples %>%
  group_by(sex) %>%
  summarize(
    # Total samples
    Total_DBS_samples = n(),
    # total samples tested for malaria
    tested_for_malaria = sum(hml33 == 0, na.rm = TRUE),
    # RDT and microscopy tested (both hml32 and hml35)
    RDT_and_microscopy_tested = sum((hml32 == 0 | hml32 == 1) & (hml35 == 0 | hml35 == 1), na.rm = TRUE),
    # RDT tested (either 0 or 1)
    RDT_tested = sum(hml35 %in% c(0, 1), na.rm = TRUE),
    # Malaria RDT positive (hml35 == 1)
    RDT_pos = sum(hml35 == 1, na.rm = TRUE),
    # Malaria RDT negative (hml35 == 0)
    RDT_neg = sum(hml35 == 0, na.rm = TRUE),
    # Microscopy tested (hml32 == 0 or 1)
    microscopy_tested = sum(hml32 == 0 | hml32 == 1, na.rm = TRUE),
    # Microscopy positive
    microscopy_pos = sum(hml32 == 1, na.rm = TRUE),
    # Microscopy negative
    microscopy_neg = sum(hml32 == 0, na.rm = TRUE),
    # RDT positive, Microscopy negative
    RDTpos_micneg = sum(hml35 == 1 & hml32 == 0, na.rm = TRUE),
    # Microscopy positive, RDT negative
    micpos_RDTneg = sum(hml35 == 0 & hml32 == 1, na.rm = TRUE),
    #Pf (hml32a =1)
    Pf_positive = sum(hml32a == 1, na.rm = TRUE),
    #Pm (hml32b =1)
    Pm_positive = sum(hml32b == 1, na.rm = TRUE),
    #Po (hml32c =1)
    Po_positive = sum(hml32c == 1, na.rm = TRUE),
  ) %>%
  # Ungroup the data (optional, if you want to work with a flat dataframe later)
  ungroup()


# Breakdown of number of samples collected per month in each district:
DBS_collection_month_by_region <- DBS_samples %>%
  group_by(DHSREGNA) %>%
  summarize(
    # Total samples
    Total_mal_tested_samples = n(),
    # Monthly breakdown:
    Nov_2019 = sum(hv006 == 11, na.rm = TRUE),
    Dec_2019 = sum(hv006 == 12, na.rm = TRUE),
    Jan_2020 = sum(hv006 == 1, na.rm = TRUE),
    Feb_2020 = sum(hv006 == 2, na.rm = TRUE),
    March_2020 = sum(hv006 == 3, na.rm = TRUE),
    April_2020 = sum(hv006 == 4, na.rm = TRUE),
    May_2020 = sum(hv006 == 5, na.rm = TRUE),
    June_2020 = sum(hv006 == 6, na.rm = TRUE),
    July_2020 = sum(hv006 == 7, na.rm = TRUE),
  ) %>%
  # Ungroup the data (optional, if you want to work with a flat dataframe later)
  ungroup()



#-----------------------------MAP of high prev/low prev clusters in RW19-----------------------------
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
library(ggpattern)
library(ggpubr)
library(readxl)
library(survey)
library(writexl)
library(scales)
library(sf)
library(remotes)
library(rgdal)
library(viridis)
library(maps)
library(cartography)
library(tidyverse)
library(forcats)



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
district<- read_sf("C:/Users/jzuromsk/Documents/DHS_2019/District_Boundaries.shp")


# download mapping data (the geography file)
world <- ne_countries(scale = "medium", returnclass = "sf") 
rwanda <-  subset(world, admin == "Rwanda")


# Data is df of all clusters, with >15% malaria prevalence = "high" and <15% prev = "low" in "trans_intens" variable
# Also need latitude (LATNUM) and longitude (LONGNUM) for clusters


# Supp Figure 1: MAP of high prev/ low prev clusters in RW 
cluster_colors<-c('maroon4','turquoise')
shapes<-c(19,1)
ggplot() + 
  #geom_sf(data=admin10) + #Rwanda Provinces
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) + # Rwanda country outline and fill
  geom_sf(data=lakes10, fill="lightblue")+ # Lakes in the area
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey55", size=2 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey55", size=2 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey55", size=2 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
           color="grey55", size=2 , fontface="italic") +
  theme(legend.key.size = unit(0.6, 'cm'), 
        legend.key.height = unit(0.6, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_point(data = dbs_master_vars_clean2_qPCR2, aes(x=LONGNUM, y=LATNUM, fill=trans_intens), 
             size = 2.3, color = "black", shape = 21, stroke = 0.1) + # Data points
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+ scale_shape_manual(values=shapes)+ #Image view (how much of the world do we see?)
  labs(x="",y="",shape="Inclusion in Analysis", fill="Transmission Intensity")


