##-----------------------------------------------------------------------
#                                Pf qPCR data
#------------------------------------------------------------------------

library(rdhs)
library(dplyr)
library(haven)
library(here)
library(labelled)
library(devtools)
library(microbenchmark)
library(survey)
library(readr)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(sf) #see ubuntu issues here: https://rtask.thinkr.fr/installation-of-r-4-0-on-ubuntu-20-04-lts-and-tips-for-spatial-packages/
library(ggspatial)
library(scatterpie)
library(sp)
library(ggplot2)
library(scales)

##### --------------- Re-run when additional qPCR data is available -----------------

### 1. Load sample data into data frames
load_sample_data <- function(path = "C:\\Users\\jzuromsk\\Documents\\DHS_2019\\") {
  # Construct full file paths
  pf_qpcr_tested_path <- file.path(path, "Pf_qPCR_tested_samples.csv")
  pf_qpcr_positive_path <- file.path(path, "Pf_qPCR_positive_samples.csv")
  noisy_samples_path <- file.path(path, "Pf_qPCR_noisy_samples.csv")
  
  # Load the data
  Pf_qPCR_tested_samples <<- read.csv(pf_qpcr_tested_path, stringsAsFactors = FALSE) # n= 7174 samples
  Pf_qPCR_positive_samples <<- read.csv(pf_qpcr_positive_path, stringsAsFactors = FALSE) # n= 638 samples
  Pf_qPCR_noisy_samples <<- read.csv(noisy_samples_path, stringsAsFactors = FALSE) # n= 5 samples
  
  cat("Files loaded successfully:\n",
      "- Pf_qPCR_tested_samples\n",
      "- Pf_qPCR_positive_samples\n",
      "- Pf_qPCR_noisy_samples\n")
}

load_sample_data()  # assumes files are in your working directory


### 2. Create MAIN data frame containing sample status and category- subset, species tested, species-specific qPCR result
#make a new data frame with all scanned ss_malneg, ss_malpos, and ss_highprev samples to track progress
qPCR_RW19DHS <- DBSsamples_instudy_final #13,940 samples
qPCR_RW19DHS <- qPCR_RW19DHS %>% 
  select(barcode, REGION, CLUSTER, bin_cat)
qPCR_RW19DHS <- unique(qPCR_RW19DHS) # Samples = 13,933 (7 samples were scanned into bags twice)
qPCR_RW19DHS <- qPCR_RW19DHS %>% 
  filter(bin_cat %in% c("ss_malneg", "ss_malpos", "ss_highprev")) # 7,194
# Add a column Pf_qPCR_tested with 0 for all samples initially to the whole tracking file
qPCR_RW19DHS$Pf_qPCR_tested <- 0




# Remove duplicate samples from Pf_qPCR_tested # total samples tested = 7174, total unique samples = 7165
Pf_qPCR_tested_samples <- Pf_qPCR_tested_samples %>%
  select(barcode)
Pf_qPCR_tested_samples_unique <- unique(Pf_qPCR_tested_samples)




# Determine which samples were not qPCR tested by comparing tested samples with list of DBS samples scanned OR accidentally added into study 
# 7194 total ss_malneg, ss_malpos, ss_highprev samples expected in study (7188 scanned into bags + 9 unscanned and originally omitted BUT actually extracted and qPCRed)
# Make list of DBS samples expected to be qPCRed
expected_DBS_qPCR <- DBSsamples_instudy_final %>% 
  filter(bin_cat %in% c("ss_malneg", "ss_malpos", "ss_highprev")) #7,197 samples
# Identify samples not qPCR tested by comparing expected_DBS_qPCR and Pf_qPCR_tested_samples
missing_DBS <- setdiff(expected_DBS_qPCR$barcode, Pf_qPCR_tested_samples$barcode)

# Extract rows from expected_DBS_qPCR where barcode is missing in Pf_qPCR_tested_samples
missing_samples_datafram <- expected_DBS_qPCR[expected_DBS_qPCR$barcode %in% missing_DBS, ]
# Extract rows from REFERENCE_scanning_DBS_samples_final where SAMPLEID is missing in list_scanned_DBS_samples
missing_samples_datafram <- REFERENCE_scanning_DBS_samples_final[REFERENCE_scanning_DBS_samples_final$barcode %in% missing_sample_ids, ]



# Add a column Pf_qPCR_result with 0 for all samples initially
qPCR_RW19DHS$Pf_qPCR_result <- 0
# Mark the positive samples as 1 in Pf_qPCR_result
qPCR_RW19DHS$Pf_qPCR_result[qPCR_RW19DHS$barcode %in% Pf_qPCR_positive_samples$barcode] <- 1


# Add a column Pf_qPCR_result with 0 for all samples initially
qPCR_RW19DHS$Pf_qPCR_result <- 0
# Add a column Po_qPCR_tested with 0 for all samples initially to the whole tracking file
qPCR_RW19DHS$Po_qPCR_tested <- 0
# Add a column Po_qPCR_result with 0 for all samples initially
qPCR_RW19DHS$Po_qPCR_result <- 0
# Add a column Pm_qPCR_tested with 0 for all samples initially to the whole tracking file
qPCR_RW19DHS$Pm_qPCR_tested <- 0
# Add a column Pm_qPCR_result with 0 for all samples initially
qPCR_RW19DHS$Pm_qPCR_result <- 0


# Mark the positive samples as 1 in Pf_qPCR_result
Pf_qPCR_tested_samples$Pf_qPCR_result[Pf_qPCR_tested_samples$barcode %in% Pf_qPCR_positive_samples$barcode] <- 1

#make a new data frame with all scanned ss_malneg, ss_malpos, and ss_highprev samples to track progress
qPCR_sample_tracking <- list_scanned_DBS_samples
qPCR_sample_tracking <- qPCR_sample_tracking %>%
  filter(bin_cat %in% c("ss_malneg", "ss_malpos", "ss_highprev"))
# Add a column Pf_qPCR_tested with 0 for all samples initially to the whole tracking file
qPCR_sample_tracking$Pf_qPCR_tested <- 0

# Open qPCR posititve samples list
file_path <- "C:\\Users\\jzuromsk\\Documents\\DHS_2019\\Pf_qPCR_positive_samples.csv"
# Load the CSV file into a dataframe 
Pf_qPCR_positive_samples <- read.csv(file_path, stringsAsFactors = FALSE)

# Open qPCR TESTED samples list
file_path <- "C:\\Users\\jzuromsk\\Documents\\DHS_2019\\Pf_qPCR_tested samples.csv"
# Load the CSV file into a dataframe 
Pf_qPCR_tested_samples <- read.csv(file_path, stringsAsFactors = FALSE)
# Filter to include only the "barcode" column
Pf_qPCR_tested_samples <- Pf_qPCR_tested_samples %>%
  select(barcode)

# Filter to see if there are any repeated samples
Pf_qPCR_tested_samples_unique <- unique(Pf_qPCR_tested_samples)
duplicates <- Pf_qPCR_tested_samples[duplicated(Pf_qPCR_tested_samples$SAMPLEID) | duplicated(Pf_qPCR_tested_samples$SAMPLEID, fromLast = TRUE), ]

#---------------------------Remove noisy/indeterminate samples----------------

#Make directory for noisy qPCR samples
# Open list of noisy qPCR samples
file_path <- "C:\\Users\\jzuromsk\\Documents\\DHS_2019\\Pf_qPCR_noisy_samples.csv"
# Load the CSV file into a dataframe 
noisy_PfqPCR_samples <- read.csv(file_path, stringsAsFactors = FALSE)
# Filter to include only the "barcode" column
noisy_PfqPCR_samples <- noisy_PfqPCR_samples %>%
  select(barcode)

#Add metadata to look at where the noisy samples are from, etc

# Rename the column SAMPLEID to barcode
colnames(noisy_PfqPCR_samples)[colnames(noisy_PfqPCR_samples) == "SAMPLEID"] <- "barcode"
# Merge the sample_list dataframe with the desired columns from DBS_samples
noisy_PfqPCR_samples <- merge(noisy_PfqPCR_samples, DBS_samples[, c("barcode", "CLUSTER", "DHSREGNA", "hml32", "hml32a", "hml32b", "hml32c", "LATNUM", "LONGNUM", "geometry")], 
                           by = "barcode", 
                           all.x = TRUE)

#------------------------Tracking Prevalence and Progress------------------------

#  FOR PREVALENCE TRACKING
# Add a column Pf_qPCR_result with 0 for all samples initially
Pf_qPCR_tested_samples$Pf_qPCR_result <- 0

# Mark the positive samples as 1 in Pf_qPCR_result
Pf_qPCR_tested_samples$Pf_qPCR_result[Pf_qPCR_tested_samples$SAMPLEID %in% Pf_qPCR_positive_samples$SAMPLEID] <- 1

#Mark all NOISY samples as 2 if in noisy_samples
# Rename the column SAMPLEID to barcode
colnames(noisy_PfqPCR_samples)[colnames(noisy_PfqPCR_samples) == "barcode"] <- "SAMPLEID"
Pf_qPCR_tested_samples$Pf_qPCR_result[Pf_qPCR_tested_samples$SAMPLEID %in% noisy_PfqPCR_samples$SAMPLEID] <- 2

# Rename the column SAMPLEID to barcode
colnames(Pf_qPCR_tested_samples)[colnames(Pf_qPCR_tested_samples) == "SAMPLEID"] <- "barcode"

# Add the random_selection variable to qPCR_sample_tracking
Pf_qPCR_tested_samples$random_selection <- ifelse(
  Pf_qPCR_tested_samples$barcode %in% selected_DBS_trial1$barcode, 
  1, 
  0
)

###### FOR PROGRESS TRACKING
# Rename the column SAMPLEID to barcode
colnames(qPCR_sample_tracking)[colnames(qPCR_sample_tracking) == "SAMPLEID"] <- "barcode"
# Add the random_selection variable to qPCR_sample_tracking
qPCR_sample_tracking$random_selection <- ifelse(
  qPCR_sample_tracking$barcode %in% selected_DBS_trial1$barcode, 
  1, 
  0
)

#Add a column for Pf_qPCR_tested to the tested samples
Pf_qPCR_tested_samples$Pf_qPCR_tested <- 1
# Mark the tested samples as 1 in qPCR_sample_tracking
qPCR_sample_tracking$Pf_qPCR_tested[qPCR_sample_tracking$barcode %in% Pf_qPCR_tested_samples$barcode] <- 1

##Mark all NOISY samples as 2 if in noisy_samples
# Rename the column SAMPLEID to barcode
colnames(noisy_PfqPCR_samples)[colnames(noisy_PfqPCR_samples) == "SAMPLEID"] <- "barcode"
qPCR_sample_tracking$Pf_qPCR_tested[qPCR_sample_tracking$barcode %in% noisy_PfqPCR_samples$barcode] <- 2

# Add metadata ()
# Merge the sample_list dataframe with the desired columns from DBS_samples
qPCR_sample_tracking <- merge(qPCR_sample_tracking, DBS_samples[, c("barcode", "hml32", "hml32a", "hml32b", "hml32c", "LATNUM", "LONGNUM", "geometry")], 
                                by = "barcode", 
                                all.x = TRUE)
# Add metadata ()
# Merge the sample_list dataframe with the desired columns from DBS_samples
Pf_qPCR_tested_samples <- merge(Pf_qPCR_tested_samples, DBS_samples[, c("barcode", "CLUSTER", "DHSREGNA", "hml32", "hml32a", "hml32b", "hml32c", "LATNUM", "LONGNUM", "geometry")], 
                            by = "barcode", 
                            all.x = TRUE)


#-------------------------------Malaria prevalence map calculations-------------------------

#----------------------------------------BY CLUSTER---------------------------
# Function to calculate malaria prevalence by cluster
calculate_percentages <- function(Pf_qPCR_tested_samples) {
  
  Pf_qPCR_by_cluster <- Pf_qPCR_tested_samples %>%
    group_by(CLUSTER, DHSREGNA) %>%
    summarize(
      # Total samples tested per cluster
      Total_samples_tested = sum(Pf_qPCR_tested == 1, na.rm = TRUE),
      
      # Percentage of Pf positive
      Pf_Positive = sum(Pf_qPCR_result == 1, na.rm = TRUE) / Total_samples_tested * 100)
}

# Use the function to calculate percentages for malaria prevalence
Pf_qPCR_by_cluster <- calculate_percentages(Pf_qPCR_tested_samples)

#add geographic points to table of prevalence
Map_Pf_qPCR_by_cluster <- Pf_qPCR_by_cluster %>%
  left_join(DBS_samples %>% 
              select(CLUSTER, LATNUM, LONGNUM, geometry), 
            by = "CLUSTER")

Map_Pf_qPCR_by_cluster <- unique(Map_Pf_qPCR_by_cluster)

#----------------------------------------BY REGION---------------------------
# Function to calculate malaria prevalence by region
calculate_percentages <- function(Pf_qPCR_tested_samples) {
  
  Pf_qPCR_by_REGION <- Pf_qPCR_tested_samples %>%
    group_by(DHSREGNA) %>%
    summarize(
      # Total samples tested per region
     #Total_samples_tested = n(),
      Total_samples_tested = sum(Pf_qPCR_tested == 1, na.rm = TRUE),
      
      # Percentage of Pf positive
      Pf_Positive = sum(Pf_qPCR_result == 1, na.rm = TRUE) / Total_samples_tested * 100)
}

# Use the function to calculate percentages for malaria prevalence
Pf_qPCR_by_REGION <- calculate_percentages(Pf_qPCR_tested_samples)


#FOR ONLY RANDOM SELECTION SAMPLES
# Filter the data for random_selection = 1
qPCR_sample_tracking_random_select <- Pf_qPCR_tested_samples %>%
  filter(random_selection == 1)

# Update the function to calculate percentages
calculate_percentages_ss <- function(qPCR_sample_tracking_random_select) {
 
   Pf_qPCR_by_REGION_random_select <- qPCR_sample_tracking_random_select %>%
     group_by(DHSREGNA) %>%
     summarize(
       # Total samples tested per REGION
       #Total_samples_tested = n(),
       Total_samples_tested = sum(Pf_qPCR_tested == 1, na.rm = TRUE),
       
       # Percentage of Pf positive
       Pf_Positive = sum(Pf_qPCR_result == 1, na.rm = TRUE) / Total_samples_tested * 100)
}

# Use the function to calculate percentages for malaria prevalence
Pf_qPCR_by_REGION_random_select <- calculate_percentages(qPCR_sample_tracking_random_select)






#______________________________________________________________________________________________
#____________________________Malaria progress map calculations (ALL SAMPLES)_____________________________________
#____________________________________________________________________________________________

#----------------------------- BY CLUSTER
# Function to calculate percent samples qPCR tested by cluster
calculate_percentages <- function(qPCR_sample_tracking) {
  
  Pf_qPCR_progress_by_cluster <- qPCR_sample_tracking %>%
    group_by(CLUSTER, REGION) %>%
    summarize(
      # Total samples tested per cluster
      Total_samples = n(),
      Total_samples_tested = sum(Pf_qPCR_tested == 1, na.rm = TRUE),
      
      # Percentage of samples tested
      percent_qPCR_tested = sum(Pf_qPCR_tested == 1, na.rm = TRUE) / Total_samples * 100)
}

# Use the function to calculate percentages for Pf qPCR progress
Pf_qPCR_progress_by_cluster <- calculate_percentages(qPCR_sample_tracking)

#add geographic points to table of Pf qPCR progress
Pf_qPCR_progress_by_cluster <- merge(Pf_qPCR_progress_by_cluster, DBS_samples [, c("CLUSTER", "LATNUM", "LONGNUM", "geometry")], 
      by = "CLUSTER", 
      all.x = TRUE)

Pf_qPCR_progress_by_cluster <- unique(Pf_qPCR_progress_by_cluster)

#----------------------------- BY REGION
# Function to calculate percent samples qPCR tested by region

calculate_percentages <- function(qPCR_sample_tracking) {
  
  Pf_qPCR_progress_by_region <- qPCR_sample_tracking %>%
    group_by(REGION) %>%
    summarize(
      # Total samples
      Total_samples = n(),
      # Total samples tested per region
      Total_samples_tested = sum(Pf_qPCR_tested == 1, na.rm = TRUE),
      
      # Percentage of samples tested
      percent_qPCR_tested = sum(Pf_qPCR_tested == 1, na.rm = TRUE) / Total_samples * 100)
}

# Use the function to calculate percentages for Pf qPCR progress
Pf_qPCR_progress_by_region <- calculate_percentages(qPCR_sample_tracking)

#add geographic points to table of Pf qPCR progress



write.csv(selected_DBS_trial1, file = "selected_DBS_trial1.csv", row.names = FALSE)


#_________________________________________________________________________________
#                      MAPPING malaria prevalence by cluster (ALL SAMPLES)
#_________________________________________________________________________________

#load required libraries for base maps
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(rnaturalearth) 
library(rnaturalearthdata)
library(sf) #see ubuntu issues here: https://rtask.thinkr.fr/installation-of-r-4-0-on-ubuntu-20-04-lts-and-tips-for-spatial-packages/
library(ggspatial)
library(ggrepel)
library(base)
library(scatterpie)
library(sp)
library(rdhs)
library(ggplot2)
library(scales)
library(dplyr)

#get admin shape file from naturalearth 
rivers10 <- ne_download(scale = "large", type = 'rivers_lake_centerlines',
                        category = 'physical', returnclass = "sf")
lakes10 <- ne_download(scale = "large", type = 'lakes',
                       category = 'physical', returnclass = "sf")
oceans10 <- ne_download(scale = "large", type = "coastline",
                        category = 'physical', returnclass = "sf")
sov110 <- ne_download(scale="large", type = "sovereignty",
                      category = "cultural", returnclass = "sf")
admin10 <- ne_download(scale="large", type = "admin_1_states_provinces_lines",
                       category = "cultural", returnclass = "sf")
admin110 <- ne_download(scale="large", type = "populated_places",
                        category = "cultural", returnclass = "sf")


# Plot the points with color based on the percent Pf positive in each cluster
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
  geom_point(data = Map_Pf_qPCR_by_cluster, 
             aes(x = LONGNUM, y = LATNUM, fill = Pf_Positive), 
             size = 2, color = "black", shape = 21, stroke = 0.5) +
  scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(color = "Pf Positivity", title = "Pf Positivity by Cluster")
theme_void()




  
#____________________________________________________________________________
#                               Mapping Pf qPCR progress (ALL SAMPLES)
#____________________________________________________________________________


#      Progress for each cluster
# Plot the points with color based on the percent of samples per cluster tested for Pf by qPCR
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
  geom_point(data = Pf_qPCR_progress_by_cluster, 
             aes(x = LONGNUM, y = LATNUM, fill = percent_qPCR_tested), 
             size = 2, color = "black", shape = 21, stroke = 0.5) +
  scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(color = "Percent tested", title = "Percent of samples in each cluster tested by Pf qPCR")
theme_void()












#___________________________________________________________________________
#                               NEEVA'S MAPPING CODE
#____________________________________________________________________________



#can filter from above calls for your country if naturalearth gives correct boundaries
#admin10_ug <- dplyr::filter(admin10, ADM0_A3 == "UGA")
#admin10_tz <-dplyr::filter(admin10, ADM0_A3 == "TZA")
#admin110_ug <-dplyr::filter(admin110, ADM0_A3 == "UGA")
#admin110_cd <- dplyr::filter(admin110, ADM0_A3 == "COD")

#load other shape files if the natural earth ones are incorrect for your country -
#use my rwanda files or else regions will be incorrectly named (in french)
admin10_rw <- st_read("C:\\Users\\jzuromsk\\Documents\\NWYMappingDemoFiles\\rwa_adm1_2006_NISR_WGS1984_20181002.shp")
admin110_rw <-st_read("C:\\Users\\jzuromsk\\Documents\\NWYMappingDemoFiles\\rwa_adm2_2006_NISR_WGS1984_20181002.shp")
natlpark <- st_read("C:\\Users\\jzuromsk\\Documents\\NWYMappingDemoFiles\\EastAfricaParks.kml")

#Must calculate frequencies 
RWHFfreqs <- read.csv("C:\\Users\\jzuromsk\\Documents\\NWYMappingDemoFiles\\dataRW23F.csv")

#Different countries will have different names for admins, I make a function for it as I have to graph mutations in multiple places
mapzone <- function(forgraphing, by){
  districtorg <- forgraphing %>% group_by(mutation_name, !!sym(by))
  districtFreq <- districtorg %>%
    summarise(distFreq=mean(sitefreqWholeNum))
  return (districtFreq)
}

br <- c(0,0.1,2.5,5,7.5,10,15,20,30,40,50,100)

selectMutationADM <- function(mapzone, maparea, mutation, preload, mine){
  Mutprovs <- filter(mapzone, grepl(c(mutation), mutation_name)) %>% rename({{ preload }} := {{ mine }})
  Mutprovs$distFreq[Mutprovs$distFreq == 0] <- 0.00001
  Mutprovs <- right_join(maparea, Mutprovs, by=preload)
  Mutprovs$discrete_freq <- cut(Mutprovs$distFreq, breaks = br, dig.lab = 5)
  return(Mutprovs)
}
selectMutationHF <- function(countryHFfreq, mutation, br) {
  Mutfreqs<- filter(countryHFfreq, grepl(c(mutation), mutation_name))
  Mutfreqs$sitefreqWholeNum[Mutfreqs$sitefreqWholeNum == 0] <- 0.00001
  Mutfreqs$discrete_freq <- cut(Mutfreqs$sitefreqWholeNum, breaks = br, dig.lab=5)
  return(Mutfreqs)
}

#**in a function so you can select different regions - 
#*swap in the commented lines with "district" to see the difference
RWdistfreqs <- mapzone(RWHFfreqs, by = "province")
#RWdistfreqs <- mapzone(RWHFfreqs, by = "district")
RW561Hprovs <- selectMutationADM(RWdistfreqs, admin110_rw, "k13-Arg561His", "ADM1_EN", "province")
#RW561Hprovs <- selectMutationADM(RWdistfreqs, admin110_rw, "k13-Arg561His", "ADM2_EN", "district")
RW561HFfreqs <- selectMutationHF(RWHFfreqs,"k13-Arg561His", br)

# Palette
#make alpha 1 (legend will reflect color)
pal <- hcl.colors(12, "Rocket", rev = TRUE, alpha = 0.8)
#custom labels
labs <- c(0,0.1,2.5,5,7.5,10,15,20,30,40,50,100)
labs_plot <- c("0","0.1-2.5","2.5-5","5-7.5", "7.5-10", "10-15","15-20","20-30", "30-40", "40-50",">50")


##Chloropleth
#**Chloropleth shows the average mutation frequency for each province (based on the HFs in that province)
#* and shows the mutation frequency of each HF in each dot
chloropleth <- ggplot()+
  geom_sf(data=sov110, color='grey80', size=20, alpha = 0.2, stroke=2.5, fill = "grey60") +
  geom_sf(data=admin10, color="grey40", size= 0.6, alpha = 0.1) +
  geom_sf(data = RW561Hprovs,
          aes(fill = discrete_freq), color = NA) +
  scale_fill_manual(values = pal, drop = FALSE, na.value = "grey80", labels = labs_plot,
                    guide = guide_legend(direction = "vertical",
                                         nrow = 12, label.position = "right",
                                         name = "Pf Frequency")) +
  geom_sf(data=admin110_rw, color="grey40", size= 0.6, alpha = 0.1) +
  geom_sf(data=rivers10, color="cyan4", size=0.5, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=oceans10, color="grey40", fill ="aliceblue", size= 0.8) +
  annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = RW561HFfreqs, aes(x = lon, y = lat, fill=discrete_freq), size = 3,
             shape = 21, stroke = 1,
             alpha =0.8) +
  annotate("text", x = 30, y = -0.8, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.15, y = -2.65, label = "Burundi", 
           color="grey60", size=4.5 , fontface="italic") +
  annotate("text", x = 28.9, y = -1.2, label = "DRC", 
           color="grey60", size=6 , fontface="italic") +
  annotate("text", x = 31.3, y = -1.8, label = "Tanzania", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 29.7, y = -1.8, label = "Rwanda", 
           color="grey60", size=5 , fontface="italic") +
  coord_sf(xlim = c(28.8, 31.5), ylim = c(0.2,-2.8), expand = TRUE) +
  theme_void()+
  labs(fill = "% R561H frequency") +
  theme(legend.text=element_text(size=12))
chloropleth

health_facilities <- read.csv("C:/Users/dgiesbrecht-local/Downloads/dataRW23F_fac.csv")

##Points
#plotting location of facilities
facilityloc <- ggplot()+
  geom_sf(data=natlpark, color="palegreen", fill="green2", size=1,alpha =0.1) +
  geom_sf(data=sov110, color='black', size=20, alpha = 0.2) +
  geom_sf(data=rivers10, color="cyan4", size=0.5, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=oceans10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=admin10, color="grey40", size= 0.6, alpha = 0.1) +
  annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = health_facilities, aes(x = lon, y = lat), size = 1.5,
             shape = 21, color= "grey40", fill = "cyan2", stroke = 1,
             alpha =0.8) +
  annotate("text", x = 30, y = -0.8, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.15, y = -2.65, label = "Burundi", 
           color="grey60", size=4.5 , fontface="italic") +
  annotate("text", x = 28.75, y = -1.2, label = "DRC", 
           color="grey60", size=6 , fontface="italic") +
  annotate("text", x = 31.3, y = -1.8, label = "Tanzania", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 29.7, y = -1.8, label = "Rwanda", 
           color="grey60", size=5 , fontface="italic") +
  ##*ADD to add labels to points**
  # geom_text_repel(data = health_facilities, aes(x =lon, y= lat, label = name),
  #                 point.size = NA,
  #                 size = 2,
  #                 color="grey20", fontface="bold",min.segment.length = 0.22, max.overlaps = 10) +
  coord_sf(xlim = c(28.6, 31.6), ylim = c(0.3,-2.8), expand = TRUE) +
  theme_void()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))
facilityloc

##PieCharting
#**I would not recommend my method for determining radii as the scale function hides math
#*and I have not determined how to make a legend for it
#*These pie charts are sized based on how many samples are represented and the wedges represent mutation frequency
#
#PrepColumnsforCharts 
figure2_k13561 <- filter(RWHFfreqs, grepl("k13-Arg561His", mutation_name)) %>% mutate(.,WT = 100-sitefreqWholeNum)%>% spread(mutation_name, sitefreqWholeNum)%>% rename("R561H"="k13-Arg561His")
figure2_k13561$radius <- rescale(figure2_k13561$number, to = c(2, 5))


base <- ggplot()+
  geom_sf(data=rivers10, color="cyan4", size=0.5, alpha=0.5) +
  geom_sf(data=sov110, color='grey10', size=2.5, alpha = 0.2,stroke = 2.5,
          fill = ifelse(sov110$POSTAL == "RW", "grey80", "grey60"))+
  geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +
  geom_sf(data=admin10, color="grey80", size= 0.4) +
  geom_sf(data=admin110, color="grey80", size= 0.4) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.2, y = -2.6, label = "Burundi", 
           color="grey60", size=4 , fontface="italic") +
  annotate("text", x = 28.8, y = -1.5, label = "DRC", 
           color="grey60", size=6 , fontface="italic") +
  annotate("text", x = 30.8, y = -1.25, label = "Tanzania", 
           color="grey60", size=4 , fontface="italic")

#SingleMutation
R561HPies<- base +
  geom_scatterpie(aes(x=lon, y=lat, r = (radius/50)),
                  data = figure2_k13561,
                  cols = c("R561H","WT"),
                  sorted_by_radius= TRUE,
                  color = "grey20",
                  legend_name = "Pfkelch13",
                  alpha=.8)+
  scale_fill_manual(values = c("firebrick","grey80")) +
  coord_sf(xlim = c(28.7, 31), ylim = c(-1,-2.9), expand = TRUE) +
  #This will add a pie-size legend to your figure
  #geom_scatterpie_legend(figure2_k135612$radius*2, x=28.7, y=2.3, labeller = function(x) round(x * 100/2)) +
  theme_void()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))
R561Pies


#combiningMulitple-Haplotypes
#please consider my math is designed for Pooled sampling and 
#would not be appropriate for individual represention
figure2_k13675 <- filter(RWHFfreqs, grepl("k13-Ala675Val", mutation_name)) %>% mutate(.,WT = 100-sitefreqWholeNum)%>% spread(mutation_name, sitefreqWholeNum)%>% rename("A675V"="k13-Ala675Val")
figure2_k13675$radius <- rescale(figure2_k13675$number, to = c(2, 5))
combinedk13Pie <- figure2_k13561 %>% select(name,R561H)
combinedk13Pie <- left_join(figure2_k13675,combinedk13Pie) %>% select(-WT) %>% mutate(totalMT=A675V+R561H) %>% mutate( WT = 100-totalMT)

multipie <- base + 
  geom_scatterpie(aes(x=lon, y=lat, r = (radius/50)), 
                  data = combinedk13Pie, 
                  cols = c("R561H","A675V","WT"), 
                  sorted_by_radius = TRUE,
                  color = "grey20",
                  alpha=.8)+
  scale_fill_manual(values = c("firebrick","cyan3","grey80")) +
  coord_sf(xlim = c(28.7, 31), ylim = c(-1,-2.9), expand = TRUE) +
  #geom_scatterpie_legend((figure2_k13441$radius/50)[1:18], x=28.8, y=-1.5, labeller = function(x) (x*100)) +
  #geom_scatterpie_legend((figure2_k13441$radius/50), x=28.8, y=-1.5)+
  theme_void()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))
multipie
