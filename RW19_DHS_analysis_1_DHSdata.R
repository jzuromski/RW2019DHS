library(rdhs)
library(dplyr)
library(haven)
library(here)
library(labelled)
library(devtools)
library(microbenchmark)
library(survey)
library(readr)

#----------------------------- Obtain the data sets from DHS website and load into R ----------------

## Step 1: Configure DHS access by setting up your credentials
set_rdhs_config(email = "jenna_zuromski@brown.edu",
                project = "Temporal analysis of malaria prevalence and geospatial risk in Rwanda",
                config_path = "rdhs.json",
                cache_path = "DHS_2019",
                data_frame = "data.table::as.data.table",
                global = FALSE)


# and now let's then grab this data by specifying the countryIds and the survey year starts
data <- dhs_data(tagIds = c("33"), countryIds = "RW",breakdown="subnational",surveyYearStart = 2018)
     
#obtain survey datasets
surveys <- dhs_surveys(countryIds =  "RW", #rwanda
                       surveyType = "DHS",
                       surveyYearStart = 2018) #return the desired surveys

datasets <- dhs_datasets(surveyIds = surveys$SurveyId, fileFormat = "FL", fileType = ("PR")) #household member recode
str(datasets)
downloads <- get_datasets(datasets$FileName, clear_cache = TRUE)

cluster_data <- cdpr

# read in our dataset
cdpr2 <- readRDS(downloads$RWPR81FL)

# let's look at the variable_names
head(get_variable_labels(cdpr))

# rapid diagnostic test search
questions <- search_variable_labels(datasets$FileName, search_terms = c("RDT", "HIV", "malaria", 'Net'))

table(questions$dataset_filename)

downloaded_data <- get_datasets(datasets$FileName, clear_cache = TRUE)

#read datasets
#pull certain variables of interest
vars <- search_variable_labels(
  datasets$FileName,
  search_terms = c("malaria rapid test",
                   "date", "interview",
                   "diarrhea recently", "species",
                   "fever .* two weeks",
                   "breaths", 
                   "malaria", 
                   "HIV",
                   "bar code",
                   "species",
                   "result",
                   "cluster", "household", "urban", "rural", "Ease of Access to Treatment",
                   "Province", "region", "Sample Weight", "weight",
                   "net", "vaccination",
                   "treatment", "housing type",
                   "blood sample ID number")) #pull variables related to these terms

extract <- extract_dhs(vars, add_geo = TRUE) #extract the data

rwanda_data <- rbind_labelled(extract[2], variables = vars) #only include household member recode but with barcodes
rwanda_hiv_data <- rbind_labelled(extract[1], variables = vars)

# Remove specific columns by name
rwanda_data_2 <- rwanda_hiv_data %>% select(-hv015, -hv044, -hv225, -hv228, -hv230a, -hv238, -hv242, -hv252, 
                                          -sh124b, -hv120, -sb315f, -sb338, -sb339, -ha65, -ha70, -hc2, -hc7, 
                                          -hc8, -hc9, -hc10, -hc12, -hc13, -hc2b, -hb65, -hml36, -sb247, -mha70, -sb505a)

# Clean up variables

#for malaria, set 6 (Test undetermined), 7 (sample not found in lab database), and 9(missing) to NA 
rwanda_data_2$hml32[which(rwanda_data_2$hml32 == 6)] = NA
rwanda_data_2$hml32[which(rwanda_data_2$hml32 == 7)] = NA
rwanda_data_2$hml32[which(rwanda_data_2$hml32 == 9)] = NA
rwanda_data_2$hml32a[which(rwanda_data_2$hml32a == 9)] = NA
rwanda_data_2$hml32b[which(rwanda_data_2$hml32b == 9)] = NA
rwanda_data_2$hml32c[which(rwanda_data_2$hml32c == 9)] = NA
rwanda_data_2$hml32d[which(rwanda_data_2$hml32d == 9)] = NA
rwanda_data_2$hml32e[which(rwanda_data_2$hml32e == 9)] = NA
rwanda_data_2$hml32f[which(rwanda_data_2$hml32f == 9)] = NA
rwanda_data_2$hml32g[which(rwanda_data_2$hml32g == 9)] = NA
rwanda_data_2$hml35[which(rwanda_data_2$hml35 == 9)] = NA


#intial look at data for 
length(which(rwanda_data_2$ha62 == 1))

#-------------------------- Pull out individuals who have DBS barcodes

# Ensure the column is of character type (if not already)
rwanda_data_2$ha62 <- as.character(rwanda_data_2$ha62)
rwanda_data_2$hb62 <- as.character(rwanda_data_2$hb62)

# Regular expression for a 5-character string containing both letters and numbers
pattern <- "^(?=.*[A-Za-z])(?=.*[0-9])[A-Za-z0-9]{5}$"

# Count the number of rows that match the pattern
HIV_Barcode_ID_Women <- sum(grepl(pattern, rwanda_data_2$ha62, perl = TRUE))
HIV_Barcode_ID_Men <- sum(grepl(pattern, rwanda_data_2$hb62, perl = TRUE))

# Print the result, 13,941
cat("Number HIV barcodes Women:", HIV_Barcode_ID_Women, "\n") # n=7354
cat("Number HIV barcodes Men:", HIV_Barcode_ID_Men, "\n") # n=6587
cat("Number HIV barcodes total:", total_matching_barcodes, "\n") #13941

# Calculate the total number of matching barcodes
total_matching_barcodes <- HIV_Barcode_ID_Women + HIV_Barcode_ID_Men
# Print the result, 13,941
cat("Number of rows with a 5-character barcode containing both letters and numbers:", total_matching_barcodes, "\n")

# Extract the 5-character barcodes from ha62 and hb62
HIV_Barcode_ID_Women1 <- rwanda_data_2$ha62[grepl(pattern, rwanda_data_2$ha62, perl = TRUE)]
HIV_Barcode_ID_Men1 <- rwanda_data_2$hb62[grepl(pattern, rwanda_data_2$hb62, perl = TRUE)]

# Combine the barcodes into a single vector
all_barcodes <- c(HIV_Barcode_ID_Men1, HIV_Barcode_ID_Women1)

# Create a new dataframe with a single column labeled "HIV barcodes"
hiv_barcodes_df <- data.frame("HIV barcodes" = all_barcodes)

#------------------------------------------------------------------------------------##
#Create new dataframe called DBS_samples to retain only individuals who provided DBS 
#------------------------------------------------------------------------------------##

# Regular expression for a 5-character string containing both letters and numbers
pattern <- "^(?=.*[A-Za-z])(?=.*[0-9])[A-Za-z0-9]{5}$"

# Identify rows where ha62 or hb62 contains the 5-character barcode
rows_with_barcodes <- grepl(pattern, rwanda_data_2$ha62, perl = TRUE) | 
  grepl(pattern, rwanda_data_2$hb62, perl = TRUE)

# Create new column "barcodes" in the rwanda_data_2 dataframe to provide DBS barcode
rwanda_data_2 <- rwanda_data_2 %>%
  mutate(
    barcode = case_when(
      !is.na(ha62) & grepl(pattern, ha62, perl = TRUE) ~ ha62,
      !is.na(hb62) & grepl(pattern, hb62, perl = TRUE) ~ hb62,
      TRUE ~ NA_character_
    )
  )

# Create new column "sex" in the rwanda_data_2 dataframe to provide sex
rwanda_data_2 <- rwanda_data_2 %>%
  mutate(
    sex = case_when(
        hv104 == 1 ~ "male",  # If hv104 is 1, assign "male"
        hv104 == 2 ~ "female",  # If hv104 is 2, assign "female"
        TRUE ~ NA_character_  # Otherwise, assign NA (if there are any other values in hv104)
      )
    )

# Create new column "barcodes" in the DBS_samples dataframe to provide DBS barcode
DBS_samples <- rwanda_data_2 %>%
  filter(rows_with_barcodes) %>%
  mutate(
    barcode = case_when(
      !is.na(ha62) & grepl(pattern, ha62, perl = TRUE) ~ ha62,
      !is.na(hb62) & grepl(pattern, hb62, perl = TRUE) ~ hb62,
      TRUE ~ NA_character_
    )
  )
#------------------------------------------------------------------------------
#       Rename columns for DBS_samples
#------------------------------------------------------------------------------

DBS_samples_varname <- DBS_samples

# Rename the columns
colnames(DBS_samples_varname)[colnames(DBS_samples_varname) == "hml32a"] <- "Pf"
colnames(DBS_samples_varname)[colnames(DBS_samples_varname) == "hml32b"] <- "Pm"
colnames(DBS_samples_varname)[colnames(DBS_samples_varname) == "hml32c"] <- "Po"
colnames(DBS_samples_varname)[colnames(DBS_samples_varname) == "hml32d"] <- "Pv"
colnames(DBS_samples_varname)[colnames(DBS_samples_varname) == "hml32"] <- "blood_smear_result"
colnames(DBS_samples_varname)[colnames(DBS_samples_varname) == "hml35"] <- "RDT_result"
colnames(DBS_samples_varname)[colnames(DBS_samples_varname) == "hv105"] <- "Age"
colnames(DBS_samples_varname)[colnames(DBS_samples_varname) == "hv104"] <- "Sex"


#---------------------------------------------------------------------------##
#Look at the individuals who provided DBS AND were tested for malaria 
#---------------------------------------------------------------------------##

###Make a dataset called mal_tested_DBS in which all individuals tested for malaria have DBS
# Pull all malaria-negative samples (where hml32 or hml35 is equal to 0)
MICneg_dbs <- DBS_samples[!is.na(DBS_samples$hml32) & DBS_samples$hml32 == 0, ]
RDTneg_dbs <- DBS_samples[!is.na(DBS_samples$hml35) & DBS_samples$hml35 == 0, ]

# Filter rows where any of the specified columns contain a 1
# The following variables are important: 
  #HML32 Final result of malaria from blood smear test
  #HML32A Presence of species: falciparum (Pf)
  #HML32B Presence of species: malariae (Pm)
  #HML32C Presence of species: ovale (Po)
  #HML32D Presence of species: vivax (Pv)
  #HML35 Result of malaria rapid test

##Make a df for blood smear positive cases with DBS (n=36)
# Ensure the hml32 column is numeric
DBS_samples$hml32 <- as.numeric(DBS_samples$hml32)

# Filter DBS_samples to retain only rows where a 1 is present in the column hml32 (blood smear positive)
MICpos_dbs <- DBS_samples[!is.na(DBS_samples$hml32) & DBS_samples$hml32 == 1, ]

##Make a df for RDT positive cases with DBS (n=86)
# Ensure the hml35 column is numeric
DBS_samples$hml35 <- as.numeric(DBS_samples$hml35)
# Filter DBS_samples to retain only rows where a 1 is present in the column hml32
RDTpos_dbs <- DBS_samples[!is.na(DBS_samples$hml35) & DBS_samples$hml35 == 1, ]

## Combine the blood smear and RDT-positive dataframes into Malaria_positive_DBS (n=101)
Malaria_positive_DBS <- rbind(MICpos_dbs, RDTpos_dbs)
# Remove duplicate rows
Malaria_positive_DBS <- unique(Malaria_positive_DBS)

# Filter to keep only rows where both hml32 and hml35 are 1 (n=21)
doublepos_DBS <- Malaria_positive_DBS %>%
  filter(hml32 == 1 & hml35 == 1)

# CHECK on the filter to keep only rows where both hml32 and hml35 are 1 by looking at entire DBS samples df (n=21)
doublepos_DBS1 <- DBS_samples %>%
  filter(hml32 == 1 & hml35 == 1)

#Combine MICneg, MICpos, RDTneg, RDTpos DBS samples into one dataframe
mal_tested_DBS <- rbind(MICpos_dbs, RDTpos_dbs, MICneg_dbs, RDTneg_dbs)
#remove duplicates **n= 7354**
mal_tested_DBS <- unique(mal_tested_DBS)

# Preview the filtered data
head(mal_tested_DBS)


#-------------------------------------------------------------------------------------------------##
#     Change interview date variable (hv008a) data format from Century Day Code (CDC) to DDMMYY
#-------------------------------------------------------------------------------------------------##

# Load necessary library
library(lubridate)

#make a new dataframe to test in
mal_tested_DBS1 <- mal_tested_DBS

# Ensure that 'hv008a' is a character or factor before converting
mal_tested_DBS1$hv008a <- as.character(mal_tested_DBS1$hv008a)
# Convert 'hv008a' to numeric
mal_tested_DBS1$hv008a <- as.numeric(mal_tested_DBS1$hv008a)
# Inspect the first few rows of the 'hv008a' column
print(head(mal_tested_DBS1$hv008a))

# Create function to convert CDC to Date
cdc_to_date <- function(cdc) {
  if (is.na(cdc) || !is.numeric(cdc) || cdc <= 0) {
    return(NA)  # Return NA for invalid or missing CDC values
  }
  # Base date is January 1, 1900
  base_date <- as.Date("1900-01-01")
  # Calculate the actual date by adding CDC days to the base date
  actual_date <- base_date + days(cdc - 1)  # Subtract 1 because CDC starts from day 1
  # Format the date as DDMMYY
  formatted_date <- format(actual_date, "%d%m%y")
  return(formatted_date)
}

# Ensure 'hv008a' is numeric
mal_tested_DBS1$hv008a <- as.numeric(mal_tested_DBS1$hv008a)
# Apply the function to the 'hv008a' variable
mal_tested_DBS1$interview_DMY <- sapply(mal_tested_DBS1$hv008a, function(x) cdc_to_date(x))
# Ensure that 'interview_DMY' is properly assigned and converted to character
mal_tested_DBS1$interview_DMY <- as.character(mal_tested_DBS1$interview_DMY)

#-------------------------------------------------------------------------------------------------------------
#               Look at all malaria-tested AND malaria-positive samples from DHS
#-------------------------------------------------------------------------------------------------------------

# Ensure the hml32, hml33, hml35 columns are numeric
rwanda_data_2$hml32 <- as.numeric(rwanda_data_2$hml32)
rwanda_data_2$hml35 <- as.numeric(rwanda_data_2$hml35)
rwanda_data_2$hml33 <- as.numeric(rwanda_data_2$hml33)

# Pull all malaria-negative samples (where hml32 or hml35 is equal to 0)
MICneg <- rwanda_data_2[!is.na(rwanda_data_2$hml32) & rwanda_data_2$hml32 == 0, ] #n=10981
RDTneg <- rwanda_data_2[!is.na(rwanda_data_2$hml35) & rwanda_data_2$hml35 == 0, ] #n=10856

# # Make malaria-tested dataframe by retaining only rows where a 0 is present in the column hml33 (0= tested)
malaria_tested <- rwanda_data_2[!is.na(rwanda_data_2$hml33) & rwanda_data_2$hml33 == 0, ] #n=11053

# # Make microscopy-positive dataframe by retaining only rows where a 1 is present in the column hml32
MICpos <- rwanda_data_2[!is.na(rwanda_data_2$hml32) & rwanda_data_2$hml32 == 1, ] #n=70

# Make RDT-positive dataframe by retaining only rows where a 1 is present in the column hml35
RDTpos <- rwanda_data_2[!is.na(rwanda_data_2$hml35) & rwanda_data_2$hml35 == 1, ] #n=185

## Combine the blood smear and RDT-positive dataframes into Malaria_positive_cases and remove duplicate rows (n=207)
Malaria_positive_cases <- rbind(MICpos, RDTpos) #n=255
Malaria_positive_cases <- unique(Malaria_positive_cases) # Remove duplicates, n=207

# Filter to keep only rows where both hml32 and hml35 are 1 (n=48)
doublepos_cases <- Malaria_positive_cases %>%
  filter(hml32 == 1 & hml35 == 1)

# CHECK on the filter to keep only rows where both hml32 and hml35 are 1 by looking at entire DHS samples df (n=48)
doublepos_cases <- rwanda_data_2 %>%
  filter(hml32 == 1 & hml35 == 1)


# Combine MICneg, MICpos, RDTneg, RDTpos DBS samples into one dataframe and remove duplicates
mal_tested_cases <- rbind(MICpos, RDTpos, MICneg, RDTneg) #n=22092
mal_tested_cases <- unique(mal_tested_cases) #remove duplicates **n= 11053**


# Summarize data
# 1. make new "malaria" binary variable
rwanda_data_2 <- rwanda_data_2 %>%
  mutate(malaria= ifelse(hml32 == 1 | hml35 == 1, 1, 0),
         both_pos = ifelse(hml32 == 1 & hml35 ==1, 1, 0))

sum(rwanda_data_2$malaria == 1, na.rm = TRUE) #207 malaria positive samples (matches what I have above)



# Make new binary variable for malaria (1= positive by RDT or microscopy, 0 = negative for malaria by both RDT and microscopy)
# Ensure the hml32, hml33, hml35 columns are numeric
DBS_samples$hml32 <- as.numeric(DBS_samples$hml32)
DBS_samples$hml35 <- as.numeric(DBS_samples$hml35)
DBS_samples$hml33 <- as.numeric(DBS_samples$hml33)


DBS_samples <- DBS_samples %>%
  mutate(malaria= ifelse(hml32 == 1 | hml35 == 1, 1, 0),
  both_pos = ifelse(hml32 == 1 & hml35 ==1, 1, 0))


sum(DBS_samples$malaria == 1, na.rm = TRUE) #101
sum(is.na(DBS_samples$malaria))


sum(DBS_samples$hml32 == 0, na.rm = TRUE) #7317
sum(DBS_samples$hml32 == 1, na.rm = TRUE) #36

sum(DBS_samples$hml33 == 0, na.rm = TRUE) #7354


for(i in 1:nrow(DBS_samples)){
  if(is.na(DBS_samples$hml35[i]) & is.na(DBS_samples$hml32[i]) == TRUE){ #if both are NA
    DBS_samples$malaria1[i] <- NA #set to NA
  }
  #if one is NA and the other isn't, take non-na value
  else if(is.na(DBS_samples$hml35[i]==T)){#if only hml35 is NA
    DBS_samples$malaria1[i] <- DBS_samples$hml32[i] #take value from hml32
  }
  else if(is.na(DBS_samples$hml32[i]==T)){#if only hml32 is NA
    DBS_samples$malaria1[i] <- DBS_samples$hml35[i] #take value from hml35
    
  }
  #if neither are NA, take positive if there is one
  else if(DBS_samples$hml35[i] == 1){#if rdt is positive
    DBS_samples$malaria1[i] <- 1 #set to positive
  }
  else if(DBS_samples$hml32[i] == 1){#if microscopy is positive
    DBS_samples$malaria1[i] <- 1 #set to positive
  }
  else{ #otherwise (if both are 0)
    DBS_samples$malaria1[i] <- 0 #set to negative
  }
}

sum(DBS_samples$malaria1 == 1, na.rm = TRUE) #101


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




#-----------------------------------------------------------
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



#------------------------------------------------------------------------------------
# Calculate malaria prevalence by cluster
#-----------------------------------------------------------------------------------

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

# Now, if you want to filter for malaria-positive clusters (e.g., Mic or RDT positive)
Mic_pos_cluster <- mal_prevalence_by_cluster %>%
  filter(!is.na(Mic_Positive) & Mic_Positive > 0)

#add geographic points to table of prevalence by microscopy
Map_mic_mal_prev_cluster <- Mic_pos_cluster %>%
  left_join(map_malaria_positive_cases %>% 
              select(hv001, LATNUM, LONGNUM, geometry), 
            by = "hv001")

Map_mic_mal_prev_cluster <- unique(Map_mic_mal_prev_cluster)

RDT_pos_cluster <- mal_prevalence_by_cluster %>%
  filter(!is.na(RDT_Positive) & RDT_Positive > 0)

#add geographic points to table of prevalence by microscopy
Map_RDT_mal_prev_cluster <- RDT_pos_cluster %>%
  left_join(map_malaria_positive_cases %>% 
              select(hv001, LATNUM, LONGNUM, geometry), 
            by = "hv001")

Map_RDT_mal_prev_cluster <- unique(Map_RDT_mal_prev_cluster)


# Combine the positive clusters into one data frame
Malaria_prevalence_by_cluster <- bind_rows(Mic_pos_cluster, RDT_pos_cluster)

# Remove duplicate rows
Malaria_prevalence_by_cluster <- unique(Malaria_positive_clusters)

# Merge the geographic points into the Malaria_positive_clusters dataframe
map_Malaria_prevalence_by_cluster <- Malaria_prevalence_by_cluster %>%
  left_join(map_malaria_positive_cases %>% 
              select(hv001, LATNUM, LONGNUM, geometry), 
            by = "hv001")

map_Malaria_prevalence_by_cluster <- unique(map_Malaria_prevalence_by_cluster) #remove duplicates

write_csv(map_Malaria_prevalence_by_cluster, 'Malaria_prevalence_by_cluster.csv')

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


# Breakdown of number of samples collected per month in each cluster:
DBS_collection_month_by_cluster <- DBS_samples %>%
  group_by(CLUSTER) %>%
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

# Breakdown of 
