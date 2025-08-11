#--------------------------------------------------------------------------------------#
#                                 Sample selection                             
#--------------------------------------------------------------------------------------#

library(rdhs)
library(dplyr)
library(haven)
library(here)
library(labelled)
library(devtools)
library(microbenchmark)
library(survey)
library(tidyr)


#--------------------------------------------------------------------------------------#
#                     Trial 1: Selecting 50% of samples from each cluster  
#--------------------------------------------------------------------------------------#

#df is selected_DBS_trial1
# Randomly select 50% of samples in each cluster (cluster = hv001)
selected_DBS_trial1 <- DBS_samples %>%
  group_by(hv001) %>%
  sample_frac(0.5) %>%
  ungroup()

# Verify the result
head(selected_DBS_trial1)

# Summary table
summary_table <- selected_DBS_trial1 %>%
  summarise(
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
  )

library(dplyr)

# Create summary for selected_DBS_trial1
summary_selected_DBS_trial1 <- selected_DBS_trial1 %>%
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
  )

# Create summary for DBS_samples
summary_DBS_samples <- DBS_samples %>%
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
  )

# Add source column to identify the dataframe
summary_selected_DBS_trial1 <- summary_selected_DBS_trial1 %>%
  mutate(source = "selected_DBS_trial1")

summary_DBS_samples <- summary_DBS_samples %>%
  mutate(source = "DBS_samples")

# Combine the two summary tables
combined_summary1 <- bind_rows(summary_selected_DBS_trial1, summary_DBS_samples)

# Move 'Summary_Type' to the first column
combined_summary1 <- combined_summary1 %>%
  select(source, everything())

#____________________________________________________________________________________________
#  Make a map showing number of samples per cluster in selected trial 1 to compare to all DBS


# Load necessary libraries
library(ggplot2)
library(dplyr)


# Step 1: Make a df for malaria-tested DBS with geometry, latitude, and longitude
map_selected_DBS_trial1 <- selected_DBS_trial1 %>%
  select(hv001, LATNUM, LONGNUM, geometry)


# Step 2: Group by cluster and count the number of samples in each cluster
cluster_counts <- map_selected_DBS_trial1 %>%
  group_by(geometry) %>%
  summarise(sample_count = n())

# Step 3: Join the counts back to the original dataset
map_selected_DBS_trial1_clustercounts <- map_selected_DBS_trial1 %>%
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
  geom_point(data = map_selected_DBS_trial1_clustercounts, 
             aes(x = LONGNUM, y = LATNUM, fill = sample_count), 
             size = 1, color = "black", shape = 21, stroke = 0.5) +
  scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(color = "Samples per Cluster", title = "Selected DBS Trial 1: Samples Per Cluster")
theme_void()


#--------------------------------------------------------------------------------------#
#   Trial 2: Taking samples selected in trial 1 AND adding all malaria-positive cases
#--------------------------------------------------------------------------------------#

# Add the malaria-positive DBS to the df
selected_DBS_trial2 <- rbind(Malaria_positive_DBS, selected_DBS_trial1)

# Remove duplicate samples
selected_DBS_trial2 <- unique(selected_DBS_trial2)

# Create summary for selected_DBS_trial2
summary_selected_DBS_trial2 <- selected_DBS_trial2 %>%
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
  )

# Add source column to identify the dataframe
summary_selected_DBS_trial2 <- summary_selected_DBS_trial2 %>%
  mutate(source = "selected_DBS_trial2")

# Create summary for Malaria_positive_DBS
summary_Malaria_positive_DBS <- Malaria_positive_DBS %>%
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
  )

# Add source column to identify the dataframe
summary_Malaria_positive_DBS <- summary_Malaria_positive_DBS %>%
  mutate(source = "Malaria-positive DBS")


# Combine the four summary tables
combined_summary1_2_malpos <- bind_rows(summary_selected_DBS_trial1, 
                                        summary_selected_DBS_trial2, 
                                        summary_Malaria_positive_DBS,
                                        summary_DBS_samples)

# Move 'Summary_Type' to the first column
combined_summary1_2_malpos <- combined_summary1_2_malpos %>%
  select(source, everything())

#_______________________________________________________________________________________________________
#Calculate PERCENTAGES to determine major changes in sample stats when adding malaria-positive samples
#_______________________________________________________________________________________________________


# Function to calculate percentages for the summary statistics
calculate_percentages <- function(selected_DBS_trial1) {
  total_samples <- nrow(selected_DBS_trial1)
  
  selected_DBS_trial1  %>%
    summarise(
      Unique_Clusters = n_distinct(hv001),  # Number of unique clusters (stays the same)
      Sex_Distribution_Male = sum(hv104 == 1, na.rm = TRUE) / total_samples * 100,  # Percentage of males
      Sex_Distribution_Female = sum(hv104 == 2, na.rm = TRUE) / total_samples * 100,  # Percentage of females
      age_15_30 = sum(hv105 >= 15 & hv105 <= 30, na.rm = TRUE) / total_samples * 100, #Percentage age 15-30
      age_31_50 = sum(hv105 >= 31 & hv105 <= 50, na.rm = TRUE) / total_samples * 100, #Percentage age 31-50,
      age_51_70 = sum(hv105 >= 51 & hv105 <= 70, na.rm = TRUE) / total_samples * 100, #Percentage age 51-70,
      age_71_96 = sum(hv105 >= 71 & hv105 <= 96, na.rm = TRUE) / total_samples * 100, #Percentage age 71-96,
      Sex_Distribution_Male = sum(hv104 == 1, na.rm = TRUE) / total_samples * 100,  # Percentage of males
      Sex_Distribution_Female = sum(hv104 == 2, na.rm = TRUE) / total_samples * 100,  # Percentage of females
      Tested_for_Malaria_Yes = sum(hml33 == 0, na.rm = TRUE) / total_samples * 100,  # Percentage of those tested for malaria
      Blood_Smear_Positive = sum(hml32 == 1, na.rm = TRUE) / total_samples * 100,  # Percentage blood smear positive
      RDT_Positive = sum(hml35 == 1, na.rm = TRUE) / total_samples * 100,  # Percentage RDT positive
    )
}

# Calculate percentages for selected_DBS_trial1
percentage_selected_DBS_trial1 <- calculate_percentages(selected_DBS_trial1)

# Calculate percentages for selected_DBS_trial2
percentage_selected_DBS_trial2 <- calculate_percentages(selected_DBS_trial2)

# Calculate percentages for DBS_samples
percentage_DBS_samples <- calculate_percentages(DBS_samples)

# Combine the three percentage summary tables
combined_percentage_summary1_2 <- bind_rows(percentage_selected_DBS_trial1, 
                                            percentage_selected_DBS_trial2, 
                                            percentage_DBS_samples)

#____________________________________________________________________________________________
#  Make a map showing number of samples per cluster in selected trial 1 to compare to all DBS


# Load necessary libraries
library(ggplot2)
library(dplyr)


# Step 1: Make a df for malaria-tested DBS with geometry, latitude, and longitude
map_selected_DBS_trial2 <- selected_DBS_trial2 %>%
  select(hv001, LATNUM, LONGNUM, geometry)


# Step 2: Group by cluster and count the number of samples in each cluster
cluster_counts <- map_selected_DBS_trial2 %>%
  group_by(geometry) %>%
  summarise(sample_count = n())

# Step 3: Join the counts back to the original dataset
map_selected_DBS_trial2_clustercounts <- map_selected_DBS_trial2 %>%
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
  geom_point(data = map_selected_DBS_trial2_clustercounts, 
             aes(x = LONGNUM, y = LATNUM, fill = sample_count), 
             size = 1, color = "black", shape = 21, stroke = 0.5) +
  scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(color = "Samples per Cluster", title = "Selected DBS Trial 2: Samples Per Cluster")
theme_void()

#--------------------------------------------------------------------------------------#
#   
#--------------------------------------------------------------------------------------#

# Add the malaria-positive DBS to the df
selected_DBS_trial2 <- rbind(Malaria_positive_DBS, selected_DBS_trial1)


