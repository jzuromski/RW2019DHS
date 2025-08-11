#------------------------------------------------------------------------------#
#                           Finalizing the DHS sample set 
#------------------------------------------------------------------------------#

  
#--------------------------------------------------------------------------------------------
#                   Make new variable "malaria_test_result" to indicate positivity
#--------------------------------------------------------------------------------------------

# Create a new variable `malaria_result` in `DBS_samples`
DBS_samples$malaria_result <- ifelse(
  (!is.na(DBS_samples$hml32) & DBS_samples$hml32 == 1) | 
    (!is.na(DBS_samples$hml35) & DBS_samples$hml35 == 1), 
  1, # Positive for malaria
  0  # Negative for malaria (including cases where values are NA)
)

# Check the number of positive and negative samples
positive_count <- sum(DBS_samples$malaria_result == 1)
negative_count <- sum(DBS_samples$malaria_result == 0)

# Print the counts
cat("Number of malaria-positive samples:", positive_count, "\n") #n= 101
cat("Number of malaria-negative samples:", negative_count, "\n") #n= 13840

# Optional: View the first few rows of the data frame to verify changes
head(DBS_samples)

  
#-----------------------------------------------------------------------------#
#             Making a data frame with number of samples by region
#-----------------------------------------------------------------------------#
  
  # Assuming your data frame is called 'Samples by region' and the region variable is 'DHSREGNA'
  
  # Count the number of samples for each region and convert to data frame
  region_counts_all_barcodes <- as.data.frame(table(DBS_samples$DHSREGNA))

# Rename the columns of the resulting data frame
names(region_counts_all_barcodes) <- c("Region", "Sample_Count")

# Display the data frame
print(region_counts_all_barcodes)


#--------------------------------------------------------------------------
#       Making data frame for sample scanning into categories (bins)
#--------------------------------------------------------------------------

# First, make a new variable "bin_cat" 
# indicate whether samples were randomly selected into the qPCR sample subset (selected_DBS_trial2)

# Check if 'selected_DBS_trial2' has any rows
if (nrow(selected_DBS_trial2) > 0) {
  
  # Ensure both data frames have a common identifier column
  if ("barcode" %in% colnames(DBS_samples) && "barcode" %in% colnames(selected_DBS_trial2)) {
    
    # Create 'bin_cat' variable based on membership in 'selected_DBS_trial_2'
    DBS_samples$bin_cat <- ifelse(
      DBS_samples$barcode %in% selected_DBS_trial2$barcode, 
      "Selected",  # Indicates samples in the subset
      "Not Selected"  # Indicates samples not in the subset
    )
    
  } else {
    stop("Error: The 'barcode' column is missing from one or both data frames.")
  }
  
} else {
  stop("Error: 'selected_DBS_trial2' is empty.")
}

#----------------------------------------------------------------------------------

# NEXT, change the value of the variable "bin_cat" to ss_malneg or ss_malpos based on DHS malaria testing

# Update 'bin_cat' for "selected" samples based on 'malaria_result'
DBS_samples$bin_cat <- ifelse(
  DBS_samples$bin_cat == "Selected" & DBS_samples$malaria_result == 1, 
  "ss_malpos",  # Malaria-positive
  ifelse(
    DBS_samples$bin_cat == "Selected" & DBS_samples$malaria_result == 0, 
    "ss_malneg",  # Malaria-negative
    DBS_samples$bin_cat  # Keep existing value if not "Selected"
  )
)

# View the first few rows to verify changes
head(DBS_samples)


#-----------------------------------------------------------------------------------

# DESIGNATE SAMPLES TO THE HIGH PREVALENCE SUBSAMPLE CATEGORY

# Define the clusters of interest
selected_clusters <- c(25, 94, 183, 217, 218, 242, 245, 276, 
                       319, 324, 341, 369, 381, 390, 426, 469)

# Update the 'bin_cat' variable for samples that meet both conditions
DBS_samples$bin_cat <- ifelse(
  DBS_samples$hv001 %in% selected_clusters & DBS_samples$bin_cat == "Not Selected", 
  "ss_highprev", 
  DBS_samples$bin_cat  # Retain existing value if conditions are not met
)


#----------------------------------------------------------------------------------------------

# Organize samples NOT selected for inclusion in study by designating "bin_cat" = REGION

#----------------------------------------------------------------------------------------------

DBS_samples_final <- DBS_samples

# Update 'bin_cat' for samples labeled as "Not Selected"
DBS_samples_final$bin_cat <- ifelse(
  DBS_samples_final$bin_cat == "Not Selected", 
  paste0("omitted_", DBS_samples_final$DHSREGNA),  # Create new label with region value
  DBS_samples_final$bin_cat  # Retain existing value if not "Not Selected"
)



#----------------------------------------------------------------------------------------------
#                             Check your work using summary tables
#----------------------------------------------------------------------------------------------

# Create a summary table counting samples by 'hv001' and 'bin_cat'
summary_table <- DBS_samples_final %>%
  group_by(hv001, bin_cat) %>%
  summarise(count = n()) %>%
  arrange(hv001, bin_cat)  # Optional: Sort by 'hv001' and 'bin_cat'

# Count the number of samples in each 'bin_cat' category
total_counts <- table(DBS_samples$bin_cat)
print (total_counts)

# Not Selected  ss_highprev    ss_malneg    ss_malpos 
# 6748           188           6904          101 


total_counts_final <- table(DBS_samples_final$bin_cat)
print(total_counts_final)

# omitted_Bugesera  omitted_Burera    omitted_Gakenke   omitted_Gasabo      omitted_Gatsibo 
# 252                230                204                293                229 
# omitted_Gicumbi   omitted_Gisagara  omitted_Huye      omitted_Kamonyi     omitted_Karongi 
# 199                219                180                221                209 
# omitted_Kayonza   omitted_Kicukiro  omitted_Kirehe    omitted_Muhanga     omitted_Musanze 
# 222                315                254                187                234 
# omitted_Ngoma     omitted_Ngororero  omitted_Nyabihu  omitted_Nyagatare   omitted_Nyamagabe 
# 273                212                243                239                191 
# omitted_Nyamasheke  omitted_Nyanza  omitted_Nyarugenge  omitted_Nyaruguru omitted_Rubavu 
# 198                211                275                201                246 
# omitted_Ruhango   omitted_Rulindo   omitted_Rusizi    omitted_Rutsiro     omitted_Rwamagana 
# 205                203                161                245                197 
# ss_highprev       ss_malneg          ss_malpos 
# 188               6904                101 

#------------------------------------------------------------------------------------------------------
#           Final DBS sample data frame for scanning
#-------------------------------------------------------------------------------------------------------

# Create the new data frame with the specified columns in the desired order
scanning_DBS_samples_final <- DBS_samples_final %>%
  select(barcode, bin_cat, CLUSTER, DHSREGNA, ADM1NAME, LATNUM, LONGNUM, geometry, malaria_result)

write.csv(scanning_DBS_samples_final, 'scanning_DBS_samples_final.csv')


