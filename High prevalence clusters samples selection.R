#   Selecting additional samples from high-prevalence clusters
#------------------------------------------------------------------

# Filter 'DBS_samples' to include only rows where 'bin_cat' is "Not Selected"
unselected_samples <- DBS_samples[DBS_samples$bin_cat == "Not Selected", ]

# Make a df with samples NOT selected in our original study that are from
# high malaria prevalence clusters (>15% prevalence by RDT OR microscopy, n= 16)

# Define the vector of clusters to filter
clusters_to_include <- c(25, 94, 183, 217, 218, 242, 245, 276, 319, 324, 341, 369, 381, 390, 426, 469)

# Filter the data frame for samples belonging to these clusters
high_prev_unselected <- unselected_samples[unselected_samples$hv001 %in% clusters_to_include, ]

# Count number of samples by cluster in DBS that were not included in random selection
high_prev_unselected <- unselected_samples[unselected_samples$hv001 %in% clusters_to_include, ]
#Results: n= 188
# Cluster number    25  94 183 217 218 242 245 276 319 324 341 369 381 390 426 469 
# Number of samples 9  14  15  15  10  10   7  13  11  12  11  13  13  12  11  12 

# Compare the total number of DBS samples in each high-prevalence cluster
filtered_samples <- DBS_samples[DBS_samples$hv001 %in% clusters_to_include, ]
sample_count_by_cluster <- table(filtered_samples$hv001)
print (sample_count_by_cluster)
#Results: n= 407
# Cluster number    25  94 183 217 218 242 245 276 319 324 341 369 381 390 426 469 
#Number of samples  20  28  29  36  22  23  18  26  21  28  24  26  29  28  25  24 


#   WE WILL INCLUDE REMAINING SAMPLES FROM THE FOLLOWING CLUSTERS IN OUR HIGH PREVALENCE SAMPLING BIN

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



