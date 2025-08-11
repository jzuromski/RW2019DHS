#----------------------------------------------------------------------------
#                Collating scanned DBS sample output sheets
#----------------------------------------------------------------------------

# Purpose: Compare output files between individual scans and the compiled master

# Load necessary library
library(readr)  # Use read_csv and read_tsv for consistency

# File paths
base_path <- "C:/Users/jzuromsk/Documents/DHS_2019"

# Load reference file for scanning
ref_file <- file.path(base_path, "scanning_DBS_samples_final.csv")
REFERENCE_scanning_DBS_samples_final <- read_csv(ref_file)

# Load manually concatenated final file (n = 13,931)
manual_file <- file.path(base_path, "FINAL_outputfile_samples_processed_241127to241205_CLEAN.csv")
manualcat_FINALoutputfile_samples_processed <- read_csv(manual_file)

# Load individual output files
scan_241127 <- read_tsv(file.path(base_path, "outputfile_samples_processed_241127_CLEAN.tsv"))
scan_241128_205 <- read_tsv(file.path(base_path, "outputfile_samples_processed_241128to241205_CLEAN.tsv"))
scan_241129_203 <- read_tsv(file.path(base_path, "outputfile_samples_processed_241129to241203_CLEAN.tsv"))

# Combine scanned outputs and remove duplicates
scanned_DBS_samples <- bind_rows(scan_241129_203, scan_241128_205, scan_241127) %>%
  distinct()  # Removes duplicate rows
# Remove first row if it's identical to column names
scanned_DBS_samples <- scanned_DBS_samples[-1, ]


# View dimensions for verification
cat("Rows in combined scan output:", nrow(scanned_DBS_samples), "\n")
cat("Rows in manual file:", nrow(manualcat_FINALoutputfile_samples_processed), "\n")



list_scanned_DBS_samples <- scanned_DBS_samples %>%
  select(SAMPLEID, CLUSTER, REGION, malaria_result, bin_cat, OUTBINBAG, BAGNUM)
#remove duplicates **n= 13924
list_scanned_DBS_samples <- unique(list_scanned_DBS_samples)


##### Look for any duplicate samples

###File concatenated in R
# Identify duplicates based on SAMPLEID
duplicate_sample_ids <- scanned_DBS_samples$SAMPLEID[duplicated(scanned_DBS_samples$SAMPLEID)]

# Extract rows with duplicate SAMPLEID values into a new dataframe
#Concatenated in R
duplicates_scanned_DBS_samples <- scanned_DBS_samples[scanned_DBS_samples$SAMPLEID %in% duplicate_sample_ids, ] 

###File concatenated manually
# Identify duplicates based on SAMPLEID
duplicate_sample_ids_ <- manualcat_FINALoutputfile_samples_processed$Barcode[duplicated(manualcat_FINALoutputfile_samples_processed$Barcode)]
#Concatenated manually
duplicates_manualcat_FINALoutputfile_samples_processed <- manualcat_FINALoutputfile_samples_processed[manualcat_FINALoutputfile_samples_processed$SAMPLEID %in% duplicate_sample_ids, ]



#------ RESULTS: both the manually and R- concatenated final lists are identical. 
# 7 total samples were scanned into bags twice
# 3 of the 7 were ss_malneg samples-- G2F8V (Rusizi, cluster 129), Y6H2O (Gakenke, cluster 279), and C6C4O (Nyarugenge, cluster 375)

##### Make a list of scanned samples
# Select the specific columns and create a new dataframe
list_scanned_DBS_samples <- manualcat_FINALoutputfile_samples_processed %>%
  select(Barcode, CLUSTER, REGION, malaria_result, bin_cat, OUTBINBAG, BAGNUM)
#remove duplicates **n= 13934
list_scanned_DBS_samples <- unique(list_scanned_DBS_samples)
#Rename the column to barcode
colnames(list_scanned_DBS_samples)[colnames(list_scanned_DBS_samples) == "Barcode"] <- "barcode"

# total number of samples scanned into bags = 13,940


#_____________________________________________________________________________
#                    WHICH SAMPLES are MISSING?
#_____________________________________________________________________________


# Identify missing SAMPLEIDs from list_scanned_DBS_samples by checking against REFERENCE_scanning_DBS_samples_final
missing_sample_ids <- setdiff(REFERENCE_scanning_DBS_samples_final$barcode, list_scanned_DBS_samples$barcode)

# Extract rows from REFERENCE_scanning_DBS_samples_final where SAMPLEID is missing in list_scanned_DBS_samples
missing_samples_dataframe <- REFERENCE_scanning_DBS_samples_final[REFERENCE_scanning_DBS_samples_final$barcode %in% missing_sample_ids, ]

############### RESULTS: 
# 8 samples missing from this list, but they were actually scanned and punched, so must not have been scanned before being bagged into ss_mal_neg bag

# exporting missing samples dataframe as a CSV: 
  write.csv(missing_samples_dataframe, file = "missing_samples_dataframe.csv", row.names = FALSE)
  
  
#__________________________________________________________________________________________
#      Look at actual DBS entered into the study vs samples qPCRed

# Load file of DBS included in the study (list_scanned_DBS_samples + 8 samples that were qPCRed but not on the original scanned list)
manual_file <- file.path(base_path, "DBSsamples_instudy_final.csv")
DBSsamples_instudy_final <- read_csv(manual_file)
DBSsamples_instudy_final <- unique(DBSsamples_instudy_final)
 # 13,940 samples scanned!

# Identify missing SAMPLEIDs from DBSsamples_instudy_final by checking against REFERENCE_scanning_DBS_samples_final
missing_sample_ids <- setdiff(REFERENCE_scanning_DBS_samples_final$barcode, DBSsamples_instudy_final$barcode)

# Extract rows from REFERENCE_scanning_DBS_samples_final where SAMPLEID is missing in list_scanned_DBS_samples
missing_samples_datafram <- REFERENCE_scanning_DBS_samples_final[REFERENCE_scanning_DBS_samples_final$barcode %in% missing_sample_ids, ]

  
  