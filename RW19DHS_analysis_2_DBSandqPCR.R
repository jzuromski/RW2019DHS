library(rdhs)
library(dplyr)
library(haven)
library(here)
library(labelled)
library(devtools)
library(microbenchmark)
library(survey)
library(readr)
library(tidyr)
library(stats)
library(tidyverse)
library(readxl)
library(writexl)
library(srvyr)
library(broom)
library(purrr)
options(survey.lonely.psu="adjust")


## Original, untouched data sets:
# RecodeHouseMember2019 = "Household Member Recode",  # PR - individuals, includes HIV & malaria results
# RecodeHIV2019 =         "HIV Test Results Recode",  # AR - HIV-specific info
# RecodeHousehold2019 =   "Household Recode",         # HR - household-level context
# RecodeGeographic2019 =  "Geographic Data",          # GE - for altitude, mapping
# RecodeIndividuals2019 = "Individual Recode",        # IR - women 15–49
# RecodeMen2019 =         "Men's Recode",             # MR - men 15–59
# RecodeCouples2019 =     "Couples' Recode"           # CR 


#----------------------------Step 1: Obtain RW DHS 2019/20 data sets from DHS website and load into R ----------------

## Step 1: Obtain and organize Rwanda 2019/20 survey data sets and recodes
# 1.1: Configure DHS access by setting up your credentials
set_rdhs_config(email = "jenna_zuromski@brown.edu",
                project = "Temporal analysis of malaria prevalence and geospatial risk in Rwanda",
                config_path = "rdhs.json",
                cache_path = "DHS_2019",
                data_frame = "data.table::as.data.table",
                global = FALSE)

## 1.2: Pull Rwanda 2019/20 survey and list available datasets for that survey
surveys2 <- dhs_surveys(countryIds = "RW", surveyYear = 2019)
rw_survey_id <- surveys2$SurveyId[surveys2$SurveyType == "DHS"][1] #"RW2019DHS"

## List available datasets for the Rwanda 2019/20 survey
available_datasets <- dhs_datasets(surveyIds = rw_survey_id)

## 1.3: Define list of datasets and recodes and download them
needed_file_types <- c(
  "Household Member Recode",  # PR - individuals, includes HIV & malaria results
  "HIV Test Results Recode",  # AR - HIV-specific info
  "Household Recode",         # HR - household-level context
  "Individual Recode",        # IR - women 15–49
  "Men's Recode",             # MR - men 15–59
  "Geographic Data",          # GE - for altitude, mapping
  "Couples' Recode"           # CR 
)

# Filter to just those datasets
datasets_to_get <- available_datasets %>%
  filter(FileType %in% needed_file_types)

## 1.4: Download and read the datasets
downloaded_files <- get_datasets(dataset_filenames = datasets_to_get$FileName, clear_cache = TRUE) #download files

# 1.5: Read in the datasets for each data set or recode
RecodeHouseMember2019 <- readRDS(downloaded_files$RWPR81FL)
RecodeHIV2019 <- readRDS(downloaded_files$RWAR81FL)
RecodeHousehold2019 <- readRDS(downloaded_files$RWHR81FL)
RecodeGeographic2019 <- readRDS(downloaded_files$RWGE81FL)
RecodeIndividuals2019 <- readRDS(downloaded_files$RWIR81FL)
RecodeMen2019 <- readRDS(downloaded_files$RWMR81FL)
RecodeCouples2019 <- readRDS(downloaded_files$RWCR81FL)

# 1.6: Create working dataframes for each recode
PR19RW <- RecodeHouseMember2019
AR19RW <- RecodeHIV2019
HR19RW <- RecodeHousehold2019 
GE19RW <- RecodeGeographic2019
IR19RW <- RecodeIndividuals2019
MR19RW <- RecodeMen2019
CR19RW <- RecodeCouples2019

# 1.7: Download geospatial covariate data from the DHS website and read into R
GC19RW <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\DHS_Materials\\RW_2019_DHS_unzipped\\RWGC81FL\\RWGC81FL.csv", stringsAsFactors = FALSE)
geospatial_covar <- GC19RW


#----------------------------Step 2: Create a CORE data set by merging all data sets based on DBS barcode ----------------


# Step 2.1: Create new dataframe called dbs_master to retain only individuals who provided DBS in household member recode 

# Regular expression for a 5-character string containing both letters and numbers
pattern <- "^(?=.*[A-Za-z])(?=.*[0-9])[A-Za-z0-9]{5}$"

# Ensure the column is of character type (if not already)
PR19RW$ha62 <- as.character(PR19RW$ha62)
PR19RW$hb62 <- as.character(PR19RW$hb62)

# Create new column "barcodes" in the PR19RW to provide DBS barcode
PR19RW <- PR19RW %>%
  mutate(
    barcode = case_when(
      !is.na(ha62) & grepl(pattern, ha62, perl = TRUE) ~ ha62,
      !is.na(hb62) & grepl(pattern, hb62, perl = TRUE) ~ hb62,
      TRUE ~ NA_character_ ))

# Create a dataframe containing only individuals in household member recode who have an HIV (DBS) sample barcode
dbs_master <- PR19RW %>%
  filter(!is.na(barcode)) #582 variables

# Add variable "r" = 1 as a constant to count rows later on
dbs_master$one <- 1

# Print number of males and females in dbs_master
sum(dbs_master$hv104 == 1, na.rm = TRUE) #6587 males
sum(dbs_master$hv104 == 2, na.rm = TRUE) #7354 females

# 2.2: Merge data of household members from PR with HIV sample barcodes with HIV recode
hiv_clean <- AR19RW %>%
  select(hivclust = hivclust, hivnumb = hivnumb, hivline = hivline, hiv01, hiv03, hiv05) #13,937 individuals
# Merge PR (household members) with AR (HIV) by cluster, household, line #586 variables
dbs_master <- dbs_master %>%
  left_join(hiv_clean, by = c("hv001" = "hivclust", "hv002" = "hivnumb", "hvidx" = "hivline"))

# 2.3: Merge dbs_master (PR + AR) with men's individual recode (MR)
dbs_master_m <- dbs_master %>% filter(hv104 == 1)
dbs_master_m <- dbs_master_m %>%
  left_join(MR19RW, by = c("hv001" = "mv001", "hv002" = "mv002", "hvidx" = "mv003")) # 808 MR19RW variables - 3 equivalents + 586 dbs_master variables= 1391 variables

# 2.4: Merge dbs_master (PR + AR) with women's individual recode (IR)
dbs_master_f <- dbs_master %>% filter(hv104 == 2)
dbs_master_f <- dbs_master_f %>%
  left_join(IR19RW, by = c("hv001" = "v001", "hv002" = "v002", "hvidx" = "v003")) # 5117 IR19RW variables - 3 equivalents + 586 dbs_master variables= 5700 variables

# 2.5: Combine men (PR + AR + IR) and women (PR + AR + IR)
dbs_master <- bind_rows(dbs_master_m, dbs_master_f) # 6505 variables

# 2.6: Add geographic data to to dbs_master
GPS2019 <- GE19RW %>%
  select(DHSCLUST, DHSREGNA, DHSREGCO, ADM1NAME, ADM1DHS, URBAN_RURA, LATNUM, LONGNUM, ALT_DEM, geometry) #10 variables
# Merge GPS data by cluster #6514 variables
dbs_master <- dbs_master %>%
  left_join(GPS2019, by = c("hv001" = "DHSCLUST"))  


# 2.7: Add geospatial covariate data to dbs_master #6648 variables
# Merge data sets
dbs_master <- dbs_master %>%
  left_join(geospatial_covar, by = c("hv001" = "DHSCLUST"))     

# 2.8: Make a GPS/geographic df
coords<-dbs_master %>%
  select("hv001","DHSREGNA","DHSREGCO","URBAN_RURA","ALT_DEM","LATNUM","LONGNUM","geometry")
  
coords <- unique(coords)

dbs_master_bc <- PR19RW %>%
  select(ha62, hb62, hml34)


#----------------------------Step 3: Create new data frame with variables relevant to this analysis-----------------------------------------


# 3.1: Remove columns that are entirely NA  # 3152 variables
cleaned_data <- dbs_master %>%
  select(where(~!all(is.na(.))))
# 3.1.1: Run a check to confirm that only whole columns of NA were removed
  # Math:
    #Number of samples = (total_na_before - total_na_after) / (6648 variables in original - 3152 variables in cleaned data) 
    total_na_before <- sum(is.na(dbs_master))
    total_na_after <- sum(is.na(cleaned_data)) 
dbs_master <- cleaned_data 

# 3.2: Make a master list of variables to keep for analysis

# 3.2.1: Obtain all variables and their descriptions
variable_labels <- var_label(dbs_master)
str(variable_labels)
variable_labels_unlisted <- unlist(variable_labels)

# 3.2.2: Ensure the labels align with the variables in dbs_master
vars <- data.frame(
  Variable = names(variable_labels_unlisted),
  Description = variable_labels_unlisted
)

# 3.2.3: If the result is a named list or list of named vectors, extract the names and labels
if (is.list(variable_labels)) {
  # Extract the names and descriptions
  variable_names <- names(variable_labels)
  descriptions <- sapply(variable_labels, function(x) ifelse(is.null(x), NA, x))
  
  # Create a data frame from the variable names and descriptions
  vars <- data.frame(
    Variable = variable_names,
    Description = descriptions
  )
} else {
  # If already a named vector, create the data frame directly
  vars <- data.frame(
    Variable = names(variable_labels),
    Description = variable_labels
  )}


# 3.2.4: write as csv and manually add missing descriptions of variables
write.csv(vars, file = "RW19DHSvariables.csv", row.names = FALSE)

# 3.2.5: Confirm that sample data has been aligned correctly during recode merging
## The following should be the same within rows: 
  ## Barcode for HIV blood sample (hiv01, ha62, hb62, mha62)
  ## Anemia level (ha57, mha57, mnv457, v457)
  ## Others: study indexes, wealth index, height/age percentile, Body Mass Index, hemoglobin level, cluster 

# Run checks on combined data set- all data points should be equivalent between rows unless NA
# Example
check <- cleaned_data%>%
  select(hiv01, ha62, hb62, mha62) #Barcode for HIV blood sample
check <- cleaned_data%>%
  select(ha57, mha57,v457, mnv457) #anemia level
check <- cleaned_data%>%
  select(ha40, mha40, mnv445) #Body mass index
check <- cleaned_data%>%
  select(hv001, hmhidx, hvidx, idxh4, mcaseid) #indexes and case ID
check <- cleaned_data%>%
  select(ha53, mha53, mnv453, v453) #Hemoglobin level (g/dl - 1 decimal)
check <- survey19%>%
  select(DHSREGCO, sdistrict, hv024) #District

check <- cleaned_data%>%
  select(hiv05, ha69, hb69)

# Extract rows with nonequivalent data in two variables to determine if data is misaligned:
check <- check %>%
  filter(ha40 != mnv445)

#any(check$ha53 != check$v453, na.rm = TRUE) #if returns FALSE, then values are identical

# 3.3: Create new data frame with only these variables
# 3.3.1: Create variable list
vars_to_keep <- c(
  "ADM1DHS", "ADM1NAME", "ALT_DEM", "DHSREGCO", "DHSREGNA", "barcode", "geometry", "caseid", "LATNUM", "LONGNUM", "URBAN_RURA",
  "ha0", "ha1", "ha2", "ha3", "ha4", "ha5", "ha6", "ha11", "ha12", "ha12a", "ha12b", "ha40", "ha41", "ha50", "ha53", "ha54", "ha56", "ha57", "ha66", 
  "hb1", "hb2", "hb3", "hb4", "hb5", "hb6", "hb50", "hb66", "hiv01", "hiv03", "hiv05", "hhid", "hmhidx", "hmlidx", 
  "hml1", "hml2", "hml7", "hml10", "hml11", "hml12", "hml13", "hml4", "hml18", "hml20", "hml21", "hml22", "hml32", "hml32a", "hml32b", "hml32c", "hml35", 
  "hvidx", "hv001", "hv002", "hv003", "hv004", "hv005", "hv006", "hv007", "hv008", "hv008a", "hv009", "hv012", "hv013", "hv014", "hv016", 
  "hv021", "hv022", "hv023", "hv024", "hv025", "hv028", "hv040", "hv102", "hv104", "hv105", "hv106", "hv107", "hv201", "hv204", "hv205", "hv213", 
  "hv214", "hv215", "hv216", "hv227", "hv228", "hv235", "hv244", "hv246", "hv246a", "hv246b", "hv246c", "hv246d", "hv246e", "hv246f", 
  "hv246g", "hv246h", "hv246i", "hv270", "hv270a", "hv271",  "hv271a", "hv807c", "hv807d", "hv807m", "hv807y", "idxdis", "idxh4", 
  "mcaseid", "mv005", "mv006", "mv007", "mv008", "mv008a", "mv016", "mv023", "mv024", "mv106", "mv107", "mv133", "mv155", "mv481", "mv717", 
  "s1108ij", "s1108ik", "s1108il", "s1108im", "s1108in", "s1108io", "sb211f", "sb239", "sb240", "sb241", "sb249", "sb253", "sb254", "sb332", "sb333", 
  "sh08", "sh104a", "sh135a", "sm815ji", "sm815jk", "sm815jm", "sm815jn", "sm815jo", "sm815jp", 
  "v005", "v106", "v107", "v139", "v140", "v155", "v201", "v208", "v209", "v210", "v238", "v461", "v481", "v717", 
  "All_Population_Count_2020", "Aridity_2020", "Day_Land_Surface_Temp_2020", "Diurnal_Temperature_Range_2020", "Enhanced_Vegetation_Index_2020", 
  "Irrigation", "ITN_Coverage_2020", "Land_Surface_Temperature_2020", "Malaria_Incidence_2020", "Malaria_Prevalence_2020", "Maximum_Temperature_2020",
  "Mean_Temperature_2020", "Minimum_Temperature_2020", "Temperature_January", "Temperature_February", "Temperature_March", "Temperature_April", "Temperature_May",
  "Temperature_June", "Temperature_July", "Temperature_August", "Temperature_September", "Temperature_October", "Temperature_November", "Temperature_December",
  "Precipitation_2020", "Rainfall_2020", "Wet_Days_2020")

# 3.3.2: Make a data frame containing only variables to keep for the analysis 
dbs_master_vars <- dbs_master[, vars_to_keep] #200 variables

#----------------------------Step 4: Create functions for cleaning data---------------------------------------------------

#-----// f_replace_values_in_columns: Replaces values in specified columns with a specified input
# Function:
f_replace_values_in_columns <- function(data, columns, values_to_replace, replacement_value) {
  for (col in columns) {
    if (!col %in% names(data)) {
      warning(paste("Column", col, "not found in the dataframe. Skipping."))
      next
    }
    
    # Count how many replacements will be made
    n_changes <- sum(data[[col]] %in% values_to_replace, na.rm = TRUE)
    
    # Make the replacements
    data[[col]][data[[col]] %in% values_to_replace] <- replacement_value
    
    # Report the number of changes
    message(sprintf("Replaced %d values in column '%s'.", n_changes, col))
  }
  return(data)
}

# Use case: 
df <- f_replace_values_in_columns(
  data = df,
  columns = c("",""), #either name the columns or name a string of columns
  values_to_replace = c(8, 9), #values = 8 or 9
  replacement_value = NA  #can also be numerical, etc.
)


#-----// f_combine_mf_variables: Combines variables for male/female recodes, then checks that there are no mismatches
# Function:
f_combine_mf_variables <- function(data, var_female, var_male, new_var, drop_original = FALSE) {
  # Combine the two variables
  data[[new_var]] <- ifelse(!is.na(data[[var_female]]), 
                            data[[var_female]], 
                            data[[var_male]])
  # Create a check column
  check_col <- paste0(new_var, "_check")
  data[[check_col]] <- ifelse(!is.na(data[[var_female]]) & data[[new_var]] != data[[var_female]], 
                              "Mismatch_female",
                              ifelse(!is.na(data[[var_male]]) & data[[new_var]] != data[[var_male]], 
                                     "Mismatch_male", 
                                     "OK"))
  # Count mismatches
  mismatches <- table(data[[check_col]])
  
  # If all rows are OK, remove the check column
  if (all(mismatches == mismatches["OK"])) {
    data[[check_col]] <- NULL
    message("✔️ All values match. Check column removed.")
  } else {
    message("⚠️ Mismatches found:")
    print(mismatches) }
  return(data) }

# Use case: 
df <- f_combine_mf_variables(
  data = df,
  var_female = "variable1",
  var_male = "variable2",
  new_var = "new_variable",
)





#-----// f_merge_data_by_keys: Merges data based on a "key" (ex: barcode)
# Function:
f_merge_data_by_keys <- function(
    main_df,            # The main dataframe you're adding to
    source_df,          # The dataframe you're taking variables from
    by_keys,            # A character vector of key(s) to join on
    vars_to_add = NULL  # Optional: a character vector of variables to add (if NULL, add all non-key vars)
) {
  # Check keys exist in both dataframes
  missing_keys_main <- setdiff(by_keys, names(main_df))
  missing_keys_source <- setdiff(by_keys, names(source_df))
  
  if (length(missing_keys_main) > 0 || length(missing_keys_source) > 0) {
    stop(paste("Missing key(s):",
               paste(c(missing_keys_main, missing_keys_source), collapse = ", "))) }
  
  # Subset source_df to just the keys + selected variables (or all if NULL)
  if (is.null(vars_to_add)) {
    vars_to_merge <- setdiff(names(source_df), by_keys)
  } else {
    vars_to_merge <- vars_to_add }
  
  # Create subset for merging
  merge_subset <- source_df[, c(by_keys, vars_to_merge), drop = FALSE]
  # Merge
  merged_df <- merge(main_df, merge_subset, by = by_keys, all.x = TRUE)
  return(merged_df) }

# Use case:
df <- f_merge_data_by_keys(
  main_df = df,
  source_df = df2,
  by_keys = c("barcode"),  # Or c("barcode", "hv001") if cluster too
  vars_to_add = c("qPCR_plate", "pf_CT", "pf_SQ")
)


#-----// Function to summarize variables using svyby or svymean
f_batch_svy_summary <- function(outcome, group_vars, design_obj, method = "total") {
  # Determine which survey function to use
  survey_fun <- if (method == "mean") svymean else svytotal
  
  # Loop through grouping variables
  results <- map(group_vars, function(group_var) {
    # Construct formula for outcome and grouping
    outcome_formula <- as.formula(paste0("~", outcome))
    group_formula <- as.formula(paste0("~", group_var))
    
    # Apply svyby
    result <- tryCatch({
      svyby(formula = outcome_formula, by = group_formula, design = design_obj,
            FUN = survey_fun, vartype = c("se", "ci"), na.rm = TRUE, survey.lonely.psu = "adjust" ) %>%
        mutate(group_var = group_var) }, 
      error = function(e) { message(paste("Error with variable:", group_var)) return(NULL) })
    return(result)  })
  
  # Combine results
  bind_rows(results) }

# Use case:
# For DHS19_allDBS (with "r" as the outcome) using svytotal
group_vars1 <- c("hv104", "age_cat10", "hv270", "hv106", "hv246", 
                 "hv201_cat", "hml1_cat", "hml20", "hml10", "bednetper_cat")

results_r <- batch_svy_summary(outcome = "r", group_vars = group_vars1, design_obj = DHS19_allDBS)

# For continuous variables summarized by malaria status using svymean
continuous_results <- batch_svy_summary(outcome = "prop_bednet", group_vars = c("pf"), design_obj = DHS19, method = "mean") %>%
  bind_rows(
    batch_svy_summary(outcome = "prop_slept", group_vars = c("pf"), design_obj = DHS19, method = "mean")
  )


#----------------------------Step 5: Clean the variables-------------------------------

# 4.1: Make missing values = NA
# to count NA: sum(is.na(dbs_master_vars$your_variable))
dbs_master_vars_clean <- dbs_master_vars


# For variables where 8 = unknown and 9 = missing, replace with NA
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("ha50", "hb50", "ha57", "hv106", "hv206", "hv207", "hv208", "hv209", "hv210", "hv211", "hv212", "hv221", "hv227",
  "hv228", "hv235", "hv243a", "hv243b", "hv243c", "hv243d", "hv243e", "hv244", "hv246"),
  values_to_replace = c(8, 9),
  replacement_value = NA)

# For malaria variables where 6= Test undetermined, 7= sample not found in lab database, and 9= missing, replace with NA
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("hml32", "hml32a", "hml32b", "hml32c", "hml35"),
  values_to_replace = c(6, 7, 9),
  replacement_value = NA)

# For variables where 98= unknown and 99= missing, replace with NA
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("hml1", "hml4", "hv107", "hv246a", "hv246b", 
              "hv246c", "hv246d", "hv246e", "hv246f", "hv246g", "hv246h", "hv246i"),
  values_to_replace = c(98, 99),
  replacement_value = NA)

# For variables where 998 = unknown and 999= missing, replace with NA
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("hv204"),
  values_to_replace = c(998, 999),
  replacement_value = NA)

# For variables where 994= not present, 995= refused, 996= other, 999= missing, replace with NA
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("ha53", "ha56"),
  values_to_replace = c(994, 995, 996, 999),
  replacement_value = NA)

# For variables where 9998= flagged and 9999= missing, replace with NA
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("ha4", "ha5", "ha11", "ha40", "ha41"),
  values_to_replace = c(9998, 9999),
  replacement_value = NA)

# For variables where 9994= not present, 9995 = refused, 9996= other, 9999= missing, replace with NA
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("ha2", "ha3"),
  values_to_replace = c(9994, 9995, 9996, 9999),
  replacement_value = NA)

# For variables where 99998= flagged and 99999= missing, replace with NA
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("ha6", "ha12", "ha12a", "ha12b"),
  values_to_replace = c(99996, 99998, 99999),
  replacement_value = NA)

# Clean up individual variables

# For hv204 time to get water source (minutes), 996= on-premise replaced with 0 minutes
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("hv204"),
  values_to_replace = c(996),
  replacement_value = 0)


# For hv246 (owns livestock), replace no(2) = 0 and yes = 1
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = c("hv246"),
  values_to_replace = c(2),
  replacement_value = 0)


# 4.2: Combine variables for male/female recodes

# Use f_combine_mf_variables to make new variables to combine male/female survey answers

# Participant has health insurance (binary)
dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "v481",
  var_male = "mv481",
  new_var = "health_insurance",)

# Participant under age 18 (binary)
dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "ha50",
  var_male = "hb50",
  new_var = "under_18",) #binary

# Make under age 18(1) = 1, NOT under age 18(2)= 0
dbs_master_vars_clean <- f_replace_values_in_columns(
  data = dbs_master_vars_clean,
  columns = "under_18",
  values_to_replace = c(2),
  replacement_value = 0)

# Occupation (categorical)
dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "v717",
  var_male = "mv717",
  new_var = "occupation",)

# Literacy (categorical)
dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "v155",
  var_male = "mv155",
  new_var = "literacy",) #categorical (nominal)

# Age in years
dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "ha1",
  var_male = "hb1",
  new_var = "age_years",) #quantitative

# Chest pain 
dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "s1108io",
  var_male = "sm815jp",
  new_var = "sympt_chest_pain",) #binary

dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "s1108ij",
  var_male = "sm815jk",
  new_var = "sympt_cough",) #binary

dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "s1108il",
  var_male = "sm815jm",
  new_var = "sympt_night_sweats",) #binary

dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "s1108ik",
  var_male = "sm815ji",
  new_var = "sympt_fever",) #binary

dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "s1108in",
  var_male = "sm815jo",
  new_var = "sympt_fatigue",) #binary

dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "s1108im",
  var_male = "sm815jn",
  new_var = "sympt_weightloss",) #binary

dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "ha3",
  var_male = "hb3",
  new_var = "height",) #quantitative, continuous

dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "ha2",
  var_male = "hb2",
  new_var = "weight",) #quantitative, continuous

dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "ha66",
  var_male = "hb66",
  new_var = "highest_educational_level",) # categorical, ordinal

dbs_master_vars_clean <- f_combine_mf_variables(
  data = dbs_master_vars_clean,
  var_female = "ha4",
  var_male = "hb4",
  new_var = "heightage_percentile",) #quantitative, continuous

#215 variables


## Clean HIV weight

# 4 samples were not found in HIV recode. 146 had weight = 0
# Samples are weighted by cluster and sex, so each cluster has the same HIV weight. Some clusters have different weights for m/f.

# If HIV weight was 0, this individual was excluded from the HIV prevalence calculation due to various reasons, 
#such as not being tested, having missing test results, being outside the target group, or data cleaning procedures
# HOWEVER, we care about selection into the HIV sample pool, as this is why we have their DBS.
# Since samples are weighted by cluster and sex, the samples with HIV weight = 0 that we have should be matched with their counterparts,
# HIV weights were manually added back into the data frame for samples with NA or 0 weights based on cluster and sex.


HIVcheck <- dbs_master_vars_clean %>%
  select(barcode, hv001, LATNUM, LONGNUM, hv104, hiv01, hiv05)


write.csv(HIVcheck, file = "HIVweightcheck19.csv", row.names = FALSE)

# Manually match all NA and weight = 0 with the same HIV weight in that for their cluster and sex 

# Join the corrected weights by barcode
dbs_master_vars_clean <- dbs_master_vars_clean %>%
  left_join(corrected_HIVweight, by = "barcode", suffix = c("", "_new")) %>%
  mutate(
    # Replace hiv05 with new value if available
    hiv05 = ifelse(!is.na(hiv05_new), hiv05_new, hiv05)
  ) %>%
  # Remove temporary columns
  select(-hiv05_new, -hv001_new, -LATNUM_new, -LONGNUM_new, -hv104_new, -hiv01_new, -hiv05_new)


# Check that the variables still align
check <- dbs_master_vars_clean%>%
  select(ha4, hb4, heightage_percentile) #Hemoglobin level (g/dl - 1 decimal)

# Extract rows with nonequivalent data in two variables to determine if data is misaligned:
check <- check %>%
  filter(ha4 != heightage_percentile)


#----------------------------Step 6: Make new variables-----------------------------

# MAKE new df in case these new variables get messed up
dbs_master_vars_clean2 <- dbs_master_vars_clean #215 variables

#-- 5.1: Make categorical variable for age (age_cat), 

# 10 year intervals by mid-decade, 0-14 (no data points), 15-24, 25-34, 35-44, 45-54, 55-59
dbs_master_vars_clean2$age_cat10 <- cut(dbs_master_vars_clean2$age_years,
breaks = c(14, 24, 34, 44, 54, Inf),
labels = c(1, 2, 3, 4, 5), include.lowest = TRUE)

# 5-year intervals, 0-14 (no data points), 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59
dbs_master_vars_clean2$age_cat5 <- cut(dbs_master_vars_clean2$age_years,
breaks = c(14, 19, 24, 29, 34, 39, 44, 49, 54, 59),
labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), include.lowest = TRUE)

#-- 5.3: Make an elevation binary (elev1500, above or below)
dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>% mutate(elev1500_bin = case_when(
  ALT_DEM>=1500 ~ ">= 1500",
  ALT_DEM<1500 ~ "<1500"))

#-- 5.5: Make elevation a categorical variable (ALT_DEM_cat)
dbs_master_vars_clean2$ALT_DEM_cat <- cut(dbs_master_vars_clean2$ALT_DEM,
                      breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf),
                      labels = c(1, 2, 3, 4, 5, 6), include.lowest = TRUE)

#-- 5.6: Make household bed net ownership binary variable (hml1_cat, 0 = does not own net, 1 = owns net)
dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>% 
  mutate(hml1_cat = case_when(
hml1 == 0 ~ 0,
hml1 > 0 ~ 1))

#-- 5.7: Make bed net sufficiency per household members (1 per 1.8 de jure members) binary variable (net_1.8)
dbs_master_vars_clean2$hh_1.8 <- (dbs_master_vars_clean2$hv012)/1.8 # make hh_1.8 variable for number of de jure household members / 1.8
dbs_master_vars_clean2$net_1.8 <- (dbs_master_vars_clean2$hh_1.8)/(dbs_master_vars_clean2$hml1) # make net_1.8 variable hh_1.8 / number of nets in household
#fix values with 0 bed nets
dbs_master_vars_clean2$net_1.8[dbs_master_vars_clean2$net_1.8 == Inf]<-0
# Make binary variable for 1 bed net per 1.8 hh members (bednetper_cat)
dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>% mutate(bednetper_cat = case_when(
net_1.8 > 1 ~ 1,
net_1.8 < 1 ~ 0))


#-- 5.8: Make water source a binary variable (hv201_cat, 1 = piped, 0 = unpiped)
dbs_master_vars_clean2$hv201_cat <- cut(dbs_master_vars_clean2$hv201, breaks=c(0, 12, Inf), labels=c(1,0), include.lowest = TRUE) 


#--- Make rain categorical variables from DHS-----------------

# Rename variables
dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>%
  rename(
    temp2020 = Mean_Temperature_2020,
    rain2020 = Rainfall_2020,
    malp2020 = Malaria_Prevalence_2020)



# Make avg rain categorical variable (by cluster)
# Obtain average temperature across 500 clusters
overall_avg_rain2020DHS <- mean(geospatial_covar$Rainfall_2020, na.rm = TRUE)
summary(geospatial_covar$Rainfall_2020, useNA = "always")
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#874.6  1156.4  1221.6  1236.4  1292.1  1683.1 


# Create a new categorical variable based on the comparison
dbs_master_vars_clean2$rain_catDHS <- ifelse(
  dbs_master_vars_clean2$rain2020 >= overall_avg_rain2020,
  "at or above avg. rain",
  "below avg. rain" )


#--- Make rain categorical variables from CHIRPS database-----------------

library("chirps")

# pull coordinates and dates for all survey years 
#ccoord19 <- as.data.frame(st_drop_geometry(coords))[, c("hv001", "lat", "long")]
ccoord19 <- data.frame(lon=c(coords$LONGNUM), lat=c(coords$LATNUM))

#survey months are 11-2019 to 07-2020
dates19 <- c("2019-09-09", "2020-08-20") 

#pull precipitation data for all clusters during dates range
precip_data19 <- get_chirps(ccoord19, dates19, server = "ClimateSERV") #writing: substituting ENGCRS["Undefined Cartesian SRS with unknown unit"] for missing CRS (Varun had to make a custom CRS for me based on what the RW government uses)
precip_data19 <- precip_data19 %>% separate(date, into = c("year", "month", "day"), sep = "-")

#average precipitation for each month
p_monthly_avg19 <- precip_data19 %>%
  group_by(id, lat, lon, year, month) %>%
  summarize(avg_chirps19 = mean(chirps), .groups = "drop")

#export so we don't have to do the lengthy fetch process every time 
write_csv(p_monthly_avg19, "C:\\Users\\jzuromsk\\Documents\\DHS_2019\\avg_precip_chirps19.csv")

#attach precipitation data to rw_svy by survey month (1 month lag for rainfall)
#chirps <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\avg_precip_chirps19.csv")

#convert precipitation average into columns for each month
chirps <- p_monthly_avg19 %>% pivot_wider(names_from = month, values_from = avg_chirps19)
chirps$hv001 <- chirps$id


# Convert column indices to names first
cols_to_use <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")
chirps_collapsed <- chirps %>%
  group_by(hv001, lat, lon) %>%
  summarise(across(all_of(cols_to_use), ~ .[which(!is.na(.))[1]])) %>%
  ungroup()


#attach to survey data & match based on survey month
dbs_master_vars_clean2 <- left_join(dbs_master_vars_clean2, chirps_collapsed, by = "hv001")

#create rain variable, which is the prior month's average in mm (241 variables)
dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>% mutate(rain = case_when(hv006 == 10 ~ `09`, hv006 == 11 ~ `10`, 
                                             hv006 == 12 ~ `11`, hv006 == 01 ~ `12`,
                                             hv006 == 02 ~ `01`, hv006 == 03 ~ `02`, 
                                             hv006 == 04 ~`03`, hv006 == 05 ~ `04`,
                                             hv006 == 06 ~ `05`, hv006 == 07 ~ `06`,
                                             hv006 == 08 ~ `07`))

# Make avg rain categorical variable (by cluster)
# Obtain average temperature across 500 clusters
overall_avg_rainMONTHLY <- mean(dbs_master_vars_clean2$rain, na.rm = TRUE)
summary(dbs_master_vars_clean2$rain, useNA = "always")
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.09218 1.75930 3.96398 3.54853 4.98409 6.87369 


# Create a new categorical variable based on the comparison
dbs_master_vars_clean2$rain_cat <- ifelse(
  dbs_master_vars_clean2$rain >= overall_avg_rainMONTHLY,
  "at or above avg. rain",
  "below avg. rain" )


#--- Make temp categorical variables from DHS-----------------



#----- Match cluster monthly temperature to participants by survey month (242 variables)

dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>% mutate(dhs_temp = case_when(hv006 == 11 ~ Temperature_November,
                                                   hv006 == 12 ~ Temperature_December, hv006 == 1 ~ Temperature_January,
                                                   hv006 == 2 ~ Temperature_February, hv006 == 3 ~ Temperature_March, 
                                                   hv006 == 4 ~ Temperature_April, hv006 == 5 ~ Temperature_May,
                                                   hv006 == 6 ~ Temperature_June, hv006 == 7 ~ Temperature_July,
                                                   hv006 == 8 ~ Temperature_August, hv006 == 9 ~ Temperature_September,
                                                   hv006 == 10 ~ Temperature_October))

# Make avg temperature categorical variable (by cluster)
# Obtain average temperature across 500 clusters
overall_avg_temp2020 <- mean(dbs_master_vars_clean2$dhs_temp, na.rm = TRUE)
summary(dbs_master_vars_clean2$dhs_temp, useNA = "always")
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#13.74   17.67   19.12   18.76   20.07   21.93 

# Create a new categorical variable based on the comparison
dbs_master_vars_clean2$temp_cat <- ifelse(
  dbs_master_vars_clean2$dhs_temp >= overall_avg_temp2020,
  "at or above avg. temp",
  "below avg. temp" )




#----------Create landcover variable---------------

#--- Add Landcover data to the dataset (% of each landcover type)

dbs_master_vars_clean2 <- f_merge_data_by_keys(
  main_df = dbs_master_vars_clean2,
  source_df = lc_summary,
  by_keys = c("hv001"),  # Or c("barcode", "hv001") if cluster too
  vars_to_add = c("water_percent", "trees_percent", "flooded_vegetation_percent",
                  "crops_percent", "built_area_percent", "bare_ground_percent",
                  "snowice_percent", "clouds_percent", "rangeland_percent")
)

# Create scale of percent land use by dividing by 10

# Categories with >5% coverage
land_vars <- c(
  "water_percent",
  "trees_percent",
  "crops_percent",
  "built_area_percent",
  "rangeland_percent"
)

# create new variables divided by 10 and renamed
for (var in land_vars) {
  new_var <- paste0(var, "10")
  dbs_master_vars_clean2[[new_var]] <- dbs_master_vars_clean2[[var]] / 10
}

#-----------Livestock ownership binary-------
#Binary for overall livestock ownership
dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>%
  mutate(owns_livestock = if_else(hv246 >= 1, 1, 0))
         

# Create binary farm animal ownership variables (0 = does not own, 1 = owns)
dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>%
  mutate(
    owns_cattle_traditional = ifelse(hv246a >= 1, 1, 0),
    owns_cattle             = ifelse(hv246b >= 1, 1, 0),
    owns_bulls              = ifelse(hv246c >= 1, 1, 0),
    owns_goats              = ifelse(hv246d >= 1, 1, 0),
    owns_sheep              = ifelse(hv246e >= 1, 1, 0),
    owns_poultry            = ifelse(hv246f >= 1, 1, 0),
    owns_pigs               = ifelse(hv246g >= 1, 1, 0),
    owns_rabbit             = ifelse(hv246h >= 1, 1, 0),
    owns_equine             = ifelse(hv246i >= 1, 1, 0)  # for horses, donkeys, mules
  )


#----------------------------Step 7: Add qPCR data to the master data set-------------------------------------

#New data frame with qPCR data
dbs_master_vars_clean2_qPCR <- dbs_master_vars_clean2 #263 variables


# 6.1: Add the category for each sample (ss_malneg, ss_malpos, ss_highprev, omitted_district)

## Load file of DBS included in the study (list_scanned_DBS_samples + 9 samples that were qPCRed but not on the original scanned list)
DBSsamples_study19_final <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\DBSsamples_instudy_final.csv", stringsAsFactors = FALSE)

# Filter to include only the "barcode" and "bin_cat" columns
DBSsamples_study19_final <- DBSsamples_study19_final %>%
  select(barcode, bin_cat)
DBSsamples_study19_final <- unique(DBSsamples_study19_final) # 13,934 samples scanned!

# use function to add bin_cat to the master data frame
dbs_master_vars_clean2_qPCR <- f_merge_data_by_keys(
  main_df = dbs_master_vars_clean2_qPCR,
  source_df = DBSsamples_study19_final,
  by_keys = c("barcode"),
  vars_to_add = c("bin_cat"))

# 6.2: Identify the random selection samples

# ## Load file of random selection samples
random_selection_samples19 <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\random_selection_samples_final.csv", stringsAsFactors = FALSE)
random_selection_samples19 <- unique(random_selection_samples19) #6,963 samples

# Create the variable "random selection" and make all samples = 0 initially
dbs_master_vars_clean2_qPCR$random_selection <- 0
# Samples designated 1 if part of random selection
dbs_master_vars_clean2_qPCR$random_selection[dbs_master_vars_clean2_qPCR$barcode %in% random_selection_samples19$barcode] <- 1
# check the sum of random selection samples
sum(dbs_master_vars_clean2_qPCR$random_selection == 1, na.rm = TRUE) #6,963 samples (6957 expected +6 unscanned and originally omitted BUT actually extracted and qPCRed)

# 6.3: Make binary variable for each sample having been qPCR tested

# Add a column Pf_qPCR_tested with 0 for all samples initially
dbs_master_vars_clean2_qPCR$Pf_qPCR_tested <- 0
# Add a column Po_qPCR_tested with 0 for all samples initially
dbs_master_vars_clean2_qPCR$Po_qPCR_tested <- 0
# Add a column Pm_qPCR_tested with 0 for all samples initially
dbs_master_vars_clean2_qPCR$Pm_qPCR_tested <- 0


# 6.4: Add DNA extracted and qPCR tested variables to the data set
# Load qPCR TESTED samples lists (all extracted samples were tested for Pf)
Pf_qPCR_tested_samples <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\Pf_qPCR_tested_samples.csv", stringsAsFactors = FALSE)
Po_qPCR_tested_samples <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\Po_qPCR_tested_samples.csv", stringsAsFactors = FALSE)
Pm_qPCR_tested_samples <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\Pm_qPCR_tested_samples.csv", stringsAsFactors = FALSE)


# Add a column "dnaextracted" with 0 for all samples initially 
dbs_master_vars_clean2_qPCR$dnaextracted <- 0
# Mark the extracted samples as 1 in "extracted" if in "Pf_qPCR_tested_samples" (all samples extracted were tested for Pf)
dbs_master_vars_clean2_qPCR$dnaextracted[dbs_master_vars_clean2_qPCR$barcode %in% Pf_qPCR_tested_samples$barcode] <- 1
sum(dbs_master_vars_clean2_qPCR$dnaextracted == 1, na.rm = TRUE) #7164 samples extracted

# Mark the species_qPCR_tested samples as 1 if in "species"_qPCR_tested
dbs_master_vars_clean2_qPCR$Pf_qPCR_tested[dbs_master_vars_clean2_qPCR$barcode %in% Pf_qPCR_tested_samples$barcode] <- 1
dbs_master_vars_clean2_qPCR$Po_qPCR_tested[dbs_master_vars_clean2_qPCR$barcode %in% Po_qPCR_tested_samples$barcode] <- 1
dbs_master_vars_clean2_qPCR$Pm_qPCR_tested[dbs_master_vars_clean2_qPCR$barcode %in% Pm_qPCR_tested_samples$barcode] <- 1


# Check number of samples extracted and tested for each species
sum(dbs_master_vars_clean2_qPCR$dnaextracted == 1, na.rm = TRUE) #7,164 samples extracted
sum(dbs_master_vars_clean2_qPCR$Pf_qPCR_tested == 1, na.rm = TRUE) #7,164 Pf tested
sum(dbs_master_vars_clean2_qPCR$Po_qPCR_tested == 1, na.rm = TRUE) #0 Po tested
sum(dbs_master_vars_clean2_qPCR$Pm_qPCR_tested == 1, na.rm = TRUE) #0 Pm tested


# 6.5: Add the qPCR data for positive samples
# Load Pf_positive sample data csv as a data frame
Pf_qPCR_positive_samples <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\P.falciparum_positive_samples_FINAL.csv", stringsAsFactors = FALSE)
Po_qPCR_positive_samples <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\P.ovale_positive_samples_FINAL.csv", stringsAsFactors = FALSE)
Pm_qPCR_positive_samples <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\P.malariae_positive_samples_FINAL.csv", stringsAsFactors = FALSE)



#--------------------Cherrypicking Pm and Po positive samples--------------------------------------

# Figure out which samples have already been taken for Pf sequencing:

qPCR_positive <- dbs_master_vars_clean2_qPCR2 %>%
  select(barcode,qPCR_plate,po,pm,nonpf,species,species_count,malaria,pf_CT,pf_SQ,po_CT,po_SQ,pm_CT,pm_SQ)

qPCR_positive <- qPCR_positive[!is.na(qPCR_positive$malaria) & qPCR_positive$malaria == 1, ]


# Create the new data frame to obtain Po only samples


take_for_po <- Po_qPCR_positive_samples %>%
  filter(!barcode %in% Pf_qPCR_positive_samples$barcode)

Pm_qPCR_positive_samples$barcode <- trimws(Pm_qPCR_positive_samples$barcode)

take_for_pm <- Pm_qPCR_positive_samples %>%
  filter(!is.na(Pm_qPCR_positive_samples$pm_CT))

take_for_pm <- take_for_pm %>%
  filter(!barcode %in% Pf_qPCR_positive_samples$barcode)

take_for_pm <- take_for_pm %>%
  filter(!barcode %in% Po_qPCR_positive_samples$barcode)


# Use function to add the qPCR plate number, species_CT, and species_SQ to the data frame by barcode
dbs_master_vars_clean2_qPCR <- f_merge_data_by_keys(
  main_df = dbs_master_vars_clean2_qPCR,
  source_df = Pf_qPCR_positive_samples,
  by_keys = c("barcode"),
  vars_to_add = c("qPCR_plate", "pf_CT", "pf_SQ"))

dbs_master_vars_clean2_qPCR <- f_merge_data_by_keys(
  main_df = dbs_master_vars_clean2_qPCR,
  source_df = Po_qPCR_positive_samples,
  by_keys = c("barcode"),
  vars_to_add = c("po_CT", "po_SQ"))


# Make new df so that you can go back to dbs_master_vars_clean2_qPCR when you have all minor species data
dbs_master_vars_clean2_qPCR2 <- dbs_master_vars_clean2_qPCR


dbs_master_vars_clean2_qPCR2 <- f_merge_data_by_keys(
  main_df = dbs_master_vars_clean2_qPCR2,
  source_df = Pm_qPCR_positive_samples,
  by_keys = c("barcode"),
  vars_to_add = c("pm_CT", "pm_SQ"))


# 6.6: Remove noisy samples from final data
# Open and load list of noisy qPCR samples
noisy_PfqPCR_samples <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\Pf_qPCR_noisy_samples.csv", stringsAsFactors = FALSE) #5 samples
noisy_PoqPCR_samples <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\Po_qPCR_noisy_samples.csv", stringsAsFactors = FALSE)
noisy_PmqPCR_samples <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\Pm_qPCR_noisy_samples.csv", stringsAsFactors = FALSE)

#Mark all NOISY samples as 2 if in noisy_samples
dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested[dbs_master_vars_clean2_qPCR2$barcode %in% noisy_PfqPCR_samples$barcode] <- 2
dbs_master_vars_clean2_qPCR2$Po_qPCR_tested[dbs_master_vars_clean2_qPCR2$barcode %in% noisy_PoqPCR_samples$barcode] <- 2
dbs_master_vars_clean2_qPCR2$Pm_qPCR_tested[dbs_master_vars_clean2_qPCR2$barcode %in% noisy_PmqPCR_samples$barcode] <- 2

# Confirm the number of noisy samples
sum(dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested == 2, na.rm = TRUE) #5
sum(dbs_master_vars_clean2_qPCR2$Po_qPCR_tested == 2, na.rm = TRUE) #
sum(dbs_master_vars_clean2_qPCR2$Pm_qPCR_tested == 2, na.rm = TRUE) #

# Sum number Pf_qPCR tested samples minus noisy samples
sum(dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested == 1, na.rm = TRUE) #7,159
sum(dbs_master_vars_clean2_qPCR2$Po_qPCR_tested == 1, na.rm = TRUE) #
sum(dbs_master_vars_clean2_qPCR2$Pm_qPCR_tested == 1, na.rm = TRUE) #

# Determine the % of samples in each region that have been qPCRed

# Determine the % of samples in each cluster that have been qPCRed


#----------------------------Step 8: Create variables for qPCR data (new df)------------------------------------------------------------


# 7.1: Create malaria species binary variables
dbs_master_vars_clean2_qPCR2 <- dbs_master_vars_clean2_qPCR2 %>% mutate(pf = ifelse(is.na(pf_CT), 0, 1)) #pf
dbs_master_vars_clean2_qPCR2 <- dbs_master_vars_clean2_qPCR2 %>% mutate(po = ifelse(is.na(po_CT), 0, 1)) #po
dbs_master_vars_clean2_qPCR2 <- dbs_master_vars_clean2_qPCR2 %>% mutate(pm = ifelse(is.na(pm_CT), 0, 1)) #pm

# Check sum of positive samples for each species
sum(dbs_master_vars_clean2_qPCR2$pf == 1, na.rm = TRUE) #637 Pf positive samples
sum(dbs_master_vars_clean2_qPCR2$po == 1, na.rm = TRUE) #218 Po positive samples
sum(dbs_master_vars_clean2_qPCR2$pm == 1, na.rm = TRUE) #276 Pm positive samples

# 7.2: Create non-Pf binary variable
dbs_master_vars_clean2_qPCR2$nonpf <- ifelse(dbs_master_vars_clean2_qPCR2$pm == 1 | dbs_master_vars_clean2_qPCR2$po == 1, 1, 0)
# Check sum of positive samples for each species
sum(dbs_master_vars_clean2_qPCR2$nonpf == 1, na.rm = TRUE) #468 non-Pf positive samples

# 7.3: Create mixed infection variable (species)
dbs_master_vars_clean2_qPCR2$species <- ifelse(dbs_master_vars_clean2_qPCR2$pf == 1 & dbs_master_vars_clean2_qPCR2$po == 0 & dbs_master_vars_clean2_qPCR2$pm == 0, "pf",
                                        ifelse(dbs_master_vars_clean2_qPCR2$pf == 0 & dbs_master_vars_clean2_qPCR2$po == 0 & dbs_master_vars_clean2_qPCR2$pm == 1, "pm",
                                        ifelse(dbs_master_vars_clean2_qPCR2$pf == 0 & dbs_master_vars_clean2_qPCR2$po == 1 & dbs_master_vars_clean2_qPCR2$pm == 0, "po",
                                        ifelse(dbs_master_vars_clean2_qPCR2$pf == 1 & dbs_master_vars_clean2_qPCR2$po == 1 & dbs_master_vars_clean2_qPCR2$pm == 0, "pf_po",
                                        ifelse(dbs_master_vars_clean2_qPCR2$pf == 1 & dbs_master_vars_clean2_qPCR2$po == 0 & dbs_master_vars_clean2_qPCR2$pm == 1, "pf_pm",
                                        ifelse(dbs_master_vars_clean2_qPCR2$pf == 0 & dbs_master_vars_clean2_qPCR2$po == 1 & dbs_master_vars_clean2_qPCR2$pm == 1, "pm_po",
                                        ifelse(dbs_master_vars_clean2_qPCR2$pf == 1 & dbs_master_vars_clean2_qPCR2$po == 1 & dbs_master_vars_clean2_qPCR2$pm == 1, "pf_pm_po",
                                        ifelse(dbs_master_vars_clean2_qPCR2$pf == 0 & dbs_master_vars_clean2_qPCR2$po == 0 & dbs_master_vars_clean2_qPCR2$pm == 0, "none", "none"))))))))

# 7.4: #Create species_count variable This sums the number of non-missing values across the columns pf, pm, po, and pv for each row. na.rm = TRUE ensures that missing values (NA) are ignored in the summation.
dbs_master_vars_clean2_qPCR2$species_count <- rowSums(dbs_master_vars_clean2_qPCR2[, c("pf", "pm", "po")], na.rm = TRUE)

# 7.5: Create infection complexity variable (infection)
dbs_master_vars_clean2_qPCR2 <- dbs_master_vars_clean2_qPCR2 %>% mutate(infection = case_when(species_count>1 ~ "co",species_count==1 ~ "mono", species_count==0 ~ "none"))
sum(dbs_master_vars_clean2_qPCR2$infection == "co", na.rm = TRUE) #141 co-infection samples


# 7.6: Create malaria binary variable for malaria case counts (malaria, 1= any species present, 0= no malaria)
dbs_master_vars_clean2_qPCR2 <- dbs_master_vars_clean2_qPCR2 %>% mutate(malaria = case_when(species_count>0 ~ 1, species_count==0 ~ 0))







#----------------------------Step 9: Make a survey set for RW19 survey (random sample + remaining samples in high prevalence clusters)----------------------------------------------

# Random selection1: 50% of samples from all clusters (random_selection) (6957 samples + 6 extras that were scanned = 6963)
# Random selection2: Random Selection1 + any malaria positive samples (RDT or microscopy) that were not selected randomly (7005 samples + 6 extras that were scanned)
# qPCR19 data set: Random selection 2 + samples in high prevelance clusters that were not included in random selection 2 (7164 samples)
# Pf tested samples: qPCR19 data set (7164) - 5 noisy samples

# survey19_r_hp data set: Random Selection1 + samples in high prevelance clusters that were not included in random selection 1
dbs_master_vars_clean2_qPCR2 <- dbs_master_vars_clean2_qPCR2 %>%
  mutate(survey19_r_hp = if_else(random_selection == 1 | hv001 %in% highprev_clusters, 1, 0))
sum(dbs_master_vars_clean2_qPCR2$survey19_r_hp == 1, na.rm = TRUE) #7168 samples in the data set
sum(dbs_master_vars_clean2_qPCR2$survey19_r_hp == 1 & dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested == 1, na.rm = TRUE) #7127 tested
sum(dbs_master_vars_clean2_qPCR2$survey19_r_hp == 1 & dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested == 0, na.rm = TRUE) #36 not tested



#inclusion into study based on Pf_qPCR_tested status AND Random Selection1 + high prevelance cluster samples
dbs_master_vars_clean2_qPCR2 <- dbs_master_vars_clean2_qPCR2 %>% 
  mutate(selectpf19 = if_else(dbs_master_vars_clean2_qPCR2$survey19_r_hp == 1 & dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested == 1, 1, 0))
sum(dbs_master_vars_clean2_qPCR2$selectpf19 == 1, na.rm = TRUE) #7127 samples tested for random sample + extra high prev samples




# Make variables designating samples selected in original random selection trials 1 and 2 (trial 1 + any unselected samples that were RDT or microscopy pos by DHS)
dbs_master_vars_clean2_qPCR2$selected_DBS_trial1[dbs_master_vars_clean2_qPCR2$barcode %in% selected_DBS_trial1$barcode] <- 1
sum(dbs_master_vars_clean2_qPCR2$selected_DBS_trial1 == 1, na.rm = TRUE) #6957
sum(dbs_master_vars_clean2_qPCR2$selected_DBS_trial1 == 1 & dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested == 0, na.rm = TRUE) #16 samples in random selection were not Pf qPCR tested


dbs_master_vars_clean2_qPCR2$selected_DBS_trial2[dbs_master_vars_clean2_qPCR2$barcode %in% selected_DBS_trial2$barcode] <- 1
sum(dbs_master_vars_clean2_qPCR2$selected_DBS_trial2 == 1, na.rm = TRUE) #7005
sum(dbs_master_vars_clean2_qPCR2$selected_DBS_trial2 == 1 & dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested == 0, na.rm = TRUE) #16 samples in random selection + Pf positives were not Pf qPCR tested (so all 32 extra malaria positives were Pf qPCR tested)


sum(dbs_master_vars_clean2_qPCR2$selectpf19 == 1, na.rm = TRUE) #7127 samples tested for random sample + extra high prev samples
sum(dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested == 1, na.rm = TRUE) #7159
sum(dbs_master_vars_clean2_qPCR2$survey19_r_hp == 1 & dbs_master_vars_clean2_qPCR2$bin_cat == "ss_malpos", na.rm = TRUE)  #70 samples in high prev and random sample that are mal pos
sum(dbs_master_vars_clean2_qPCR2$random_selection == 1 & dbs_master_vars_clean2_qPCR2$bin_cat == "ss_malpos", na.rm = TRUE)  #53 samples in random selection that are mal pos
sum(dbs_master_vars_clean2_qPCR2$bin_cat == "ss_malpos", na.rm = TRUE) #101 DBS samples are mal pos
sum(dbs_master_vars_clean2_qPCR2$random_selection == 1, na.rm = TRUE) #6963




#----------------------------Step 10: Calculate Malaria Transmission Intensity Variables and Weights----------------------------------------------

#-- 9.1: Make transmission intensity variable (high/low)

# High transmission clusters (hv001) from 2019 DHS analysis 1 file
highprev_clusters <- c(25, 94, 183, 217, 218, 242, 245, 276, 319, 324, 341, 369, 381, 390, 426, 469)

#mark all samples "low" initially 
dbs_master_vars_clean2_qPCR2$trans_intens <- "low" 
# Mark samples in high transmission clusters "high"
dbs_master_vars_clean2_qPCR2$trans_intens[dbs_master_vars_clean2_qPCR2$hv001 %in% high_prev_clusters] <- "high"

# Check: Filter and count the number of rows in dbs_master_vars_clean2a where hv001 matches high prev
n_rows <- dbs_master_vars_clean2_qPCR2 %>%
  filter(hv001 %in% high_prev_clusters) %>%
  nrow() # 407 samples in high prevalence

# Check: high and low transmission SAMPLE NUMBERS
addmargins(table(dbs_master_vars_clean2_qPCR2$trans_intens, useNA = "always")) #407 high prev, 13534 low prev samples


#--------------New df for qPCR tested samples------------------

# Make new df for qPCR tested samples
rw19_pftested <- dbs_master_vars_clean2_qPCR2[!is.na(dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested) & dbs_master_vars_clean2_qPCR2$Pf_qPCR_tested == 1, ] #7159 samples (7164 total samples tested -5 noisy/indeterminant samples)
sum(rw19_pftested$pf == 1, na.rm = TRUE) #636 pf-positive samples


# Make new df for Pf qPCR tested random selection + high prev samples
selectpf19 <- dbs_master_vars_clean2_qPCR2[!is.na(dbs_master_vars_clean2_qPCR2$selectpf19) & dbs_master_vars_clean2_qPCR2$selectpf19 == 1, ] #7127 samples tested
sum(selectpf19$pf == 1, na.rm = TRUE) #616 pf-positive samples



#-- 9.2: Make transmission intensity weights (high/low)

# Count number of individuals in high/low clusters in all DBS data set
overall <- dbs_master_vars_clean2_qPCR2 %>% group_by(trans_intens) %>% count()

# Count number of individuals in high/low clusters in Pf qPCR tested random selection + high prev samples
pfsampled19 <- selectpf19 %>% group_by(trans_intens) %>% count()

#forming the numerator of the transmission intensity weight
high_ov <- overall[overall$trans_intens=="high",]$n / sum(overall$n)
low_ov <- overall[overall$trans_intens=="low",]$n / sum(overall$n)

#forming the denominator of the transmission intensity weight
high_samp <- pfsampled19[pfsampled19$trans_intens=="high",]$n / sum(pfsampled19$n)
low_samp <- pfsampled19[pfsampled19$trans_intens=="low",]$n / sum(pfsampled19$n)

#final transmission intensity weight
high_wt <- high_ov/high_samp
low_wt <- low_ov/low_samp

#add transmission intensity weights onto study datasets 
dbs_master_vars_clean2_qPCR2<-dbs_master_vars_clean2_qPCR2 %>% mutate(trans_wt=case_when(
  trans_intens=="high"~high_wt,
  trans_intens=="low"~low_wt))




#----------------------------Step 11: Calculate Inverse Propensity for Selection Weight----------------------------------------------

#calculate the final weights
#inverse propensity of selection, HIV sampling, & transmission intensity weighting

# Propensity score for samples included in the study
#hv006=month of household interview
#hv024= region
#hv025= urban/rural (urban = 1, rural = 2)
#ALT_DEM= altitude
#hv104= sex
#highest_educational_level= highest level of education
#hv246= number of livestock total
#hml1= number of mosquito nets household owns
#hml20= Person slept under an LLIN net
#hv270= Wealth index quintile
#age_cat10= Age (categorical- 15-24=1, 25-34=2, 35-44=3, 45-54=4, 55-59=5)
#Not used due to high NA: hml10= net is treated, hv201= source of drinking water




#Used highest_educational_level (merge of m/f ha66 and hb66) instead of hv106, as hv106 has 7 NA and all variables have the same standardized levels. **475 of these differ in their answers
ps_model <- glm(selectpf19 ~ hv006+hv024+hv025+ALT_DEM+hv104+highest_educational_level+hv246+hml1+hml20+hv270+age_cat10+dhs_temp+rain,family=binomial("logit"), data=dbs_master_vars_clean2_qPCR2) 
#Add ps to dataframe
dbs_master_vars_clean2_qPCR2$ps <- predict(ps_model, dbs_master_vars_clean2_qPCR2, type = "response")
# Count NA propensity scores
sum(is.na(dbs_master_vars_clean2_qPCR2$ps)) #0 NA values


summary(dbs_master_vars_clean2_qPCR2$ps, useNA = "always") 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.4280  0.4969  0.5118  0.5112  0.5261  0.5845 


# Make standardized and unstandardized inverse propensity weights

#unstandardized inverse propensity weights
dbs_master_vars_clean2_qPCR2$ipwt_u <- ifelse(dbs_master_vars_clean2_qPCR2$selectpf19==1, 1/dbs_master_vars_clean2_qPCR2$ps, 1/(1-dbs_master_vars_clean2_qPCR2$ps))
summary(dbs_master_vars_clean2_qPCR2$ipwt_u, useNA = "always")
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.711   1.930   1.996   2.000   2.067   2.385 


#standardized inverse propensity weights
p_exposure <- sum(dbs_master_vars_clean2_qPCR2$selectpf19) / nrow(dbs_master_vars_clean2_qPCR2) #Pf qPCR tested random selection + high prev samples/ all DBS samples
dbs_master_vars_clean2_qPCR2$ipwt <- ifelse(dbs_master_vars_clean2_qPCR2$selectpf19==1, p_exposure/dbs_master_vars_clean2_qPCR2$ps, (1-p_exposure)/(1-dbs_master_vars_clean2_qPCR2$ps))
summary(dbs_master_vars_clean2_qPCR2$ipwt, useNA = "always")
#Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
#0.8544  0.9698  0.9982  1.0000  1.0281  1.1915 




#----------------------------Step 12: Make Corrected HIV Weight Variable-------------------


# Add corrected HIV weight variable (weight divided by 1,000,000, as instructed by DHS)
dbs_master_vars_clean2_qPCR2$hiv_weight <- (dbs_master_vars_clean2_qPCR2$hiv05)/1000000


#----------------------------Step 13: Calculate Final Weights---------


## HIV and inverse propensity weight
# Add HIV AND inverse propensity weight variables (HIV weight * IP weight)
dbs_master_vars_clean2_qPCR2$hiv_ipwt_weight <- dbs_master_vars_clean2_qPCR2$hiv_weight*dbs_master_vars_clean2_qPCR2$ipwt
summary(dbs_master_vars_clean2_qPCR2$hiv_ipwt_weight, useNA = "always")
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.09333 0.83189 0.97176 0.99899 1.13060 3.47476 



## Calculate the final weights
#wt: HIV * IPSW * trans_wt
dbs_master_vars_clean2_qPCR2$final_weight<-(dbs_master_vars_clean2_qPCR2$hiv_ipwt_weight)*(dbs_master_vars_clean2_qPCR2$trans_wt) 
summary(dbs_master_vars_clean2_qPCR2$final_weight, useNA = "always")
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.09578 0.83691 0.99225 1.01210 1.15720 3.56595 


# Confirm no NA weights
sum(is.na(dbs_master_vars_clean2_qPCR2$hiv_weight))
sum(is.na(dbs_master_vars_clean2_qPCR2$ipwt))
sum(is.na(dbs_master_vars_clean2_qPCR2$trans_wt))
sum(is.na(dbs_master_vars_clean2_qPCR2$final_weight))


#----------------------------Step 14: Make additional variables for glms-------------------

dbs_master_vars_clean2_qPCR2$one <- 1

# Make final survey19 df for Pf qPCR tested random selection + high prev samples (Our analysis will NOT include the extra DHS-reported malaria-postive samples)
survey19 <- dbs_master_vars_clean2_qPCR2 %>% 
  filter(selectpf19 == 1)

#factor variables
survey19$age_cat10 <- as.factor(survey19$age_cat10) #Age in 10-year increments
survey19$hv270 <- as.factor(survey19$hv270) #wealth index (combined)
survey19$highest_educational_level <- as.factor(survey19$highest_educational_level) #highest educational level attained
survey19$hv024 <- as.factor(survey19$hv024) #Region
survey19$hv006 <- as.factor(survey19$hv006) #Month of interview
survey19$hv006 <- as.factor(survey19$hv006) #Month of interview

#education binary = educat_bin
survey19 <- survey19 %>% mutate(educat_bin = case_when(
  highest_educational_level == "0" ~ "primary school or below",
  highest_educational_level == "1" ~ "primary school or below",
  highest_educational_level == "2" ~ "secondary or higher",
  highest_educational_level == "3" ~ "secondary or higher"))

#wealth binary for glms = wealth_bin
survey19 <- survey19 %>% mutate(wealth_bin = case_when(
  hv270 == "1" ~ "wealth quintiles 1 & 2",
  hv270 == "2" ~ "wealth quintiles 1 & 2",
  hv270 == "3" ~ "quintiles 3+",
  hv270 == "4" ~ "quintiles 3+",
  hv270 == "5" ~ "quintiles 3+"))

#regroup age categories = age_bin
survey19 <- survey19 %>% mutate(age_bin = case_when(
  age_cat10 == "1" ~ "0-24 years",
  age_cat10 == "2" ~ "24 years or older",
  age_cat10 == "3" ~ "24 years or older",
  age_cat10 == "4" ~ "24 years or older",
  age_cat10 == "5" ~ "24 years or older"))

# Make Land cover/ land use variables scaled by 10 percentage points so effect size is easier to read
# Land use variables
land_vars19 <- c("water_percent","trees_percent","flooded_vegetation_percent","crops_percent",
  "built_area_percent","bare_ground_percent","snowice_percent","clouds_percent","rangeland_percent")

# create new variables divided by 10 and rename with "10" at the end (dividing by a factor of 10)
for (var in land_vars19) {
  new_var <- paste0(var, "10")
  survey19[[new_var]] <- survey19[[var]] / 10
}





#----------------------------Step 15: Make an under 40 CT values data set-------------------

# 13.1: Make under 40 CT values data set
survey19_40<-survey19

survey19_40$pf_CT[survey19_40$pf_CT > 40]<-NA
survey19_40$po_CT[survey19_40$po_CT > 40]<-NA
survey19_40$pm_CT[survey19_40$pm_CT > 40]<-NA

## CHECKS:

# count number of NAs in each data set
sum(is.na(survey19$pf_CT)) #6511
sum(is.na(survey19_40$pf_CT)) #6542

# count number of pf CT values <40 in each data set
sum(survey19$pf_CT < 40, na.rm = TRUE) # 585 samples with pf_CT < 40
sum(survey19$pf_CT > 40, na.rm = TRUE) # 31 samples with pf_CT > 40
sum(survey19_40$pf_CT < 40, na.rm = TRUE) # 585 samples with pf_CT < 40
sum(survey19_40$pf_CT > 40, na.rm = TRUE) # 0 samples with pf_CT > 40

# 13.3: Create malaria species binary variables
survey19_40 <- survey19_40 %>% mutate(pf = ifelse(is.na(pf_CT), 0, 1)) #pf
survey19_40 <- survey19_40 %>% mutate(po = ifelse(is.na(po_CT), 0, 1)) #po
survey19_40 <- survey19_40 %>% mutate(pm = ifelse(is.na(pm_CT), 0, 1)) #pm


# Check sum of positive samples for each species
sum(survey19$pf == 1, na.rm = TRUE) #616 Pf positive samples
sum(survey19_40$pf == 1, na.rm = TRUE) #585 Pf positive samples >40 CT
sum(survey19$po == 1, na.rm = TRUE) #213 Po positive samples
sum(survey19_40$po == 1, na.rm = TRUE) #94 Po positive samples >40 CT
sum(survey19$pm == 1, na.rm = TRUE) #271 Pm positive samples
sum(survey19_40$pm == 1, na.rm = TRUE) #255 Pm positive samples >40 CT

# 13.4: Create non-Pf binary variable
survey19_40$nonpf <- ifelse(survey19_40$pm == 1 | survey19_40$po == 1, 1, 0)


# 13.5: Create mixed infection variable (species)
survey19_40$species <- ifelse(survey19_40$pf == 1 & survey19_40$po == 0 & survey19_40$pm == 0, "pf",
                       ifelse(survey19_40$pf == 0 & survey19_40$po == 0 & survey19_40$pm == 1, "pm",
                       ifelse(survey19_40$pf == 0 & survey19_40$po == 1 & survey19_40$pm == 0, "po",
                       ifelse(survey19_40$pf == 1 & survey19_40$po == 1 & survey19_40$pm == 0, "pf_po",
                       ifelse(survey19_40$pf == 1 & survey19_40$po == 0 & survey19_40$pm == 1, "pf_pm",
                       ifelse(survey19_40$pf == 0 & survey19_40$po == 1 & survey19_40$pm == 1, "pm_po",
                       ifelse(survey19_40$pf == 1 & survey19_40$po == 1 & survey19_40$pm == 1, "pf_pm_po",
                       ifelse(survey19_40$pf == 0 & survey19_40$po == 0 & survey19_40$pm == 0, "none", "none"))))))))


# 13.5: #Create species_count variable This sums the number of non-missing values across the columns pf, pm, po, and pv for each row.
survey19_40$species_count <- rowSums(survey19_40[, c("pf", "pm", "po")], na.rm = TRUE)

# 13.6: Create infection complexity variable (infection)
survey19_40 <- survey19_40 %>% mutate(infection = case_when(species_count>1 ~ "co",species_count==1 ~ "mono", species_count==0 ~ "none"))

# 13.7: Create malaria binary variable for malaria case counts (malaria, 1= any species present, 0= no malaria)
survey19_40 <- survey19_40 %>% mutate(malaria = case_when(species_count>0 ~ 1, species_count==0 ~ 0))



#----------------------------Step 16: DHS analysis designs-----------------------------------

#DHS19_allDBS: wt = HIV*ipwt*trans_wt (all DBS)
DHS19_allDBS<-svydesign(id=dbs_master_vars_clean2_qPCR2$hv021, strata=dbs_master_vars_clean2_qPCR2$hv023, weights=dbs_master_vars_clean2_qPCR2$final_weight, data=dbs_master_vars_clean2_qPCR2, nest=TRUE)
DHS19_allDBS<-as_survey_design(DHS19_allDBS)

#DHS19_allDBS_nowt: unweighted (all DBS)
dbs_master_vars_clean2_qPCR2$no_wt <- 1
DHS19_allDBS_nowt<-svydesign(id=dbs_master_vars_clean2_qPCR2$hv021, strata=dbs_master_vars_clean2_qPCR2$hv023, weights=dbs_master_vars_clean2_qPCR2$no_wt, data=dbs_master_vars_clean2_qPCR2, nest=TRUE)
DHS19_allDBS_nowt<-as_survey_design(DHS19_allDBS_nowt)

#DHS19: wt = HIV*ipwt*trans_wt (DHS19 survey)
DHS19<-svydesign(id=survey19$hv021, strata=survey19$hv023, weights=survey19$final_weight, data=survey19, nest=TRUE)
DHS19<-as_survey_design(DHS19)

#DHS19_nowt: no weight, for unweighted proportions
survey19$no_wt <- 1
DHS19_nowt<-svydesign(id=survey19$hv021, strata=survey19$hv023, weights=survey19$no_wt, data=survey19, nest=TRUE)
DHS19_nowt<-as_survey_design(DHS19_nowt)

#DHS19_40: wt (DHS19 survey, but 40 cycles CT cutoff dataset)
DHS19_40<-svydesign(id=survey19_40$hv021, strata=survey19_40$hv023, weights=survey19_40$final_weight, data=survey19_40, nest=TRUE)
DHS19_40<-as_survey_design(DHS19_40)

#DHS19_40_nowt: wt (DHS19 survey, but 40 cycles CT cutoff dataset)
survey19_40$no_wt <- 1
DHS19_40_nowt<-svydesign(id=survey19_40$hv021, strata=survey19_40$hv023, weights=survey19_40$no_wt, data=survey19_40, nest=TRUE)
DHS19_40_nowt<-as_survey_design(DHS19_40_nowt)


#svydesign for high & low transmission cluster strata
highpf_svy<-survey19 %>% filter(trans_intens=="high")
lowpf_svy<-survey19 %>% filter(trans_intens=="low")

DHS19_highprev<-svydesign(id=highpf_svy$hv021, strata=highpf_svy$hv023, weights=highpf_svy$final_weight, data=highpf_svy, nest=TRUE)
DHS19_highprev<-as_survey_design(DHS19_highprev)

DHS19_lowprev<-svydesign(id=lowpf_svy$hv021, strata=lowpf_svy$hv023, weights=lowpf_svy$final_weight, data=lowpf_svy, nest=TRUE)
DHS19_lowprev<-as_survey_design(DHS19_lowprev)


#svydesign for male and female participants
survey19_f<-survey19 %>% filter(hv104==2) # n= female
DHS19_f<-svydesign(id=survey19_f$hv021, strata=survey19_f$hv023, weights=survey19_f$final_weight, data=survey19_f, nest=TRUE)
DHS19_f<-as_survey_design(DHS19_f)

survey19_m<-survey19 %>% filter(hv104==1) # n= male
DHS19_m<-svydesign(id=survey19_m$hv021, strata=survey19_m$hv023, weights=survey19_m$final_weight, data=survey19_m, nest=TRUE)
DHS19_m<-as_survey_design(DHS19_m)


write.csv(survey19, file = "survey19.csv", row.names = FALSE)
write.csv(survey19_40, file = "survey19_40.csv", row.names = FALSE)
write.csv(dbs_master_vars_clean2_qPCR2, file = "dbs_master_vars_clean2_qPCR2.csv", row.names = FALSE)

#----------- Various statistics for publication----------

# Obtaining stats on samples tested per cluster
summary(cluster_survey19$tested_for_pf, useNA = "always")
