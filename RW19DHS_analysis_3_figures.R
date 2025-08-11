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

# Create new dataframe called dbs_master, retaining only individuals in household member recode who have an HIV (DBS) sample barcode
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

# 3.2.1: Get all variables and their descriptions
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
df <- f_merge_variables_by_keys(
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
corrected_HIVweight <- read.csv("C:\\Users\\jzuromsk\\Documents\\DHS_2019\\HIVweightcheck19.csv")

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
overall_avg_rain2020 <- mean(geospatial_covar$Rainfall_2020, na.rm = TRUE)
summary(geospatial_covar$Rainfall_2020, useNA = "always")
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#874.6  1156.4  1221.6  1236.4  1292.1  1683.1 


# Create a new categorical variable based on the comparison
dbs_master_vars_clean2$rain_cat <- ifelse(
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

#create rain variable, which is the prior month's average in mm (240 variables)
dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>% mutate(rain = case_when(hv006 == 10 ~ `09`, hv006 == 11 ~ `10`, 
                                             hv006 == 12 ~ `11`, hv006 == 01 ~ `12`,
                                             hv006 == 02 ~ `01`, hv006 == 03 ~ `02`, 
                                             hv006 == 04 ~`03`, hv006 == 05 ~ `04`,
                                             hv006 == 06 ~ `05`, hv006 == 07 ~ `06`,
                                             hv006 == 08 ~ `07`))



#--- Make temp categorical variables from DHS-----------------

# Make avg temperature categorical variable (by cluster)
# Obtain average temperature across 500 clusters
overall_avg_temp2020 <- mean(geospatial_covar$Mean_Temperature_2020, na.rm = TRUE)
summary(geospatial_covar$Mean_Temperature_2020, useNA = "always")
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#16.83   18.11   19.38   19.16   19.96   21.45

# Create a new categorical variable based on the comparison
dbs_master_vars_clean2$temp_cat <- ifelse(
  dbs_master_vars_clean2$temp2020 >= overall_avg_temp2020,
  "at or above avg. temp",
  "below avg. temp" )

#----- Match cluster monthly temperature to participants by survey month (242 variables)

dbs_master_vars_clean2 <- dbs_master_vars_clean2 %>% mutate(dhs_temp = case_when(hv006 == 11 ~ Temperature_November,
                                                   hv006 == 12 ~ Temperature_December, hv006 == 1 ~ Temperature_January,
                                                   hv006 == 2 ~ Temperature_February, hv006 == 3 ~ Temperature_March, 
                                                   hv006 == 4 ~ Temperature_April, hv006 == 5 ~ Temperature_May,
                                                   hv006 == 6 ~ Temperature_June, hv006 == 7 ~ Temperature_July,
                                                   hv006 == 8 ~ Temperature_August, hv006 == 9 ~ Temperature_September,
                                                   hv006 == 10 ~ Temperature_October))

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
dbs_master_vars_clean2_qPCR <- dbs_master_vars_clean2 #251 variables


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
sum(dbs_master_vars_clean2_qPCR2$pm == 1, na.rm = TRUE) #86 Pm positive samples

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
#hv040= altitude
#hv104= sex
#highest_educational_level= highest level of education
#hv246= number of livestock total
#hml1= number of mosquito nets household owns
#hml20= Person slept under an LLIN net
#hv270= Wealth index quintile
#hv105_cat= Age (categorical)
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
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
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

#survey19$landcover <- as.factor(survey19$landcover) #Landcover


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
  age_cat10 == "5" ~ "24 years or older",
  age_cat10 == "6" ~ "0-24 years"))



#landcover re-classification = landuse_bin
survey19 <- survey19 %>% mutate(landuse_bin = case_when(
  landcover == "1" ~ "forest or woodland",
  landcover == "2" ~ "forest or woodland",
  landcover == "3" ~ "forest or woodland",
  landcover == "4" ~ "crop/grassland",
  landcover == "8" ~ "crop/grassland",))

#----------------------------Step 15: Make an under 40 CT values data set-------------------

# 13.1: Make under 40 CT values data set
survey19_40<-survey19

survey19_40$pf_CT[survey19_40$pf_CT > 40]<-NA
survey19_40$po_CT[survey19_40$po_CT > 40]<-NA
survey19_40$pm_CT[survey19_40$po_CT > 40]<-NA

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
sum(survey19_40$pf == 1, na.rm = TRUE) #585 Pf positive samples >
sum(survey19$po == 1, na.rm = TRUE) #213 Po positive samples
sum(survey19_40$po == 1, na.rm = TRUE) #94 Po positive samples
sum(survey19$pm == 1, na.rm = TRUE) #271 Pm positive samples
sum(survey19_40$pm == 1, na.rm = TRUE) #271 Pm positive samples

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


# 13.5: #Create species_count variable This sums the number of non-missing values across the columns pf, pm, po, and pv for each row. na.rm = TRUE ensures that missing values (NA) are ignored in the summation.
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


#----------------------------Step 17: Obtain prevalence data for maps/tables and written results-----
#prop.table(svytable(~pf, DHS_design))
total_est<-svyciprop(~malaria, DHS19, method="lo")
pf_est<-svyciprop(~pf, DHS19, method="lo")
pm_est<-svyciprop(~pm, DHS19, method="lo")
po_est<-svyciprop(~po, DHS19, method="lo")
nonpf_est<-svyciprop(~nonpf, DHS19, method="lo")


svyciprop(~malaria, DHS19, method="lo")
svyciprop(~pf, DHS19, method="lo")
svyciprop(~po, DHS19, method="lo")
svyciprop(~pm, DHS19, method="lo")
svyciprop(~nonpf, DHS19, method="lo")

#40-cycle CT cutoff data & prevalence
svyciprop(~malaria, DHS19_40, method="lo")
svyciprop(~pf, DHS19_40, method="lo")
svyciprop(~po, DHS19_40, method="lo")
svyciprop(~pm, DHS19_40, method="lo")
svyciprop(~nonpf, DHS19_40, method="lo")


#WEIGHTED Pf TESTED sample counts by district (45 cycles)
district_countPFtestedwt19<-as.data.frame(svyby(~one, ~DHSREGNA, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
  rename(one_se = se, one_count = one, District = DHSREGNA)

#district_countMALwt19<-as.data.frame(svyby(~malaria, ~DHSREGNA, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
#  rename(malaria_se = se, malaria_count = malaria)
district_countPFwt19<-as.data.frame(svyby(~pf, ~DHSREGNA, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
  rename(pf_se = se, pf_count = pf, District = DHSREGNA)
#district_countPOwt19<-as.data.frame(svyby(~po, ~DHSREGNA, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
#  rename(po_se = se, po_count = po)
#as.data.frame(svyby(~pm, ~DHSREGNA, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
#  rename(pm_se = se, pm_count = pm)

#prevalence counts by district (40 cycles)
#district_countMALwt19_40<-as.data.frame(svyby(~malaria, ~DHSREGNA, DHS19_40, svytotal, survey.lonely.psu="adjust")) %>%
#  rename(malaria_se_40 = se, malaria_count_40 = malaria)
district_countPFwt19_40<-as.data.frame(svyby(~pf, ~DHSREGNA, DHS19_40, svytotal, survey.lonely.psu="adjust")) %>%
  rename(pf_se_40 = se, pf_count_40 = pf, District = DHSREGNA)
#district_countPOwt19_40<-as.data.frame(svyby(~po, ~DHSREGNA, DHS19_40, svytotal, survey.lonely.psu="adjust")) %>%
  #rename(po_se_40 = se, po_count_40 = po)
#as.data.frame(svyby(~pm, ~DHSREGNA, DHS19_40, svytotal, survey.lonely.psu="adjust")) %>%
#rename(pm_se_40 = se, pm_count_40 = po)


# Prevalence counts by PROVINCES (45 cycles)
# Weighted sample counts by province
province_countPFtestedwt19<-as.data.frame(svyby(~one, ~ADM1NAME, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
  rename(one_se = se, one_count = one, Province = ADM1NAME)
# Weighted Pf counts by province
province_countPFwt19<-as.data.frame(svyby(~pf, ~ADM1NAME, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
  rename(pf_se = se, pf_count = pf, Province = ADM1NAME)





#----------------------------Mixed infection weighted and unweighted counts--------------------------
#mixed infection unweighted counts (table s4)
mixed_count19<-as.data.frame(survey19 %>% group_by(species) %>% summarise(Total=n()/nrow(.)))
mixed_count19$count<-(mixed_count19$Total)*7127 #total number of samples tested
print(mixed_count19)
infection19<-as.data.frame(survey19 %>% group_by(infection) %>% summarise(Percentage=n()/nrow(.)))

#mixed infection unweighted counts (40 cycles cutoff)
mixed_count19_40<-as.data.frame(survey19_40 %>% group_by(species) %>% summarise(Total=n()/nrow(.)))
mixed_count19_40$count<-(mixed_count19_40$Total)*7127
print(mixed_count19_40)
infection19_40<-as.data.frame(survey19_40 %>% group_by(infection) %>% summarise(Percentage=n()/nrow(.)))

#mixed infection weighted counts (45 cycles)
w_mixed_count19<-as.data.frame(svyby(~one, ~species, DHS19, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~one, ~infection, DHS19, svytotal, survey.lonely.psu="adjust"))
print(w_mixed_count19)

#mixed infection weighted counts (40 cycles cutoff)
w_mixed_count19_40<-as.data.frame(svyby(~one, ~species, DHS19_40, svytotal, survey.lonely.psu="adjust"))
as.data.frame(svyby(~one, ~infection, DHS19_40, svytotal, survey.lonely.psu="adjust"))
print(w_mixed_count19_40)


#----------------------------Summmary tables: Supplemental table 2 Total weighted counts for study population covariates-----------------------

#elev1500_bin <- elev1500 (above or below 1500m)
#hv006=month of household interview
#ADM1DHS<- hv024 =region (numerical)
#hv025= urban/rural (urban = 1, rural = 2)
#hv040= altitude
#ALT_DEM_cat <-hv040_cat (0, 500, 1000, 1500, 2000, 2500, Inf altitude in m)
#hv104= sex
#age_cat10 <- hv105_cat= Age (categorical)
#highest_educational_level <- hv106= highest educational level
#hv201_cat <- source of drinking water (1 = piped, 0 = unpiped)
#hv270= Wealth index quintile
#hv246= number of livestock total
#bednetper_cat <- y/n 1 bed net per 1.8 de jure household members
#hml1_cat <- bed net ownership (0 = does not own net, 1 = owns net)
#hml10= net is treated
#hml20= Person slept under an LLIN net

# SUPP Table #2: Comparison of Population Used for Molecular Screening to Overall DHS Population (weighted counts)
rw19vars<-c("hv104","age_cat10","hv270","highest_educational_level","owns_livestock","hv201_cat","hml1_cat","hml20","hml10",
            "bednetper_cat","ADM1DHS","hv025","elev1500_bin","hv006", "ALT_DEM_cat")
svyvars<-c("hv104","age_cat10","hv270","highest_educational_level","owns_livestock","hv201_cat","hml1_cat","hml20","hml10",
           "bednetper_cat","ADM1DHS","hv025","elev1500_bin","hv006","landcover")



DHS_count <- function(outcome, group_vars, design_obj, method) {
  outcome_formula <- as.formula(paste0("~", outcome))
  group_formula <- as.formula(paste0("~", group_vars))
  survey_fun <- method
  
  m <- svyby(
    formula = outcome_formula,
    by = group_formula,
    design = design_obj,
    FUN = survey_fun,
    vartype = c("se", "ci"),
    survey.lonely.psu = "adjust",
    na.rm = TRUE
  )
  
  df <- as.data.frame(m)
  names(df)[1] <- "Level"
  df$Level <- as.character(df$Level)
  df$Variable <- group_vars
  df
}

# Call the function

# Weighted counts, all DBS samples
supptbl2_DHS19_allDBS <- map_dfr(rw19vars, ~DHS_count(outcome = "one", group_vars = .x, design_obj = DHS19_allDBS, method = svytotal))

# Unweighted counts, all DBS samples
supptbl2_DHS19_allDBS_nowt <- map_dfr(rw19vars, ~DHS_count(outcome = "one", group_vars = .x, design_obj = DHS19_allDBS_nowt, method = svytotal))

# Weighted counts, survey samples only
supptbl2_DHS19 <- map_dfr(rw19vars, ~DHS_count(outcome = "one", group_vars = .x, design_obj = DHS19, method = svytotal))

# Unweighted counts, survey samples only
supptbl2_DHS19_nowt <- map_dfr(rw19vars, ~DHS_count(outcome = "one", group_vars = .x, design_obj = DHS19_nowt, method = svytotal))





#----------------------------Table 1 numbers (study population by malaria species)------------------------
# WEIGHTED P. falciparum positive counts
Table1_19 <- map_dfr(rw19vars, ~DHS_count(outcome = "pf", group_vars = .x, design_obj = DHS19, method = svytotal))
Table1_19_nowt <- map_dfr(rw19vars, ~DHS_count(outcome = "pf", group_vars = .x, design_obj = DHS19_nowt, method = svytotal))


# Weighted counts are LOWER than raw counts, which is expected since we sampled all DBS from high-transmission clusters and thus are overrepresenting the high-risk clusters



#Po <- batch_svy_summary(outcome = "po", group_vars = rw19vars, design_obj = DHS19)
#Pm <- batch_svy_summary(outcome = "pm", group_vars = rw19vars, design_obj = DHS19)



#----------------------------Weighted prevalence table background characteristics-------------
#hv104= sex (male = 1, female = 2)
#age_bin <- age (0-24 years or 24+ years old)
#hv105 = age
#wealth_bin = wealth (wealth quintiles 1 & 2 OR quintiles 3+)
#educat_bin = educat (primary or below, secondary or higher)
#elev1500_bin <- elev1500 (above or below 1500m)
#hv025= urban/rural
#ALT_DEM_cat <-hv040_cat (0, 500, 1000, 1500, 2000, 2500, Inf altitude in m)
#age_cat10 <- hv105_cat= Age (categorical)
#hv201_cat <- source of drinking water (1 = piped, 0 = unpiped)
#hv246= Owns livestock or no
#bednetper_cat <- y/n 1 bed net per 1.8 de jure household members
#hml1_cat <- bed net ownership (0 = does not own net, 1 = owns net)
#hml10= net is treated
#hml20= Person slept under an LLIN net\
#ha54= currently pregnant
#hiv03 = HIV blood test result
#hv270= wealth index combined
#occupation= occupation


library(dplyr)
library(srvyr)

# Your survey design object: DHS19

# Assuming malaria indicator variable is called "pf" (1=positive, 0=negative)

# Function to calculate weighted prevalence and sample size by a single variable
get_prevalence_table <- function(varname) {
  DHS19 %>%
    group_by(.data[[varname]]) %>%
    summarise(
      `Percent Pf positive` = survey_mean(pf, vartype = "ci", na.rm = TRUE) * 100,
      `Number of samples` = unweighted(n())
    ) %>%
    mutate(`Background characteristic` = paste0(varname, " = ", as.character(.data[[varname]]))) %>%
    select(`Background characteristic`, `Percent Pf positive`, `Number of samples`)
}


variables <- c("hv104", "age_bin", "wealth_bin", "educat_bin", "elev1500_bin", "hv025", 
               "ALT_DEM_cat", "age_cat10", "hv201_cat", "hv246", "bednetper_cat", 
               "hml1_cat", "hml10", "hml20", "ha54", "hiv03", "hv270", "occupation")

all_tables <- lapply(variables, get_prevalence_table) %>%
  bind_rows()




#----------------------------Bivariate Associations glms--------------------------------------
#hv104= sex (male = 1, female = 2)
#age_bin <- age (0-24 years or 24+ years old)
#hv105 = age
#wealth_bin = wealth (wealth quintiles 1 & 2 OR quintiles 3+)
#educat_bin = educat (primary or below, secondary or higher)
#elev1500_bin <- elev1500 (above or below 1500m)
#hv025= urban/rural
#ALT_DEM_cat <-hv040_cat (0, 500, 1000, 1500, 2000, 2500, Inf altitude in m)
#age_cat10 <- hv105_cat= Age (categorical)
#hv201_cat <- source of drinking water (1 = piped, 0 = unpiped)
#hv246= number of livestock total
#bednetper_cat <- y/n 1 bed net per 1.8 de jure household members
#hml1_cat <- bed net ownership (0 = does not own net, 1 = owns net)
#hml10= net is treated
#hml20= Person slept under an LLIN net


f_glms_svy <- function(vars, outcome, design) {
  results <- lapply(vars, function(var) {
    # Create the formula
    fml <- as.formula(paste0(outcome, " ~ ", var))
    
    # Fit the model
    model <- svyglm(fml, design = design, family = quasibinomial("identity"))
    
    # Get tidy summary and confidence intervals
    tidy_model <- tidy(model)
    conf_df <- as.data.frame(confint(model))
    names(conf_df) <- c("conf.low", "conf.high")
    
    # Add significance stars
    tidy_model$signif <- cut(
      tidy_model$p.value,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", ""),
      right = FALSE   )
    
    # Combine tidy and CI results
    out <- cbind(tidy_model, conf_df)
    
    # Add a column to indicate the predictor used
    out$predictor <- var
    return(out) })
  
  # Combine all model results into one data frame
  final_df <- do.call(rbind, results)
  
  return(final_df) }


# Define variables and report what the baseline of the variable is
report_baselines <- function(data, vars) {
  library(haven)
  library(dplyr)
  
  baseline_info <- lapply(vars, function(var) {
    if (!var %in% names(data)) {
      return(data.frame(variable = var, type = "NOT FOUND", baseline = NA, comparisons = NA))
    }
    
    var_data <- data[[var]]
    
    # Handle haven_labelled
    if (inherits(var_data, "haven_labelled")) {
      if (is.numeric(as.numeric(var_data))) {
        var_data <- as.numeric(var_data)
      } else {
        var_data <- as_factor(var_data)  # convert to factor with value labels
      }
    }
    
    # Now check type and extract baseline/comparison
    if (is.factor(var_data)) {
      levels_vec <- levels(var_data)
      baseline <- levels_vec[1]
      comparisons <- paste(levels_vec[-1], collapse = ", ")
      type <- "factor"
    } else if (is.numeric(var_data) && length(unique(var_data)) == 2) {
      sorted_vals <- sort(unique(var_data))
      baseline <- sorted_vals[1]
      comparisons <- sorted_vals[2]
      type <- "binary numeric"
    } else if (is.numeric(var_data)) {
      baseline <- NA
      comparisons <- NA
      type <- "continuous"
    } else {
      baseline <- NA
      comparisons <- NA
      type <- class(var_data)  }
    data.frame(variable = var,
               type = type,
               baseline = baseline,
               comparisons = comparisons)  })
  
  do.call(rbind, baseline_info) }


vars <-c("hv104","age_bin","hv105","wealth_bin","educat_bin","hv201_cat","hv246",
         "hml1_cat","hml20","bednetper_cat","hv025","elev1500_bin","temp_cat","rain_cat","under_18","health_insurance",
         "owns_cattle_traditional", "owns_cattle", "owns_bulls", "owns_goats", "owns_sheep", "owns_poultry", "owns_pigs", "owns_rabbit", "owns_equine")
 
         
# Convert variables to factors
vars_to_factor <- c("age_bin","wealth_bin","educat_bin","elev1500_bin","temp_cat","rain_cat","bednetper_cat", "hv246", 
                    "owns_cattle_traditional", "owns_cattle", "owns_bulls", "owns_goats", "owns_sheep", "owns_poultry", "owns_pigs", "owns_rabbit", "owns_equine", "health_insurance")
survey19[vars_to_factor] <- lapply(survey19[vars_to_factor], factor)
#Look at baseline compatator and variable types
report_baselines(survey19, vars)




# Define the confines of the function

# For survey19
# List vars, outcome, design
vars <-c("hv104","age_bin","under_18","wealth_bin","educat_bin","hv201_cat",
              "hml1_cat","hml20","bednetper_cat","hv025","elev1500_bin","temp_cat","rain_cat","health_insurance","hv246",
         "owns_cattle_traditional", "owns_cattle", "owns_bulls", "owns_goats", "owns_sheep", "owns_poultry", "owns_pigs", "owns_rabbit", "owns_equine") #land
outcome <- "pf"
design <- DHS19
# Check baseline comparator and variable types
report_baselines(survey19, vars)

# Run the function
pfglms_results19 <- f_glms_svy(vars, outcome, design)


# For survey19 FEMALES

vars <-c("age_bin","wealth_bin","educat_bin","hv246","hv201_cat",
         "hml1_cat","hml20","bednetper_cat","hv025","elev1500_bin","temp_cat","rain_cat","under_18","health_insurance","ha54","hv227","hv246",
         "owns_cattle_traditional", "owns_cattle", "owns_bulls", "owns_goats", "owns_sheep", "owns_poultry", "owns_pigs", "owns_rabbit", "owns_equine") #females

# Convert variables to factors
vars_to_factor <- c("age_bin","wealth_bin","educat_bin","elev1500_bin","temp_cat","rain_cat","bednetper_cat", "health_insurance","hv246","ha54",
                    "owns_cattle_traditional", "owns_cattle", "owns_bulls", "owns_goats", "owns_sheep", "owns_poultry", "owns_pigs", "owns_rabbit", "owns_equine")
survey19_f[vars_to_factor] <- lapply(survey19_f[vars_to_factor], factor)
#Look at baseline compatator
report_baselines(survey19_f, vars)


outcome <- "pf"
design <- DHS19_f

# Run the function
pfglms_results19_f <- f_glms_svy(vars, outcome, design)


# ////////////////////
# For survey19 MALES

vars <-c("age_bin","wealth_bin","educat_bin","hv246","hv201_cat",
         "hml1_cat","hml20","bednetper_cat","hv025","elev1500_bin","temp_cat","rain_cat","under_18","health_insurance","hv227","hv246",
         "owns_cattle_traditional", "owns_cattle", "owns_bulls", "owns_goats", "owns_sheep", "owns_poultry", "owns_pigs", "owns_rabbit", "owns_equine") #males

# Convert variables to factors
vars_to_factor <- c("age_bin","wealth_bin","educat_bin","elev1500_bin","temp_cat","rain_cat","bednetper_cat", "health_insurance","hv246",
                    "owns_cattle_traditional", "owns_cattle", "owns_bulls", "owns_goats", "owns_sheep", "owns_poultry", "owns_pigs", "owns_rabbit", "owns_equine")
survey19_m[vars_to_factor] <- lapply(survey19_m[vars_to_factor], factor)
#Look at baseline compatator
report_baselines(survey19_m, vars) #males
outcome <- "pf"
design <- DHS19_m

# Run the function
pfglms_results19_m <- f_glms_svy(vars, outcome, design)



#-----------------------------Figure 4: Bivariate associations between demographic and environ risk factors (forest plot)----------------------------


# Remove intercept from the plot
pfglms_results19_m_noint <- pfglms_results19_m %>% 
  filter(term != "(Intercept)")

#Look at the terms
unique(pfglms_results19_m_noint$term)

#forest plot (FEMALES survey19_f)
ggplot() + geom_hline(yintercept = 0, linetype='dashed')+
  coord_flip(expand=TRUE)+
  geom_pointrange(data=pfglms_results19_noint, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, 
                                        color="cyan4"), shape=15, size=1, position=position_dodge2(width = 0.9), fatten=0.1)+
  geom_point(data=pfglms_results19_noint, aes(x=term, y=estimate, color="cyan4"), shape=19, size=2)+
  # Rename x-axis variables
  scale_x_discrete(labels = c(
    "wealth_binwealth quintiles 1 & 2"     = "Low Wealth (Q1 or Q2) vs higher wealth (Q3+)",
    "temp_catbelow avg. temp"       = "Below vs. at or above avg temp",
    "rain_catbelow avg. rain"       = "Below vs. at or above avg rainfall",
    "sympt_weightloss"    = "Reported Weight Loss vs. No",
    "sympt_night_sweats"  = "Reported Night Sweats vs. No",
    "sympt_fever" = "Reported Fever vs. No",
    "sympt_fatigue" = "Reported Fatigue vs. No",
    "sympt_cough" = "Reported Cough vs. No",
    "sympt_chest_pain" = "Reported Chest Pain vs. No",
    "age_bin24 years or older" = "24+ years old vs. 0-24 years",
    "hv025" = "Rural vs. Urban",
    "hv104" = "Female vs. Male",
    "hv246b" = "Owns Cows vs No",
    "hv105"          = "Age (continuous)",
    "educat_binsecondary or higher"     = "Secondary or above vs. primary school education or below",
    "elev1500_bin>= 1500"   = "Elevation ≥1500m vs. <1500m",
    "hv201_cat0"     = "Unpiped vs piped drinking water source",
    "hv246"          = "Owns livestock vs. No",
    "bednetper_cat"  = "Adequate Bednet Access (1 per 1.8 Members) vs. No",
    "hml1_cat"       = "Owns Bednet vs. No",
    "hml10"          = "Slept Under Treated Net",
    "hml20"          = "Slept Under LLIN vs. No",
    "hv244"          = "Owns land usable for agriculture",
    "hv227"          = "Has mosquito bed net for sleeping vs. No",
    "ha54"           = "Currently pregnant vs. Not",
    "owns_cattle_traditional" = "Owns traditional milk cows vs. Not",
    "owns_cattle"    = "Owns modern milk cows vs. Not",
    "owns_bulls"     = "Owns bulls vs. Not",
    "owns_goats"     = "Owns goats vs. Not", 
    "owns_sheep"     = "Owns sheep vs. Not", 
    "owns_poultry"   = "Owns chickens/poultry vs. Not", 
    "owns_pigs"      = "Owns pigs vs. Not", 
    "owns_rabbit"    = "Owns rabbits vs. Not", 
    "owns_equine"    = "Owns horses/donkeys/mules vs. Not",
    "health_insurance" = "Has health insurance vs. Not",
    "under_18"       = "18+ years old vs. 0-18 years")) +
  labs(y="Bivariate associations of Pf prevalence")



# Both male and female in one graph
pfglms_results19_f_noint$sex <- "Female"
pfglms_results19_m_noint$sex <- "Male"

pfglms_results19_combined <- rbind(pfglms_results19_f_noint, pfglms_results19_m_noint)

ggplot(pfglms_results19_combined, aes(x = term, y = estimate, color = sex)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  coord_flip(expand = TRUE) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  shape = 15, size = 1,
                  position = position_dodge(width = 0.5), fatten = 0.1) +
  geom_point(shape = 19, size = 2,
             position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
  scale_x_discrete(labels = c(
    "wealth_binwealth quintiles 1 & 2"     = "Low Wealth (Q1 or Q2) vs higher wealth (Q3+)",
    "temp_catbelow avg. temp"       = "Below vs. at or above avg temp",
    "rain_catbelow avg. rain"       = "Below vs. at or above avg rainfall",
    "sympt_weightloss"    = "Reported Weight Loss vs. No",
    "sympt_night_sweats"  = "Reported Night Sweats vs. No",
    "sympt_fever" = "Reported Fever vs. No",
    "sympt_fatigue" = "Reported Fatigue vs. No",
    "sympt_cough" = "Reported Cough vs. No",
    "sympt_chest_pain" = "Reported Chest Pain vs. No",
    "age_bin24 years or older" = "24+ years old vs. 0-24 years",
    "hv025" = "Rural vs. Urban",
    "hv104" = "Female vs. Male",
    "hv246b" = "Owns Cows vs No",
    "hv105"          = "Age (continuous)",
    "educat_binsecondary or higher"     = "Secondary or above vs. primary school education or below",
    "elev1500_bin>= 1500"   = "Elevation ≥1500m vs. <1500m",
    "hv201_cat0"     = "Unpiped vs piped drinking water source",
    "hv246"          = "Owns livestock vs. No",
    "bednetper_cat"  = "Adequate Bednet Access (1 per 1.8 Members) vs. No",
    "hml1_cat"       = "Owns Bednet vs. No",
    "hml10"          = "Slept Under Treated Net",
    "hml20"          = "Slept Under LLIN vs. No",
    "hv244"          = "Owns land usable for agriculture",
    "hv227"          = "Has mosquito bed net for sleeping vs. No",
    "ha54"           = "Currently pregnant vs. Not",
    "owns_cattle_traditional" = "Owns traditional milk cows vs. Not",
    "owns_cattle"    = "Owns modern milk cows vs. Not",
    "owns_bulls"     = "Owns bulls vs. Not",
    "owns_goats"     = "Owns goats vs. Not", 
    "owns_sheep"     = "Owns sheep vs. Not", 
    "owns_poultry"   = "Owns chickens/poultry vs. Not", 
    "owns_pigs"      = "Owns pigs vs. Not", 
    "owns_rabbit"    = "Owns rabbits vs. Not", 
    "owns_equine"    = "Owns horses/donkeys/mules vs. Not",
    "health_insurance" = "Has health insurance vs. Not",
    "under_18"       = "18+ years old vs. 0-18 years")) +
  labs(y = "Bivariate associations of Pf prevalence",
       color = "Sex") +
  theme_minimal()






#hv104= sex
#age_bin <- age (0-24 years or 24+ years old)
#hv105 = age
#wealth_bin = wealth (wealth quintiles 1 & 2 OR quintiles 3+)
#educat_bin = educat (primary or below, secondary or higher)
#elev1500_bin <- elev1500 (above or below 1500m)
#hv025= urban/rural
#ALT_DEM_cat <-hv040_cat (0, 500, 1000, 1500, 2000, 2500, Inf altitude in m)
#age_cat10 <- hv105_cat= Age (categorical)
#hv201_cat <- source of drinking water (1 = piped, 0 = unpiped)
#hv246= number of livestock total
#bednetper_cat <- y/n 1 bed net per 1.8 de jure household members
#hml1_cat <- bed net ownership (0 = does not own net, 1 = owns net)
#hml10= net is treated
#hml20= Person slept under an LLIN net


#-----------------------------Supp Fig 1: MAP of high prev/low prev clusters in RW19 BY MICROSCOPY/RDT-----------------------------
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



 
#-----------------------------Fig: MAP of WEIGHTED Pf prevalence by cluster AND weighted high/low Pf transmission clusters----------------------------
# Obtain weighted pf counts per cluster
cluster_pfcountwt19<-as.data.frame(svyby(~pf, ~hv001, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
  select(hv001, pf) %>%
  rename(pf_count = pf)

# Obtain weighted sample counts per cluster
cluster_selectwt19<-as.data.frame(svyby(~one, ~hv001, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
  select(hv001, one) %>%
  rename(sample_count = one)

# Join Pf counts and sample counts into one df
cluster_prevPFwt19 <- cluster_selectwt19 %>%
  left_join(cluster_pfcountwt19 %>% 
              select(hv001, pf_count), 
            by = "hv001")

# Create a pf_prevalence variable that calculates Pf count/sample count * 100
cluster_prevPFwt19$pf_prevalence_wt <- (cluster_prevPFwt19$pf_count / cluster_prevPFwt19$sample_count) * 100

# Make a transmission intensity (high or low) variable, with prevalence > 15 being high transmission
cluster_prevPFwt19$trans_intenspf19 <- ifelse(cluster_prevPFwt19$pf_prevalence_wt > 15, "high", "low")


# Add geographic points to prevalence df for mapping
cluster_prevPFwt19 <- cluster_prevPFwt19 %>%
  left_join(coords, by = "hv001")

# Specify that only clusters with prevalence >0 will be filled with color.
cluster_prevPFwt19$pf_prevalence_fill <- ifelse(cluster_prevPFwt19$pf_prevalence_wt == 0, NA, cluster_prevPFwt19$pf_prevalence_wt)

# Sum of clusters with detected malaria
sum(cluster_prevPFwt19$pf_prevalence_wt > 0, na.rm = TRUE) #244 clusters with >0% weighted Pf prevalence

#### Add transmission split based on RDT/microscopy
# Add a column "trans_intens_RDTmic" with low for all samples initially 
cluster_prevPFwt19$trans_intens_RDTmic <- "low"
# Mark the extracted samples as 1 in "extracted" if in "Pf_qPCR_tested_samples" (all samples extracted were tested for Pf)
cluster_prevPFwt19$trans_intens_RDTmic[cluster_prevPFwt19$hv001 %in% high_prev_clusters] <- "high"

# Add RDT and microscopy prevelance
cluster_prevPFwt19 <- cluster_prevPFwt19 %>%
  left_join(cluster_malaria %>% 
              select(hv001, prev_malaria_rdt, prev_malaria_mic), 
            by = "hv001")

# MAP: WEIGHTED Pf prevalence by cluster
#Plot the points on a MAP with color based on the cluster Pf prevalence
ggplot() +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) + # Rwanda country outline and fill
  geom_sf(data=lakes10, fill="lightblue")+ # Lakes in the area
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=2 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
           color="grey60", size=2 , fontface="italic") +
  geom_point(data = cluster_prevPFwt19,
             aes(x = LONGNUM, y = LATNUM, fill = pf_prevalence_fill), 
             size = 2.5, color = "black", shape = 21, stroke = 0.05) +
  scale_fill_viridis_c(option = "plasma") + # Adjust the color scale as needed
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void() +
  labs(fill = "Pf prevalence", title = "Weighted Cluster Pf Prevalence")
theme_void()


# MAP: WEIGHTED Pf transmission intensity (high/low) by cluster
#Plot the points on a MAP with color based on the cluster Pf transmission intensity
# *** The numbers look ridiculous because weighting brings some clusters to 1 sample tested
#HOWEVER, the high/low transmission intensity maps are identical for weighted and unweighted cluster prevalence calculations
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
  geom_point(data = cluster_prevPFwt19, aes(x=LONGNUM, y=LATNUM, fill=trans_intenspf19), 
             size = 2.3, color = "black", shape = 21, stroke = 0.1) + # Data points
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+ scale_shape_manual(values=shapes)+ #Image view (how much of the world do we see?)
  labs(x="",y="",shape="Inclusion in Analysis", fill="Transmission Intensity")



#-----------------------------UNWEIGHTED prevalence by cluster (Pf qPCR data)-------------------
# All samples malaria prev by cluster
cluster_survey19 <- survey19 %>%
  group_by(hv001) %>% 
  summarize(
    tested_for_pf = sum(one == 1, na.rm = TRUE), 
    pf = sum(pf == 1, na.rm = TRUE), 
    prev_pf = (pf / tested_for_pf) * 100, 
    long = mean(LONGNUM), 
    lat = mean(LATNUM), 
    observations = n() 
  )

# New variable for high prev and low prev
cluster_survey19$trans_intenspf19 <- ifelse(cluster_survey19$prev_pf > 15, "high", "low")


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
  geom_point(data = cluster_survey19, aes(x=long, y=lat, fill=trans_intenspf19), 
             size = 2.3, color = "black", shape = 21, stroke = 0.1) + # Data points
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+ scale_shape_manual(values=shapes)+ #Image view (how much of the world do we see?)
  labs(x="",y="",shape="Inclusion in Analysis", fill="Transmission Intensity")




#-----------------------------Example MAP: malaria-positive cases by cluster----------------------------



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

# Supp Figure 2: distribution of clusters where malaria species were identified



#-----------------------------Supplemental table 3: study population description------

# counts (numerator and denominator for all proportions)


#calculate 95% CIs for:

# 40 % female
as.data.frame(svyby(~one, ~hv104, DHS19, svytotal, survey.lonely.psu="adjust"))

# 14% 15-24
as.data.frame(svyby(~select, ~hv105_cat, DHS19, svytotal, survey.lonely.psu="adjust"))

# 76% rural
as.data.frame(svyby(~select, ~urban, DHS19, svytotal, survey.lonely.psu="adjust"))

# 80% primary or no education
as.data.frame(svyby(~select, ~educat, DHS19, svytotal, survey.lonely.psu="adjust"))

# 60% primary, 20% preschool/none
as.data.frame(svyby(~select, ~hv106, DHS19, svytotal, survey.lonely.psu="adjust"))

# 83% household bed nets
as.data.frame(svyby(~select, ~hml1_cat, DHS19, svytotal, survey.lonely.psu="adjust"))

# 68% slept under an LLIN 
as.data.frame(svyby(~select, ~hml20, DHS19, svytotal, survey.lonely.psu="adjust"))

# 41% did NOT have 1 net per 1.8
as.data.frame(svyby(~select, ~bednetper_cat, DHS19, svytotal, survey.lonely.psu="adjust"))


# 45%, 45% and 57% of Po, Pm and Pv infections with another infection
rw_svy$po
rw_svy$pm
rw_svy$pv


# 23.6% overall prevalence
sum(rw_svy$malaria)


# 8.3% any non-falciparum prevalence
sum(rw_svy$nonpf)


#-----------------------------Proportion of households with bed nets and slept under net last night--------------
# Make proportion of each cluster with household bednets (prop_bednet)
prop_bednet<-as.data.frame(svyby(~hml1_cat, ~hv001, DHS19, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust"))
prop_bednet$prop_bednet<-prop_bednet$hml1_cat
survey19<-left_join(survey19, prop_bednet[,c("hv001","prop_bednet")],by="hv001")

# Make proportion of each cluster that slept under a net last night (prop_slept) (weighted)
prop_slept<-as.data.frame(svyby(~hml20, ~hv001, DHS19, svymean, vartype=c('se','ci'), survey.lonely.psu="adjust"))
prop_slept$prop_slept<-prop_slept$hml20
survey19<-left_join(survey19, prop_slept[,c("hv001","prop_slept")],by="hv001")








#-----------------------------MAP change in Pf district prev between 2014/15 and 2019/20-------------------------------

# Download 2014/15 district pf prevalence 
svy_prevbydistrict14_old<-read_csv("C:/Users/jzuromsk/Documents/DHS_2019/DHS_2014/svy_prevbydistrict14_old.csv")

#Make new df with weighted district-level pf prev from 2014/15 survey
district_delta_pfprev_wt <- svy_prevbydistrict14_old %>%
  select(District, pf_weighted_prev14)

district_prevPFwt19 <- district_prevPFwt19 %>%
  rename(District = DHSREGNA, pf_prevalence_wt19 =  pf_prevalence_wt)

#Add 2019/20 weighted district-level pf prev
district_delta_pfprev_wt <- district_delta_pfprev_wt %>%
  left_join(district_prevPFwt19 %>% 
              select(District, pf_prevalence_wt19),
            by = "District")

district_prevPFwt19$pfprev_wt_delta <- ((district_prevPFwt19$pf_prevalence_wt - district_prevPFwt19$pf_weighted_prev14) / district_prevPFwt19$pf_weighted_prev14) * 100


# Obtain percent differences between district Pf prevalences from 2014/15 and 2019/20
district_delta_pfprev_wt <- district_delta_pfprev_wt %>%
  mutate(
    pfprev_wt_delta = ((pf_prevalence_wt - pf_weighted_prev14) / pf_weighted_prev14) * 100
  )




# MAP the prevalence differences
# Used datawrapper https://app.datawrapper.de/





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
  geom_sf(data = district, aes(fill = district_prevPFwt19$pfprev_wt_delta)) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", name = "Δ Prevalence (%)")
  #geom_point(data = dbs_master_vars_clean2_qPCR2, aes(x=LONGNUM, y=LATNUM, fill=trans_intens), 
           #  size = 2.3, color = "black", shape = 21, stroke = 0.1) + # Data points
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+ scale_shape_manual(values=shapes)+ #Image view (how much of the world do we see?)
  #labs(x="",y="",, fill="Transmission Intensity")


    
    
    
    
#-----------------------------Supp tables 5 and 6---- 


#---------------------------Supplemental Table. District Level Malaria Prevalence at Different Cycle Cutoffs. 



#Data sets:
  #Weighted qPCR sample counts per district: district_countPFtestedwt19
  #Weighted Pf counts per district all: district_countPFwt19
  #Weighted Pf counts per district >40 CT: district_countPFwt19_40




# Join all Pf counts and sample counts into one df
district_prevPFwt19 <- district_countPFtestedwt19 %>%
  left_join(district_countPFwt19 %>% 
              select(District, pf_count, pf_se), 
            by = "District")

# Create a pf_prevalence variable that calculates Pf count/sample count * 100
district_prevPFwt19$pf_prevalence_wt <- (district_prevPFwt19$pf_count / district_countPFtestedwt19$select_count) * 100

# Join <40 Pf CT counts and sample counts into one df
district_prevPFwt19 <- district_prevPFwt19 %>%
  left_join(district_countPFwt19_40 %>% 
              select(District, pf_count_40, pf_se_40), 
            by = "District")

# Create a <40 CT pf_prevalence variable that calculates Pf count/sample count * 100
district_prevPFwt19$pf_prevalence_wt_40 <- (district_prevPFwt19$pf_count_40 / district_countPFtestedwt19$select_count) * 100


#-----------------------------Supplemental Table 5. Differences in District Level Prevalence by PCR Cutoff

# In the df above, make a delta_prevalence variable
district_prevPFwt19$delta_prevalence19 <- (district_prevPFwt19$pf_prevalence_wt - district_prevPFwt19$pf_prevalence_wt_40)


district_prevPFwt19 <- district_prevPFwt19 %>%
  rename(District = DHSREGNA)

summary(district_prevPFwt19$delta_prevalence19)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.3876  0.4229  0.6032  1.7354


#--------------------------------------------Weighted Pf counts and Prevalence by PROVINCES (45 cycles)----------
# Weighted sample counts by province
province_countPFtestedwt19<-as.data.frame(svyby(~one, ~ADM1NAME, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
  rename(one_se = se, one_count = one, Province = ADM1NAME)
# Weighted Pf counts by province
province_countPFwt19<-as.data.frame(svyby(~pf, ~ADM1NAME, DHS19, svytotal, survey.lonely.psu="adjust")) %>%
  rename(pf_se = se, pf_count = pf, Province = ADM1NAME)


#Add 2019/20 weighted province-level pf counts
province_prevPFwt19 <- province_countPFtestedwt19 %>%
  left_join(province_countPFwt19 %>% 
              select(Province, pf_count, pf_se),
            by = "Province")

province_prevPFwt19$pfprev_wt19 <- (province_prevPFwt19$pf_count / province_prevPFwt19$one_count) * 100


#--------------------------------------------Weighted Pf counts and Prevalence by PROVINCES (45 cycles, FEMALES ONLY)----------
# Weighted sample counts by province
province_countPFtestedwt19_f<-as.data.frame(svyby(~one, ~ADM1NAME, DHS19_f, svytotal, survey.lonely.psu="adjust")) %>%
  rename(one_se = se, one_count = one, Province = ADM1NAME)
# Weighted Pf counts by province
province_countPFwt19_f<-as.data.frame(svyby(~pf, ~ADM1NAME, DHS19_f, svytotal, survey.lonely.psu="adjust")) %>%
  rename(pf_se = se, pf_count = pf, Province = ADM1NAME)


#Add 2019/20 weighted province-level pf counts
province_prevPFwt19_f <- province_countPFtestedwt19_f %>%
  left_join(province_countPFwt19 %>% 
              select(Province, pf_count, pf_se),
            by = "Province")

province_prevPFwt19_f$pfprev_wt19 <- (province_prevPFwt19_f$pf_count / province_prevPFwt19_f$one_count) * 100







#-------------------Obtain % NA in our variables- by sex------------------------------------------------------------

#  Obtain % NA in our variables- by sex- for multiple variables

f_na_summary_by_sex <- function(df, variables) {
  result <- data.frame(Variable = character(),
                       NA_Female = numeric(),
                       NA_Male = numeric(),
                       stringsAsFactors = FALSE)
  
  for (var in variables) {
    # Skip variables not in the data
    if (!var %in% names(df)) next
    
    # Filter for sex groups
    female <- df[df$hv104 == 2, ]
    male <- df[df$hv104 == 1, ]
    
    # Calculate NA percentages
    na_female <- sum(is.na(female[[var]])) / nrow(female) * 100
    na_male <- sum(is.na(male[[var]])) / nrow(male) * 100
    
    # Append to results
    result <- rbind(result, data.frame(
      Variable = var,
      NA_Female = round(na_female, 2),
      NA_Male = round(na_male, 2)
    )) }
   return(result) }

# Make a list of variables to check
# vars_to_keep <- c("variable1", "variable2")
# Call the function
na_sex_summary <- f_na_summary_by_sex(dbs_master_vars_clean2_qPCR, vars_to_keep)
print(na_sex_summary)




#  Obtain % NA in our variables- by sex- for single variables (only prints, does not make df)
f_check_na_by_sex <- function(df, variable_name) {
  # Ensure the variable exists in the dataframe
  if (!variable_name %in% names(df)) {
    stop("Variable not found in the data frame.")}
  # Total NAs in the variable
  total_na <- sum(is.na(df[[variable_name]]))
  cat("Total NA values in", variable_name, ":", total_na, "\n\n")
  # Split by sex and calculate NA percentages
  df_sex <- df %>%
    filter(hv104 %in% c(1, 2)) %>%  # Keep only males and females
    mutate(sex = ifelse(hv104 == 1, "Male", "Female")) %>%
    group_by(sex) %>%
    summarize(
      Total = n(),
      NA_Count = sum(is.na(.data[[variable_name]])),
      NA_Percent = round(100 * NA_Count / Total, 2)
    )
  print(df_sex) }


# call this function
f_check_na_by_sex(dbs_master_vars_clean2_qPCR, "mv024")





library(survey)  # Load survey package for complex survey design
library(dplyr)   # For data manipulation
library(broom)   # For tidying model output

f_run_dhs_model <- function(data, model_type = "logistic", outcome, predictors, na_cutoff = 0.25) {
  
  # Filter out predictors with too much missingness
  missing_props <- sapply(data[predictors], function(x) mean(is.na(x)))  # Calculate % NA for each predictor
  kept_predictors <- names(missing_props[missing_props <= na_cutoff])   # Keep predictors with NA <= cutoff
  excluded_predictors <- setdiff(predictors, kept_predictors)           # Track excluded predictors
  
  # Inform user of excluded variables
  if (length(excluded_predictors) > 0) {
    message("Excluded variables due to NA threshold: ", paste(excluded_predictors, collapse = ", "))
  }
  
  # Prepare data for modeling
  model_data <- data %>%
    select(all_of(c(outcome, kept_predictors, "hv021", "hv022", "hiv05"))) %>%  # Select necessary variables
    filter(!is.na(.data[[outcome]]))  # Remove rows with missing outcome
  
  # Scale HIV weights
  model_data <- model_data %>%
    mutate(hiv05 = hiv05 / 1e6)  # DHS recommends dividing weights by 1,000,000
  
  # Create survey design object
  svy_design <- svydesign(
    ids = ~hv021,         # Primary sampling unit
    strata = ~hv022,      # Stratification
    weights = ~hiv05,    # Sample weights
    data = model_data,
    nest = TRUE          # Recommended for DHS data
  )
  
  # Build model formula
  model_formula <- as.formula(paste(outcome, "~", paste(kept_predictors, collapse = " + ")))  # Construct model formula
  
  # Fit model using appropriate survey function
  if (model_type == "logistic") {
    model <- svyglm(model_formula, design = svy_design, family = quasibinomial())  # Logistic regression
  } else if (model_type == "linear") {
    model <- svyglm(model_formula, design = svy_design, family = gaussian())       # Linear regression
  } else {
    stop("Unsupported model type. Choose 'logistic' or 'linear'.")
  }
  
  # Return tidy summary of results
  return(tidy(model))
}

results <- f_run_dhs_model(
  data = dbs_master_vars_clean2_qPCR,
  model_type = "logistic",
  outcome = "pf",
  predictors = c("age_years", "hv271", "ha53", "occupation", "net_1.8"),
  na_cutoff = 0.25)







#-------------------------------Checking variable alignment-------------

# Check that the variables still align
check <- dbs_master%>%
  select(barcode, hv103, mv135, hv102, mv024, mv101, hv024, v024, v101, v139, v140, hv001, DHSREGNA, DHSREGCO, ADM1DHS, ADM1NAME, sdistrict) #Hemoglobin level (g/dl - 1 decimal)

# Extract rows with nonequivalent data in two variables to determine if data is misaligned:
check <- check %>%
  filter(hv024 != v024)










#-------------------------------Renaming variables/Clean the geographical covariates data (GC19RW)------------


# Rename important columns
dbs_master <- dbs_master %>%
  rename(
    region = DHSREGNA, 
    cluster_altitude = ALT_DEM,
    interview_month = hv006,
    interview_year = hv007,
    interview_day = hv016,
    biomarker_day = hv807d,
    biomarker_month = hv807m,
    biomarker_year = hv807y,
    n_household_members = hv009, 
    sex = hv104, # male = 1, female = 2
    age = hv105,
    urban1_rural2 = hv025, # urban = 1, rural = 2
    tested_for_malaria = hml33, # tested = 1
    malaria_rdt = hml35, # positive = 1
    malaria_bloodsmear = hml32, # positive = 1
    Pf = hml32a, 
    Pm = hml32b,
    Po = hml32c,
    electricity = hv206,
    owns_radio = hv207,
    owns_television = hv208,
    owns_refrigerator = hv209,
    owns_bicycle = hv210,
    owns_motorcycle = hv211,
    owns_vehicle = hv212,
    floor_material = hv213,
    wall_material = hv214,
    roof_material = hv215,
    bedrooms = hv216,
    has_bednet = hv227,
    water_source = hv235,
    owns_cellphone = hv243a,
    owns_computer = hv243e,
    owns_agriland = hv244,
    owns_farmanimals = hv246,
    n_bednets = hml1,
  )


#-------------------------------DHS RDT and microscopy sensitivity------------

library(caret)

analysis_data <- dbs_master_vars_clean2_qPCR2 %>% 
  filter(Pf_qPCR_tested == 1)

analysis_subset <- analysis_data %>%
  filter(!is.na(pf) & !is.na(hml35))

# Construct 2x2 tables for sensitivity and specificity
table_rdt <- table(
  qPCR = analysis_subset$pf,
  RDT  = analysis_subset$hml35
)


# For Pf by qPCR and RDT positivity
confusionMatrix(
  data = as.factor(analysis_subset$hml35),
  reference = as.factor(analysis_subset$pf),
  positive = "1"
)

# For malaria positivity by qPCR and microscopy positivity
analysis_subset <- analysis_data %>%
  filter(!is.na(malaria) & !is.na(hml35))

# For Pf by qPCR and RDT positivity
confusionMatrix(
  data = as.factor(analysis_subset$hml35),
  reference = as.factor(analysis_subset$malaria),
  positive = "1"
)

# Analyze discordant results (RDT+/qPCR− or RDT−/qPCR+ OR Microscopy+/qPCR− or Microscopy−/qPCR+)
analysis_data %>%
  filter(pf == 1 & hml35 == 0) %>%
  summarize(median_pf_SQ = median(pf_SQ, na.rm = TRUE),
            min_pf_SQ = min(pf_SQ, na.rm = TRUE),
            max_pf_SQ = max(pf_SQ, na.rm = TRUE))
#median_pf_SQ min_pf_SQ max_pf_SQ
#1        3.339     0.001  11219.02


analysis_data %>%
  filter(malaria == 1 & hml35 == 0) %>%
  summarize(median_pf_SQ = median(pf_SQ, na.rm = TRUE),
            min_pf_SQ = min(pf_SQ, na.rm = TRUE),
            max_pf_SQ = max(pf_SQ, na.rm = TRUE))

summary(dbs_master_vars_clean2_qPCR2$pf_SQ, useNA = "always")
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#  0.00      1.18      8.61    940.09     93.78 249837.40     13304 


analysis_data <- analysis_data %>%
  mutate(parasite_density_group = case_when(
    pf_SQ < 1 ~ "<1",
    pf_SQ >= 1 & pf_SQ < 100 ~ "1–99",
    pf_SQ >= 100 & pf_SQ < 1000 ~ "100–999",
    pf_SQ >= 1000 ~ "≥1000"
  ))

malariatesting19 <- dbs_master_vars_clean2_qPCR2 %>% 
  filter(!Pf_qPCR_tested == 0) 



malariatesting19 <- malariatesting19 %>% 
  filter(!is.na(malariatesting19$hml35))

sum(!is.na(malariatesting19$hml32)) #3786 microscopy-tested samples
sum(!is.na(malariatesting19$hml35)) #3786 RDT-tested samples

malariatesting19 <- malariatesting19 %>%
  select(barcode,
         pf,
         po,
         pm,
         malaria,
         hml32,
         hml35,
         hml32a,
         hml32b,
         pf_SQ,
         po_SQ,
         pm_SQ,
         Pf_qPCR_tested,
         Po_qPCR_tested,
         Pm_qPCR_tested,)

# what do qPCR suspected positives look like?