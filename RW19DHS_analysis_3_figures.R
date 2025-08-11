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