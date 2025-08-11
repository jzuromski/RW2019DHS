##---- STEP 1: Load libraries

library(survey)
library(stats)
library(tidyverse)
library(readxl)
library(writexl)
library(haven)
library(srvyr)
library(broom)
library(purrr)
library(dplyr)
library(rdhs)
options(survey.lonely.psu="adjust") #Adjusts how the "survey" package deals with strata containing only one primary sampling unit (PSU)

#Claudia's code is found here: https://github.com/claudiagaither/rwanda_nonpf/blob/main/studypop%20%26%20glms.R#L305

##---- STEP 2: Import data (household recode) ---------------------------------------


##---- STEP 3: Select variables from 2019/20 RW DHS FULL dataset --------------------

##---- STEP 4: Clean missing data by replacing blanks with NA -----------------------

##---- STEP 5: Pull barcodes and corresponding data from all columns they are given in (in RW 2014 n=14) ---------
#~From CG: 
# rw1 <- rw15[,c("ha62_1","hb62_1","ha57_1","ha69_1","hb69_1","hv104_01","hv105_01","hv106_01","hml10_1","hml20_01","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
# rw1 <- rw15[,c("ha62_1","hb62_1","ha57_1","ha69_1","hb69_1","hv104_01","hv105_01","hv106_01","hml10_1","hml20_01","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
# rw2 <- rw15[,c("ha62_2","hb62_2","ha57_2","ha69_2","hb69_2","hv104_02","hv105_02","hv106_02","hml10_2","hml20_02","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
# rw3 <- rw15[,c("ha62_3","hb62_3","ha57_3","ha69_3","hb69_3","hv104_03","hv105_03","hv106_03","hml10_3","hml20_03","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
# rw4 <- rw15[,c("ha62_4","hb62_4","ha57_4","ha69_4","hb69_4","hv104_04","hv105_04","hv106_04","hml10_4","hml20_04","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
# rw5 <- rw15[,c("ha62_5","hb62_5","ha57_5","ha69_5","hb69_5","hv104_05","hv105_05","hv106_05","hml10_5","hml20_05","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
# rw6 <- rw15[,c("ha62_6","hb62_6","ha57_6","ha69_6","hb69_6","hv104_06","hv105_06","hv106_06","hml10_6","hml20_06","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]
# rw7 <- rw15[,c("ha62_7","hb62_7","ha57_7","ha69_7","hb69_7","hv104_07","hv105_07","hv106_07","hml10_7","hml20_07","hv270","hv246","hv201","hml1","hv025","hv040","hv006","hv001","hv002","hv003","hv005","hv012","hv021","hv023","hv024")]

##---- STEP 6: Rename variables to remove trailing numbers -----------------------------
#~From CG: 
# names1 = c("ha62_1","hb62_1","ha57_1","ha69_1","hb69_1","hv104_01","hv105_01","hv106_01","hml10_1","hml20_01")
# names2 = c("ha62_2","hb62_2","ha57_2","ha69_2","hb69_2","hv104_02","hv105_02","hv106_02","hml10_2","hml20_02")
# names3 = c("ha62_3","hb62_3","ha57_3","ha69_3","hb69_3","hv104_03","hv105_03","hv106_03","hml10_3","hml20_03")
# names4 = c("ha62_4","hb62_4","ha57_4","ha69_4","hb69_4","hv104_04","hv105_04","hv106_04","hml10_4","hml20_04")
# names5 = c("ha62_5","hb62_5","ha57_5","ha69_5","hb69_5","hv104_05","hv105_05","hv106_05","hml10_5","hml20_05")
# names6 = c("ha62_6","hb62_6","ha57_6","ha69_6","hb69_6","hv104_06","hv105_06","hv106_06","hml10_6","hml20_06")
# names7 = c("ha62_7","hb62_7","ha57_7","ha69_7","hb69_7","hv104_07","hv105_07","hv106_07","hml10_7","hml20_07")
# names = c("ha62","hb62","ha57","ha69","hb69","hv104","hv105","hv106","hml10","hml20")
# rw1<- rw1 %>% rename_at(all_of(names1),~names)
# rw2<- rw2 %>% rename_at(all_of(names2),~names)
# rw3<- rw3 %>% rename_at(all_of(names3),~names)
# rw4<- rw4 %>% rename_at(all_of(names4),~names)
# rw5<- rw5 %>% rename_at(all_of(names5),~names)
# rw6<- rw6 %>% rename_at(all_of(names6),~names)
# rw7<- rw7 %>% rename_at(all_of(names7),~names)

##---- STEP 7: Drop empty barcode rows & separate by gender (a & b)
#~From CG: 
# rw1a<-rw1 %>% drop_na(ha62)
# rw1b<-rw1 %>% drop_na(hb62)
# rw2a<-rw2 %>% drop_na(ha62)
# rw2b<-rw2 %>% drop_na(hb62)
# rw3a<-rw3 %>% drop_na(ha62)
# rw3b<-rw3 %>% drop_na(hb62)
# rw4a<-rw4 %>% drop_na(ha62)
# rw4b<-rw4 %>% drop_na(hb62)
# rw5a<-rw5 %>% drop_na(ha62)
# rw5b<-rw5 %>% drop_na(hb62)
# rw6a<-rw6 %>% drop_na(ha62)
# rw6b<-rw6 %>% drop_na(hb62)
# rw7a<-rw7 %>% drop_na(ha62)
# rw7b<-rw7 %>% drop_na(hb62)

##---- STEP 8: Combine barcode datasets into a single data frame
#~From CG:
# rw15_bc<-rbind(rw1a,rw1b,rw2a,rw2b,rw3a,rw3b,rw4a,rw4b,rw5a,rw5b)

##---- STEP 9: Import qPCR data and select/merge relevant columns
#~From CG:
# qpcr_bc <- read_excel("...")
# qpcr_bc$h62 <- qpcr_bc$sample
# qpcr_bc <- qpcr_bc[, c("h62", "var_CT", "pm_CT", "po_CT", "pv_CT")]
# h62s <- qpcr_bc$h62
# list of barcodes for which we have qPCR data
# h62s<-qpcr_bc$h62

#!!!!!!!!!#---- STEP 10: Attempt to create single HIV weight and barcode based on the sex of the individual
# ISSUES WITH THIS, POSSIBLY FROM MISSING VALUES OR 9999 VALUES
#~From CG:
#creating single hiv weight & barcode based on hv104, ends up super wonky with lots of repeated barcodes and 9999 values
#rw_svya <- rw_svya %>% mutate(h62 = case_when(
#  hv104==1 ~ hb62, hv104==2 ~ ha62)) 
#rw_svya <- rw_svya %>% mutate(h69 = case_when(
#  hv104==1 ~ hb69, hv104==2 ~ ha69))
#rw_svyb <- rw_svyb %>% mutate(h62 = case_when(
#  hv104==1 ~ hb62, hv104==2 ~ ha62)) 
#rw_svyb <- rw_svyb %>% mutate(h69 = case_when(
#  hv104==1 ~ hb69, hv104==2 ~ ha69))

#values where hv104 != h62/h69
# Identify mismatches, where h62 is NA. These rows stored in new df as rw_mismatch
#rw_mismatch<-rw15_bc[is.na(rw15_bc$h62),]

##---- STEP 11: Compare mismatch data with full dataset (quality check to see if full data set differs from mismatched)
#~From CG:
#mismatch dataset compared to full rw15_bc
#sex (not comparable!)
#hist(rw15_bc$hv104)
#hist(rw_mismatch$hv104)
#age (not 100% comparable!)
#hist(rw15_bc$hv105)
#hist(rw_mismatch$hv105)
#region (basically identical)
#hist(rw15_bc$hv024)
#hist(rw_mismatch$hv024)
#education level (similar but not identical)
#hist(rw15_bc$hv106)
#hist(rw_mismatch$hv106)


##---- STEP 12: Create new variables (a binary variable hm11_cat that indicates whether a household has more than 1 bed net)
# **Variable: Household Bed Net Ownership
#~From CG:
##new variables for rw15_bc 
#household bed net y/n (hml1_cat)
# rw15_bc <- rw15_bc %>% mutate(hml1_cat = case_when(
 # hml1 == 0 ~ 0,
 # hml1 > 0 ~ 1))

# **Variable: Household Bed Net Ownership
#~From CG:
#elevation binary (elev1500, above or below)
# rw15_bc <- rw15_bc %>% mutate(elev1500 = case_when(
#  hv040>=1500 ~ ">= 1500",
#  hv040<1500 ~ "<1500"))

#categorical variable for elevation (hv040_cat) <- categorizes elevation into 6 different ranges  
# rw15_bc$hv040_cat <- cut(rw15_bc$hv040,
#                         breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf),
#                         labels = c(1, 2, 3, 4, 5, 6), include.lowest = TRUE)

# **Variable: Bed Net Sufficiency per Household Members (1 per 1.8 members)
#~From CG:
#1 bednet per 1.8 household members y/n (bednetper_cat)
# rw15_bc$hh_1.8 <- (rw15_bc$hv012)/1.8
#rw15_bc$net_1.8 <- (rw15_bc$hh_1.8)/(rw15_bc$hml1)
#fix values with 0 bed nets
#rw15_bc$net_1.8[rw15_bc$net_1.8 == Inf]<-0
#rw15_bc <- rw15_bc %>% mutate(bednetper_cat = case_when(
#  net_1.8 > 1 ~ 1,
#  net_1.8 < 1 ~ 0))

# **Variable: Water source
#~From CG:
#binary variable for water source (hv201_cat, 1 = piped, 0 = unpiped)
# rw15_bc$hv201_cat <- cut(rw15_bc$hv201, breaks=c(0, 12, Inf), labels=c(1,0), include.lowest = TRUE) 

# **Variable: Age, splits into categorical variable with 6 categories
#~From CG:
#categorical variable for age (hv105_cat)
#rw15_bc$hv105_cat <- cut(rw15_bc$hv105,
#                         breaks = c(0, 15, 24, 35, 44, 55, Inf),
#                         labels = c(6, 1, 2, 3, 4, 5), include.lowest = TRUE)



##---- STEP 13: Fix missing values
#~From CG:
#hml10 missing values
#summary(rw15_bc$hml10)
#rw15_bc$hml10[is.na(rw15_bc$hml10)]<-12
#summary(rw15_bc$hml10)


##---- STEP 14: Identify which individuals were selected into study based on barcode values 
# 1= selected, 0= not selected
#~From CG:

#data weights---- 
#inverse propensity of selection, HIV sampling, & transmission intensity weighting

#selection into study based on both original barcode columns
#rw15_bc$selecta <- ifelse(rw15_bc$ha62 %in% h62s,1,0)
#rw15_bc$selectb <- ifelse(rw15_bc$hb62 %in% h62s,1,0)
#rw15_bc <- rw15_bc %>% mutate(select = case_when(selecta==1 ~ 1, selectb==1 ~ 1)) 

#replace NAs with 0s in select
#rw15_bc$select[is.na(rw15_bc$select)]<-0

##---- STEP 15: Propensity score calculation
#Purpose: Fits a logistic regression model (glm) to estimate the propensity score of being selected (select) based on various covariates.
#Output: The predicted propensity scores are stored in rw15_bc$ps.
#Variables used: 
  #hv006=month of household interview
  #hv024= region
  #hv025= urban/rural
  #hv040= altitude
  #hv104= sex
  #hv106= highest level of education
  #hv246= number of livestock total
  #hml1= number of mosquito nets household owns
  #hml20= Person slept under an LLIN net
  #hv270= Wealth index quintile
  #hv105_cat= Age (categorical)
  #family=binomial("logit")
#~From CG:
#propensity score with full barcodes dataset
#removed hml10 and hv201_cat from the glm because of missing data, does not change ps values a lot
#ps_model <- glm(select ~ hv006+hv024+hv025+hv040+hv104+hv106+hv246+hml1+hml20+hv270+hv105_cat,family=binomial("logit"), data=rw15_bc)

#add ps to dataframe
#rw15_bc$ps <- predict(ps_model, rw15_bc, type = "response")

##---- STEP 16: Inverse probability weighting
#Purpose: Calculates unstandardized (ipwt_u) and standardized (ipwt) inverse probability weights to adjust for selection bias.
#Application: These weights are critical for ensuring that the analysis reflects the general population despite selection biases.
#~From CG:
#unstandardized
#rw15_bc$ipwt_u <- ifelse(rw15_bc$select==1, 1/rw15_bc$ps, 1/(1-rw15_bc$ps))
#standardized
#p_exposure <- sum(rw15_bc$select) / nrow(rw15_bc)
#rw15_bc$ipwt <- ifelse(rw15_bc$select==1, p_exposure/rw15_bc$ps, (1-p_exposure)/(1-rw15_bc$ps))

##---- STEP 17: Merge survey subset with qPCR data
#~From CG:
#study data subset 
#merge survey subset with qPCR data
#rw_svya<-rw15_bc %>% filter(selecta==1) #This filters the rw15_bc dataset to include only rows where the selecta variable is 1, creating a subset called rw_svya.
#rw_svyb<-rw15_bc %>% filter(selectb==1) #Similarly, this filters rw15_bc for rows where selectb is 1, creating another subset called rw_svyb.
#rw_svya<-distinct(rw_svya) #remove duplicate rows
#rw_svyb<-distinct(rw_svyb) #remove duplicate rows

#rename variables
#rw_svya$h62<-rw_svya$ha62 
#rw_svya$h69<-rw_svya$ha69
#rw_svyb$h62<-rw_svyb$hb62
#rw_svyb$h69<-rw_svyb$hb69

#combine the data sets row-wise, then remove any duplicates
#rw_svy<-rbind(rw_svya, rw_svyb)
#rw_svy<-distinct(rw_svy)


##---- STEP 18: Transmission intensity weights
#~From CG:
#transmission intensity weights
#import transmission intensity data by cluster
#cluster_trans <- read_excel("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/summary datasets/DHS_cluster.xlsx")
#add region names to rw_svy
#rw_svy <- left_join(rw_svy, cluster_trans[,c("hv001","region")],by="hv001")

#transmission intensity matching 
#Adds the trans_intens (transmission intensity) variable to both rw15_bc and rw_svy by merging them with cluster_trans on hv001.
#rw15_bc <- left_join(rw15_bc, cluster_trans[,c("hv001","trans_intens")],by="hv001")
#rw_svy <- left_join(rw_svy, cluster_trans[,c("hv001","trans_intens")],by="hv001")
#check counts (creates frequency table of the transmission intensity variable, INCLUDING NA)
#addmargins(table(rw15_bc$trans_intens, useNA = "always"))


##---- STEP 19: Summarize number of individuals in high/low transmission clusters
#~From CG:
#summarize number of individuals in high/low clusters in all barcodes data set
#overall <- rw15_bc %>% group_by(trans_intens) %>% count()
#summarize number of individuals in high/low clusters in sampled data set
#sampled <- rw_svy %>% group_by(trans_intens) %>% count()


##---- STEP 20: Calculate the transmission intensity weights
#~From CG:
#forming the numerator of the transmission intensity weight
#Calculate the proportion of individuals in high and low transmission intensity clusters in the overall dataset.
#high_ov <- overall[overall$trans_intens=="high",]$n / sum(overall$n)
#low_ov <- overall[overall$trans_intens=="low",]$n / sum(overall$n)

#forming the denominator of the transmission intensity weight
#Calculate the proportion of individuals in high and low transmission intensity clusters in the SAMPLED dataset.
#high_samp <- sampled[sampled$trans_intens=="high",]$n / sum(sampled$n)
#low_samp <- sampled[sampled$trans_intens=="low",]$n / sum(sampled$n)

#final transmission intensity weight
#Calculate the transmission intensity weights by dividing the overall proportion by the sampled proportion for both high and low transmission clusters
#high_wt <- high_ov/high_samp
#low_wt <- low_ov/low_samp


##---- STEP 21: Adjust HIV sample weight
#~From CG:
#replace missing HIV weight values with 1
#rw_svy$h69[rw_svy$h69 == 0]<-1000000 #Replaces any missing HIV weight values (where h69 is 0) with 1,000,000.

#HIV and ip weights for rw_svy
#rw_svy$hiv_wt <- (rw_svy$h69)/1000000 #hiv_wt: Calculates the HIV weight by dividing h69 by 1,000,000.
#rw_svy$hiv_ipw <- rw_svy$hiv_wt*rw_svy$ipwt #Calculates the HIV inverse probability weight by multiplying hiv_wt by the individual probability weight ipwt.


##---- STEP 22: Add qPCR data to the rw survey data
#~From CG:
#add qPCR data to rw_svy-----
#rw_svy<-merge(qpcr_bc,rw_svy, by="h62") #Merges the qPCR dataset qpcr_bc with rw_svy on the h62 variable.


##---- STEP 23: Create Variables
# **Variable: Malaria Species Binary Variables
#~From CG:
#create malaria species binary variables
#rw_svy <- rw_svy %>% mutate(pf = ifelse(is.na(var_CT), 0, 1))
#rw_svy <- rw_svy %>% mutate(pm = ifelse(is.na(pm_CT), 0, 1))
#rw_svy <- rw_svy %>% mutate(po = ifelse(is.na(po_CT), 0, 1))
#rw_svy <- rw_svy %>% mutate(pv = ifelse(is.na(pv_CT), 0, 1))

# **Variable: nonPf
#~From CG:
#non-pf binary variable
#rw_svy$nonpf <- ifelse(rw_svy$pm == 1 | rw_svy$po == 1 | rw_svy$pv == 1, 1, 0)

# **Variable: nonPf
#~From CG:
#mixed infection variable (species) 
#rw_svy$species <- ifelse(rw_svy$pf == 1 & rw_svy$po == 0 & rw_svy$pm == 0 & rw_svy$pv == 0, "pf",
#                   ifelse(rw_svy$pf == 0 & rw_svy$po == 0 & rw_svy$pm == 1 & rw_svy$pv == 0, "pm",
#                   ifelse(rw_svy$pf == 0 & rw_svy$po == 1 & rw_svy$pm == 0 & rw_svy$pv == 0, "po",
#                   ifelse(rw_svy$pf == 0 & rw_svy$po == 0 & rw_svy$pm == 0 & rw_svy$pv == 1, "pv",
#                    ifelse(rw_svy$pf == 1 & rw_svy$po == 1 & rw_svy$pm == 0 & rw_svy$pv == 0, "pf_po",
#                    ifelse(rw_svy$pf == 1 & rw_svy$po == 0 & rw_svy$pm == 1 & rw_svy$pv == 0, "pf_pm",
#                    ifelse(rw_svy$pf == 1 & rw_svy$po == 0 & rw_svy$pm == 0 & rw_svy$pv == 1, "pf_pv",
#                   ifelse(rw_svy$pf == 1 & rw_svy$po == 0 & rw_svy$pm == 1 & rw_svy$pv == 1, "pf_pm_pv",
#                   ifelse(rw_svy$pf == 1 & rw_svy$po == 1 & rw_svy$pm == 1 & rw_svy$pv == 0, "pf_pm_po",
#                   ifelse(rw_svy$pf == 0 & rw_svy$po == 1 & rw_svy$pm == 1 & rw_svy$pv == 0, "pm_po",
#                   ifelse(rw_svy$pf == 0 & rw_svy$po == 1 & rw_svy$pm == 1 & rw_svy$pv == 1, "pm_po_pv",
#                    ifelse(rw_svy$pf == 0 & rw_svy$po == 0 & rw_svy$pm == 1 & rw_svy$pv == 1, "pm_pv",
#                   ifelse(rw_svy$pf == 0 & rw_svy$po == 0 & rw_svy$pm == 0 & rw_svy$pv == 0, "none", "none")))))))))))))


##---- STEP 24: Create "RW40 data set with cycle threshold values below 40
#~From CG:
#under 40 CT values data set (values above 40 are generally considered unreliable in this context)
#rw_svy40<-rw_svy #This creates a copy of the rw_svy dataset, which will be modified in subsequent steps.
#CT_cutoff<-40 #Sets the CT (Cycle Threshold) cutoff value to 40.
#Replaces values greater than 40 in the var_CT column with NA (missing data). The same is done for pm_CT, po_CT, and pv_CT.
#rw_svy40$var_CT[rw_svy40$var_CT > CT_cutoff]<-NA
#rw_svy40$pm_CT[rw_svy40$pm_CT > CT_cutoff]<-NA
#rw_svy40$po_CT[rw_svy40$po_CT > CT_cutoff]<-NA
#rw_svy40$pv_CT[rw_svy40$pv_CT > CT_cutoff]<-NA

##---- STEP 25: Create malaria variables
#~From CG:
#Create CT count variable (CT_count) AND This sums the number of non-missing values across the columns pf, pm, po, and pv for each row. na.rm = TRUE ensures that missing values (NA) are ignored in the summation.
#rw_svy$CT_count <- rowSums(rw_svy[, c("pf", "pm", "po", "pv")], na.rm = TRUE)

##---- STEP 26: Create infection complexity variable (infection)
#Explaination of code: 
  #uses the dplyr package to add a new variable infection to the rw_svy dataset.
  #case_when(...): Creates categorical labels based on the value of CT_count:
  #"co": Coinfection (multiple species) if CT_count is greater than 1.
  #"mono": Monoinfection if CT_count is equal to 1.
  #"none": No infection if CT_count is 0.
#~From CG:
#rw_svy <- rw_svy %>% mutate(infection = case_when(CT_count>1 ~ "co",CT_count==1 ~ "mono",CT_count==0 ~ "none"))

##---- STEP 27: Create malaria binary variable to count malaria cases
#Create binary malaria variable
#~From CG:
#malaria binary variable (1= any species present, 0= no malaria)
#rw_svy <- rw_svy %>% mutate(malaria = case_when(CT_count>0 ~ 1, CT_count==0 ~ 0))

##---- STEP 28: Analyze malaria species data
#create malaria infection variables for rw_svy40
#code explaination: mutate(pf = ifelse(is.na(var_CT), 0, 1)) creates a new variable pf in rw_svy40, where 1 indicates the presence of Plasmodium falciparum (if var_CT is not NA), and 0 indicates its absence. Same for other species.
#~From CG:
#rw_svy40 <- rw_svy40 %>% mutate(pf = ifelse(is.na(var_CT), 0, 1))
#rw_svy40 <- rw_svy40 %>% mutate(pm = ifelse(is.na(pm_CT), 0, 1))
#rw_svy40 <- rw_svy40 %>% mutate(po = ifelse(is.na(po_CT), 0, 1))
#rw_svy40 <- rw_svy40 %>% mutate(pv = ifelse(is.na(pv_CT), 0, 1))

#mixed infection for rw_svy40
# code explaination: ifelse(..., "pf", ...) uses a series of ifelse statements to assign a value to species based on which malaria species are present:
#"pf": Only P. falciparum is present.
#"pf_po", "pf_pm", etc.: Combinations of different species.
#"none": No species are present.
#~From CG:
#rw_svy40$species <- ifelse(rw_svy40$pf == 1 & rw_svy40$po == 0 & rw_svy40$pm == 0 & rw_svy40$pv == 0, "pf",
#                   ifelse(rw_svy40$pf == 0 & rw_svy40$po == 0 & rw_svy40$pm == 1 & rw_svy40$pv == 0, "pm",
#                    ifelse(rw_svy40$pf == 0 & rw_svy40$po == 1 & rw_svy40$pm == 0 & rw_svy40$pv == 0, "po",
#                   ifelse(rw_svy40$pf == 0 & rw_svy40$po == 0 & rw_svy40$pm == 0 & rw_svy40$pv == 1, "pv",
#                    ifelse(rw_svy40$pf == 1 & rw_svy40$po == 1 & rw_svy40$pm == 0 & rw_svy40$pv == 0, "pf_po",
#                    ifelse(rw_svy40$pf == 1 & rw_svy40$po == 0 & rw_svy40$pm == 1 & rw_svy40$pv == 0, "pf_pm",
#                   ifelse(rw_svy40$pf == 1 & rw_svy40$po == 0 & rw_svy40$pm == 0 & rw_svy40$pv == 1, "pf_pv",
#                    ifelse(rw_svy40$pf == 1 & rw_svy40$po == 0 & rw_svy40$pm == 1 & rw_svy40$pv == 1, "pf_pm_pv",
#                   ifelse(rw_svy40$pf == 1 & rw_svy40$po == 1 & rw_svy40$pm == 1 & rw_svy40$pv == 0, "pf_pm_po",
#                    ifelse(rw_svy40$pf == 0 & rw_svy40$po == 1 & rw_svy40$pm == 1 & rw_svy40$pv == 0, "pm_po",
#                    ifelse(rw_svy40$pf == 0 & rw_svy40$po == 1 & rw_svy40$pm == 1 & rw_svy40$pv == 1, "pm_po_pv",
#                   ifelse(rw_svy40$pf == 0 & rw_svy40$po == 0 & rw_svy40$pm == 1 & rw_svy40$pv == 1, "pm_pv",
#                   ifelse(rw_svy40$pf == 0 & rw_svy40$po == 0 & rw_svy40$pm == 0 & rw_svy40$pv == 0, "none", "none")))))))))))))

#CT_count for rw_svy40
  # Code explaination: You are counting the sum of all malaria species columns, with na.rm ensuring that "NA" are ignored
# NEW VARIABLE is CT_count, representing total number of malaria species in each person in the rw_svy40 dataset
#~From CG:
#rw_svy40$CT_count <- rowSums(rw_svy40[, c("pf", "pm", "po", "pv")], na.rm = TRUE)

#Create malaria binary variable
#Code explaination:
  #mutate(): This function is used to create or modify variables within a data frame.
  #case_when(): This function is used to create a binary variable (malaria) based on the value of CT_count.
  #If CT_count is greater than 0, the individual is marked as having malaria (malaria = 1).
  #If CT_count is 0, the individual is marked as not having malaria (malaria = 0).
#~From CG:
#malaria binary for rw_svy40
#rw_svy40 <- rw_svy40 %>% mutate(malaria = case_when(CT_count>0 ~ 1, CT_count==0 ~ 0))

#non-pf binary variable for rw_svy40
#rw_svy40$nonpf <- ifelse(rw_svy40$pm == 1 | rw_svy40$po == 1 | rw_svy40$pv == 1, 1, 0)

##---- STEP 29: Prepare for ecological data matching (line 305 of CG's code here: https://github.com/claudiagaither/rwanda_nonpf/blob/main/studypop%20%26%20glms.R#L305)
