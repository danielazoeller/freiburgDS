setwd("~/Documents/MDS/Projekte/CORD")

#########################################################################
###                     See script_task_b_cf_pku.r                    ###
###               Crack FHIR Resources using fhircrackr               ###
#########################################################################

library(fhircrackr)
library(tidyverse)
library(ggplot2)

search_request <- paste0(
  'https://mii-agiop-cord.life.uni-leipzig.de/fhir/',
  'Condition?',
  'code=E70.0,E70.1,E84.0,E84.1,E84.8,E84.80,E84.87,E84.88,E84.9',
  '&_include=Condition:subject'
)

# define design 
design <- list(
  Conditions = list(
    resource = "//Condition",
    cols = list(
      condition_id = "id",
      code = "code/coding/code",
      display = "code/coding/display",
      text = "code/text",
      system = "code/coding/system",
      patient_id = "subject/reference",
      encounter_id = "encounter/reference",
      recorded_date = "recordedDate",
      onset_period_start = "onsetPeriod/start",
      onset_period_end = "onsetPeriod/end"
    ),
    style = list(
      sep="|",
      brackets = c("[", "]"),
      rm_empty_cols = FALSE
    )
  ),
  Patients = list(
    resource = "//Patient",
    cols = list(
      patient_id = "identifier/value",
      name_use = "name/use",
      name_family = "name/family",
      name_given = "name/given",
      gender = "gender",
      birthdate = "birthDate"
    ),
    style = list(
      sep="|",
      brackets = c("[", "]"),
      rm_empty_cols = FALSE
    )
  ) 
)

# download fhir bundles
bundles <- fhir_search(request = search_request, max_bundles = 50,verbose =2,log_errors = 2)

# crack fhir bundles
dfs <- fhir_crack(bundles, design)

# save raw patients dataframe
patients_raw <- dfs$Patients

# unnest raw patients dataframe columns name/use and name/family
patients_tmp <- fhir_melt(patients_raw,
                          columns = c('name_use','name_family'),
                          brackets = c('[',']'), sep = '|', all_columns = TRUE,)

# remove brackets from cells
patients_tmp <- fhir_rm_indices(patients_tmp, brackets = c("[", "]") )

# filter by official name/use
patients_tmp <- patients_tmp[patients_tmp$name_use == 'official',]

# calculate age in years by birthdate
patients_tmp$age <- round( as.double( as.Date( Sys.time() ) - as.Date( patients_tmp$birthdate ) ) / 365.25, 2 )

# remove duplicate patients
patients <- patients_tmp[!duplicated(patients_tmp$patient_id),]

# save raw conditions dataframe
conditions_raw <- dfs$Conditions

# unnest raw conditions dataframe columns code/coding/code, code/coding/display, code/coding/system
conditions_tmp <- fhir_melt(conditions_raw,
                            columns = c('code','display','system'),
                            brackets = c('[',']'), sep = '|', all_columns = TRUE,)
conditions_tmp <- fhir_melt(conditions_tmp,
                            columns = c('code','display','system'),
                            brackets = c('[',']'), sep = '|', all_columns = TRUE,)

# remove brackets from cells
conditions_tmp <- fhir_rm_indices(conditions_tmp, brackets = c("[", "]") )

# filter conditions by system = icd-10-gm
conditions_tmp <- conditions_tmp[conditions_tmp$system == 'http://fhir.de/CodeSystem/dimdi/icd-10-gm',]

# remove duplicate patients
conditions <- conditions_tmp[!duplicated(conditions_tmp$patient_id),]

# remove Patient/ from subject/reference and Encounter from encounter/reference
conditions$patient_id <- sub("Patient/", "", conditions[,6])
conditions$encounter_id <- sub("Encounter/", "", conditions[,7])

# separate patient_id into Airolo, Bapu, Cynthia institution_id
conditions$institution_id <- unlist(strsplit(conditions$patient_id,'-P-'))[ c(TRUE,FALSE) ]

# merge all patients and conditions data by patient_id
df_merged <- merge(patients, conditions, by = "patient_id")

# if necessary or wanted, filter by Airolo, Bapu, Cynthia, default: all data
#df_merged <- df_merged[grep('Airolo', df_merged$patient_id),]
#df_merged <- df_merged[grep('Bapu', df_merged$patient_id),]
#df_merged <- df_merged[grep('Cynthia', df_merged$patient_id),]


# rename gender values
df_merged$gender [df_result$gender == "female"] <- "f"
df_merged$gender [df_result$gender == "male"] <- "m"
df_merged$gender [df_result$gender == ""] <- "NA"


#########################################################################
###                    Reduce DataFrame to relevant                   ###
###                      + Little Formatations                        ###
#########################################################################
# group codes
df_merged$code_grouped <- ifelse(df_merged$code %in% c('E84.0','E84.1','E84.8','E84.80','E84.87','E84.88','E84.9'), "E84",
                                 ifelse(df_merged$code %in% c('E70.0', 'E70.1'), "E70", NA))

library(lubridate)

df_merged$start_month <- month(as.Date(df_merged$onset_period_start,"%Y-%m-%d"))
df_merged$end_month <- month(as.Date(df_merged$onset_period_end,"%Y-%m-%d"))
df_merged$sex <- as.factor(df_merged$gender)
df_merged$code_f <- as.factor(df_merged$code)
df_merged$code_grouped_f <- as.factor(df_merged$code_grouped)
df_merged$id <- seq(1,nrow(df_merged))

# prefinal dataframe with relevant columns
df_result <- df_merged[,c('id','code','code_f', 'display','text',
                        'code_grouped','code_grouped_f',
                        'start_month', 'end_month',
                        'gender', 'sex','age')]


#########################################################################
###                         Upload to Opal                            ###
#########################################################################

set.seed(9203)
resB <- split(df_result, sample(rep(1:2, (nrow(df_result)/2))))

#Server 101
upload_opal(resB[[2]], "administrator", "datashield_test&", "http://192.168.56.101:8080", "CORD4", "BlockB")

#Server 100
upload_opal(resB[[1]], "administrator", "datashield_test&", "http://192.168.56.100:8080", "CORD4", "BlockB")

save(resB, file="DatenBlockB.RData")
