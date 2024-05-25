library(dplyr)
library(duckdb)

here::i_am('no-show-model-validation.Rproj')

con <- dbConnect(duckdb(), 'data/noshow_data.duckdb')


#############################################################
# DEMOGRAPHICS
#
#############################################################
pats <- 
  vroom::vroom('data/raw/No_Show_Predictive_Model_Patients_2024_05_14.txt',
               col_select = c(PAT_ID, BIRTH_DATE, PAT_SEX, PATIENT_RACE,
                              ETHNIC_GROUP_NM, LANGUAGE_NM, MARITAL_STATUS_NM,
                              IS_ADOPTED_YN)) %>% 
  rename_with(tolower)


# Check one row per-patient
assertthat::are_equal(
  n_distinct(pats$pat_id), 
  nrow(pats))


duckdb::dbWriteTable(con, 'patients', pats)

rm(pats)



#############################################################
# HOMELESSNESS
#
#############################################################
hml <- 
  vroom::vroom(
    'data/raw/No_Show_Predictive_Model_Patients_Homelessness_2024_05_14.txt',
    col_select = c(PAT_ID, DOCUMENTATION_DATE)) %>% 
  rename_with(tolower)


duckdb::dbWriteTable(con, 'homelessness', hml)

rm(hml)


#############################################################
# ENCOUNTERS
#
# Old file has 820,031 obs
#   - included anciallary clinics

# New file has 565,999 obs
#   - only includes main clinic
#   - does not include Magnolia

#############################################################
encs <- 
  vroom::vroom('data/raw/No_Show_Predictive_Model_Encounters_2024_05_14.txt',
               col_select = c(PAT_ID, APPT_MADE_DATE, APPT_TIME, 
                              LAST_PREDICTED_NS, PAT_ENC_CSN_ID,
                              DEPARTMENT_ID, DEPARTMENT_NAME, APPT_STATUS,
                              ENC_TYPE, VISIT_TYPE, PRODUCT_TYPE,
                              BMI_VALUE, SMOKING_TOBACCO_USE,
                              RECENT_NOSHOW_COUNT)) %>% 
  rename_with(tolower)


# Check one row per-encounter CSN
assertthat::are_equal(
  n_distinct(encs$pat_enc_csn_id), 
  nrow(encs))


duckdb::dbWriteTable(con, 'encounters', encs)

rm(encs)



#############################################################
# ANALYTIC DATA
#
#############################################################

noshow_first_eligible_encounter <- 
  dbGetQuery(con, statement = readr::read_file(
    'data/_create_nowshow_first_eligible_encounter.sql'))


dbWriteTable(con, 
             'noshow_first_eligible_encounter', 
             noshow_first_eligible_encounter)


# 92,457 patients
dbGetQuery(con, 
           "select count(distinct pat_id) from noshow_first_eligible_encounter")


# Double check existing tables
dbGetQuery(con, "show tables")


# Disconnect from database
dbDisconnect(con)
