### Preamble ###
# Purpose: EDA of Department of Education Data
# Author: Rachael Lam
# Date: March 10 2021
# Contact: rachael.lam@mail.utoronto.ca
# Pre-req: None

### Workspace Set-Up ###
# install.packages("here")
# install.packages("devtools")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("performance")
# install.packages("see")
# install.packages("broom")

library(here)
library(devtools)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(performance)
library(see)
library(broom)

### Load Dataset ###
raw_data <- read.csv("inputs/data/CRDC-2015-16-School-Data.csv")


### Cleaning Data ###
data <- raw_data %>%
  as_tibble() %>%
  filter(grepl("No", SCH_STATUS_ALT)) %>% # removing rows where schools are alternative, charter, magnet, special ed, and juvenile justice facility
  filter(grepl("No", SCH_STATUS_CHARTER)) %>%
  filter(grepl("No", SCH_STATUS_MAGNET)) %>%
  filter(grepl("No", SCH_STATUS_SPED)) %>%
  filter(grepl("No", JJ))

### Schools that only offer primary school grade (PS) ###
pri <- data %>%
  filter(grepl("Yes", SCH_GRADE_PS)) %>%
  filter_at(vars(SCH_GRADE_KG, SCH_GRADE_G01, SCH_GRADE_G02, SCH_GRADE_G03, SCH_GRADE_G04, SCH_GRADE_G05, SCH_GRADE_G06, 
                 SCH_GRADE_G07,SCH_GRADE_G08, SCH_GRADE_G09, SCH_GRADE_G10, SCH_GRADE_G11, SCH_GRADE_G12, SCH_GRADE_UG), all_vars(!. %in% c("Yes")))

### Schools that only offer elementary school grades (K-7) ###
ele <- data %>%
  filter(grepl("No", SCH_GRADE_PS)) %>%
  filter_at(vars(SCH_GRADE_KG, SCH_GRADE_G01, SCH_GRADE_G02, SCH_GRADE_G03, SCH_GRADE_G04, SCH_GRADE_G05), all_vars(. %in% c("Yes"))) %>% # only selecting schools that offer K-5 so total numbers don't affect total amounts
  filter_at(vars(SCH_GRADE_G06, SCH_GRADE_G07,SCH_GRADE_G08, SCH_GRADE_G09, SCH_GRADE_G10, SCH_GRADE_G11, 
                 SCH_GRADE_G12, SCH_GRADE_UG), all_vars(!. %in% c("Yes")))

### Schools that only offer middle school grades (6-8) ###
mid <- data %>%
  filter(grepl("No", SCH_GRADE_PS)) %>%
  filter_at(vars(SCH_GRADE_G06, SCH_GRADE_G07,SCH_GRADE_G08), all_vars(. %in% c("Yes"))) %>%
  filter_at(vars(SCH_GRADE_KG, SCH_GRADE_G01, SCH_GRADE_G02, SCH_GRADE_G03, SCH_GRADE_G04, SCH_GRADE_G05, 
                 SCH_GRADE_G09, SCH_GRADE_G10, SCH_GRADE_G11, SCH_GRADE_G12, SCH_GRADE_UG), all_vars(!. %in% c("Yes")))

### Schools that only offer high school grades (9-12) ###
high <- data %>%
  filter(grepl("No", SCH_GRADE_PS)) %>%
  filter_at(vars(SCH_GRADE_G09, SCH_GRADE_G10, SCH_GRADE_G11, SCH_GRADE_G12), all_vars(. %in% c("Yes"))) %>%
  filter_at(vars(SCH_GRADE_KG, SCH_GRADE_G01, SCH_GRADE_G02, SCH_GRADE_G03, SCH_GRADE_G04, SCH_GRADE_G05, SCH_GRADE_G06, 
                 SCH_GRADE_G07, SCH_GRADE_G08, SCH_GRADE_UG), all_vars(!. %in% c("Yes"))) %>%
  select(-contains("GED")) %>% # remove GED programs
  select(-contains("GT")) %>% # remove Gifted and Talented enrollment
  select(-contains("DUAL")) %>% # remove dual enrollment (college credit in high school)
  select(-contains("ALG")) %>% # remove algebra
  select(-contains("GEO")) %>% # remove geography
  select(-contains("ADVM")) %>% # remove advance math
  select(-contains("CALC")) %>% # remove calculus
  select(-contains("SCI")) %>% # remove science
  select(-contains("SSCLASSES")) %>% # remove single sex classes
  select(-contains("AP")) %>% # remove AP classes
  select(-contains("IBENR")) %>% # remove IB program
  select(-contains("SATACT")) %>% # remove SAT ACT exams
  select(-contains("SSATHLETICS")) %>% # remove single sex athletics
  select(-contains("SSSPORTS")) %>% # remove single sex sports
  select(-contains("SSTEAMS")) %>% # remove single sex teams
  select(-contains("SSPART")) %>% # remove single sex athletics participation
  select(-contains("HB")) %>% # remove harassment and bullying
  select(-contains("UG")) %>% # remove ungraded grade
  select(-contains("LEP")) %>% # remove english competancy
  select(-contains("IDEA")) %>% # remove disabilities
  select(-contains("504")) # remove disabilities

### Schools that offer all grades (K-12) ###
all <- data %>%
  filter(grepl("No", SCH_GRADE_PS)) %>%
  filter_at(vars(SCH_GRADE_KG, SCH_GRADE_G01, SCH_GRADE_G02, SCH_GRADE_G03, SCH_GRADE_G04, SCH_GRADE_G05, SCH_GRADE_G06, 
                 SCH_GRADE_G07,SCH_GRADE_G08, SCH_GRADE_G09, SCH_GRADE_G10, SCH_GRADE_G11, SCH_GRADE_G12), all_vars(. %in% c("Yes"))) %>%
  filter_at(vars(SCH_GRADE_UG), all_vars(!. %in% c("Yes")))

### Schools that offer grades 1-12 ###
nok <- data %>%
  filter(grepl("No", SCH_GRADE_PS)) %>%
  filter_at(vars(SCH_GRADE_G01, SCH_GRADE_G02, SCH_GRADE_G03, SCH_GRADE_G04, SCH_GRADE_G05, SCH_GRADE_G06, 
                 SCH_GRADE_G07,SCH_GRADE_G08, SCH_GRADE_G09, SCH_GRADE_G10, SCH_GRADE_G11, SCH_GRADE_G12), all_vars(. %in% c("Yes"))) %>%
  filter_at(vars(SCH_GRADE_KG, SCH_GRADE_UG), all_vars(!. %in% c("Yes")))

### High school EDA ###
# Seeing if non JJ schools still have instances of corporal punishment
high %>%
  ggplot(aes(SCH_CORPINSTANCES_IND, fill = SCH_CORPINSTANCES_IND)) +
  geom_bar()

corp_high <- high %>%
  filter_at(vars(SCH_CORPINSTANCES_IND), all_vars(. %in% c("Yes")))

# sum students as WH or POC (AM - Indigenous, AS - Asian American, BL - Black, HI - Hispanic, HP - Hawaiian/Pacific Islander, TR - two or more races)
high_sum <- high %>%
  mutate(TOT_DAYSMISSED_POC = select(., "SCH_DAYSMISSED_AM_F", "SCH_DAYSMISSED_AM_M", "SCH_DAYSMISSED_AS_F", "SCH_DAYSMISSED_AS_M", "SCH_DAYSMISSED_BL_F", 
                                     "SCH_DAYSMISSED_BL_M", "SCH_DAYSMISSED_HI_F", "SCH_DAYSMISSED_HI_M", "SCH_DAYSMISSED_HP_F", "SCH_DAYSMISSED_HP_M", 
                                     "SCH_DAYSMISSED_TR_F", "SCH_DAYSMISSED_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DAYSMISSED_WH = select(., "SCH_DAYSMISSED_WH_F", "SCH_DAYSMISSED_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_ARR_POC = select(., "SCH_DISCWODIS_ARR_AM_F", "SCH_DISCWODIS_ARR_AM_M", "SCH_DISCWODIS_ARR_AS_F",
                                        "SCH_DISCWODIS_ARR_AS_M", "SCH_DISCWODIS_ARR_BL_F", "SCH_DISCWODIS_ARR_BL_M", "SCH_DISCWODIS_ARR_HI_F", 
                                        "SCH_DISCWODIS_ARR_HI_M", "SCH_DISCWODIS_ARR_HP_F", "SCH_DISCWODIS_ARR_HP_M", "SCH_DISCWODIS_ARR_TR_F", 
                                        "SCH_DISCWODIS_ARR_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_ARR_WH = select(., "SCH_DISCWODIS_ARR_WH_F", "SCH_DISCWODIS_ARR_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_EXPWE_POC = select(., "SCH_DISCWODIS_EXPWE_AM_F", "SCH_DISCWODIS_EXPWE_AM_M", "SCH_DISCWODIS_EXPWE_AS_F", "SCH_DISCWODIS_EXPWE_AS_M",
                                          "SCH_DISCWODIS_EXPWE_BL_F", "SCH_DISCWODIS_EXPWE_BL_M", "SCH_DISCWODIS_EXPWE_HI_F", "SCH_DISCWODIS_EXPWE_HI_M",
                                          "SCH_DISCWODIS_EXPWE_HP_F", "SCH_DISCWODIS_EXPWE_HP_M", "SCH_DISCWODIS_EXPWE_TR_F", "SCH_DISCWODIS_EXPWE_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_EXPWE_WH = select(., "SCH_DISCWODIS_EXPWE_WH_F", "SCH_DISCWODIS_EXPWE_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_EXPWOE_POC = select(., "SCH_DISCWODIS_EXPWOE_AM_F", "SCH_DISCWODIS_EXPWOE_AM_M", "SCH_DISCWODIS_EXPWOE_AS_F", "SCH_DISCWODIS_EXPWOE_AS_M",
                                           "SCH_DISCWODIS_EXPWOE_BL_F", "SCH_DISCWODIS_EXPWOE_BL_M", "SCH_DISCWODIS_EXPWOE_HI_F", "SCH_DISCWODIS_EXPWOE_HI_M",
                                           "SCH_DISCWODIS_EXPWOE_HP_F", "SCH_DISCWODIS_EXPWOE_HP_M", "SCH_DISCWODIS_EXPWOE_TR_F", "SCH_DISCWODIS_EXPWOE_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_EXPWOE_WH = select(., "SCH_DISCWODIS_EXPWOE_WH_F", "SCH_DISCWODIS_EXPWOE_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_EXPZT_POC = select(.,"SCH_DISCWODIS_EXPZT_AM_F", "SCH_DISCWODIS_EXPZT_AM_M", "SCH_DISCWODIS_EXPZT_AS_F", "SCH_DISCWODIS_EXPZT_AS_M",
                                          "SCH_DISCWODIS_EXPZT_BL_F", "SCH_DISCWODIS_EXPZT_BL_M", "SCH_DISCWODIS_EXPZT_HI_F", "SCH_DISCWODIS_EXPZT_HI_M",
                                          "SCH_DISCWODIS_EXPZT_HP_F", "SCH_DISCWODIS_EXPZT_HP_M", "SCH_DISCWODIS_EXPZT_TR_F", "SCH_DISCWODIS_EXPZT_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_EXPZT_WH = select(., "SCH_DISCWODIS_EXPZT_WH_F", "SCH_DISCWODIS_EXPZT_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_ISS_POC = select(., "SCH_DISCWODIS_ISS_AM_F", "SCH_DISCWODIS_ISS_AM_M", "SCH_DISCWODIS_ISS_AS_F", "SCH_DISCWODIS_ISS_AS_M",
                                        "SCH_DISCWODIS_ISS_BL_F", "SCH_DISCWODIS_ISS_BL_M", "SCH_DISCWODIS_ISS_HI_F", "SCH_DISCWODIS_ISS_HI_M",
                                        "SCH_DISCWODIS_ISS_HP_F", "SCH_DISCWODIS_ISS_HP_M", "SCH_DISCWODIS_ISS_TR_F", "SCH_DISCWODIS_ISS_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_ISS_WH = select(., "SCH_DISCWODIS_ISS_WH_F", "SCH_DISCWODIS_ISS_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_MULTOOS_POC = select(., "SCH_DISCWODIS_MULTOOS_AM_F", "SCH_DISCWODIS_MULTOOS_AM_M", "SCH_DISCWODIS_MULTOOS_AS_F", "SCH_DISCWODIS_MULTOOS_AS_M",
                                            "SCH_DISCWODIS_MULTOOS_BL_F", "SCH_DISCWODIS_MULTOOS_BL_M", "SCH_DISCWODIS_MULTOOS_HI_F", "SCH_DISCWODIS_MULTOOS_HI_M",
                                            "SCH_DISCWODIS_MULTOOS_HP_F", "SCH_DISCWODIS_MULTOOS_HP_M", "SCH_DISCWODIS_MULTOOS_TR_F", "SCH_DISCWODIS_MULTOOS_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_MULTOOS_WH = select(., "SCH_DISCWODIS_MULTOOS_WH_F", "SCH_DISCWODIS_MULTOOS_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_REF_POC = select(., "SCH_DISCWODIS_REF_AM_F", "SCH_DISCWODIS_REF_AM_M", "SCH_DISCWODIS_REF_AS_F", "SCH_DISCWODIS_REF_AS_M",
                                        "SCH_DISCWODIS_REF_BL_F", "SCH_DISCWODIS_REF_BL_M", "SCH_DISCWODIS_REF_HI_F", "SCH_DISCWODIS_REF_HI_M",
                                        "SCH_DISCWODIS_REF_HP_F", "SCH_DISCWODIS_REF_HP_M", "SCH_DISCWODIS_REF_TR_F", "SCH_DISCWODIS_REF_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_REF_WH = select(., "SCH_DISCWODIS_REF_WH_F", "SCH_DISCWODIS_REF_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_SINGOOS_POC = select(., "SCH_DISCWODIS_SINGOOS_AM_F", "SCH_DISCWODIS_SINGOOS_AM_M", "SCH_DISCWODIS_SINGOOS_AS_F",
                                            "SCH_DISCWODIS_SINGOOS_AS_M", "SCH_DISCWODIS_SINGOOS_BL_F", "SCH_DISCWODIS_SINGOOS_BL_M",
                                            "SCH_DISCWODIS_SINGOOS_HI_F", "SCH_DISCWODIS_SINGOOS_HI_M", "SCH_DISCWODIS_SINGOOS_HP_F",
                                            "SCH_DISCWODIS_SINGOOS_HP_M", "SCH_DISCWODIS_SINGOOS_TR_F", "SCH_DISCWODIS_SINGOOS_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_SINGOOS_WH = select(., "SCH_DISCWODIS_SINGOOS_WH_F", "SCH_DISCWODIS_SINGOOS_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_TFRALT_POC = select(., "SCH_DISCWODIS_TFRALT_AM_F", "SCH_DISCWODIS_TFRALT_AM_M", "SCH_DISCWODIS_TFRALT_AS_F", "SCH_DISCWODIS_TFRALT_AS_M",
                                           "SCH_DISCWODIS_TFRALT_BL_F", "SCH_DISCWODIS_TFRALT_BL_M", "SCH_DISCWODIS_TFRALT_HI_F", "SCH_DISCWODIS_TFRALT_HI_M",  
                                           "SCH_DISCWODIS_TFRALT_HP_F", "SCH_DISCWODIS_TFRALT_HP_M", "SCH_DISCWODIS_TFRALT_TR_F", "SCH_DISCWODIS_TFRALT_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_DISCWODIS_TFRALT_WH = select(., "SCH_DISCWODIS_TFRALT_WH_F", "SCH_DISCWODIS_TFRALT_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_POC = select(., "SCH_ENR_AM_F", "SCH_ENR_AM_M", "SCH_ENR_AS_F", "SCH_ENR_AS_M", "SCH_ENR_BL_F", "SCH_ENR_BL_M", "SCH_ENR_HI_F", "SCH_ENR_HI_M",
                          "SCH_ENR_HP_F", "SCH_ENR_HP_M", "SCH_ENR_TR_F", "SCH_ENR_TR_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_WH = select(., "SCH_ENR_WH_F", "SCH_ENR_WH_M")  %>% rowSums(na.rm = TRUE)) %>%
  select(LEA_STATE, LEA_STATE_NAME, LEAID, LEA_NAME, SCHID, SCH_NAME, SCH_NAME, COMBOKEY, SCH_GRADE_G09, SCH_GRADE_G10,
         SCH_GRADE_G11, SCH_GRADE_G12, SCH_FTE_ADM_WFED, SCH_FTE_ADM_WOFED, SCH_FTE_AID_WFED, SCH_FTE_AID_WOFED, SCH_FTE_SUP_WFED, SCH_FTE_SUP_WOFED,
         SCH_FTE_TEACH_WOFED, SCH_FTECOUNSELORS, SCH_FTESECURITY_GUA, SCH_FTESECURITY_LEO, SCH_FTESERVICES_NUR, SCH_FTESERVICES_SOC, SCH_FTETEACH_CERT, 
         SCH_FTETEACH_FY, SCH_FTETEACH_NOTCERT, SCH_FTETEACH_SY, SCH_FTETEACH_TOT, SCH_NPE_WFED, SCH_NPE_WOFED, SCH_OOSINSTANCES_WODIS, SCH_RSINSTANCES_MECH_WODIS,
         SCH_RSINSTANCES_PHYS_WODIS, SCH_RSINSTANCES_SECL_WODIS, TOT_DAYSMISSED_POC, TOT_DAYSMISSED_WH, TOT_DISCWODIS_ARR_POC, TOT_DISCWODIS_ARR_WH, TOT_DISCWODIS_EXPWE_POC, TOT_DISCWODIS_EXPWE_WH, 
         TOT_DISCWODIS_EXPWOE_POC, TOT_DISCWODIS_EXPWOE_WH, TOT_DISCWODIS_EXPZT_POC, TOT_DISCWODIS_EXPZT_WH, TOT_DISCWODIS_ISS_POC, TOT_DISCWODIS_ISS_WH,
         TOT_DISCWODIS_MULTOOS_POC, TOT_DISCWODIS_MULTOOS_WH, TOT_DISCWODIS_REF_POC, TOT_DISCWODIS_REF_WH, TOT_DISCWODIS_SINGOOS_POC, TOT_DISCWODIS_SINGOOS_WH,
         TOT_DISCWODIS_TFRALT_POC, TOT_DISCWODIS_TFRALT_WH, SCH_SAL_ADM_WFED, SCH_SAL_ADM_WOFED, SCH_SAL_AID_WFED, SCH_SAL_AID_WOFED, SCH_SAL_SUP_WFED,
         SCH_SAL_SUP_WOFED, SCH_SAL_TEACH_WFED, SCH_SAL_TEACH_WOFED, SCH_SAL_TOTPERS_WFED, SCH_SAL_TOTPERS_WOFED, SCH_TEACHERS_CURR_TOT, TOT_DAYSMISSED_F, TOT_DAYSMISSED_M,
         TOT_DISCWODIS_ARR_F, TOT_DISCWODIS_ARR_M, TOT_DISCWODIS_EXPZT_F, TOT_DISCWODIS_EXPZT_M, TOT_DISCWODIS_EXPWE_F, TOT_DISCWODIS_EXPWE_M, TOT_DISCWODIS_EXPWOE_F,
         TOT_DISCWODIS_EXPWOE_M, TOT_DISCWODIS_ISS_F, TOT_DISCWODIS_ISS_M, TOT_DISCWODIS_MULTOOS_F, TOT_DISCWODIS_MULTOOS_M, TOT_DISCWODIS_REF_F, TOT_DISCWODIS_REF_M,
         TOT_DISCWODIS_SINGOOS_F, TOT_DISCWODIS_SINGOOS_M, TOT_DISCWODIS_TFRALT_F, TOT_DISCWODIS_TFRALT_M, TOT_ENR_M, TOT_ENR_F)

# only focusing on suspensions
disc_high <- high %>%
  select(-contains("ABSENT")) %>% # absent from school
  select(-contains("CORP")) %>% # corporal punishment
  select(-contains("ARR")) %>% # arrests
  select(-contains("EXPZT")) %>% # expulsions
  select(-contains("REF")) %>% # referred to law enforcement
  select(-contains("TFRALT")) %>% # transferred to alternative school for disciplinary reasons
  select(-contains("GRADE")) %>% # we know that we're focusing on 9-12 so we are removing the columns
  select(-contains("OFFENSE")) %>% # all offenses ie. battery, robbery, fights
  select(-contains("PS")) %>% # all preschool columns
  select(-contains("RET")) %>% # retention
  select(-contains("STATUS")) %>% # we removed all schools that have an alternative status, don't need columns
  select(-contains("CREDIT")) # credit recovery

# totalling all students who have received suspensions 
disc_high_sum <- disc_high %>%
  filter_at(vars(SCH_SAL_TOTPERS_WOFED, SCH_SAL_TEACH_WOFED, SCH_SAL_AID_WOFED, SCH_SAL_SUP_WOFED, SCH_SAL_ADM_WOFED, SCH_FTE_TEACH_WOFED), all_vars(. > 0)) %>% # -9 and -5 is unreported
  mutate(TOT_DAYSMISSED = select(., "TOT_DAYSMISSED_F", "TOT_DAYSMISSED_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_EXPWE = select(., "TOT_DISCWODIS_EXPWE_F", "TOT_DISCWODIS_EXPWE_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_EXPWOE = select(.,  "TOT_DISCWODIS_EXPWOE_F", "TOT_DISCWODIS_EXPWOE_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_ISS = select(., "TOT_DISCWODIS_ISS_F", "TOT_DISCWODIS_ISS_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_MULTOOS = select(., "TOT_DISCWODIS_MULTOOS_F", "TOT_DISCWODIS_MULTOOS_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_SINGOOS = select(., "TOT_DISCWODIS_SINGOOS_F", "TOT_DISCWODIS_SINGOOS_M")  %>% rowSums(na.rm = TRUE)) %>%
  mutate(TOT_ENR = select(., "TOT_ENR_F", "TOT_ENR_M")  %>% rowSums(na.rm = TRUE)) %>%
  transform(SPEND_PER_STUDENT_WOFED = round(SCH_SAL_TOTPERS_WOFED / TOT_ENR, digits = 2)) %>% # finding the cost per student
  transform(SPEND_PER_STUDENT_TEACH_WOFED = round(SCH_SAL_TEACH_WOFED / TOT_ENR, digits = 2)) %>%
  transform(SPEND_PER_STUDENT_AID_WOFED = round(SCH_SAL_AID_WOFED / TOT_ENR, digits = 2)) %>%
  transform(SPEND_PER_STUDENT_SUP_WOFED = round(SCH_SAL_SUP_WOFED / TOT_ENR, digits = 2)) %>%
  transform(SPEND_PER_STUDENT_ADM_WOFED = round(SCH_SAL_ADM_WOFED / TOT_ENR, digits = 2)) %>%
  transform(SPEND_PER_STUDENT_NPE_WOFED = round(SCH_NPE_WOFED / TOT_ENR, digits = 2)) %>%
  transform(TEACH_PER_STUDENT = round(TOT_ENR / SCH_FTE_TEACH_WOFED, digits = 2)) %>%
  transform(DAYSMISSED_PER_100 = round(((TOT_DAYSMISSED / TOT_ENR) * 100), digits = 2)) %>%
  transform(ISS_PER_100 = round(((TOT_ISS / TOT_ENR) * 100), digits = 2)) %>%
  transform(MULTOOS_PER_100 = round(((TOT_MULTOOS / TOT_ENR) * 100), digits = 2)) %>%
  transform(SINGOOS_PER_100 = round(((TOT_SINGOOS / TOT_ENR) * 100), digits = 2)) %>%
  select(LEA_STATE, LEA_STATE_NAME, LEAID, LEA_NAME, SCHID, SCH_NAME, SCH_NAME, COMBOKEY, TOT_ENR, TOT_DAYSMISSED, DAYSMISSED_PER_100, TOT_EXPWE, TOT_EXPWOE, TOT_ISS, 
         ISS_PER_100, TOT_MULTOOS, MULTOOS_PER_100, TOT_SINGOOS, SINGOOS_PER_100, SCH_SAL_TOTPERS_WOFED, SCH_SAL_TEACH_WOFED, SCH_SAL_AID_WOFED, SCH_SAL_SUP_WOFED, 
         SCH_SAL_ADM_WOFED, SPEND_PER_STUDENT_WOFED, SPEND_PER_STUDENT_TEACH_WOFED, SPEND_PER_STUDENT_AID_WOFED, SPEND_PER_STUDENT_SUP_WOFED, SPEND_PER_STUDENT_ADM_WOFED, 
         SPEND_PER_STUDENT_NPE_WOFED, SCH_FTE_TEACH_WOFED, TEACH_PER_STUDENT) %>%
  filter(SPEND_PER_STUDENT_SUP_WOFED < 1000000) # removing outlier of 2346486.911, leading me to believe that it was incorrect reporting

# split into republican states and democratic states from the 2016 election
rep <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "ALABAMA" | LEA_STATE_NAME == "ALASKA" | LEA_STATE_NAME == "ARIZONA" | LEA_STATE_NAME == "ARKANSAS" | LEA_STATE_NAME == "FLORIDA"
         | LEA_STATE_NAME == "GEORGIA" | LEA_STATE_NAME == "IDAHO" | LEA_STATE_NAME == "INDIANA" | LEA_STATE_NAME == "IOWA" | LEA_STATE_NAME == "KANSAS"
         | LEA_STATE_NAME == "KENTUCKY" | LEA_STATE_NAME == "LOUISIANA" | LEA_STATE_NAME == "MICHIGAN" | LEA_STATE_NAME == "MISSISSIPPI"
         | LEA_STATE_NAME == "MISSOURI" | LEA_STATE_NAME == "MONTANA" | LEA_STATE_NAME == "NEBRASKA" | LEA_STATE_NAME == "NORTH CAROLINA"
         | LEA_STATE_NAME == "NORTH DAKOTA" | LEA_STATE_NAME == "OHIO" | LEA_STATE_NAME == "OKLAHOMA" | LEA_STATE_NAME == "PENNSYLVANIA"
         | LEA_STATE_NAME == "SOUTH CAROLINA" | LEA_STATE_NAME == "SOUTH DAKOTA" | LEA_STATE_NAME == "TENNESSEE" | LEA_STATE_NAME == "TEXAS"
         | LEA_STATE_NAME == "UTAH" | LEA_STATE_NAME == "WEST VIRGINIA" | LEA_STATE_NAME == "WISCONSIN" | LEA_STATE_NAME == "WYOMING")

dem <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "CALIFORNIA" | LEA_STATE_NAME == "COLORADO" | LEA_STATE_NAME == "CONNECTICUT" | LEA_STATE_NAME == "DELAWARE"
         | LEA_STATE_NAME == "DISTRICT OF COLUMBIA" | LEA_STATE_NAME == "HAWAII" | LEA_STATE_NAME == "ILLINOIS" | LEA_STATE_NAME == "IOWA"
         | LEA_STATE_NAME == "MAINE" | LEA_STATE_NAME == "MARYLAND" | LEA_STATE_NAME == "MASSACHUSETTS" | LEA_STATE_NAME == "MINNESOTA"
         | LEA_STATE_NAME == "NEVADA" | LEA_STATE_NAME == "NEW HAMPSHIRE" | LEA_STATE_NAME == "NEW JERSEY" | LEA_STATE_NAME == "NEW MEXICO"
         | LEA_STATE_NAME == "NEW YORK" | LEA_STATE_NAME == "OREGON" | LEA_STATE_NAME == "RHODE ISLAND" | LEA_STATE_NAME == "VERMONT"
         | LEA_STATE_NAME == "VIRGINIA" | LEA_STATE_NAME == "WASHINGTON")

# split by funding distribution https://edlawcenter.org/research/making-the-grade-2020.html
pro <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "ALASKA" | LEA_STATE_NAME == "UTAH" | LEA_STATE_NAME == "SOUTH DAKOTA" | LEA_STATE_NAME == "NEBRASKA" | LEA_STATE_NAME == "MINNESOTA"
         | LEA_STATE_NAME == "WYOMING" | LEA_STATE_NAME == "COLORADO" | LEA_STATE_NAME == "NEW MEXICO" | LEA_STATE_NAME == "MONTANA" | LEA_STATE_NAME == "CALIFORNIA"
         | LEA_STATE_NAME == "INDIANA" | LEA_STATE_NAME == "MARYLAND" | LEA_STATE_NAME == "OHIO" | LEA_STATE_NAME == "NEW YORK" | LEA_STATE_NAME == "WISCONSIN"
         | LEA_STATE_NAME == "ARKANSAS")

flat <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "NORTH CAROLINA" | LEA_STATE_NAME == "SOUTH CAROLINA" | LEA_STATE_NAME == "TENNESSEE" | LEA_STATE_NAME == "GEORGIA"
         | LEA_STATE_NAME == "OKLAHOMA" | LEA_STATE_NAME == "DELAWARE" | LEA_STATE_NAME == "MASSACHUSETTS" | LEA_STATE_NAME == "ARIZONA"
         | LEA_STATE_NAME == "KANSAS" | LEA_STATE_NAME == "VIRGINIA" | LEA_STATE_NAME == "MISSISSIPPI" | LEA_STATE_NAME == "NORTH DAKOTA"
         | LEA_STATE_NAME == "NEW JERSEY" | LEA_STATE_NAME == "LOUISIANA" | LEA_STATE_NAME == "KENTUCKY" | LEA_STATE_NAME == "IOWA" | LEA_STATE_NAME == "IDAHO")

reg <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "FLORIDA" | LEA_STATE_NAME == "TEXAS" | LEA_STATE_NAME == "WEST VIRGINIA" | LEA_STATE_NAME == "MICHIGAN" | LEA_STATE_NAME == "OREGON"
         | LEA_STATE_NAME == "WASHINGTON" | LEA_STATE_NAME == "PENNSYLVANIA" | LEA_STATE_NAME == "CONNECTICUT" | LEA_STATE_NAME == "ALABAMA"
         | LEA_STATE_NAME == "MAINE" | LEA_STATE_NAME == "RHODE ISLAND" | LEA_STATE_NAME == "MISSOURI" | LEA_STATE_NAME == "ILLINOIS" | LEA_STATE_NAME == "NEVADA"
         | LEA_STATE_NAME == "NEW HAMPSHIRE")








  


