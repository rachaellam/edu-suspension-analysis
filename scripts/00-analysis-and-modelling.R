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
  transform(COST_PER_STUDENT_WOFED = round(SCH_SAL_TOTPERS_WOFED / TOT_ENR, digits = 2)) %>% # finding the cost per student
  transform(COST_PER_STUDENT_TEACH_WOFED = round(SCH_SAL_TEACH_WOFED / TOT_ENR, digits = 2)) %>%
  transform(COST_PER_STUDENT_AID_WOFED = round(SCH_SAL_AID_WOFED / TOT_ENR, digits = 2)) %>%
  transform(COST_PER_STUDENT_SUP_WOFED = round(SCH_SAL_SUP_WOFED / TOT_ENR, digits = 2)) %>%
  transform(COST_PER_STUDENT_ADM_WOFED = round(SCH_SAL_ADM_WOFED / TOT_ENR, digits = 2)) %>%
  transform(COST_PER_STUDENT_NPE_WOFED = round(SCH_NPE_WOFED / TOT_ENR, digits = 2)) %>%
  transform(TEACH_PER_STUDENT = round(TOT_ENR / SCH_FTE_TEACH_WOFED, digits = 2)) %>%
  transform(ISS_PER_100 = round(((TOT_ISS / TOT_ENR) * 100), digits = 2)) %>%
  transform(MULTOOS_PER_100 = round(((TOT_MULTOOS / TOT_ENR) * 100), digits = 2)) %>%
  transform(SINGOOS_PER_100 = round(((TOT_SINGOOS / TOT_ENR) * 100), digits = 2)) %>%
  select(LEA_STATE, LEA_STATE_NAME, LEAID, LEA_NAME, SCHID, SCH_NAME, SCH_NAME, COMBOKEY, TOT_ENR, TOT_DAYSMISSED, TOT_EXPWE, TOT_EXPWOE, TOT_ISS, ISS_PER_100, TOT_MULTOOS, MULTOOS_PER_100,
         TOT_SINGOOS, SINGOOS_PER_100, SCH_SAL_TOTPERS_WOFED, SCH_SAL_TEACH_WOFED, SCH_SAL_AID_WOFED, SCH_SAL_SUP_WOFED, SCH_SAL_ADM_WOFED, COST_PER_STUDENT_WOFED, COST_PER_STUDENT_TEACH_WOFED,
         COST_PER_STUDENT_AID_WOFED, COST_PER_STUDENT_SUP_WOFED, COST_PER_STUDENT_ADM_WOFED, COST_PER_STUDENT_NPE_WOFED, SCH_FTE_TEACH_WOFED, TEACH_PER_STUDENT)

# initial graphing
disc_high_sum %>%
  ggplot(aes(COST_PER_STUDENT_WOFED, TOT_DAYSMISSED)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 25000)) # zoom into the 0-25000 dollar amount

disc_high_sum %>%
  ggplot(aes(COST_PER_STUDENT_TEACH_WOFED, TOT_DAYSMISSED)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 25000))

disc_high_sum %>%
  ggplot(aes(COST_PER_STUDENT_AID_WOFED, TOT_DAYSMISSED)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 25000))

disc_high_sum %>%
  ggplot(aes(COST_PER_STUDENT_WOFED, ISS_PER_100)) +
  geom_point() +
  scale_x_continuous(labels = comma)

disc_high_sum %>%
  ggplot(aes(COST_PER_STUDENT_WOFED, MULTOOS_PER_100)) +
  geom_point() +
  scale_x_continuous(labels = comma)

disc_high_sum %>%
  ggplot(aes(COST_PER_STUDENT_WOFED, SINGOOS_PER_100)) +
  geom_point() +
  scale_x_continuous(labels = comma)

# modelling
lmper <- lm(ISS_PER_100 ~ COST_PER_STUDENT_WOFED, data = disc_high_sum)
summary(lmper)

# add a few single states to see if it supports your overall population results
# 10 most populous states
# new york (pro)
ny <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "NEW YORK")

lmny <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = ny)
summary(lmny)

lmnyiss <- lm(TOT_ISS ~ COST_PER_STUDENT_WOFED, data = ny)
summary(lmnyiss)

# pennsylvania (reg)
pa <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "PENNSYLVANIA")

lmpa <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = pa)
summary(lmpa)

lmpaiss <- lm(TOT_ISS ~ COST_PER_STUDENT_WOFED, data = pa)
summary(lmpaiss)

# illinois (reg)
il <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "ILLINOIS")

lmil <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = il)
summary(lmil)

lmiliss <- lm(TOT_ISS ~ COST_PER_STUDENT_WOFED, data = il)
summary(lmiliss)

# ohio (pro)
oh <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "OHIO")

lmoh <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = oh)
summary(lmoh)

lmohiss <- lm(TOT_ISS ~ COST_PER_STUDENT_WOFED, data = oh)
summary(lmohiss)

# texas (reg)
tx <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "TEXAS")

lmtx <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = tx)
summary(lmtx)

lmtxiss <- lm(TOT_ISS ~ COST_PER_STUDENT_WOFED, data = tx)
summary(lmtxiss)

# florida (reg)
fl <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "FLORIDA")

lmfl <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = fl)
summary(lmfl)

lmfliss <- lm(TOT_ISS ~ COST_PER_STUDENT_WOFED, data = fl)
summary(lmfliss)

# california (pro)
ca <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "CALIFORNIA")

lmca <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = ca)
summary(lmca)

lmcaiss <- lm(TOT_ISS ~ COST_PER_STUDENT_WOFED, data = ca)
summary(lmcaiss)

# georgia
ga <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "GEORGIA")

lmga <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = ga)
summary(lmga)

# north carolina
nc <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "NORTH CAROLINA")

lmnc <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = nc)
summary(lmnc)

# michigan
mi <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "MICHIGAN")

lmmi <- lm(TOT_DAYSMISSED ~ COST_PER_STUDENT_WOFED, data = mi)
summary(lmmi)