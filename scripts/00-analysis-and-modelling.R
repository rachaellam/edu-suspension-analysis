### Preamble ###
# Purpose: Analysis and Modelling of Department of Education Data
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
# install.packages("reshape2")
# install.packages("knitr)
# install.packages("palmerpenguins")
# install.packages("patchwork")

library(here)
library(devtools)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(performance)
library(see)
library(broom)
library(reshape2)
library(knitr)
library(palmerpenguins)
library(patchwork)

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
  transform(DAYSMISSED_PER_100 = round(((TOT_DAYSMISSED / TOT_ENR) * 100), digits = 2)) %>%
  transform(ISS_PER_100 = round(((TOT_ISS / TOT_ENR) * 100), digits = 2)) %>%
  transform(MULTOOS_PER_100 = round(((TOT_MULTOOS / TOT_ENR) * 100), digits = 2)) %>%
  transform(SINGOOS_PER_100 = round(((TOT_SINGOOS / TOT_ENR) * 100), digits = 2)) %>%
  select(LEA_STATE, LEA_STATE_NAME, LEAID, LEA_NAME, SCHID, SCH_NAME, SCH_NAME, COMBOKEY, TOT_ENR, TOT_DAYSMISSED, DAYSMISSED_PER_100, TOT_ISS, 
         ISS_PER_100, TOT_MULTOOS, MULTOOS_PER_100, TOT_SINGOOS, SINGOOS_PER_100, SCH_SAL_TOTPERS_WOFED, SCH_SAL_TEACH_WOFED, SCH_SAL_AID_WOFED, SCH_SAL_SUP_WOFED, 
         SCH_SAL_ADM_WOFED, SPEND_PER_STUDENT_WOFED, SPEND_PER_STUDENT_TEACH_WOFED, SPEND_PER_STUDENT_AID_WOFED, SPEND_PER_STUDENT_SUP_WOFED, SPEND_PER_STUDENT_ADM_WOFED, 
         SPEND_PER_STUDENT_NPE_WOFED) %>%
  filter(SPEND_PER_STUDENT_SUP_WOFED < 1000000) # removing outlier of 2346486.911, leading me to believe that it was incorrect reporting

# removing high-leverage data points
final <- disc_high_sum %>%
  filter(SPEND_PER_STUDENT_WOFED < 100000) %>% # there are 4
  filter(SPEND_PER_STUDENT_NPE_WOFED < 150000) # there are 7

# initial graphing

issplot <- disc_high_sum %>%
  ggplot(aes(SPEND_PER_STUDENT_WOFED, ISS_PER_100)) +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  labs(x = "Spend per student without federal funding",
       y = "Number of in school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-250,000")

issplotzoom <- disc_high_sum %>%
  ggplot(aes(SPEND_PER_STUDENT_WOFED, ISS_PER_100)) +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 50000))+
  labs(x = "Spend per student without federal funding",
       y = "Number of in school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-50,000")

issplot + issplotzoom

issnpe <- disc_high_sum %>%
  ggplot(aes(SPEND_PER_STUDENT_NPE_WOFED, ISS_PER_100)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  labs(x = "Spend per student on non-personnel (NPE) without federal funding",
       y = "Number of in school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-1,000,000")

issnpezoom <- disc_high_sum %>%
  ggplot(aes(SPEND_PER_STUDENT_NPE_WOFED, ISS_PER_100)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 10000)) +
  labs(x = "Spend per student on non-personnel (NPE) without federal funding",
       y = "Number of in school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-10,000")
  
issnpe + issnpezoom

daysmissedplot <- disc_high_sum %>%
  ggplot(aes(SPEND_PER_STUDENT_WOFED, DAYSMISSED_PER_100)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  labs(x = "Spend per student without federal funding",
       y = "Number of days missed due to out of school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-250,000")

daysmissedplotzoom <- disc_high_sum %>%
  ggplot(aes(SPEND_PER_STUDENT_WOFED, DAYSMISSED_PER_100)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 50000)) +
  labs(x = "Spend per student without federal funding",
       y = "Number of days missed due to out of school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-250,000")

daysmissedplot + daysmissedplotzoom

daysnpe <- disc_high_sum %>%
  ggplot(aes(SPEND_PER_STUDENT_NPE_WOFED, DAYSMISSED_PER_100)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  labs(x = "Spend per student on non-personnel (NPE) without federal funding",
       y = "Number of days missed due to out of school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-1,000,000")

daysnpezoom <- disc_high_sum %>%
  ggplot(aes(SPEND_PER_STUDENT_WOFED, DAYSMISSED_PER_100)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 50000)) +
  labs(x = "Spend per student on non-personnel (NPE) without federal funding",
       y = "Number of days missed due to out of school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-50,000")

daysnpe + daysnpezoom

# test and training datasets
set.seed(853)

data_split <- rsample::initial_split(disc_high_sum, prop = 0.80)

data_train <- rsample::training(data_split)
data_test <- rsample::testing(data_split)

# modelling
lmiss <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = final)

lmdays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = final)

lmsingoos <- lm(SINGOOS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = final)

lmmultoos <- lm(MULTOOS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = final)

lmtype <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_TEACH_WOFED + SPEND_PER_STUDENT_AID_WOFED + SPEND_PER_STUDENT_SUP_WOFED + SPEND_PER_STUDENT_ADM_WOFED, data = final)

# validation
vlmiss <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = data_train) #using train data for validation

vlmdays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = data_train)
summary(vlmdays)

vlmsingoos <- lm(SINGOOS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = data_train)
vlmmultoos <- lm(MULTOOS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = data_train)

vlmtype <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_TEACH_WOFED + SPEND_PER_STUDENT_AID_WOFED + SPEND_PER_STUDENT_SUP_WOFED + SPEND_PER_STUDENT_ADM_WOFED, data = data_train)
summary(vlmtype)

#iss
iss_predictions <- vlmiss %>%
  predict(data_test)
iss_rmse <- rmse(iss_predictions, data_test$ISS_PER_100)

#singoos
singoos_predictions <- vlmsingoos %>%
  predict(data_test)
singoos_rmse <- rmse(singoos_predictions, data_test$SINGOOS_PER_100)

#multioos
multoos_predictions <- vlmmultoos %>%
  predict(data_test)
multoos_rmse <- rmse(multoos_predictions, data_test$MULTOOS_PER_100)

#daysmissed
days_predictions <- vlmdays %>%
  predict(data_test)
days_rmse <- rmse(days_predictions, data_test$DAYSMISSED_PER_100)

#type
type_predictions <- vlmtype %>%
  predict(data_test)
type_rmse <- rmse(type_predictions, data_test$ISS_PER_100)

# creating tibble to put into kable later
rmse_data <-
  tibble(iss_rmse = iss_rmse,
         singoos_rmse = singoos_rmse,
         multoos_rmse = multoos_rmse,
         days_rmse = days_rmse,
         type_rmse = type_rmse)


#graphing with training data
trainissplot <- data_train %>%
  ggplot(aes(SPEND_PER_STUDENT_WOFED, ISS_PER_100)) +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  labs(x = "Spend per student without federal funding",
       y = "Number of in school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-250,000")

trainissplotzoom <- data_train %>%
  ggplot(aes(SPEND_PER_STUDENT_WOFED, ISS_PER_100)) +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 50000))+
  labs(x = "Spend per student without federal funding",
       y = "Number of in school suspensions per 100 students",
       title = "In School Suspensions x Spend $0-50,000")

trainissplot + trainissplotzoom

# model performance
performance::check_model(lmiss)
performance::check_model(lmsingoos)
performance::check_model(lmmultoos)
performance::check_model(lmtype)

# overall numbers for salary spend
salary <- disc_high_sum %>%
  select(SPEND_PER_STUDENT_TEACH_WOFED, SPEND_PER_STUDENT_AID_WOFED, SPEND_PER_STUDENT_SUP_WOFED, SPEND_PER_STUDENT_ADM_WOFED) %>%
  melt() %>%
  group_by(variable) %>%
  summarize(the_min = min(value),
            the_max = max(value),
            the_mean = mean(value),
            the_median = median(value),
            std_dev = sd(value))

salary %>%
  kable(digits = 2,
        caption = "Salary Overview Across All Schools",
        col.names = c ("Spend Per Student", "Min", "Max", "Mean", "Median", "Standard Deviation"),
        align = c('l', 'l', 'l', 'l', 'l'))

# add a few single states to see if it supports your overall population results
# 10 most populous states
# new york (pro)
ny <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "NEW YORK")

lmny <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = ny)
summary(lmny)

lmnydays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = ny)
summary(lmnydays)

# pennsylvania (reg)
pa <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "PENNSYLVANIA")

lmpa <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = pa)
summary(lmpa)

lmpadays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = pa)
summary(lmpadays)

# illinois (reg)
il <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "ILLINOIS")

lmil <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = il)
summary(lmil)

lmildays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = il)
summary(lmildays)

# ohio (pro)
oh <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "OHIO")

lmoh <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = oh)
summary(lmoh)

lmohdays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = oh)
summary(lmohdays)

# texas (reg)
tx <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "TEXAS")

lmtx <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = tx)
summary(lmtx)

lmtxdays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = tx)
summary(lmtxdays)

# florida (reg)
fl <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "FLORIDA")

lmfl <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = fl)
summary(lmfl)

lmfldays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = fl)
summary(lmfldays)

# california (pro)
ca <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "CALIFORNIA")

lmca <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = ca)
summary(lmca)

lmcadays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = ca)
summary(lmcadays)

# georgia
ga <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "GEORGIA")

lmga <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = ga)
summary(lmga)

lmgadays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = ga)
summary(lmgadays)

# north carolina
nc <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "NORTH CAROLINA")

lmnc <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = nc)
summary(lmnc)

lmncdays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = nc)
summary(lmncdays)

# michigan
mi <- disc_high_sum %>%
  filter(LEA_STATE_NAME == "MICHIGAN")

lmmi <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = mi)
summary(lmmi)

lmmidays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = mi)
summary(lmmidays)

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

lmrep <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = rep)
summary(lmrep)

lmrepdays <- lmdays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = rep)
summary(lmrepdays)

lmdem <- lm(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = dem)
summary(lmdem)

lmdemdays <- lm(DAYSMISSED_PER_100 ~ SPEND_PER_STUDENT_WOFED + SPEND_PER_STUDENT_NPE_WOFED, data = dem)
summary(lmdemdays)

### Polynomial Testing ###

test <- disc_high_sum %>%
  filter(SPEND_PER_STUDENT_WOFED < 20000)

testfit <- disc_high_sum %>%
  lm(formula = ISS_PER_100 ~ poly(SPEND_PER_STUDENT_WOFED, 2, raw = TRUE))

summary(testfit)

test %>%
  ggplot(aes(x = SPEND_PER_STUDENT_WOFED, y = ISS_PER_100)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  scale_x_continuous(labels = comma)

testfit <- lm(ISS_PER_100 ~ poly(SPEND_PER_STUDENT_WOFED, 2) + SPEND_PER_STUDENT_NPE_WOFED, data = disc_high_sum)
summary(testfit)

plot(ISS_PER_100 ~ SPEND_PER_STUDENT_WOFED, disc_high_sum)
lines(disc_high_sum$SPEND_PER_STUDENT_WOFED, predict(testfit), col = 'red')




