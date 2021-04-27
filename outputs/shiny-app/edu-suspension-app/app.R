#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
### Workspace Set-Up ###
# install.packages("tidyverse")
# install.packages("devtools")
# install.packages("dplyr")
# install.packages("scales")
# install.packages("ggplot2")
# install.packages("here")
# install.packages("shiny")

library(tidyverse)
library(devtools)
library(dplyr)
library(ggplot2)
library(here)
library(scales)
library(shiny)


### Load Dataset ###
raw_data <- read_csv(here::here("inputs/data/CRDC-2015-16-School-Data.csv"))


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
    select(LEA_STATE, LEA_STATE_NAME, LEAID, LEA_NAME, SCHID, SCH_NAME, SCH_NAME, COMBOKEY, TOT_ENR, TOT_DAYSMISSED, DAYSMISSED_PER_100, TOT_EXPWE, TOT_EXPWOE, TOT_ISS, 
           ISS_PER_100, TOT_MULTOOS, MULTOOS_PER_100, TOT_SINGOOS, SINGOOS_PER_100, SCH_SAL_TOTPERS_WOFED, SCH_SAL_TEACH_WOFED, SCH_SAL_AID_WOFED, SCH_SAL_SUP_WOFED, 
           SCH_SAL_ADM_WOFED, SCH_NPE_WOFED, SPEND_PER_STUDENT_WOFED, SPEND_PER_STUDENT_TEACH_WOFED, SPEND_PER_STUDENT_AID_WOFED, SPEND_PER_STUDENT_SUP_WOFED, SPEND_PER_STUDENT_ADM_WOFED, SPEND_PER_STUDENT_NPE_WOFED) %>%
    filter(SPEND_PER_STUDENT_SUP_WOFED < 1000000) # removing outlier of 2346486.911, leading me to believe that it was incorrect reporting

# enrollment top 10
enrollment <- disc_high_sum %>%
    group_by(LEA_STATE_NAME) %>%
    summarize(State_Enrollment = sum(TOT_ENR),
              ISS = sum(TOT_ISS))


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("State Student Enrollment and Number of In School Suspensions"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("State_Enrollment",
                        "Enrolled:",
                        min = 0,
                        max = 1250000,
                        value = 10000),
            hr(),
            helpText("Graph is filled and ordered by the number of ISS. The lighter the colour, the higher the number of in school suspensions.")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        enrollment %>%
            filter(State_Enrollment <= input$State_Enrollment) %>%
            ggplot(aes(x = reorder(LEA_STATE_NAME, -ISS), y = State_Enrollment, fill = ISS)) +
            geom_bar(stat = "identity") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                  legend.position = 'none',
                  text = element_text(size = 8)) +
            labs(x = "State",
                 y = "Enrollment") +
            scale_y_continuous(labels = comma)
        })
}
# Run the application 
shinyApp(ui = ui, server = server)