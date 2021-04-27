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
disc_high_sum <- read_csv(here::here("inputs/data/disc_high_sum.csv"))

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