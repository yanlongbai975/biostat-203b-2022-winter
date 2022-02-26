#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ICU cohort data for the first ICU stay"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("var", 
                      label = "Choose a variable to display",
                      choices = c("Admission year", "Admission month",
                                  "Admission month day", "Admission week day",
                                  "Admission hour",
                                  "Admission minute",
                                  "Admission duration",
                                  "Anchor age with gender",
                                  "Laboratory measurements",
                                  "Vital measurements in charted data",
                                  "Duration from being admitted to ICU to death"
                                  ),
                      selected = "First admission year"),
          sliderInput("bins",
                      "Number of bins (only for admission years):",
                      min = 20,
                      max = 102,
                      value = 102),
          selectInput("itemid",
                      label = "Which measurements do you want to 
                      check (only for Laboratory measurements and Vital 
                      measurements in charted data)?",
                      choices = c(
                        "Please select", 
                        "Creatinine (50912)", "Potassium (50971)",
                        "Sodium (50983)", "Chloride (50902)",
                        "Bicarbonate (50882)", "Hematocrit (51221)",
                        "White blood cell count (51301)", 
                        "Glucose (50931)","Magnesium (50960)",
                        "Calcium (50893)",
                        "Boxplot for all laboratory measurements",
                        "Heart rate (220045)", 
                        "Mean non-invasive blood pressure (220181)",
                        "Systolic non-invasive blood pressure (220179)", 
                        "Body temperature in Fahrenheit (223761)",
                        "Respiratory rate (220210)", 
                        "Boxplot for all vital measurements"),
                      selected = "Please select")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  icu = read_rds(
    "/Users/a123456/biostat-203b-2022-winter/hw3/mimiciv_shiny/icu_cohort.rds")

    output$distPlot <- renderPlot({
      if(input$var == "Admission year") {
        icu %>%
          ggplot() +
          geom_histogram(mapping = aes(x = year(admittime)), 
                         bins = input$bins) +
          labs(x = "First admission year")
      }else if(input$var == "Admission month") {
        icu %>%
          ggplot() +
          geom_bar(mapping = aes(x = lubridate::month(admittime, 
                                                      label = TRUE))) +
          labs(x = "First admission month")
      }else if(input$var == "Admission month day") {
        icu %>%
          ggplot() +
          geom_bar(mapping = aes(x = mday(admittime))) +
          labs(x = "First admission month day")
      }else if(input$var == "Admission week day") {
        icu %>%
          ggplot() +
          geom_bar(mapping = aes(x = lubridate::wday(admittime, label = TRUE, 
                                                     abbr = TRUE))) +
          labs(x = "First admission week day")
      }else if(input$var == "Admission hour") {
        icu %>%
          ggplot() +
          geom_bar(mapping = aes(x = hour(admittime))) +
          labs(x = "First admission hour")
      }else if(input$var == "Admission minute") {
        icu %>%
          ggplot() +
          geom_bar(mapping = aes(x = minute(admittime))) +
          labs(x = "First admission minute")
      }else if(input$var == "Admission duration") {
        icu %>%
          mutate(dur = as.duration(dischtime - admittime)) %>%
          filter((dur >= 0) & (dur / 86400 <= 15)) %>%
          ggplot() +
          geom_histogram(mapping = aes(x = dur / 86400)) +
          labs(x = "Hospital stay duration (days)")
      }else if(input$var == "Anchor age with gender") {
        icu %>%
        ggplot() + 
          geom_bar(mapping = aes(x = anchor_age, fill = gender))
      }else if(input$var == "Laboratory measurements") {
        icu = icu %>%
          mutate(icu, '51301' = valuenum_51301) %>%
          mutate(icu, '50882' = valuenum_50882) %>%
          mutate(icu, '51221' = valuenum_51221) %>%
          mutate(icu, '50912' = valuenum_50912) %>%
          mutate(icu, '50893' = valuenum_50893) %>%
          mutate(icu, '50971' = valuenum_50971) %>%
          mutate(icu, '50983' = valuenum_50983) %>%
          mutate(icu, '50902' = valuenum_50902) %>%
          mutate(icu, '50960' = valuenum_50960) %>%
          mutate(icu, '50931' = ifelse(valuenum_50931 < 600, valuenum_50931, 
                                      NA)) %>%
          pivot_longer(c('51301', '50882', '51221', '50912', '50893', '50971', 
                         '50983', '50902', '50960', '50931'), 
                       names_to = "labitemid", values_to = "labvaluenum")
        if(input$itemid == "Boxplot for all laboratory measurements"){
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_boxplot(mapping = aes(x = as.character(labitemid), 
                                       y = labvaluenum))
        }else if(input$itemid == "Creatinine (50912)"){
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_50912))
        }else if(input$itemid == "Potassium (50971)") {
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_50971))
        }else if(input$itemid == "Sodium (50983)") {
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_50983))
        }else if(input$itemid == "Chloride (50902)") {
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_50902))
        }else if(input$itemid == "Bicarbonate (50882)") {
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_50882))
        }else if(input$itemid == "Hematocrit (51221)") {
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_51221))
        }else if(input$itemid == "White blood cell count (51301)") {
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_51301))
        }else if(input$itemid == "Glucose (50931)") {
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_50931))
        }else if(input$itemid == "Magnesium (50960)") {
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_50960))
        }else if(input$itemid == "Calcium (50893)") {
          icu %>%
            group_by(labitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_50893))
        }else FALSE
      }else if(input$var == "Vital measurements in charted data") {
        icu = icu %>%
          mutate(icu, '220181' = 
                   ifelse(valuenum_220181 < 1000, valuenum_220181, NA)) %>%
          mutate(icu, '220179' = 
                   ifelse(valuenum_220179 < 1000, valuenum_220179, NA)) %>%
          mutate(icu, '223761' = 
                   ifelse(valuenum_223761 < 1000, valuenum_223761, NA)) %>%
          mutate(icu, '220210' = 
                   ifelse(valuenum_220210 < 1000, valuenum_220210, NA)) %>%
          mutate(icu, '220045' = 
                   ifelse(valuenum_220045 < 1000, valuenum_220045, NA)) %>%
          pivot_longer(c('220181', '220179', '223761', '220210', '220045'), 
                       names_to = "chartitemid", values_to = "chartvaluenum")
        if(input$itemid == "Boxplot for all vital measurements"){
          icu %>%
            group_by(chartitemid) %>%
            ggplot() + 
            geom_boxplot(mapping = aes(x = as.character(chartitemid),
                                       y = chartvaluenum))
        }else if(input$itemid == "Heart rate (220045)"){
          icu %>%
            group_by(chartitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_220045))
        }else if(input$itemid == "Mean non-invasive blood pressure (220181)") {
          icu %>%
            group_by(chartitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_220181))
        }else if(input$itemid == 
                 "Systolic non-invasive blood pressure (220179)") {
          icu %>%
            group_by(chartitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_220179))
        }else if(input$itemid == "Body temperature in Fahrenheit (223761)") {
          icu %>%
            group_by(chartitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_223761))
        }else if(input$itemid == "Respiratory rate (220210)") {
          icu %>%
            group_by(chartitemid) %>%
            ggplot() + 
            geom_histogram(mapping = aes(x = valuenum_220210))
        }else FALSE
      }else if(input$var == "Duration from being admitted to ICU to death") {
        title("1000")
        icu %>%
          mutate(dur_death = ifelse(is.na(dod), NA, dod - date(admittime))) %>%
          ggplot() +
          geom_histogram(mapping = aes(x = dur_death)) +
          labs(x = "Days from being admitted to ICU to death",
               title = "The number of patients died within 30 days is 5436,
               mean length of duration is 220.6446 days.") +
          theme(plot.title = element_text(hjust = 0.5))
      }
      else FALSE
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
