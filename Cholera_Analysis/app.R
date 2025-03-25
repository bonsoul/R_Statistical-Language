

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)


df <- read.csv("D:/Downloads/1chorela_cases_dataset.csv")

#EDA

any(is.na(data))

sum(is.na(data))

colSums(is.na(df))


#duplicates
duplicated(df)

df[duplicated(df), ]

df <- df %>% distinct()


df$Date.Of.Onset <- as.Date(df$Date.Of.Onset, format="%d/%m/%Y")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cholera Outbreak Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("year_range", "Select Year Range:", 
                      min = 2008, max = 2024, value = c(2008, 2024), sep = ""),
          selectInput("county", "Select County:", choices = unique(data$County), selected = unique(data$County)[1], multiple = TRUE),
          
        ),
        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             tabPanel("Trend Analysis", plotOutput("trend_plot")),
             tabPanel("Epidemic Curve", plotOutput("epi_curve")),
             tabPanel("Cases by County", plotOutput("cases_by_county")),
             tabPanel("Age Distribution", plotOutput("age_dist")),
             tabPanel("Gender Distribution", plotOutput("gender_dist")),
             tabPanel("Seasonality", plotOutput("seaonality"))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered_data <- reactive({
    df %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2],
             County %in% input$county)
  })
  
  # Line graph
  output$trend_plot <- renderPlot({
    yearly_cases <- filtered_data() %>%
      group_by(Year) %>%
      summarise(Cases = n(), .groups = "drop") %>%
      mutate(Year = as.numeric(Year))  # Ensure Year is numeric
    
    ggplot(yearly_cases, aes(x = Year, y = Cases, group = 1)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = 'red', size = 2) +
      labs(title = "Trend Analysis",
           x = "Year",
           y = "Number of Cases") +  # Fix incorrect `y -` issue
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
