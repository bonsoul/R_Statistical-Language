

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
library(readr)
library(tidyr)
library(kableExtra)
library(htmltools)
library(shinythemes)
library(forecast)
library(tidyverse)
library(rsconnect)


df <- read.csv("D:/Downloads/1chorela_cases_dataset.csv")

#EDA

any(is.na(data))

sum(is.na(data))

colSums(is.na(df))

unique(df$County)



#duplicates
duplicated(df)

df[duplicated(df), ]

df <- df %>% distinct()


df$Date.Of.Onset <- as.Date(df$Date.Of.Onset, format="%d/%m/%Y")



library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

# Load dataset (ensure this file is in your working directory)
df <- read.csv(("D:/Downloads/1chorela_cases_dataset.csv"), stringsAsFactors = FALSE)
county <- st_read("D:/Downloads/extracted/County.shp")


df1 <- df %>%
  mutate(County = case_when(
    tolower(County) %in% c("murang'a", "muranga", "murang'a") ~ "Muranga",
    tolower(County) %in% c("kajiando", "kajiado") ~ "Kajiado",
    tolower(County) %in% c("trans nzoia", "transnzoia") ~ "Trans-Nzoia",
    tolower(County) %in% c("homa bay", "homabay") ~ "Homa-Bay",
    tolower(County) %in% c("elgeyo marakwet", "elgeyo-marakwet") ~ "Elgeyo-Marakwet",
    tolower(County) %in% c("tharaka nithi", "tharaka-nithi") ~ "Tharaka-Nithi",
    tolower(County) %in% c("tanariver", "tana river") ~ "Tana-River",
    TRUE ~ County  # Keeps the original value if no match is found
  )) %>%
  filter(County != "Nan")  # Remove rows where County is "Nan"



# Define UI for the application
ui <- fluidPage(
  theme = shinytheme("cyborg"),  # Fixed theme issue
  
  # Application title
  titlePanel("Cholera Outbreak Dashboard"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range:", 
                  min = 2008, 
                  max = 2024, 
                  value = c(2008, 2024), 
                  sep = ""),
      
      
      selectInput("county", "Select County:", 
                  choices = unique(df1$County), 
                  selected = unique(df1$County)[1], 
                  multiple = TRUE)
    ),
    
    # Main panel with tabset
    mainPanel(
      tabsetPanel(
        tabPanel("Epidemic Curve", fluidRow(column(10,plotOutput("trend_plot", height = "80vh")))
        ),
        tabPanel("Cases by County", fluidRow(column(10,plotOutput("cases_by_county", height = "80vh"))) 
        ),
        tabPanel("Age Distribution", fluidRow(column(10,plotOutput("age_dist", height = "80vh")))
        ),
        tabPanel("Gender Distribution",fluidRow(column(10,plotOutput("gender_dist", height = "80vh")))
        ),
        tabPanel("Forecasting",fluidRow(column(10,plotOutput("forecast_plot", height = "80vh")))
        ),
        tabPanel("Cholera Map", fluidRow(column(10,leafletOutput("cholera_map", height = "80vh")))
        ),
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Reactive dataset based on user input
  filtered_data <- reactive({
    df %>%
      # Filter the data based on year range and selected counties
      filter(Year_Onset >= input$year_range[1] & Year_Onset <= input$year_range[2],
             County %in% input$county) %>%
      # Standardize the county names
      mutate(County = case_when(
        tolower(County) %in% c("Murang'a","Muranga", "muranga","Murang'A") ~ "Muranga",
        tolower(County) %in% c("kajiando", "Kajiado") ~ "Kajiado",
        tolower(County) %in% c("Trans Nzoia","Transnzoia") ~ "Trans-Nzoia",
        tolower(County) %in% c("Homa Bay","Homabay" ) ~ "Homa-Bay",
        tolower(County) %in% c("Elgeyo Marakwet","Elgeyo-Marakwet" ) ~ "Egeyo-Marakwet",
        tolower(County) %in% c("Tharaka Nithi","Tharaka-Nithi" ) ~ "Tharaka-Nithi",
        tolower(County) %in% c("Tanariver","Tana River" ) ~ "Tana-River",
        TRUE ~ County  # Leaving all other values unchanged
      )) %>%
      # Standardize Sex column values
      mutate(Sex = case_when(
        tolower(Sex) %in% c("male", "m", "masculine", "male ") ~ "Male",
        tolower(Sex) %in% c("female", "f", "feminine", "female ") ~ "Female",
        FALSE ~ NA_character_
      )) %>%
      # Remove rows with NA in Sex column
      filter(!is.na(Sex))
  })
  
  
  # Line graph - Trend Analysis
  output$trend_plot <- renderPlot({
    yearly_cases <- filtered_data() %>%
      group_by(Year_Onset) %>%
      summarise(Cases = n(), .groups = "drop") %>%
      mutate(Year_Onset = as.numeric(Year_Onset))  # Ensure Year is numeric
    
    ggplot(yearly_cases, aes(x = Year_Onset, y = Cases, group = 1)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = "Trend Analysis",
           x = "Year",
           y = "Number of Cases") +
      theme_minimal()
  })
  
  # Histogram - filtered_dataEpidemic Curve
  df$Date <- as.Date(df$Date, format="%Y-%m-%d")
  
  output$epi_curve <- renderPlot({
    ggplot(filtered_data(), aes(x = as.Date(Date))) +
      geom_histogram(binwidth = 30, fill = "blue", color = "white") +
      labs(title = "Epidemic Curve",
           x = "Date",
           y = "Number of Cases") +
      theme_minimal()
  })
  
  # Bar Chart - Cases by County
  output$cases_by_county <- renderPlot({
    county_cases <- filtered_data() %>%
      group_by(County) %>%
      summarise(Cases = n(), .groups = "drop")
    
    ggplot(county_cases, aes(x = reorder(County, -Cases), y = Cases, fill = County)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Cases by County",
           x = "County",
           y = "Number of Cases") +
      theme_minimal()
  })
  
  # Histogram - Age Distribution
  output$age_dist <- renderPlot({
    ggplot(filtered_data(), aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "purple", color = "white") +
      labs(title = "Age Distribution of Cases",
           x = "Age",
           y = "Frequency") +
      theme_minimal()
  })
  
  # Bar Chart - Gender Distribution
  output$gender_dist <- renderPlot({
    gender_cases <- filtered_data() %>%
      group_by(Sex) %>%
      summarise(Cases = n(), .groups = "drop")
    
    ggplot(gender_cases, aes(x = Sex, y = Cases, fill = Sex)) +
      geom_bar(stat = "identity") +
      labs(title = "Gender Distribution of Cases",
           x = "Gender",
           y = "Number of Cases") +
      theme_minimal()
  })
  
  # Time Series Model and Forecasting
  output$forecast_plot <- renderPlot({
    
    ts_cases <- ts(yearly_cases$Cases, start = min(yearly_cases$Year), frequency = 1)
    
    # Fit ARIMA model
    arima_model <- auto.arima(ts_cases)
    
    # Forecast future cases
    forecasted_cases <- forecast(arima_model, h = 5)
    
    # Plot the forecast
    autoplot(forecasted_cases) +
      ggtitle("Cholera Cases Forecast") +
      xlab("Year") +
      ylab("Number of Cases")
  })
  
  # Leaflet Cholera Map
  output$cholera_map <- renderLeaflet({
    cholera_map <- st_read("D:/Downloads/extracted/County.shp")
    
    cholera_cases_by_county <- filtered_data() %>%
      group_by(County) %>%
      summarise(Cholera_Cases = n())
    
    cholera_map <- left_join(cholera_map, cholera_cases_by_county, by = c("COUNTY" = "County"))
    cholera_map$Cholera_Cases[is.na(cholera_map$Cholera_Cases)] <- 0
    
    # Define bins for classification
    bins = c(0, 10, 50, 100, 200, 500, 1000, max(cholera_map$Cholera_Cases, na.rm = TRUE))
    
    # Define color palette
    pal = colorBin("YlOrRd", domain = cholera_map$Cholera_Cases, bins = bins)
    
    # Define labels for hover text
    labels = sprintf(
      "<strong>%s</strong><br/>Cholera Cases: %g",
      cholera_map$COUNTY, cholera_map$Cholera_Cases
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data = cholera_map) %>%
      setView(lat = -0.0, lng = 36.681660, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(Cholera_Cases),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.8,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, values = ~Cholera_Cases, opacity = 0.7, title = "Cholera Cases",
        position = "bottomleft"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
rsconnect::deployApp('D:/Downloads/CholeraDashboard')

