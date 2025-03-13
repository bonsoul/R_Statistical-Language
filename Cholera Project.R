install.packages(c("shiny", "ggplot2", "plotly", "readxl", "dplyr"))
install.packages(c("sf", "leaflet", "forecast"))



library(shiny)
library(ggplot2)
library(plotly)
library(readxl)
library(dplyr)
library(DT)
library(readr)
library(sf)        # For spatial mapping
library(leaflet)   # For interactive maps
library(forecast)


# Load dataset
data <- read_csv("D:/Downloads/2Cleaned_Final - 2Cleaned_Final.csv (2).csv")


# Ensure date format
data$`Date of Onset` <- as.Date(data$`Date of Onset`, format="%Y-%m-%d")

# Create Year column
data$Year <- format(data$`Date of Onset`, "%Y")


# Convert to sf object for mapping
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# Define UI
ui <- fluidPage(
  titlePanel("Cholera Outbreak Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_var", "Select Column:", choices = names(data)),
      selectInput("year", "Select Year:", choices = unique(data$Year))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Epidemic Curve", plotOutput("epicurve")),
        tabPanel("Cases by County", plotOutput("county_cases")),
        tabPanel("Age Distribution", plotOutput("age_dist")),
        tabPanel("Sex Distribution", plotOutput("sex_dist")),
        tabPanel("Mapping", leafletOutput("cholera_map")),
        tabPanel("ARIMA Forecast", plotOutput("arima_plot")),
        tabPanel("Data Table", DTOutput("data_table"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Epidemic Curve
  output$epicurve <- renderPlot({
    epidemic_data <- data %>%
      group_by(Year) %>%
      summarise(Cases = n())
    
    ggplot(epidemic_data, aes(x = Year, y = Cases)) +
      geom_bar(stat="identity", fill="blue") +
      labs(title = "Epidemic Curve", x = "Year", y = "Cases") +
      theme_minimal()
  })
  
  # Cases by County
  output$county_cases <- renderPlot({
    county_data <- data %>%
      group_by(County) %>%
      summarise(Cases = n()) %>%
      arrange(desc(Cases))
    
    ggplot(county_data, aes(x = reorder(County, -Cases), y = Cases)) +
      geom_bar(stat="identity", fill="red") +
      labs(title = "Cases by County", x = "County", y = "Cases") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Age Distribution
  output$age_dist <- renderPlot({
    ggplot(data, aes(x = Age)) +
      geom_histogram(fill="green", bins=30) +
      labs(title = "Age Distribution", x = "Age", y = "Frequency") +
      theme_minimal()
  })
  
  # Sex Distribution
  output$sex_dist <- renderPlot({
    ggplot(data, aes(x = Sex)) +
      geom_bar(fill="purple") +
      labs(title = "Sex Distribution", x = "Sex", y = "Count") +
      theme_minimal()
  })
  
  # Cholera Mapping
  output$cholera_map <- renderLeaflet({
    leaflet(data_sf) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude, radius = 4, color = "red",
                       popup = ~paste("County:", County, "<br>Cases:", HIV_Positive),
                       label = ~County)
  })
  
  # ARIMA Forecast
  output$arima_plot <- renderPlot({
    # Aggregate cases by month
    time_series_data <- data %>%
      group_by(Year) %>%
      summarise(Cases = n())
    
    # Convert to time series
    ts_data <- ts(time_series_data$Cases, start = as.numeric(min(time_series_data$Year)), frequency = 1)
    
    # Fit ARIMA model
    model <- auto.arima(ts_data)
    
    # Forecast next 5 years
    forecast_data <- forecast(model, h = 5)
    
    # Plot
    plot(forecast_data, main = "Cholera Cases Forecast (ARIMA Model)")
  })
  
  # Data Table
  output$data_table <- renderDT({
    datatable(data)
  })
}

# Run App
shinyApp(ui = ui, server = server)
