library(shinydashboard)
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
library(shinyWidgets)
library(forecast)
library(tidyverse)
library(rsconnect)
library(selectr)
library(shinythemes)
library(fontawesome)
library(ggthemes)


# Load dataset
df <- read.csv("Data/1chorela_cases_dataset.csv")
df <- df %>% distinct()

# Fix Date format
df$Date.Of.Onset <- as.Date(df$Date.Of.Onset, format="%d/%m/%Y")
df$Year_Onset <- year(df$Date.Of.Onset)

# Filter for years 2008 to 2024
df <- df %>% filter(Year_Onset >= 2008, Year_Onset <= 2024)

# Standardize County
df <- df %>%
  mutate(County = case_when(
    tolower(County) %in% c("murang'a", "muranga") ~ "Muranga",
    tolower(County) %in% c("kajiando", "kajiado") ~ "Kajiado",
    tolower(County) %in% c("trans nzoia", "transnzoia") ~ "Trans-Nzoia",
    tolower(County) %in% c("homa bay", "homabay") ~ "Homa-Bay",
    tolower(County) %in% c("elgeyo marakwet", "elgeyo-marakwet") ~ "Elgeyo-Marakwet",
    tolower(County) %in% c("tharaka nithi", "tharaka-nithi") ~ "Tharaka-Nithi",
    tolower(County) %in% c("tanariver", "tana river") ~ "Tana-River",
    TRUE ~ County
  )) %>%
  filter(!is.na(County))

# UI
# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Cholera Outbreak Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Bar Plot", tabName = "Bar_Plot", icon = icon("chart-line")),
      menuItem("Cases by Year", tabName = "cases_year", icon = icon("calendar")),
      menuItem("Cases by County", tabName = "cases_county", icon = icon("map-marked-alt")),
      menuItem("Age Distribution", tabName = "age_dist", icon = icon("user")),
      menuItem("Gender Distribution", tabName = "gender_dist", icon = icon("venus-mars")),
      menuItem("Forecasting", tabName = "forecast", icon = icon("chart-bar")),
      menuItem("Cholera Map", tabName = "map", icon = icon("globe-africa"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBox("52,320", subtitle = "Total Number Of Cases", icon = icon("virus"), color = "red"),
                valueBox("7,534 Nairobi", subtitle = "Most Affected County", icon = icon("map-marked-alt"), color = "orange"),
                valueBox("2 Nyeri", subtitle = "Least Affected County", icon = icon("map-pin"), color = "green")
              ),
              fluidRow(
                valueBox("1,046.4", subtitle = "Average Cases per County", icon = icon("balance-scale"), color = "yellow"),
                valueBox("2023, March", subtitle = "Peak Month", icon = icon("calendar-alt"), color = "maroon"),
                valueBox("42", subtitle = "Number of Counties Affected", icon = icon("globe-africa"), color = "fuchsia")
              ),
              fluidRow(
                box(
                  title = span(icon("clipboard-list"), " Analysis Summary"),
                  width = 12,
                  solidHeader = TRUE,
                  background = "black",
                  HTML('
      <div style="font-size:16px; line-height:1.7; color: white;">
        <p><i class="fas fa-info-circle" style="color:#00aced;"></i> <strong style="color:#00aced;">Overview:</strong> This dashboard gives a <span style="color:#1E90FF;">comprehensive view</span> of Cholera outbreaks in <strong>Kenya</strong> from <strong>2008 to 2024</strong>.</p>
        
        <p><i class="fas fa-lightbulb" style="color:#32CD32;"></i> <strong style="color:#32CD32;">Key Insights:</strong> Discover trends, the <span style="color:#FF4500;"><strong>most affected</strong></span> and <span style="color:#00FF7F;"><strong>least affected</strong></span> counties, plus demographic breakdowns by <span style="color:#DA70D6;">age</span> and <span style="color:#FF69B4;">gender</span>.</p>
        
        <p><i class="fas fa-chart-bar" style="color:#FFA500;"></i> <strong style="color:#FFA500;">Visual Tools:</strong> Explore <strong>epidemic curves</strong>, <strong>forecast models</strong>, and a <strong>dynamic map</strong> to trace outbreak progression in time and space.</p>
        
        <p><i class="fas fa-compass" style="color:#BA55D3;"></i> <strong style="color:#BA55D3;">How to Use:</strong> Use the <strong>sidebar menu</strong> to navigate between sections and interact with the data for <span style="color:#87CEEB;">informed insights</span>.</p>
      </div>
    ')
                )
              )
              
              
              
      ),
      tabItem(tabName = "Bar_Plot",
              fluidRow(
                box(title = "Cholera Cases per Year", width = 12, plotOutput("bar_plot", height = "70vh"))
              )
      ),
      tabItem(tabName = "cases_year",
              fluidRow(
                box(title = "Cases by Year", width = 12, plotOutput("cases_by_year_plot", height = "70vh"))
              )
      ),
      tabItem(tabName = "cases_county",
              fluidRow(
                box(title = "Cases by County", width = 12, plotOutput("cases_by_county", height = "70vh"))
              )
      ),
      tabItem(tabName = "age_dist",
              fluidRow(
                box(title = "Age Distribution of Cases", width = 12, plotOutput("age_dist", height = "70vh"))
              )
      ),
      tabItem(tabName = "gender_dist",
              fluidRow(
                box(title = "Gender Distribution of Cases", width = 12, plotOutput("gender_dist", height = "70vh"))
              )
      ),
      tabItem(tabName = "forecast",
              fluidRow(
                box(title = "Forecasting Cholera Cases", width = 12, plotOutput("forecast_plot", height = "70vh"))
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Interactive Cholera Map", width = 12, leafletOutput("cholera_map", height = "80vh"))
              )
      )
    )
  )
)


# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    df %>%
      mutate(Sex = case_when(
        tolower(Sex) %in% c("male", "m", "masculine", "male ") ~ "Male",
        tolower(Sex) %in% c("female", "f", "feminine", "female ") ~ "Female",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Sex))
  })
  
  yearly_cases <- reactive({
    filtered_data() %>%
      group_by(Year_Onset) %>%
      summarise(Cases = n(), .groups = "drop")
  })
  
  output$overview_plot <- renderPlot({
    county_cases <- filtered_data() %>%
      count(County, sort = TRUE)
    
    ggplot(county_cases, aes(x = reorder(County, -n), y = n, fill = County)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Cholera Cases by County",
           x = "County", y = "Cases") +
      theme_minimal()
  })
  
  output$bar_plot <- renderPlot({
    filtered_data() %>%
      mutate(Year = year(Date.Of.Onset)) %>%
      count(Year) %>%
      ggplot(aes(y = factor(Year), x = n)) +
      geom_col(fill = "steelblue") +
      labs(
        title = "Cholera Cases per Year",
        x = "Number of Cases",
        y = "Year"
      ) +
      theme_minimal()
  })
  
  
  output$cases_by_year_plot <- renderPlot({
    yearly_data <- yearly_cases()
    
    # Ensure all years are present, even if there are no cases in those years
    all_years <- seq(min(yearly_data$Year_Onset), max(yearly_data$Year_Onset))
    complete_data <- data.frame(Year_Onset = all_years)
    yearly_data <- left_join(complete_data, yearly_data, by = "Year_Onset")
    yearly_data$Cases[is.na(yearly_data$Cases)] <- 0  # Assign 0 for missing cases
    
    ggplot(yearly_data, aes(x = Year_Onset, y = Cases)) +
      geom_line(group = 1, color = "darkred", size = 1.2) +
      geom_point(size = 4, color = "darkblue", shape = 21, fill = "white") +
      scale_x_continuous(breaks = all_years) +  # Ensure all years are included in the axis
      labs(title = "Cholera Cases by Year", x = "Year", y = "Cases") +
      theme_minimal() +  # Clean minimal theme
      theme(
        plot.title = element_text(family = "Arial", size = 16, face = "bold", color = "darkblue", hjust = 0.5),
        plot.background = element_rect(fill = "#f2f2f2", color = NA),  # Light grey background
        panel.background = element_rect(fill = "white", color = "gray"),
        panel.grid.major = element_line(color = "lightgray", size = 0.5),
        panel.grid.minor = element_line(color = "lightgray", size = 0.2),
        axis.title = element_text(size = 12, face = "bold", color = "darkblue"),
        axis.text = element_text(size = 10, color = "black"),
        axis.ticks = element_line(color = "darkgray")
      ) +
      theme(legend.position = "none")  
  })
  
  
  
  output$cases_by_county <- renderPlot({
    county_data <- filtered_data() %>%
      count(County, sort = TRUE)
    
    ggplot(county_data, aes(x = reorder(County, -n), y = n, fill = County)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Cases by County", x = "County", y = "Cases") +
      theme_minimal()
  })
  
  output$age_dist <- renderPlot({
    ggplot(filtered_data(), aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "purple", color = "white") +
      labs(title = "Age Distribution", x = "Age", y = "Count") +
      theme_minimal()
  })
  
  output$gender_dist <- renderPlot({
    gender_data <- filtered_data() %>%
      count(Sex)
    
    ggplot(gender_data, aes(x = Sex, y = n, fill = Sex)) +
      geom_bar(stat = "identity") +
      labs(title = "Gender Distribution", x = "Gender", y = "Cases") +
      theme_minimal()
  })
  
  output$forecast_plot <- renderPlot({
    ts_cases <- ts(yearly_cases()$Cases, start = min(yearly_cases()$Year_Onset), frequency = 1)
    model <- auto.arima(ts_cases)
    forecasted <- forecast(model, h = 5)
    
    autoplot(forecasted) +
      labs(title = "Forecasted Cholera Cases", x = "Year", y = "Cases")
  })
  
  output$cholera_map <- renderLeaflet({
    counties <- st_read("Data/County_Shape/County.shp")
    
    county_cases <- filtered_data() %>%
      count(County)
    
    counties <- counties %>%
      rename(County = COUNTY) %>%
      left_join(county_cases, by = "County") %>%
      mutate(n = ifelse(is.na(n), 0, n))
    
    pal <- colorBin("YlOrRd", domain = counties$n, bins = c(0, 10, 50, 100, 200, 500, max(counties$n, na.rm = TRUE)))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Cholera Cases: %g",
      counties$County, counties$n
    ) %>% lapply(HTML)
    
    leaflet(data = counties) %>%
      setView(lat = -0.0236, lng = 37.9062, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(n),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Cholera Cases", position = "bottomleft")
  })
}

# Run App
shinyApp(ui = ui, server = server)
