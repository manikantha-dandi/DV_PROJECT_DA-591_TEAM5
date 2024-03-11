
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)



# UI
ui <- fluidPage(
  titlePanel("AQI Trends in Selected Cities"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("cities", "Select Cities:",
                         choices = c("Ahmedabad", "Delhi", "Gurugram", "Lucknow", "Patna"),
                         selected = c("Ahmedabad", "Delhi")),
      selectInput("parameter", "Select Parameter:", 
                  choices = c("AQI"),
                  selected = "AQI")
      
    ),
    mainPanel(
      plotlyOutput("line_plot"),
      verbatimTextOutput("line_plot_analysis"),
      plotOutput("top_cities_plot"),
      verbatimTextOutput("top_cities_plot_analysis"),
      plotOutput("top_cities_lowest_plot"),
      verbatimTextOutput("top_cities_lowest_plot_analysis"),
      plotOutput("delhi_aqi_pie_chart"),
      verbatimTextOutput("delhi_aqi_pie_chart_analysis"),
      plotOutput("plot1"),  
      verbatimTextOutput("plot1_analysis"),
      plotOutput("pm25_vs_pm10"),
      verbatimTextOutput("pm25_vs_pm10_analysis"),
      plotlyOutput("box_plot"),
      verbatimTextOutput("box_plot_analysis"),
      plotOutput("o3_distribution"),
      verbatimTextOutput("o3_distribution_analysis")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load dataset
  data <- reactive({
    read.csv("D:/02nd sem/data_visualization/city_day.csv", header = TRUE) %>%
      na.omit()  # Remove NA values
  })
  
  # Generate plot for top 5 cities with highest AQI
  output$top_cities_plot <- renderPlot({
    city_aqi <- data() %>%
      group_by(City) %>%
      summarise(mean_AQI = mean(AQI, na.rm = TRUE)) %>%
      arrange(desc(mean_AQI)) %>%
      slice_head(n = 5)
    
    ggplot(city_aqi, aes(x = reorder(City, -mean_AQI), y = mean_AQI, fill = City)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 5 Cities with Highest AQI",
           x = "City",
           y = "Mean AQI") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$top_cities_plot_analysis <- renderText({
    " \n\n This plot showcases the top 5 cities with the highest AQI levels. Notably, Delhi stands out 
    with the highest mean AQI value, highlighting significant air quality concerns in the region. 
    Following closely is Patna, ranking second-highest in mean AQI, indicating persistent pollution 
    challenges. Interestingly, Vishakhapatnam secures the fifth-highest mean AQI value, signaling 
    varying degrees of air pollution across regions, which can stem from diverse factors like 
    industrial activities, vehicular emissions, and geographical feature."
  })
  
  # Generate plot for top 4 cities with lowest AQI
  output$top_cities_lowest_plot <- renderPlot({
    city_aqi <- data() %>%
      group_by(City) %>%
      summarise(mean_AQI = mean(AQI)) %>%
      arrange(mean_AQI) %>%
      slice_head(n = 4)
    
    ggplot(city_aqi, aes(x = reorder(City, mean_AQI), y = mean_AQI, fill = City)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 4 Cities with Lowest AQI",
           x = "City",
           y = "Mean AQI") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$top_cities_lowest_plot_analysis <- renderText({
    "This plot illustrates the top 4 cities with the lowest AQI. Chandigarh, Amaravati, Hyderabad, 
     and Kolkata exhibit the lowest mean AQI values among all cities. Additionally, 
    it's worth noting that these cities consistently demonstrate relatively cleaner air quality 
    compared to others in the dataset."
  })
  
  # Generate pie chart for AQI bucket distribution in Delhi
  output$delhi_aqi_pie_chart <- renderPlot({
    delhi_data <- data() %>%
      filter(City == "Delhi")
    
    aqi_bucket_counts <- table(delhi_data$AQI_Bucket)
    
    pie(aqi_bucket_counts,
        main = "AQI Bucket Distribution in Delhi",
        col = rainbow(length(aqi_bucket_counts)),
        labels = paste(names(aqi_bucket_counts), ": ", aqi_bucket_counts))
  })
  
  output$delhi_aqi_pie_chart_analysis <- renderText({
    "This pie chart visualizes the distribution of AQI buckets in Delhi. The data reveals that out of 
    924 total days observed, 360 days fall under the moderate category, indicating a moderate level of 
    air pollution. Additionally, 315 days are categorized as very poor, highlighting significant air 
    quality challenges. Furthermore, 328 days are classified as poor, indicating further deterioration 
    in air quality. Moreover, 117 days fall under the severe category, signifying a critical level of 
    pollution that poses health risks. Lastly, 104 days are marked as satisfactory, suggesting relatively 
    cleaner air quality conditions on these days."
  })
  
  # Plot AQI records per city
  output$plot1 <- renderPlot({
    city_counts <- data() %>%
      count(City)
    
    ggplot(city_counts, aes(x = n, y = reorder(City, n))) +
      geom_bar(stat = "identity", fill = "#30a2da") +
      labs(x = "Number of Records", y = "Name of City", title = "AQI Records per City") +
      theme_minimal()
  })
  
  
  # Prepare data for selected cities
  selected_data <- reactive({
    aqi_data <- data.frame(
      City = rep(c("Ahmedabad", "Delhi", "Gurugram", "Lucknow", "Patna"), each = 6),
      Year = rep(2015:2020, 5),
      AQI = sample(50:500, 30, replace = TRUE) # Sample AQI values
    )
    
    aqi_data %>%
      filter(City %in% input$cities)
  })
  
  output$plot1_analysis <- renderText({
    "The graph illustrates the number of AQI records per city. Among the cities included in the graph, 
    Hyderabad, Delhi, Vishakhapatnam, Amaravati, Amritsar, Kolkata, Chandigarh, Patna, and Gurugram 
    exhibit the highest number of AQI records, presented in descending order. Notably, Hyderabad emerges 
    as the city with the highest number of records, indicating a significant amount of data available 
    for air quality monitoring in Hyderabad compared to other cities in the dataset."
  })
  
  # Prepare line plot
  output$line_plot <- renderPlotly({
    fig <- selected_data() %>%
      plot_ly(x = ~Year, y = ~AQI, color = ~City, mode = 'lines+markers') %>%
      layout(
        title = "AQI Trends in Selected Cities",
        xaxis = list(title = "Year"),
        yaxis = list(title = "AQI")
      )
    
    fig
  })
  
  # Plot PM2.5 vs PM10
  output$pm25_vs_pm10 <- renderPlot({
    cleaned <- data()
    ggplot(cleaned, aes(x = PM2.5, y = PM10)) +
      geom_point() +
      labs(title = "PM2.5 vs PM10", x = "PM2.5", y = "PM10")
  })
  
  # Data analysis text for PM2.5 vs PM10 plot
  output$pm25_vs_pm10_analysis <- renderText({
    "This scatter plot illustrates the relationship between PM2.5 and PM10 concentrations. 
    The data suggests a notable interaction between PM2.5 concentrations ranging from 0 to 250 µg/m³ 
    and PM10 concentrations ranging from 0 to 375 µg/m³. This correlation hints at potential sources 
    or factors influencing particulate matter concentrations within this range, such as combustion 
    processes, industrial emissions, or local environmental conditions."
  })
  
  
  # Generate box plot
  output$box_plot <- renderPlotly({
    fig <- selected_data() %>%
      plot_ly(x = ~City, y = ~get(input$parameter), type = "box")
    
    fig
  })
  
  # Box plot analysis
  output$box_plot_analysis <- renderText({
    "This box plot visualizes the distribution of the selected parameter AQI 
    across the selected cities. Outliers, if present, can be observed outside the whiskers 
    extending from the box. Analyzing the box plot can provide insights into the variability 
    and spread of the parameter values among the selected cities."
  })
  
  # Plot O3 distribution
  output$o3_distribution <- renderPlot({
    cleaned <- data()
    ggplot(cleaned, aes(x = O3)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "O3 Distribution", x = "O3", y = "Frequency")
  })
  
  # Data analysis text for O3 distribution plot
  output$o3_distribution_analysis <- renderText({
    "This histogram depicts the distribution of O3 (Ozone) levels. The data indicates that O3 levels 
    ranging from 0 to 50 ppb (parts per billion) are prevalent, with a higher frequency observed 
    in this range. Conversely, O3 levels between 50 to 100 ppb exhibit a comparatively lower 
    occurrence, suggesting a reduction in concentration. Furthermore, O3 levels ranging from 
    100 to 150 ppb are notably scarce, indicating minimal occurrences within this concentration range."
  })
  
}



# Shiny App
shinyApp(ui, server)