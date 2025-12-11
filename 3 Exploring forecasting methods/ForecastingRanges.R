library(shiny)
library(dplyr)
library(tsibble)
library(fpp3)
library(readr)
library(tidyr)
library(ggplot2)
library(zoo)
library(bslib) 

# ==============================================================================
# UI: Simplified Interface
# ==============================================================================
ui <- page_sidebar(
  sidebar = sidebar(
    # SIMPLIFIED: No file input, just the controls
    selectInput("country", "Country:", choices = NULL),
    
    radioButtons("type", "Type:", 
                 choices = c("Imports", "Exports"), 
                 inline = TRUE),
    
    selectInput("model_type", "Model:", 
                choices = c("ETS", "ARIMA", "Mean" = "MEAN")),
    
    # Compact sliders
    sliderInput("train_year", "Train Until:",
                min = 1990, max = 2025, value = 2010, step = 1,
                sep = ""),
    
    sliderInput("horizon", "Years Ahead:",
                min = 1, max = 20, value = 10)
  ),
  
  # Main Display Area
  card(
    card_header("Forecast"),
    plotOutput("forecastPlot", height = "450px")
  ),
  
  card(
    card_header("Accuracy"),
    tableOutput("accuracyTable")
  )
)

# ==============================================================================
# SERVER: Logic
# ==============================================================================
server <- function(input, output, session) {
  
  dataset <- reactive({
    df <- read.csv("prescriptive data/Final_Country_Waste_Analysis copy.csv", header=TRUE) %>%
      select(Country, Year, Imports, Exports) %>%
      mutate(
        Imports = Imports * 1e9, 
        Exports = Exports * 1e9 
      ) %>%
      as_tsibble(key = Country, index = Year) %>%
      fill_gaps() %>%
      mutate(
        Imports = zoo::na.approx(Imports, rule = 2),
        Exports = zoo::na.approx(Exports, rule = 2)
      ) %>%
      pivot_longer(cols = c(Imports, Exports), 
                   names_to = "Trade_Type", 
                   values_to = "Amount")
    
    return(df)
  })
  
  observeEvent(dataset(), {
    countries <- unique(dataset()$Country)
    updateSelectInput(session, "country", choices = countries, selected = countries[1])
    
    # Dynamic slider range
    min_y <- min(dataset()$Year)
    max_y <- max(dataset()$Year)
    # Changed max_y - 1 to max_y to allow full range selection
    updateSliderInput(session, "train_year", min = min_y, max = max_y, value = median(c(min_y, max_y)))
  })
  
  # --- Filter Data ---
  country_data <- reactive({
    req(dataset(), input$country)
    dataset() %>%
      filter(Country == input$country, Trade_Type == input$type)
  })
  
  # --- Fit & Forecast ---
  forecast_data <- reactive({
    req(country_data())
    
    cutoff <- input$train_year
    train_set <- country_data() %>% filter(Year <= cutoff)
    
    m_def <- switch(input$model_type,
                    "ETS" = ETS(Amount),
                    "ARIMA" = ARIMA(Amount),
                    "MEAN" = MEAN(Amount))
    
    fit <- train_set %>% model(Selected_Model = m_def)
    fc <- fit %>% forecast(h = input$horizon)
    
    return(list(fit = fit, fc = fc, train = train_set))
  })
  
  # --- Plot ---
  output$forecastPlot <- renderPlot({
    req(forecast_data())
    
    fc_obj <- forecast_data()$fc
    train_data <- forecast_data()$train
    
    # --- Create Bridge Data to close the gap ---
    # We take the last point of history and the first point of forecast
    last_hist <- train_data %>% 
      as_tibble() %>% 
      filter(Year == max(Year)) %>% 
      select(Year, Amount)
    
    first_fc <- fc_obj %>% 
      as_tibble() %>% 
      filter(Year == min(Year)) %>% 
      mutate(Amount = .mean) %>% # Use the mean forecast value
      select(Year, Amount)
    
    bridge <- bind_rows(last_hist, first_fc)
    
    fc_obj %>%
      autoplot(train_data, level = c(80, 95)) +
      # Add the bridge line manually
      geom_line(data = bridge, aes(x = Year, y = Amount), color = "#4a8bad", size = 1) +
      geom_vline(xintercept = input$train_year, linetype = "dashed", color = "red") +
      annotate("text", x = input$train_year, y = Inf, label = "Cutoff", 
               vjust = 2, color = "red", angle = 90) +
      labs(title = paste(input$country, "-", input$type),
           y = "Amount (Kg)") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # --- Table ---
  output$accuracyTable <- renderTable({
    req(forecast_data())
    
    fc_obj <- forecast_data()$fc
    full_hist <- country_data()
    test_set <- full_hist %>% filter(Year > input$train_year)
    
    if(nrow(test_set) == 0) return(NULL)
    
    accuracy(fc_obj, full_hist) %>%
      select(.model, RMSE, CRPS)
  }, digits = 2)
}

shinyApp(ui = ui, server = server)