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
# SECTION 1: DATA PROCESSING & IMPUTATION
# ==============================================================================
message("Starting Data Imputation...")

# 1. Load the Raw Data
raw_data <- read.csv("Core data/FOR FORECASTING.csv", stringsAsFactors = FALSE)

# 2. Reshape to "Long" format for the imputation logic
imports <- raw_data %>%
  select(Country, Year, Qty = Imports, PrimaryValue = Import_Value) %>%
  mutate(flowDesc = "Import")

exports <- raw_data %>%
  select(Country, Year, Qty = Exports, PrimaryValue = Export_Value) %>%
  mutate(flowDesc = "Export")

trade <- bind_rows(imports, exports)
trade$Year <- as.integer(trade$Year)

# 3. Impute Missing Quantities
price_ref <- trade %>%
  filter(Qty > 0, PrimaryValue > 0) %>%
  mutate(unit_price = PrimaryValue / Qty) %>%
  group_by(flowDesc) %>%
  summarise(global_unit_price = median(unit_price, na.rm = TRUE), .groups = "drop")

df_imputed <- trade %>%
  left_join(price_ref, by = "flowDesc") %>%
  group_by(Country, flowDesc) %>%
  arrange(Year) %>%
  mutate(
    # Price Calculation & Interpolation
    unit_price_raw = if_else(Qty > 0 & PrimaryValue > 0, PrimaryValue / Qty, NA_real_),
    unit_price_interp = if (sum(!is.na(unit_price_raw)) >= 2) {
      zoo::na.approx(unit_price_raw, x = Year, na.rm = FALSE)
    } else { unit_price_raw },
    unit_price_interp = zoo::na.locf(unit_price_interp, na.rm = FALSE),
    unit_price_interp = zoo::na.locf(unit_price_interp, fromLast = TRUE, na.rm = FALSE),
    unit_price_final = coalesce(unit_price_interp, global_unit_price),
    
    # Fill Quantity
    Qty_filled = if_else(
      (is.na(Qty) | (Qty == 0 & PrimaryValue > 0)) & !is.na(unit_price_final) & unit_price_final > 0,
      PrimaryValue / unit_price_final,
      Qty
    )
  ) %>%
  ungroup()

# 4. Reconstruct Final Dataset
df_imputed_wide <- df_imputed %>%
  select(Country, Year, flowDesc, Qty_filled, PrimaryValue) %>%
  pivot_wider(names_from = flowDesc, values_from = c(Qty_filled, PrimaryValue)) %>%
  rename(Imports = Qty_filled_Import, Exports = Qty_filled_Export,
         Import_Value = PrimaryValue_Import, Export_Value = PrimaryValue_Export)

final_dataset <- raw_data %>%
  select(-Imports, -Exports, -Import_Value, -Export_Value) %>%
  left_join(df_imputed_wide, by = c("Country", "Year")) %>%
  mutate(
    Imports = replace_na(Imports, 0),
    Exports = replace_na(Exports, 0),
    
    # --- CONVERSION TO KG (1 Million Tonnes -> Kg) ---
    Est_Recycled_Amount_Kg = Est_Recycled_Amount * 1e9,
    Est_Waste_Produced_Kg = Est_Waste_Produced * 1e9,
    Exports_Kg = Exports * 1e9,
    Imports_Kg = Imports * 1e9,
    
    # --- PRESCRIPTIVE VARIABLES ---
    Processed_Own_Waste = Est_Recycled_Amount_Kg - Exports_Kg,
    Processed_Others_Waste = Imports_Kg,
    Percent_Own_Processed = (Processed_Own_Waste / Est_Waste_Produced_Kg) * 100
  )

message("Data Imputation Complete. Launching App...")

# ==============================================================================
# SECTION 2: SHINY APP
# ==============================================================================

ui <- page_sidebar(
  title = "Prescriptive Waste Analysis (Kg)",
  sidebar = sidebar(
    selectInput("country", "Country:", choices = NULL),
    
    selectInput("type", "Variable to Forecast:", 
                choices = c(
                  "Processed Own Waste (Kg)" = "Processed_Own_Waste",
                  "Processed Others Waste (Kg)" = "Processed_Others_Waste",
                  "% Own Waste Processed" = "Percent_Own_Processed"
                )),
    
    selectInput("model_type", "Model:", 
                choices = c("ARIMA", "ETS", "Mean" = "MEAN"), 
                selected = "ARIMA"),
    
    # UPDATED: Range Slider for Start and End Year
    sliderInput("train_range", "Training Data Range:",
                min = 1990, max = 2020, 
                value = c(2000, 2015), # Default: Start 2000, End 2015
                step = 1, sep = ""),
    
    sliderInput("horizon", "Years Ahead:",
                min = 1, max = 20, value = 10)
  ),
  
  card(
    card_header("Forecast Analysis"),
    plotOutput("forecastPlot", height = "450px")
  ),
  
  card(
    card_header("Model Accuracy"),
    tableOutput("accuracyTable")
  )
)

server <- function(input, output, session) {
  
  # --- 1. Load Data ---
  dataset <- reactive({
    final_dataset %>%
      select(Country, Year, 
             Processed_Own_Waste, 
             Processed_Others_Waste, 
             Percent_Own_Processed) %>%
      pivot_longer(
        cols = c(Processed_Own_Waste, Processed_Others_Waste, Percent_Own_Processed),
        names_to = "Type",
        values_to = "Amount"
      ) %>%
      as_tsibble(key = c(Country, Type), index = Year) %>%
      fill_gaps()
  })
  
  # --- 2. Update Inputs ---
  observe({
    req(dataset())
    countries <- unique(dataset()$Country)
    updateSelectInput(session, "country", choices = countries, selected = "Spain")
    
    min_y <- min(dataset()$Year)
    max_y <- max(dataset()$Year)
    
    # Update the Range Slider
    updateSliderInput(session, "train_range", 
                      min = min_y, max = max_y - 1, 
                      value = c(min_y + 5, 2015)) 
  })
  
  # --- 3. Filter Data ---
  country_data <- reactive({
    req(dataset(), input$country, input$type)
    dataset() %>%
      filter(Country == input$country, Type == input$type)
  })
  
  # --- 4. Fit & Forecast ---
  forecast_data <- reactive({
    req(country_data())
    
    # UPDATED: Use both Start and End of the slider range
    start_year <- input$train_range[1]
    cutoff_year <- input$train_range[2]
    
    train_set <- country_data() %>% 
      filter(Year >= start_year & Year <= cutoff_year)
    
    m_def <- switch(input$model_type,
                    "ETS" = ETS(Amount),
                    "ARIMA" = ARIMA(Amount),
                    "MEAN" = MEAN(Amount))
    
    fit <- train_set %>% model(Selected_Model = m_def)
    fc <- fit %>% forecast(h = input$horizon)
    
    list(fc = fc, train = train_set, model = fit)
  })
  
  # --- 5. Plot ---
  output$forecastPlot <- renderPlot({
    req(forecast_data())
    
    fc_obj <- forecast_data()$fc
    train_data <- forecast_data()$train
    cutoff_year <- input$train_range[2]
    
    # Bridge Data
    last_hist <- train_data %>% 
      as_tibble() %>% 
      filter(Year == max(Year)) %>% 
      select(Year, Amount)
    
    first_fc <- fc_obj %>% 
      as_tibble() %>% 
      filter(Year == min(Year)) %>% 
      mutate(Amount = .mean) %>% 
      select(Year, Amount)
    
    bridge <- bind_rows(last_hist, first_fc)
    
    # Dynamic Y-Axis Label
    y_label <- if(grepl("Percent", input$type)) "Percentage (%)" else "Amount (Kg)"
    
    fc_obj %>%
      autoplot(train_data, level = c(80, 95)) +
      geom_line(data = bridge, aes(x = Year, y = Amount), color = "#4a8bad", size = 1) +
      geom_vline(xintercept = cutoff_year, linetype = "dashed", color = "red") +
      
      # UPDATED ANNOTATION: Fixed cut-off text
      annotate("text", x = cutoff_year, y = Inf, label = "Train Cutoff", 
               vjust = 1.5, hjust = 1.1, color = "red", angle = 90) +
      
      labs(title = paste("Forecast:", input$type, "-", input$country),
           subtitle = paste("Training Range:", input$train_range[1], "-", input$train_range[2]),
           y = y_label) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()))
  })
  
  # --- 6. Accuracy ---
  output$accuracyTable <- renderTable({
    req(forecast_data())
    full_data <- country_data()
    fc_obj <- forecast_data()$fc
    accuracy(fc_obj, full_data) %>%
      select(.model, RMSE, MAE, MAPE)
  }, digits = 2)
}

shinyApp(ui = ui, server = server)