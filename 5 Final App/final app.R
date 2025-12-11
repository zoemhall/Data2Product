# R SHINY APP for Interactive Global Trade Map Visualization
# To run this app:
# 1. Save this code as 'app.R' in a folder.
# 2. Ensure you have the required packages installed: install.packages(c("shiny", "dplyr", "leaflet", "sf", "rworldmap", "shinythemes", "ggplot2", "tidyr", "countrycode", "zoo", "tsibble", "fpp3"))
# 3. Open the file in RStudio and click 'Run App'.

library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(rworldmap)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(countrycode)
library(zoo) # for na.approx / na.locf

# --- FORECASTING LIBRARIES ADDED ---
library(tsibble)
library(fpp3)
# -----------------------------------

# ---- DATA LOADING AND PREPARATION --------------------------------

# NOTE: Paths use "Core data/" as per user's current code structure for existing files.
trade <- read.csv("Core data/UN_Trimmed_1988-2024.csv", stringsAsFactors = FALSE)
trade$Year <- as.integer(trade$Year)

# PRESERVE RAW COLUMNS AND RENAME FOR IMPUTATION
trade <- trade %>% 
  rename(Qty_raw = Qty, PrimaryValue_raw = PrimaryValue)

# 1) Global typical prices per flow (Import / Export)
price_ref <- trade %>%
  filter(!is.na(Qty_raw), Qty_raw > 0,
         !is.na(PrimaryValue_raw), PrimaryValue_raw > 0) %>%
  mutate(unit_price = PrimaryValue_raw / Qty_raw) %>%
  group_by(flowDesc) %>%
  summarise(global_unit_price = median(unit_price, na.rm = TRUE),
            .groups = "drop")

# 2) Country+flow specific price series + imputation
trade <- trade %>%
  left_join(price_ref, by = "flowDesc") %>%
  group_by(CountryCode, flowDesc) %>%
  arrange(Year, .by_group = TRUE) %>%
  
  # observed price only where both reported
  mutate(
    unit_price_raw = if_else(
      !is.na(Qty_raw) & Qty_raw > 0 &
        !is.na(PrimaryValue_raw) & PrimaryValue_raw > 0,
      PrimaryValue_raw / Qty_raw,
      NA_real_
    )
  ) %>%
  
  # interpolate through time (within this country+flow)
  mutate(
    unit_price_interp = zoo::na.approx(unit_price_raw, x = Year, na.rm = FALSE),
    unit_price_interp = zoo::na.locf(unit_price_interp, na.rm = FALSE),
    unit_price_interp = zoo::na.locf(unit_price_interp, fromLast = TRUE, na.rm = FALSE),
    
    # final price: prefer interpolated; otherwise fall back to global
    unit_price_final = dplyr::case_when(
      !is.na(unit_price_interp) ~ unit_price_interp,
      !is.na(global_unit_price) ~ global_unit_price,
      TRUE              ~ NA_real_
    )
  ) %>%
  
  # back-calculate missing Qty / PrimaryValue - store in imputed columns
  mutate(
    Qty_imputed = if_else(
      # IMPUTATION CONDITION:
      # Impute if Qty is NA OR (Qty is 0 and PrimaryValue is non-zero)
      (is.na(Qty_raw) | (Qty_raw == 0 & !is.na(PrimaryValue_raw) & PrimaryValue_raw > 0)) &
        !is.na(PrimaryValue_raw) &
        !is.na(unit_price_final) & unit_price_final > 0,
      PrimaryValue_raw / unit_price_final,
      Qty_raw # Use original Qty_raw if not imputed
    ),
    PrimaryValue_imputed = if_else(
      is.na(PrimaryValue_raw) &
        !is.na(Qty_imputed) &
        !is.na(unit_price_final),
      Qty_imputed * unit_price_final,
      PrimaryValue_raw # Use original PrimaryValue_raw if not imputed
    ),
    Qty_imputed_flag = (is.na(Qty_raw) | Qty_raw == 0) & !is.na(Qty_imputed),
    Value_imputed_flag = is.na(PrimaryValue_raw) & !is.na(PrimaryValue_imputed)
  ) %>%
  ungroup()

# DO NOT overwrite the columns used downstream. 
# trade$Qty and trade$PrimaryValue columns are no longer needed / used after this.

min_year <- min(trade$Year, na.rm = TRUE)
max_year <- max(trade$Year, na.rm = TRUE) # This is the max year for Raw Data Viz

# ---- WASTE DATA (NO PLACEHOLDER METRICS) ------------------------------------

# Waste-system metrics per country/year – values as-is (Gt)
waste <- read.csv("Core data/Final_Country_Waste_Analysis.csv", stringsAsFactors = FALSE)
waste$Year <- as.integer(waste$Year)

# Clean country names and create ISO3 codes for waste data
waste <- waste %>%
  mutate(
    Country_clean = dplyr::case_when(
      Country == "USA"             ~ "United States of America",
      Country == "Bosnia Herzegovina" ~ "Bosnia and Herzegovina",
      Country == "Central African Rep." ~ "Central African Republic",
      Country == "Dem. Rep. of the Congo" ~ "Democratic Republic of the Congo",
      Country == "Dominican Rep."         ~ "Dominican Republic",
      Country == "Rep. of Korea"          ~ "South Korea",
      Country == "Lao People's Dem. Rep." ~ "Laos",
      Country == "Russian Federation"  ~ "Russia",
      TRUE              ~ Country
    ),
    CountryCode = countrycode(
      Country_clean,
      origin      = "country.name",
      destination = "iso3c"
    )
  )

# Years available for waste production (where Est_Waste_Produced is not NA)
waste_years_available <- sort(unique(waste$Year[!is.na(waste$Est_Waste_Produced)]))

# Prescriptive analysis dataset: pre-computed sufficiency + industry value
forecast_data <- read.csv("Core data/Prescriptive_data.csv", stringsAsFactors = FALSE)
forecast_data$Year <- as.integer(forecast_data$Year)

forecast_data <- forecast_data %>%
  mutate(
    Country_clean = dplyr::case_when(
      Country == "USA"                   ~ "United States of America",
      Country == "Bosnia Herzegovina"    ~ "Bosnia and Herzegovina",
      Country == "Central African Rep."  ~ "Central African Republic",
      Country == "Dem. Rep. of the Congo" ~ "Democratic Republic of the Congo",
      Country == "Dominican Rep."        ~ "Dominican Republic",
      Country == "Rep. of Korea"         ~ "South Korea",
      Country == "Lao People's Dem. Rep."~ "Laos",
      Country == "Russian Federation"    ~ "Russia",
      TRUE                               ~ Country
    ),
    CountryCode = countrycode(
      Country_clean,
      origin      = "country.name",
      destination = "iso3c"
    )
  )

# Forecasting base dataset: includes imputed values for model fitting
forecast_base <- read.csv("Core data/Forecasting_data.csv", stringsAsFactors = FALSE)
forecast_base$Year <- as.integer(forecast_base$Year)

forecast_base <- forecast_base %>%
  mutate(
    Country_clean = dplyr::case_when(
      Country == "USA"                   ~ "United States of America",
      Country == "Bosnia Herzegovina"    ~ "Bosnia and Herzegovina",
      Country == "Central African Rep."  ~ "Central African Republic",
      Country == "Dem. Rep. of the Congo" ~ "Democratic Republic of the Congo",
      Country == "Dominican Rep."        ~ "Dominican Republic",
      Country == "Rep. of Korea"         ~ "South Korea",
      Country == "Lao People's Dem. Rep."~ "Laos",
      Country == "Russian Federation"    ~ "Russia",
      TRUE                               ~ Country
    ),
    CountryCode = countrycode(
      Country_clean,
      origin      = "country.name",
      destination = "iso3c"
    )
  )

# Define year range for PA slider (1990 to 2039 from initial inspection)
min_pa_year <- min(forecast_data$Year, na.rm = TRUE)
max_pa_year <- max(forecast_data$Year, na.rm = TRUE)

message("Prescriptive dataset loaded.")
message("Forecasting base dataset loaded.")

final_forecast_dataset <- forecast_base

# World map

sp_world <- getMap(resolution = "low")
world <- st_as_sf(sp_world)

world <- world %>%
  mutate(
    CountryCode = ISO3,
    name      = ADMIN
  )

# UI

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  title = "Global Plastic Waste Trade",
  
  tags$head(
    tags$style(HTML("
      /* --- GLOBAL LAYOUT & TITLES --- */
      .centered-title {
        text-align: center;
        margin-top: 20px;
        margin-bottom: 20px;
      }
      
      /* --- SIDEBAR PANEL STYLING (matches .well) --- */
      .well {
          border-radius: 15px !important; 
          border: 2px solid #ddd !important;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1); 
          background-color: #fff;
          padding: 25px; 
      }
      
      /* --- MAP CONTAINER STYLING (NO CHANGE) --- */
      .map-container {
          /* Matching the look of the .well sidebar */
          border-radius: 15px;
          border: 2px solid #ddd;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          background-color: #fff;
          padding: 15px; /* Padding for visual spacing around the map */
          height: 70vh; /* Match map height */
          overflow: hidden; /* Ensure rounded corners clip the map content */
      }
      
      #world_map {
          height: 100% !important;
      }

      #pa_map {
          height: 100% !important;
      }
      
      .sidebar-section-heading {
        font-weight: 700; 
        font-size: 1.5em; 
        color: #333;
        margin-top: 25px; 
        margin-bottom: 15px; 
        display: flex;
        align-items: center;
        padding-bottom: 5px;
      }
      .sidebar-section-heading span {
            margin-right: 10px;
            color: #007bff !important; 
      }
      
      hr.sidebar-divider {
            border-top: 1px solid #ddd;
            margin: 15px 0;
      }

      .irs--shiny .irs-bar {
            border-top: 1px solid #007bff;
            border-bottom: 1px solid #007bff;
            background: #007bff;
      }
      
      .irs--shiny .irs-handle {
            width: 10px; 
            height: 10px; 
            top: 25px;
            border-color: #007bff;
            background-color: #007bff;
            box-shadow: 0 0 0 1px #fff, 0 0 0 3px #007bff; 
      }
      
      .irs--shiny .irs-handle:hover {
            background-color: #0056b3;
            border-color: #0056b3;
      }

      .irs--shiny .irs-single,
      .irs--shiny .irs-from,
      .irs--shiny .irs-to {
            background-color: #007bff !important;
            color: #fff !important;
      }
      
      .irs--shiny .irs-single:before,
      .irs--shiny .irs-from:before,
      .irs--shiny .irs-to:before {
            border-top-color: #007bff !important;
      }
      
      .nav-tabs {
            border-bottom: none; 
      }
      .nav-tabs > li > a {
            border: 2px solid #007bff; 
            border-radius: 15px 15px 0 0;
            color: #007bff;
            background-color: #ffffff; 
            margin-right: 10px;
            transition: all 0.2s ease-in-out;
      }

      .nav-tabs > li > a:hover, 
      .nav-tabs > li > a:focus {
            color: #007bff; 
            border-color: #007bff; 
            background-color: #e6f0ff; 
      }
      
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:hover, 
      .nav-tabs > li.active > a:focus {
            color: #fff;
            cursor: default;
            background-color : #007bff; 
            border: 2px solid #007bff; 
            border-bottom-color: transparent; 
            border-radius: 15px 15px 0 0;
      }
      .tab-content {
        border-top: none;
      }

      #map_metric + .selectize-control .selectize-input,
      #pa_metric + .selectize-control .selectize-input,
      #forecast_type + .selectize-control .selectize-input,
      #forecast_model_type + .selectize-control .selectize-input,
      #forecast_country + .selectize-control .selectize-input {
            height: 38px;
            line-height: 20px;
            padding: 8px 12px;
            border-radius: 15px; 
            border: 1px solid #007bff; 
            box-shadow: 0 0 5px rgba(0, 123, 255, 0.2); 
      }
      
      .selectize-dropdown .option.selected {
            background-color: #007bff !important;
            color: #ffffff !important;
      }
      .selectize-dropdown .option.selected:hover {
            background-color: #007bff !important; 
      }
      
      .trade-button-container {
        display: flex; 
        gap: 10px; 
        margin-bottom: 15px;
      }
      
      .trade-button-container .btn {
            font-size: 0.9em;
            padding: 8px 18px; 
            border-radius: 20px; 
            flex-grow: 1; 
            max-width: 50%; 
            cursor: pointer; 
            transition: all 0.2s ease-in-out; 
      }

      .btn-unit-inactive {
            background-color: #ffffff; 
            color: #007bff; 
            border: 1px solid #007bff !important; 
            box-shadow: none !important;
      }
      
      .btn-unit-inactive:hover,
      .btn-unit-inactive:focus {
            background-color: #e6f0ff !important; 
            color: #007bff; 
            border-color: #007bff !important;
            box-shadow: 0 0 5px rgba(0, 123, 255, 0.5) !important;
            outline: none; 
      }

      .btn-unit-active {
            background-color: #007bff; 
            color: #ffffff !important; 
            border: 1px solid #007bff !important; 
            box-shadow: none !important;
      }

      .btn-unit-active:hover,
      .btn-unit-active:focus {
            background-color: #007bff !important; 
            border-color: #007bff!important;
            box-shadow: 0 0 5px rgba(0, 123, 255, 0.5) !important;
            outline: none; 
      }
      
      .modal-content {
            border-radius: 15px !important; 
            overflow: hidden;
            box-shadow: 0 5px 15px rgba(0,0,0,.5);
      }
      
      .modal-header {
            background-color: #007bff !important;
            color: #ffffff !important;
            border-bottom: 1px solid #007bff;
            border-top-left-radius: 15px;
            border-top-right-radius: 15px;
      }
      
      .modal-header .modal-title {
            color: #ffffff !important;
            font-weight: bold;
      }
      
      .modal-header .close {
            color: #ffffff !important;
            opacity: 0.8;
      }
      .modal-header .close:hover {
            opacity: 1;
      }
      
      .modal-footer {
            border-top: 1px solid #e5e5e5;
            border-radius: 0 0 15px 15px;
      }
      
      .btn-view-history {
            border-radius: 20px !important;
            font-weight: 600;
            padding: 8px 18px;
            transition: all 0.2s;
      }
      
      .modal-footer .btn {
            background-color: #007bff !important;
            color: #ffffff !important;
            border-color: #007bff !important;
            border-radius: 20px !important;
            box-shadow: none !important;
            font-weight: 600;
      }
      .modal-footer .btn:hover,
      .modal-footer .btn:focus {
            background-color: #0056b3 !important;
            border-color: #0056b3 !important;
            box-shadow: 0 0 5px rgba(0, 123, 255, 0.5) !important;
            outline: none;
      }
      
      "))
    ,
    
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update-active-class', function(message) {
        $('#' + message.inactive).removeClass('btn-unit-active').addClass('btn-unit-inactive');
        $('#' + message.active).removeClass('btn-unit-inactive').addClass('btn-unit-active');
      });
      "))
  ),
  
  tags$div(class = "centered-title",
           h1(HTML("<strong>Global Plastic Waste Trade</strong>")),
           p("Interactive analysis of global plastic waste flows, processing capacity and future trends. Navigate through the three tabs and adjust the left-panel inputs to reveal different insights and views.")
  ),
  
  tabsetPanel(
    id = "main_tabs",
    
    # --- Tab 1: Prescriptive ---
    tabPanel("Prescriptive",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 class = "well",
                 
                 tags$div(class = "sidebar-section-heading", HTML("<span style='color: #4682B4;'>&#x1f5fa;</span> Map Metric")), 
                 selectInput(
                   "pa_metric",
                   label = NULL,
                   choices = list(
                     "Percent Own Processed (%)" = "pa_sufficiency",
                     "Local Industry Value (USD)" = "pa_value"
                   ),
                   selected = "pa_sufficiency"
                 ),
                 
                 tags$hr(class = "sidebar-divider"),
                 
                 tags$div(class = "sidebar-section-heading", HTML("<span style='color: #FFD700;'>&#x23f3;</span> Select Year")), 
                 sliderInput(
                   "pa_year",
                   label = NULL,
                   min = min_pa_year,
                   max = max_pa_year,
                   value = max_year + 1, # first forecast year
                   step = 1,
                   sep = "",
                   animate = animationOptions(interval = 1000)
                 )
               ),
               
               mainPanel(
                 width = 9,
                 tags$div(class = "map-container",
                          leafletOutput("pa_map", height = "100%")
                 )
               )
             )
    ),
    
    # --- Tab 2: Forecasting ---
    tabPanel("Forecasting",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 class = "well",
                 
                 tags$div(class = "sidebar-section-heading", HTML("<span style='color: #4682B4;'>&#x1f30d;</span> Country")),
                 selectInput("forecast_country", label = NULL, choices = NULL),
                 
                 tags$hr(class = "sidebar-divider"),
                 
                 tags$div(class = "sidebar-section-heading", HTML("<span style='color: #FFD700;'>&#x1f4c8;</span> Forecast Variable")),
                 selectInput("forecast_type", label = NULL,
                             choices = c(
                               "Processed Own Waste (Kg)"    = "Processed_Own_Waste",
                               "Processed Others Waste (Kg)" = "Processed_Others_Waste",
                               "% Own Waste Processed"       = "Percent_Own_Processed"
                             )),
                 
                 tags$hr(class = "sidebar-divider"),
                 
                 tags$div(class = "sidebar-section-heading", HTML("<span style='color: #FFA500;'>&#x1f4c1;</span> Model & Range")),
                 selectInput("forecast_model_type", "Model:",
                             choices = c("ARIMA", "ETS", "Mean" = "MEAN"),
                             selected = "ARIMA"),
                 
                 sliderInput("forecast_train_range", "Training Data Range:",
                             min = 1990, max = 2020,
                             value = c(2000, 2015),
                             step = 1, sep = ""),
                 
                 tags$hr(class = "sidebar-divider"),
                 
                 tags$div(class = "sidebar-section-heading", HTML("<span style='color: #007bff;'>&#x23f0;</span> Horizon")),
                 sliderInput("forecast_horizon", "Years Ahead:",
                             min = 1, max = 20, value = 10)
               ),
               
               mainPanel(
                 width = 9,
                 
                 tags$div(class = "well", style = "margin-bottom: 20px;",
                          h3(HTML("<strong>Forecast Analysis</strong>")),
                          plotOutput("forecastPlot", height = "450px")
                 ),
                 
                 tags$div(class = "well",
                          h3(HTML("<strong>Model Accuracy</strong>")),
                          tableOutput("accuracyTable")
                 )
               )
             )
    ),
    
    # --- Tab 3: Raw Data Visualisation ---
    tabPanel(
      "Raw Data Visualisation",
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "well",
          
          tags$div(class = "sidebar-section-heading", HTML("<span style='color: #4682B4;'>&#x1f5fa;</span> Map Metric")), 
          selectInput(
            "map_metric",
            label = NULL,
            choices = list(
              "Waste produced (kg)"        = "waste_prod",
              "Waste recycled (kg)"        = "waste_recyc",
              "Waste imported (kg / USD)"  = "import",
              "Waste exported (kg / USD)"  = "export"
            ),
            selected = "export"
          ),
          
          tags$hr(class = "sidebar-divider"),
          
          tags$div(class = "sidebar-section-heading", HTML("<span style='color: #6C757D;'>&#x1f4c4;</span> Data Source")),
          
          tags$div(class = "trade-button-container",
                   actionButton("btn_data_raw",     "Original", class = "btn-unit-active"), 
                   actionButton("btn_data_imputed", "Imputed",  class = "btn-unit-inactive")
          ),
          
          uiOutput("raw_data_warning"), 
          
          tags$hr(class = "sidebar-divider"),
          
          tags$div(class = "sidebar-section-heading", HTML("<span style='color: #FFD700;'>&#x23f3;</span> Select Year")), 
          sliderInput(
            "year",
            label = NULL,
            min = min_year,
            max = max_year,
            value = 2019, 
            step = 1,
            sep = "",
            animate = animationOptions(interval = 1000)
          ),
          
          tags$hr(class = "sidebar-divider"),
          
          conditionalPanel(
            condition = "input.map_metric == 'import' || input.map_metric == 'export'",
            tags$div(class = "sidebar-section-heading", HTML("<span style='color: #FFA500;'>&#x1f4b0;</span> Trade Display Unit")), 
            
            tags$div(class = "trade-button-container",
                     actionButton("btn_qty", "Quantity (kg)", class = "btn-unit-active"), 
                     actionButton("btn_val", "Value (USD)",   class = "btn-unit-inactive")
            )
          )
        ),
        
        mainPanel(
          width = 9,
          tags$div(class = "map-container",
                   leafletOutput("world_map", height = "100%")
          )
        )
      )
    )
  )
)

# Server

server <- function(input, output, session) {
  
  clicked_country_iso <- reactiveVal(NULL)
  
  trade_display_unit <- reactiveVal("qty")
  data_source_unit   <- reactiveVal("raw")
  
  update_button_classes <- function(active_id, inactive_id) {
    session$sendCustomMessage(type = 'update-active-class', message = list(
      active   = active_id,
      inactive = inactive_id
    ))
  }
  
  observeEvent(input$btn_qty, {
    trade_display_unit("qty")
    update_button_classes("btn_qty", "btn_val")
  })
  
  observeEvent(input$btn_val, {
    trade_display_unit("val")
    update_button_classes("btn_val", "btn_qty")
  })
  
  observeEvent(input$btn_data_raw, {
    data_source_unit("raw")
    update_button_classes("btn_data_raw", "btn_data_imputed")
  })
  
  observeEvent(input$btn_data_imputed, {
    data_source_unit("imputed")
    update_button_classes("btn_data_imputed", "btn_data_raw")
  })
  
  observeEvent(input$map_metric, {
    req(input$map_metric == 'import' || input$map_metric == 'export')
    if (trade_display_unit() == "qty") {
      update_button_classes("btn_qty", "btn_val")
    } else {
      update_button_classes("btn_val", "btn_qty")
    }
    if (data_source_unit() == "raw") {
      update_button_classes("btn_data_raw", "btn_data_imputed")
    } else {
      update_button_classes("btn_data_imputed", "btn_data_raw")
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$map_metric, {
    if (input$map_metric == "waste_prod") {
      yrs <- waste_years_available
      current <- input$year
      if (is.null(current) || is.na(current)) current <- yrs[1]
      nearest <- yrs[which.min(abs(yrs - current))]
      updateSliderInput(
        session, "year",
        min = min(yrs), max = max(yrs),
        value = nearest, step = 1
      )
    } else {
      current <- input$year
      if (is.null(current) || is.na(current)) current <- max_year
      current <- max(min(current, max_year), min_year)
      updateSliderInput(
        session, "year",
        min = min_year, max = max_year,
        value = current, step = 1
      )
    }
  })
  
  output$raw_data_warning <- renderUI({
    if (data_source_unit() == "imputed") {
      tags$div(
        style = "margin-top: 20px; padding: 10px; border-radius: 5px; background-color: #ffe0e0; border: 1px solid #ff9999;",
        HTML("<b>Disclaimer:</b> The trade values displayed have been mathematically approximated (imputed) where original data was missing or zero. These are estimations!")
      )
    }
  })
  
  trade_year <- reactive({
    req(input$year)
    
    if (data_source_unit() == "raw") {
      qty_col <- "Qty_raw"
      val_col <- "PrimaryValue_raw"
    } else {
      qty_col <- "Qty_imputed"
      val_col <- "PrimaryValue_imputed"
    }
    
    trade %>%
      filter(Year == input$year) %>%
      select(CountryCode, Country, flowDesc,
             Qty = all_of(qty_col),
             PrimaryValue = all_of(val_col)) %>%
      group_by(CountryCode, Country, flowDesc) %>%
      summarise(
        total_qty   = sum(Qty, na.rm = TRUE),
        total_value = sum(PrimaryValue, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        id_cols = c(CountryCode, Country),
        names_from = flowDesc,
        values_from = c(total_qty, total_value),
        values_fill = 0
      )
  })
  
  waste_year <- reactive({
    req(input$year)
    waste %>%
      filter(Year == input$year) %>%
      transmute(
        CountryCode,
        waste_prod_kg   = Est_Waste_Produced * 1e9,
        waste_recyc_kg  = Est_Recycled_Amount * 1e9,
        own_proc_kg     = Processed_Own_Waste * 1e9,
        others_proc_kg  = Processed_Others_Waste * 1e9,
        percent_own_proc = Percent_Own_Processed
      )
  })
  
  pa_data_year <- reactive({
    req(input$pa_year)
    forecast_data %>%
      filter(Year == input$pa_year) %>%
      transmute(
        CountryCode,
        pa_sufficiency = Percent_Own_Processed,
        pa_value       = Local_Industry_Value
      )
  })
  
  world_data <- reactive({
    world %>%
      left_join(trade_year(), by = "CountryCode") %>%
      left_join(waste_year(), by = "CountryCode")
  })
  
  pa_world_data <- reactive({
    req(input$pa_year, input$pa_metric)
    pa_data <- pa_data_year()
    world %>%
      left_join(pa_data, by = "CountryCode")
  })
  
  map_metric_column <- reactive({
    trade_unit <- trade_display_unit()
    
    if (input$map_metric == "import" || input$map_metric == "export") {
      if (trade_unit == "qty") {
        return(paste0("total_qty_", tools::toTitleCase(input$map_metric)))
      } else {
        return(paste0("total_value_", tools::toTitleCase(input$map_metric)))
      }
    }
    
    switch(input$map_metric,
           waste_prod  = "waste_prod_kg",
           waste_recyc = "waste_recyc_kg")
  })
  
  map_metric_label <- reactive({
    trade_unit <- trade_display_unit()
    
    switch(input$map_metric,
           waste_prod  = "Waste produced (kg)",
           waste_recyc = "Waste recycled (kg)",
           import = if (trade_unit == "qty") "Waste imported (kg)" else "Import value (USD)",
           export = if (trade_unit == "qty") "Waste exported (kg)" else "Export value (USD)")
  })
  
  palette_reactive <- reactive({
    col <- map_metric_column()
    colorBin(
      palette = "Reds",
      domain  = world_data()[[col]],
      bins    = 7,
      na.color = "#ececec"
    )
  })
  
  pa_map_metric_column <- reactive({
    input$pa_metric
  })
  
  pa_map_metric_label <- reactive({
    switch(input$pa_metric,
           pa_sufficiency = "Plastic Waste Sufficiency Index (%)",
           pa_value       = "Value of Waste Processing Industry (USD)")
  })
  
  pa_palette_reactive <- reactive({
    col <- pa_map_metric_column()
    colorBin(
      palette = "Greens",
      domain  = pa_world_data()[[col]],
      bins    = 7,
      na.color = "#ececec"
    )
  })
  
  fmt_num <- function(x, digits = 2) {
    if (length(x) == 0 || is.na(x)) return("n/a")
    format(round(x, digits), big.mark = ",")
  }
  fmt_kg <- function(x, digits = 0) {
    if (length(x) == 0 || is.na(x)) return("n/a")
    format(round(x, digits), big.mark = ",")
  }
  
  output$world_map <- renderLeaflet({
    wd <- world_data()
    pal <- palette_reactive()
    metric_col   <- map_metric_column()
    metric_label <- map_metric_label()
    
    leaflet(
      wd,
      options = leafletOptions(
        worldCopyJump = TRUE,
        minZoom = 1,
        noWrap  = TRUE
      )
    ) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      addPolygons(
        layerId    = ~CountryCode,
        fillColor  = ~pal(wd[[metric_col]]),
        weight     = 0.8,
        color      = "#444444",
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(
          weight = 3,
          color  = "#0000FF",
          bringToFront = TRUE
        ),
        label = ~paste0(
          name, " (", input$year, "): ",
          ifelse(is.na(wd[[metric_col]]),
                 "No data",
                 format(round(wd[[metric_col]]), big.mark = ",")),
          " (", metric_label, ")"
        )
      ) %>%
      addLegend(
        "bottomright",
        pal    = pal,
        values = ~wd[[metric_col]],
        title  = paste0(metric_label, " (", input$year, ")")
      )
  })
  
  output$pa_map <- renderLeaflet({
    pd <- pa_world_data()
    pal <- pa_palette_reactive()
    metric_col   <- pa_map_metric_column()
    metric_label <- pa_map_metric_label()
    
    req(pd, metric_col) 
    
    if (!inherits(pd, "sf") || nrow(pd) == 0 || !"geometry" %in% colnames(pd) || !(metric_col %in% colnames(pd))) {
      return(leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 1))
    }
    
    leaflet(
      pd,
      options = leafletOptions(
        worldCopyJump = TRUE,
        minZoom = 1,
        noWrap  = TRUE
      )
    ) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      addPolygons(
        layerId    = ~CountryCode,
        fillColor  = ~pal(pd[[metric_col]]),
        weight     = 0.8,
        color      = "#444444",
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(
          weight = 3,
          color  = "#00FF00",
          bringToFront = TRUE
        ),
        label = ~paste0(
          name, " (", input$pa_year, "): ",
          ifelse(is.na(pd[[metric_col]]),
                 "No data",
                 format(round(pd[[metric_col]]), big.mark = ",")),
          " (", metric_label, ")"
        )
      ) %>%
      addLegend(
        "bottomright",
        pal    = pal,
        values = ~pd[[metric_col]],
        title  = paste0(metric_label, " (", input$pa_year, ")")
      )
  })
  
  observeEvent(input$world_map_shape_click, {
    click <- input$world_map_shape_click
    req(click$id)
    
    iso_code <- click$id
    clicked_country_iso(iso_code)
    
    lng <- click$lng
    lat <- click$lat
    
    if (data_source_unit() == "raw") {
      qty_col <- "Qty_raw"
      val_col <- "PrimaryValue_raw"
      trade_source_text <- paste0("Trade metrics (UN dataset, ", input$year, ", Original Data)")
    } else {
      qty_col <- "Qty_imputed"
      val_col <- "PrimaryValue_imputed"
      trade_source_text <- paste0("Trade metrics (UN dataset, ", input$year, ", Imputed Data)")
    }
    
    tr_disaggregated <- trade %>%
      filter(Year == input$year, CountryCode == iso_code) %>%
      select(flowDesc,
             Qty         = all_of(qty_col),
             PrimaryValue = all_of(val_col)) %>%
      group_by(flowDesc) %>%
      summarise(total_qty = sum(Qty, na.rm = TRUE),
                total_value = sum(PrimaryValue, na.rm = TRUE),
                .groups = "drop") %>%
      pivot_wider(names_from = flowDesc,
                  values_from = c(total_qty, total_value),
                  values_fill = NA_real_)
    
    tr_row <- tr_disaggregated
    
    cw_all <- waste[waste$CountryCode == iso_code, ]
    cw_yr  <- waste_year() %>% filter(CountryCode == iso_code)
    
    world_name <- world$name[world$CountryCode == iso_code][1]
    
    if (nrow(cw_all) > 0) {
      idx <- which(cw_all$Processed_Others_Waste > 0)
      turning_year <- if (length(idx) == 0) NA_integer_ else min(cw_all$Year[idx])
    } else {
      turning_year <- NA_integer_
    }
    
    if (nrow(cw_all) == 0) {
      waste_block <- paste0(
        "<b>", world_name, "</b><br/>",
        "<b>Waste-system metrics</b><br/>",
        "No waste-system data available for this country in any year.<br/><br/>"
      )
    } else if (nrow(cw_yr) == 0) {
      years_avail <- sort(unique(cw_all$Year))
      range_txt   <- paste0(min(years_avail), "–", max(years_avail))
      waste_block <- paste0(
        "<b>", world_name, "</b><br/>",
        "<b>Waste-system metrics (", input$year, ")</b><br/>",
        "No waste-system data available for this year.<br/>",
        "Available years for this country: ", range_txt, ".<br/><br/>"
      )
    } else {
      r <- cw_yr[1, ]
      waste_block <- paste0(
        "<b>", world_name, "</b><br/>",
        "<b>Waste-system metrics (", input$year, ", kg)</b><br/>",
        "Estimated waste produced: ", fmt_kg(r$waste_prod_kg), "<br/>",
        "Estimated recycled amount: ", fmt_kg(r$waste_recyc_kg), "<br/>",
        "Own waste processed: ",     fmt_kg(r$own_proc_kg), "<br/>",
        "Others' waste processed: ", fmt_kg(r$others_proc_kg), "<br/>",
        "Percent own processed: ",   fmt_num(r$percent_own_proc), " %<br/><br/>"
      )
    }
    
    if (nrow(tr_row) == 0 || (all(is.na(tr_row$total_qty_Import)) &&
                              all(is.na(tr_row$total_qty_Export)) &&
                              all(is.na(tr_row$total_value_Import)) &&
                              all(is.na(tr_row$total_value_Export)))) {
      trade_block <- paste0(
        "<b>", trade_source_text, "</b><br/>",
        "No trade data available for this year.<br/><br/>"
      )
    } else {
      tr <- tr_row[1, ]
      imp_qty <- coalesce(tr$total_qty_Import, 0)
      exp_qty <- coalesce(tr$total_qty_Export, 0)
      imp_val <- coalesce(tr$total_value_Import, 0)
      exp_val <- coalesce(tr$total_value_Export, 0)
      
      if (imp_qty == 0 && exp_qty == 0 && imp_val == 0 && exp_val == 0) {
        trade_block <- paste0(
          "<b>", trade_source_text, "</b><br/>",
          "No trade data available for this year.<br/><br/>"
        )
      } else {
        trade_block <- paste0(
          "<b>", trade_source_text, "</b><br/>",
          "<u>Imports</u>: ", fmt_kg(imp_qty, 0), " kg; ",
          fmt_kg(imp_val, 0), " USD<br/>",
          "<u>Exports</u>: ", fmt_kg(exp_qty, 0), " kg; ",
          fmt_kg(exp_val, 0), " USD<br/><br/>"
        )
      }
    }
    
    if (is.na(turning_year)) {
      turning_block <- "<b>Turning point:</b> this country never processes waste for others in the available waste data.<br/>"
    } else {
      turning_block <- paste0(
        "<b>Turning point:</b> begins processing others' waste around <b>", turning_year, "</b> ",
        "(Processed_Others_Waste &gt; 0).<br/>"
      )
    }
    
    graph_button <- paste0(
      "<br/><button type='button' class='btn btn-sm btn-primary btn-view-history' ",
      "onclick=\"Shiny.setInputValue('show_graph', 1, {priority:'event'})\">",
      "View waste history graph</button>"
    )
    
    popup_text <- paste0(waste_block, trade_block, turning_block, graph_button)
    
    leafletProxy("world_map") %>%
      clearPopups() %>%
      addPopups(lng, lat, popup_text, options = popupOptions(maxWidth = 400))
  })
  
  observeEvent(input$pa_map_shape_click, {
    click <- input$pa_map_shape_click
    req(click$id)
    
    iso_code <- click$id
    clicked_country_iso(iso_code) 
    
    lng <- click$lng
    lat <- click$lat
    
    pa_data <- pa_data_year() %>% filter(CountryCode == iso_code)
    
    world_name <- world$name[world$CountryCode == iso_code][1]
    
    if (nrow(pa_data) == 0 || (is.na(pa_data$pa_sufficiency[1]) && is.na(pa_data$pa_value[1]))) {
      pa_block <- paste0(
        "<b>", world_name, "</b><br/>",
        "<b>Prescriptive Analysis (", input$pa_year, ")</b><br/>",
        "No Prescriptive Analysis data available for this country in the selected year.<br/>"
      )
    } else {
      r <- pa_data[1, ]
      
      sufficiency_val <- fmt_num(r$pa_sufficiency)
      value_val       <- fmt_kg(r$pa_value, 0)
      
      pa_block <- paste0(
        "<b>", world_name, "</b><br/>",
        "<b>Prescriptive Analysis (", input$pa_year, ")</b><br/>",
        "Plastic Waste Sufficiency Index: <b>", sufficiency_val, " %</b><br/>",
        "Value of Processing Industry: <b>", value_val, " USD</b><br/>"
      )
    }
    
    popup_text <- pa_block
    
    leafletProxy("pa_map") %>%
      clearPopups() %>%
      addPopups(lng, lat, popup_text, options = popupOptions(maxWidth = 400))
  })
  
  output$waste_modal_plot <- renderPlot({
    iso <- clicked_country_iso()
    req(iso)
    
    cname <- world$name[world$CountryCode == iso][1]
    
    cw <- waste[waste$CountryCode == iso, ]
    req(nrow(cw) > 0)
    
    tr_country <- trade %>%
      filter(CountryCode == iso) %>%
      group_by(Year, flowDesc) %>%
      summarise(qty = sum(Qty_imputed, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        id_cols = Year,
        names_from = flowDesc,
        values_from = qty,
        values_fill = 0
      )
    
    cw_small <- cw %>%
      select(Year, Est_Waste_Produced, Processed_Own_Waste, Processed_Others_Waste)
    
    df_all <- full_join(cw_small, tr_country, by = "Year")
    
    df_all <- df_all %>%
      mutate(
        WasteProduced_kg   = Est_Waste_Produced  * 1e9,
        OwnProcessed_kg    = Processed_Own_Waste * 1e9,
        OthersProcessed_kg = Processed_Others_Waste* 1e9,
        Imports_kg         = if ("Import" %in% names(.)) Import else NA_real_,
        Exports_kg         = if ("Export" %in% names(.)) Export else NA_real_
      )
    
    idx <- which(df_all$OthersProcessed_kg > 0)
    turning_year <- if (length(idx) == 0) NA_integer_ else min(df_all$Year[idx])
    
    df_long <- df_all %>%
      select(Year,
             `Waste produced`          = WasteProduced_kg,
             `Own waste processed`     = OwnProcessed_kg,
             `Others' waste processed` = OthersProcessed_kg,
             `Imports (kg)`            = Imports_kg,
             `Exports (kg)`            = Exports_kg) %>%
      pivot_longer(
        cols = -Year,
        names_to = "Series",
        values_to = "Value"
      )
    
    title_text <- paste0(cname, " raw waste history (Trade data is Imputed)")
    subtitle_text <- if (!is.na(turning_year)) {
      paste0("Historical turning point year: ", turning_year)
    } else {
      "No historical turning point (no others' waste processed)"
    }
    
    g <- ggplot(df_long, aes(Year, Value, colour = Series)) +
      geom_line(size = 1) +
      labs(
        x = "Year",
        y = "Plastic (kg)",
        title = title_text,
        subtitle = subtitle_text,
        colour = ""
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title      = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle   = element_text(hjust = 0.5, face = "bold")
      )
    
    if (!is.na(turning_year)) {
      g <- g + geom_vline(xintercept = turning_year, linetype = "dashed", color = "gray")
    }
    
    g
  })
  
  observeEvent(input$show_graph, {
    showModal(modalDialog(
      title = "Waste history (kg)",
      plotOutput("waste_modal_plot", height = "400px"),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  
  output$pa_modal_plot <- renderPlot({
    iso <- clicked_country_iso()
    req(iso)
    
    cname <- world$name[world$CountryCode == iso][1]
    
    df_country <- forecast_data %>%
      filter(CountryCode == iso) %>%
      select(
        Year,
        `Sufficiency Index (%)` = Percent_Own_Processed,
        `Industry Value (USD)`  = Local_Industry_Value
      ) %>%
      pivot_longer(
        cols = -Year,
        names_to = "Series",
        values_to = "Value"
      )
    
    req(nrow(df_country) > 0)
    
    title_text <- paste0(cname, " Prescriptive Analysis History")
    
    ggplot(df_country, aes(x = Year, y = Value, colour = Series)) +
      geom_line(size = 1) +
      geom_vline(xintercept = max_year, linetype = "dashed", color = "gray50") +
      annotate("text", x = max_year + 1, y = max(df_country$Value, na.rm = TRUE), 
               label = "Forecast Start", angle = 90, hjust = 1.1, color = "gray50") +
      labs(
        x = "Year",
        y = "Metric Value",
        title = title_text,
        colour = ""
      ) +
      facet_wrap(~ Series, scales = "free_y", ncol = 1) +
      theme_minimal() +
      theme(
        legend.position    = "bottom",
        plot.title         = element_text(hjust = 0.5, face = "bold"),
        strip.background   = element_rect(fill = "#28a745"),
        strip.text         = element_text(color = "white", face = "bold")
      )
  })
  
  observeEvent(input$show_pa_graph, {
    showModal(modalDialog(
      title = "Prescriptive Analysis History",
      plotOutput("pa_modal_plot", height = "600px"),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  # ---- FORECASTING TAB SERVER LOGIC -------

  forecast_dataset <- reactive({
    final_forecast_dataset %>%
      select(
        Country, Year,
        Processed_Own_Waste,
        Processed_Others_Waste,
        Percent_Own_Processed
      ) %>%
      pivot_longer(
        cols = c(Processed_Own_Waste, Processed_Others_Waste, Percent_Own_Processed),
        names_to = "Type",
        values_to = "Amount"
      ) %>%
      mutate(CountryCode = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
      as_tsibble(key = c(Country, Type), index = Year) %>%
      fill_gaps()
  })
  
  observe({
    req(forecast_dataset())
    
    if (input$main_tabs != "Forecasting") return()
    
    countries <- unique(forecast_dataset()$Country)
    updateSelectInput(session, "forecast_country", choices = countries, selected = "Spain")
    
    min_y       <- min(forecast_dataset()$Year, na.rm = TRUE)
    max_y_train <- max_year
    
    updateSliderInput(session, "forecast_train_range", 
                      min = min_y, max = max_y_train, 
                      value = c(min_y + 5, max_y_train - 5))
  })
  
  forecast_country_data <- reactive({
    req(forecast_dataset(), input$forecast_country, input$forecast_type)
    forecast_dataset() %>%
      filter(Country == input$forecast_country, Type == input$forecast_type)
  })
  
  forecast_results <- reactive({
    req(forecast_country_data())
    
    start_year  <- input$forecast_train_range[1]
    cutoff_year <- input$forecast_train_range[2]
    
    train_set <- forecast_country_data() %>% 
      filter(Year >= start_year & Year <= cutoff_year)
    
    req(nrow(train_set) > 1) 
    
    m_def <- switch(input$forecast_model_type,
                    "ETS"   = ETS(Amount),
                    "ARIMA" = ARIMA(Amount),
                    "MEAN"  = MEAN(Amount))
    
    fit <- train_set %>% model(Selected_Model = m_def)
    fc  <- fit %>% forecast(h = input$forecast_horizon)
    
    list(fc = fc, train = train_set, model = fit)
  })
  
  output$forecastPlot <- renderPlot({
    req(forecast_results())
    
    fc_obj     <- forecast_results()$fc
    train_data <- forecast_results()$train
    cutoff_year <- input$forecast_train_range[2]
    
    last_hist <- train_data %>% 
      as_tibble() %>% 
      filter(Year == max(Year)) %>% 
      select(Year, Amount)
    
    first_fc <- fc_obj %>% 
      as_tibble() %>% 
      filter(Year == min(Year)) %>% 
      mutate(Amount = .mean) %>% 
      select(Year, Amount)
    
    bridge <- tryCatch({
      bind_rows(last_hist, first_fc)
    }, error = function(e) {
      data.frame(Year = numeric(), Amount = numeric())
    })
    
    y_label <- if (grepl("Percent", input$forecast_type)) "Percentage (%)" else "Amount (Kg)"
    
    fc_obj %>%
      autoplot(train_data, level = c(80, 95)) +
      geom_line(data = bridge, aes(x = Year, y = Amount), color = "#4a8bad", size = 1) +
      geom_vline(xintercept = cutoff_year, linetype = "dashed", color = "red") +
      annotate("text", x = cutoff_year, y = Inf, label = "Train Cutoff", 
               vjust = 1.5, hjust = 1.1, color = "red", angle = 90) +
      labs(title = paste("Forecast:", input$forecast_type, "-", input$forecast_country),
           subtitle = paste("Training Range:", input$forecast_train_range[1], "-", input$forecast_train_range[2]),
           y = y_label) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()))
  })
  
  output$accuracyTable <- renderTable({
    req(forecast_results())
    
    full_data <- forecast_country_data()
    fc_obj    <- forecast_results()$fc
    
    accuracy(fc_obj, full_data) %>%
      select(.model, RMSE, MAE, MAPE)
  }, digits = 2)
}

shinyApp(ui, server)