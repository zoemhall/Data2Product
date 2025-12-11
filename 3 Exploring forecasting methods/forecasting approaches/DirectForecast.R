library(dplyr)
library(tsibble)
library(fpp3) 
library(readr)
library(tidyr)
library(ggplot2)
library(zoo)
library(wbstats)

# ==============================================================================
# 1. DATA PREPARATION (Prescriptive Logic)
# ==============================================================================

# Load Data (Ensure files are in your directory)
RecyclingData <- read.csv("raw data/OECD_recycling_data.csv", header=TRUE)
TradeData <- read.csv("raw data/UN_Trimmed_1988-2024.csv", header=TRUE) %>%
  mutate(Qty = Qty / 1e9) # 1 Billion Kg = 1 Million Tonnes
code_mapping <- read_csv("raw data/country_code_bridge.csv")

# Get GDP Data
gdp_data <- wb_data("NY.GDP.MKTP.KD", start_date = 1990, end_date = 2024) %>%
  select(iso3c, country, date, NY.GDP.MKTP.KD) %>% 
  rename(WB_Code = iso3c, Country = country, Year = date, GDP = NY.GDP.MKTP.KD)

# Link Data
TradeData_Clean <- TradeData %>% left_join(code_mapping, by = "Country")
GDP_Clean <- gdp_data %>%
  inner_join(code_mapping, by = "WB_Code", relationship = "many-to-many") %>%
  select(Country.y, WB_Code, OECD_Code, Year, GDP) %>%
  rename(Country = Country.y)

# Calculate Regional Totals and Shares
Region_GDP_Totals <- GDP_Clean %>%
  group_by(OECD_Code, Year) %>%
  summarise(Region_Total_GDP = sum(GDP, na.rm = TRUE), .groups = 'drop')

Country_GDP_Shares <- GDP_Clean %>%
  left_join(Region_GDP_Totals, by = c("OECD_Code", "Year")) %>%
  mutate(GDP_Share = GDP / Region_Total_GDP) %>%
  select(Country, OECD_Code, Year, GDP_Share)

# Process OECD Recycling Data
Regional_Data <- RecyclingData %>%
  filter(PLASTICS_CODES %in% c("TOTAL", "REC")) %>%
  select(LOCATION_CODES, TIME_PERIOD, PLASTICS_CODES, OBS_VALUE) %>%
  pivot_wider(names_from = PLASTICS_CODES, values_from = OBS_VALUE) %>%
  rename(Region_Waste_Produced = TOTAL, Region_Recycled_Amount = REC, Year = TIME_PERIOD, OECD_Code = LOCATION_CODES) %>%
  mutate(Regional_Recycling_Rate = Region_Recycled_Amount / Region_Waste_Produced)

# Calculate Prescriptive Variables
Prescriptive_Country_Data <- Country_GDP_Shares %>%
  inner_join(Regional_Data, by = c("OECD_Code", "Year")) %>%
  mutate(
    Est_Waste_Produced = Region_Waste_Produced * GDP_Share,
    Est_Recycled_Amount = Est_Waste_Produced * Regional_Recycling_Rate
  ) %>%
  select(Country, Year, Est_Waste_Produced, Est_Recycled_Amount)

# Merge with Trade Data
Trade_Aggregated <- TradeData %>%
  group_by(Country, Year, flowCode) %>%
  summarise(Total_Qty = sum(Qty, na.rm = TRUE), .groups = 'drop') %>% 
  pivot_wider(id_cols = c(Country, Year), names_from = flowCode, values_from = Total_Qty, values_fill = 0) %>%
  rename(Imports = M, Exports = X)

# Final Dataset Creation
Waste_Processing_Analysis <- Prescriptive_Country_Data %>%
  left_join(Trade_Aggregated, by = c("Country", "Year")) %>%
  mutate(
    Imports = replace_na(Imports, 0),
    Exports = replace_na(Exports, 0),
    # THE PRESCRIPTIVE VARIABLE:
    Processed_Own_Waste = Est_Recycled_Amount - Exports 
  )

# ==============================================================================
# 2. FORECASTING PREPARATION
# ==============================================================================

Forecast_Data <- Waste_Processing_Analysis %>% 
  select(Country, Year, Processed_Own_Waste) %>%
  rename(Qty = Processed_Own_Waste) %>%
  as_tsibble(key = Country, index = Year) %>%
  fill_gaps() %>%
  mutate(Qty = zoo::na.approx(Qty, rule = 2)) %>%
  group_by_key() %>%
  filter(n() >= 10) %>% # Ensure enough data points
  ungroup()

# ==============================================================================
# 3. CALCULATE AGGREGATED ERRORS (The "Together" Step)
# ==============================================================================

# Split Training (History) and Test (Last 4 years)
last_year <- max(Forecast_Data$Year)
train_data <- Forecast_Data %>% filter(Year <= last_year - 4)

# Fit Models
fit <- train_data %>%
  model(
    ETS = ETS(Qty),
    ARIMA = ARIMA(Qty)
  )

# Forecast on Test Set
fc_test <- fit %>% forecast(h = 4)

# Calculate Accuracy per Country first
country_accuracy <- fc_test %>%
  accuracy(Forecast_Data)

# --- NEW: AGGREGATE ERRORS ---
# Summarize accuracy across ALL countries to get a global view
global_summary <- country_accuracy %>%
  group_by(.model) %>%
  summarise(
    Avg_RMSE = mean(RMSE, na.rm = TRUE),
    Median_RMSE = median(RMSE, na.rm = TRUE),
    Avg_MAE = mean(MAE, na.rm = TRUE),
    Avg_MAPE = mean(MAPE, na.rm = TRUE) # Mean Absolute Percentage Error
  )

print("Global Error Summary (Averaged across all countries):")
print(global_summary)

# --- VISUALIZE ERROR DISTRIBUTION (CLEAN) ---

# Custom colors
custom_colors <- c("ARIMA" = "#4E79A7", "ETS" = "#F28E2B")

plot_error_dist <- country_accuracy %>%
  ggplot(aes(x = .model, y = MAPE, fill = .model)) +
  
  # 1. The Boxplot: Clean, no dots
  geom_boxplot(alpha = 0.85, outlier.shape = NA, width = 0.5, color = "gray20") +
  
  # 2. Styling
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Forecast Error Distribution (MAPE)",
    y = "Mean Absolute Percentage Error (%)",
    x = NULL 
  ) +
  
  # 3. Theme
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray40", margin = margin(b = 15)),
    axis.text.x = element_text(face = "bold", color = "black"),
    axis.line.x = element_line(color = "gray80")
  ) +
  
  # 4. Zoom
  coord_cartesian(ylim = c(0, 200))

print(plot_error_dist)

# ==============================================================================
# 4. FUTURE FORECAST
# ==============================================================================

# Refit on full data for the final plot
final_fit <- Forecast_Data %>%
  model(ETS = ETS(Qty), ARIMA = ARIMA(Qty))

final_forecast <- final_fit %>% forecast(h = 10)

print("Final forecast calculation complete (variables stored in memory).")


# ==============================================================================
# 5. VISUALIZING THE DIVERGENCE (WITH JUMP-OFF POINT)
# ==============================================================================

# 1. Find the country with the biggest disagreement
divergence_check <- country_accuracy %>%
  select(Country, .model, MAPE) %>%
  pivot_wider(names_from = .model, values_from = MAPE) %>%
  mutate(Error_Difference = abs(ARIMA - ETS)) %>%
  arrange(desc(Error_Difference))

interesting_country <- divergence_check$Country[1]
arima_err <- round(divergence_check$ARIMA[1], 2)
ets_err <- round(divergence_check$ETS[1], 2)

print(paste("Plotting divergence for:", interesting_country))

# 2. Prepare data
country_fc <- fc_test %>% filter(Country == interesting_country)
country_history <- train_data %>% filter(Country == interesting_country)
country_actuals <- Forecast_Data %>% filter(Country == interesting_country)

# --- NEW: Identify the "Jump-off" point (Last training data point) ---
last_hist_point <- country_history %>%
  filter(Year == max(Year))

# 3. Create the Plot
divergence_plot <- country_fc %>%
  # Passing 'country_history' here forces fpp3 to connect the forecast lines to the history
  autoplot(country_history, level = c(80, 95)) + 
  
  # Overlay the "Truth" (Actual Test Data) in a solid black line
  geom_line(data = country_actuals, aes(y = Qty), color = "black", linewidth = 0.8) +
  
  # --- NEW: Add the Anchor Dot at the Jump-off point ---
  geom_point(data = last_hist_point, aes(y = Qty), 
             color = "#D55E00", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  
  # --- NEW: Label the Jump-off point ---
  geom_text(data = last_hist_point, aes(y = Qty, label = paste("", Year)), 
            hjust = 1.1, vjust = -0.5, fontface = "bold", size = 3.5, color = "#D55E00") +
  
  # Styling
  labs(
    title = paste("Forecast Analysis:", interesting_country),
    subtitle = paste0("Model Comparison (Lower MAPE is better):\n",
                      "• ARIMA Error: ", arima_err, "%\n",
                      "• ETS Error:    ", ets_err, "%"),
    y = "Processed Own Waste (Million Tonnes)",
    x = "Year"
  ) +
  
  scale_color_manual(values = c("ARIMA" = "#4E79A7", "ETS" = "#F28E2B")) +
  scale_fill_manual(values = c("ARIMA" = "#4E79A7", "ETS" = "#F28E2B")) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(face = "italic", color = "gray30", size = 11),
    panel.grid.minor = element_blank()
  )

print(divergence_plot)