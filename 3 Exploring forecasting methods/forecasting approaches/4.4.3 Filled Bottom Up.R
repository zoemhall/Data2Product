# Same approach as the bottom up forecasting, but using the data set which was filled
# In section 1: raw_data_for_forecasting_ALL.csv

library(dplyr)
library(tsibble)
library(fpp3) 
library(readr)
library(tidyr)
library(ggplot2)
library(zoo)
library(wbstats)
library(distributional) 

# ==============================================================================
# 1. DATA PREPARATION
# ==============================================================================

# --- Load New Data ---
# We use the new filled dataset for trade
TradeData <- read.csv("raw data/raw_data_for_forecasting_ALL.csv", header=TRUE) %>%
  mutate(Qty = Qty / 1e9) # Convert Kg to Million Tonnes

# Load Reference Data
RecyclingData <- read.csv("raw data/OECD_recycling_data.csv", header=TRUE)
code_mapping <- read_csv("raw data/country_code_bridge.csv")

# --- Get GDP Data ---
gdp_data <- wb_data("NY.GDP.MKTP.KD", start_date = 1990, end_date = 2024) %>%
  select(iso3c, country, date, NY.GDP.MKTP.KD) %>% 
  rename(WB_Code = iso3c, Country = country, Year = date, GDP = NY.GDP.MKTP.KD)

# --- Link Data ---
# Processing the NEW Trade Data
# We attempt to join by CountryCode (ISO3) first for safety, falling back to name if needed
TradeData_Clean <- TradeData %>% 
  rename(WB_Code = CountryCode) %>%
  left_join(code_mapping, by = "WB_Code") %>%
  # If the join resulted in duplicate columns (e.g. Country.x, Country.y), clean them
  mutate(Country = coalesce(Country.y, Country.x)) %>%
  select(-Country.x, -Country.y)

GDP_Clean <- gdp_data %>%
  inner_join(code_mapping, by = "WB_Code", relationship = "many-to-many") %>%
  select(Country.y, WB_Code, OECD_Code, Year, GDP) %>%
  rename(Country = Country.y)

# --- Process Regional Data (Calculating the RATE) ---
Region_GDP_Totals <- GDP_Clean %>%
  group_by(OECD_Code, Year) %>%
  summarise(Region_Total_GDP = sum(GDP, na.rm = TRUE), .groups = 'drop')

Country_GDP_Shares <- GDP_Clean %>%
  left_join(Region_GDP_Totals, by = c("OECD_Code", "Year")) %>%
  mutate(GDP_Share = GDP / Region_Total_GDP) %>%
  select(Country, OECD_Code, Year, GDP_Share)

Regional_Data <- RecyclingData %>%
  filter(PLASTICS_CODES %in% c("TOTAL", "REC")) %>%
  select(LOCATION_CODES, TIME_PERIOD, PLASTICS_CODES, OBS_VALUE) %>%
  pivot_wider(names_from = PLASTICS_CODES, values_from = OBS_VALUE) %>%
  rename(Region_Waste_Produced = TOTAL, Region_Recycled_Amount = REC, Year = TIME_PERIOD, OECD_Code = LOCATION_CODES) %>%
  mutate(
    # CALCULATE THE REGIONAL RATE (This is what we will forecast)
    Regional_Rate = Region_Recycled_Amount / Region_Waste_Produced
  )

# --- Calculate Country-Level Components ---
Component_Data <- Country_GDP_Shares %>%
  inner_join(Regional_Data, by = c("OECD_Code", "Year")) %>%
  mutate(
    # Component 1: Estimated Waste Produced
    Est_Waste_Produced = Region_Waste_Produced * GDP_Share,
    # Component 2: The Rate (Assumed same as region)
    Recycling_Rate = Regional_Rate
  ) %>%
  select(Country, Year, Est_Waste_Produced, Recycling_Rate)

# --- Prepare Exports from New Data ---
Trade_Aggregated <- TradeData_Clean %>%
  filter(flowCode == "X") %>% # We only need Exports for the formula
  group_by(Country, Year) %>%
  summarise(Exports = sum(Qty, na.rm = TRUE), .groups = 'drop')

# --- Merge & Clean ---
Full_Dataset <- Component_Data %>%
  left_join(Trade_Aggregated, by = c("Country", "Year")) %>%
  mutate(Exports = replace_na(Exports, 0)) %>%
  distinct(Country, Year, .keep_all = TRUE) %>% # Ensure uniqueness
  select(Country, Year, Est_Waste_Produced, Recycling_Rate, Exports)

# ==============================================================================
# 2. CREATE TSIBBLE & SPLIT
# ==============================================================================

Model_Data <- Full_Dataset %>%
  as_tsibble(key = Country, index = Year) %>%
  fill_gaps() %>%
  mutate(
    Est_Waste_Produced = zoo::na.approx(Est_Waste_Produced, rule = 2),
    Recycling_Rate = zoo::na.approx(Recycling_Rate, rule = 2),
    Exports = zoo::na.approx(Exports, rule = 2)
  ) %>%
  group_by_key() %>%
  filter(n() >= 10) %>%
  ungroup()

last_year <- max(Model_Data$Year)
train_data <- Model_Data %>% filter(Year <= last_year - 4)

# ==============================================================================
# 3. FORECAST 3 COMPONENTS SEPARATELY
# ==============================================================================

print("Fitting models to Waste, Rate, and Exports separately...")

# 1. Waste Produced
fit_waste <- train_data %>%
  model(
    ETS_Waste = ETS(Est_Waste_Produced),
    ARIMA_Waste = ARIMA(Est_Waste_Produced)
  )

# 2. Recycling Rate (Trending this!)
fit_rate <- train_data %>%
  model(
    ETS_Rate = ETS(Recycling_Rate),
    ARIMA_Rate = ARIMA(Recycling_Rate)
  )

# 3. Exports
fit_exp <- train_data %>%
  model(
    ETS_Exports = ETS(Exports),
    ARIMA_Exports = ARIMA(Exports)
  )

# Forecast and Convert to Tibble immediately
fc_waste_tbl <- fit_waste %>% forecast(h = 4) %>% as_tibble()
fc_rate_tbl  <- fit_rate  %>% forecast(h = 4) %>% as_tibble()
fc_exp_tbl   <- fit_exp   %>% forecast(h = 4) %>% as_tibble()

# ==============================================================================
# 4. COMPLEX COMBINATION (ERROR PROPAGATION)
# ==============================================================================

# Extract Mean and Variance for all 3 components
clean_waste <- fc_waste_tbl %>%
  mutate(Model_Type = stringr::str_remove(.model, "_Waste"), Var_W = distributional::variance(Est_Waste_Produced)) %>%
  rename(Mean_W = .mean) %>% select(Country, Year, Model_Type, Mean_W, Var_W)

clean_rate <- fc_rate_tbl %>%
  mutate(Model_Type = stringr::str_remove(.model, "_Rate"), Var_R = distributional::variance(Recycling_Rate)) %>%
  rename(Mean_R = .mean) %>% select(Country, Year, Model_Type, Mean_R, Var_R)

clean_exp <- fc_exp_tbl %>%
  mutate(Model_Type = stringr::str_remove(.model, "_Exports"), Var_E = distributional::variance(Exports)) %>%
  rename(Mean_E = .mean) %>% select(Country, Year, Model_Type, Mean_E, Var_E)

# Combine
fc_calculated <- clean_waste %>%
  inner_join(clean_rate, by = c("Country", "Year", "Model_Type")) %>%
  inner_join(clean_exp, by = c("Country", "Year", "Model_Type")) %>%
  mutate(
    # --- STEP A: Calculate Recycled Amount (Waste * Rate) ---
    Mean_Recycled = Mean_W * Mean_R,
    
    # Variance of Product (assuming independence): 
    # Var(XY) = E[X]^2*Var(Y) + E[Y]^2*Var(X) + Var(X)*Var(Y)
    Var_Recycled = (Mean_W^2 * Var_R) + (Mean_R^2 * Var_W) + (Var_W * Var_R),
    
    # --- STEP B: Calculate Final Processed (Recycled - Exports) ---
    Forecasted_Processed_Own_Waste = Mean_Recycled - Mean_E,
    
    # Variance of Difference: Var(A - B) = Var(A) + Var(B)
    Final_Variance = Var_Recycled + Var_E,
    Combined_SD = sqrt(Final_Variance),
    
    # Intervals
    Lo80 = Forecasted_Processed_Own_Waste - 1.28 * Combined_SD,
    Hi80 = Forecasted_Processed_Own_Waste + 1.28 * Combined_SD,
    Lo95 = Forecasted_Processed_Own_Waste - 1.96 * Combined_SD,
    Hi95 = Forecasted_Processed_Own_Waste + 1.96 * Combined_SD
  )

# Calculate Actuals
actuals_calculated <- Model_Data %>%
  mutate(Actual_Processed_Own_Waste = (Est_Waste_Produced * Recycling_Rate) - Exports) %>%
  select(Country, Year, Actual_Processed_Own_Waste)

# Accuracy Check
accuracy_check <- fc_calculated %>%
  left_join(actuals_calculated, by = c("Country", "Year")) %>%
  mutate(
    Error = Actual_Processed_Own_Waste - Forecasted_Processed_Own_Waste,
    Pct_Error = abs(Error / Actual_Processed_Own_Waste) * 100
  )

# ==============================================================================
# 5. VISUALIZATION 1: BOXPLOT
# ==============================================================================
# 
# country_mape <- accuracy_check %>%
#   group_by(Country, Model_Type) %>%
#   summarise(MAPE = mean(Pct_Error, na.rm = TRUE), .groups = "drop") %>%
#   filter(is.finite(MAPE))
# 
# custom_colors <- c("ARIMA" = "#4E79A7", "ETS" = "#F28E2B")
# 
# plot_boxplot <- country_mape %>%
#   ggplot(aes(x = Model_Type, y = MAPE, fill = Model_Type)) +
#   geom_boxplot(alpha = 0.85, outlier.shape = NA, width = 0.5, color = "gray20") +
#   scale_fill_manual(values = custom_colors) +
#   labs(
#     title = "Forecast Error",
#     # subtitle = "Forecasting (Waste * Rate) - Exports using new data",
#     y = "Mean Absolute Percentage Error (%)",
#     x = NULL 
#   ) +
#   theme_minimal(base_size = 14) + 
#   theme(legend.position = "none", panel.grid.major.x = element_blank()) +
#   coord_cartesian(ylim = c(0, 300))
# 
# print(plot_boxplot)

# ==============================================================================
# 6. VISUALIZATION 2: DIVERGENCE PLOT (2nd Worst Case)
# ==============================================================================

divergence_check <- country_mape %>%
  pivot_wider(names_from = Model_Type, values_from = MAPE) %>%
  mutate(Diff = abs(ARIMA - ETS)) %>%
  arrange(desc(Diff))

# Picking the 2nd worst to avoid extreme outliers
interesting_country <- divergence_check$Country[2]
arima_err <- round(divergence_check$ARIMA[2], 2)
ets_err <- round(divergence_check$ETS[2], 2)

# Prepare Data
plot_fc <- fc_calculated %>%
  # filter(Country == interesting_country) %>%
  filter(Country == Japan) %>%
  rename(.model = Model_Type, Qty = Forecasted_Processed_Own_Waste)

plot_actuals <- actuals_calculated %>%
  #filter(Country == interesting_country) %>%
  filter(Country == Japan) %>%
  rename(Qty = Actual_Processed_Own_Waste)

jump_off_year <- min(plot_fc$Year) - 1
jump_off_point <- plot_actuals %>% filter(Year == jump_off_year)

# Create connected lines
plot_lines_connected <- plot_fc %>%
  as_tibble() %>%
  select(Country, Year, .model, Qty) %>%
  bind_rows(
    jump_off_point %>% mutate(.model = "ARIMA") %>% as_tibble(),
    jump_off_point %>% mutate(.model = "ETS") %>% as_tibble()
  ) %>%
  arrange(Year)

#print(paste("Plotting Divergence for:", interesting_country))
print(paste("Plotting Divergence for:", Japan))

plot_divergence <- ggplot() +
  geom_ribbon(data = plot_fc, aes(x = Year, ymin = Lo95, ymax = Hi95, fill = .model), alpha = 0.2) +
  geom_ribbon(data = plot_fc, aes(x = Year, ymin = Lo80, ymax = Hi80, fill = .model), alpha = 0.4) +
  geom_line(data = plot_actuals, aes(x = Year, y = Qty), color = "black", size = 0.8) +

  geom_line(data = plot_lines_connected, aes(x = Year, y = Qty, color = .model), size = 1) +

  geom_point(data = jump_off_point, aes(x = Year, y = Qty),
             color = "#D55E00", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  geom_text(data = jump_off_point, aes(x = Year, y = Qty, label = paste("", Year)),
            hjust = 1.1, vjust = -0.5, fontface = "bold", size = 3.5, color = "#D55E00") +

  labs(
    #title = paste("Forecast Analysis:", interesting_country),
    title = paste("Forecast Analysis:", Japan),
    subtitle = paste0("Bottom-up with imputed data:\n",
                      "• ARIMA Error: ", arima_err, "%\n",
                      "• ETS Error:    ", ets_err, "%"),
    y = "Processed Own Waste (Million Tonnes)",
    x = "Year"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.subtitle = element_text(face = "italic", color = "gray30"))

print(plot_divergence)