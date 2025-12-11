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

# --- Load Raw Data ---
RecyclingData <- read.csv("raw data/OECD_recycling_data.csv", header=TRUE)
TradeData <- read.csv("raw data/UN_Trimmed_1988-2024.csv", header=TRUE) %>%
  mutate(Qty = Qty / 1e9) # Convert to Million Tonnes
code_mapping <- read_csv("raw data/country_code_bridge.csv")

# --- Get GDP Data ---
gdp_data <- wb_data("NY.GDP.MKTP.KD", start_date = 1990, end_date = 2024) %>%
  select(iso3c, country, date, NY.GDP.MKTP.KD) %>% 
  rename(WB_Code = iso3c, Country = country, Year = date, GDP = NY.GDP.MKTP.KD)

# --- Link Data & Process Regions ---
TradeData_Clean <- TradeData %>% left_join(code_mapping, by = "Country")

GDP_Clean <- gdp_data %>%
  inner_join(code_mapping, by = "WB_Code", relationship = "many-to-many") %>%
  select(Country.y, WB_Code, OECD_Code, Year, GDP) %>%
  rename(Country = Country.y)

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
  rename(Region_Waste_Produced = TOTAL, Region_Recycled_Amount = REC, Year = TIME_PERIOD, OECD_Code = LOCATION_CODES)

# --- Calculate Raw Components ---
Component_Data <- Country_GDP_Shares %>%
  inner_join(Regional_Data, by = c("OECD_Code", "Year")) %>%
  mutate(Est_Recycled_Amount = Region_Recycled_Amount * GDP_Share) %>%
  select(Country, Year, Est_Recycled_Amount)

Trade_Aggregated <- TradeData %>%
  group_by(Country, Year, flowCode) %>%
  summarise(Total_Qty = sum(Qty, na.rm = TRUE), .groups = 'drop') %>% 
  pivot_wider(id_cols = c(Country, Year), names_from = flowCode, values_from = Total_Qty, values_fill = 0) %>%
  rename(Imports = M, Exports = X)

# --- Merge & Clean Duplicates ---
Full_Dataset <- Component_Data %>%
  left_join(Trade_Aggregated, by = c("Country", "Year")) %>%
  mutate(Exports = replace_na(Exports, 0)) %>%
  # FIX 1: Remove duplicates immediately after join
  distinct(Country, Year, .keep_all = TRUE) %>%
  select(Country, Year, Est_Recycled_Amount, Exports)

# ==============================================================================
# 2. CREATE TSIBBLE & SPLIT DATA
# ==============================================================================

Model_Data <- Full_Dataset %>%
  as_tsibble(key = Country, index = Year) %>%
  fill_gaps() %>%
  mutate(
    Est_Recycled_Amount = zoo::na.approx(Est_Recycled_Amount, rule = 2),
    Exports = zoo::na.approx(Exports, rule = 2)
  ) %>%
  group_by_key() %>%
  filter(n() >= 10) %>%
  ungroup()

last_year <- max(Model_Data$Year)
train_data <- Model_Data %>% filter(Year <= last_year - 4)

# ==============================================================================
# 3. FORECAST COMPONENTS SEPARATELY
# ==============================================================================

print("Fitting models to components individually...")

# Model 1: Recycling Amount
fit_rec <- train_data %>%
  model(
    ETS_Recycled = ETS(Est_Recycled_Amount),
    ARIMA_Recycled = ARIMA(Est_Recycled_Amount)
  )

# Model 2: Exports
fit_exp <- train_data %>%
  model(
    ETS_Exports = ETS(Exports),
    ARIMA_Exports = ARIMA(Exports)
  )

# FIX 2: Convert to tibble immediately to avoid 'fable' distribution errors
fc_rec_tbl <- fit_rec %>% forecast(h = 4) %>% as_tibble()
fc_exp_tbl <- fit_exp %>% forecast(h = 4) %>% as_tibble()

# ==============================================================================
# 4. BOTTOM-UP CALCULATION & INTERVALS
# ==============================================================================

# Extract variances and means
clean_rec <- fc_rec_tbl %>%
  mutate(
    Model_Type = stringr::str_remove(.model, "_Recycled"),
    Var_Rec = distributional::variance(Est_Recycled_Amount)
  ) %>%
  rename(Mean_Rec = .mean) %>%
  select(Country, Year, Model_Type, Mean_Rec, Var_Rec)

clean_exp <- fc_exp_tbl %>%
  mutate(
    Model_Type = stringr::str_remove(.model, "_Exports"),
    Var_Exp = distributional::variance(Exports)
  ) %>%
  rename(Mean_Exp = .mean) %>%
  select(Country, Year, Model_Type, Mean_Exp, Var_Exp)

# Combine and Calculate Formula
fc_calculated <- clean_rec %>%
  inner_join(clean_exp, by = c("Country", "Year", "Model_Type")) %>%
  mutate(
    Forecasted_Processed_Own_Waste = Mean_Rec - Mean_Exp,
    Combined_SD = sqrt(Var_Rec + Var_Exp),
    Lo80 = Forecasted_Processed_Own_Waste - 1.28 * Combined_SD,
    Hi80 = Forecasted_Processed_Own_Waste + 1.28 * Combined_SD,
    Lo95 = Forecasted_Processed_Own_Waste - 1.96 * Combined_SD,
    Hi95 = Forecasted_Processed_Own_Waste + 1.96 * Combined_SD
  )

# Calculate Accuracy
actuals_calculated <- Model_Data %>%
  mutate(Actual_Processed_Own_Waste = Est_Recycled_Amount - Exports) %>%
  select(Country, Year, Actual_Processed_Own_Waste)

accuracy_check <- fc_calculated %>%
  left_join(actuals_calculated, by = c("Country", "Year")) %>%
  mutate(
    Error = Actual_Processed_Own_Waste - Forecasted_Processed_Own_Waste,
    Pct_Error = abs(Error / Actual_Processed_Own_Waste) * 100
  )

# ==============================================================================
# 5. VISUALIZATION 1: BOXPLOT (FIXED RANGE)
# ==============================================================================

country_mape <- accuracy_check %>%
  group_by(Country, Model_Type) %>%
  summarise(MAPE = mean(Pct_Error, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(MAPE))

custom_colors <- c("ARIMA" = "#4E79A7", "ETS" = "#F28E2B")

plot_boxplot <- country_mape %>%
  ggplot(aes(x = Model_Type, y = MAPE, fill = Model_Type)) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA, width = 0.5, color = "gray20") +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Bottom-Up Forecast Error (MAPE)",
    subtitle = "Accuracy when forecasting raw components first",
    y = "Mean Absolute Percentage Error (%)",
    x = NULL 
  ) +
  theme_minimal(base_size = 14) + 
  theme(legend.position = "none", panel.grid.major.x = element_blank()) +
  coord_cartesian(ylim = c(0, 300)) # FIX 3: Increased limit to 300%

print(plot_boxplot)

# ==============================================================================
# 6. VISUALIZATION 2: DIVERGENCE PLOT (SECOND WORST CASE)
# ==============================================================================

# Find the countries with the biggest disagreement
divergence_check <- country_mape %>%
  pivot_wider(names_from = Model_Type, values_from = MAPE) %>%
  mutate(Diff = abs(ARIMA - ETS)) %>%
  arrange(desc(Diff))

# --- CHANGE: Pick index [2] instead of [1] ---
interesting_country <- divergence_check$Country[2] 
arima_err <- round(divergence_check$ARIMA[2], 2)
ets_err <- round(divergence_check$ETS[2], 2)

# Prepare Data
plot_fc <- fc_calculated %>% 
  filter(Country == interesting_country) %>%
  rename(.model = Model_Type, Qty = Forecasted_Processed_Own_Waste)

plot_actuals <- actuals_calculated %>% 
  filter(Country == interesting_country) %>%
  rename(Qty = Actual_Processed_Own_Waste)

jump_off_year <- min(plot_fc$Year) - 1
jump_off_point <- plot_actuals %>% filter(Year == jump_off_year)

# Create connected lines dataset (explicit tibble conversion to prevent crash)
plot_lines_connected <- plot_fc %>%
  as_tibble() %>% 
  select(Country, Year, .model, Qty) %>%
  bind_rows(
    jump_off_point %>% mutate(.model = "ARIMA") %>% as_tibble(),
    jump_off_point %>% mutate(.model = "ETS") %>% as_tibble()
  ) %>%
  arrange(Year)

print(paste("Plotting Bottom-Up Divergence for (2nd worst):", interesting_country))

plot_divergence <- ggplot() +
  geom_ribbon(data = plot_fc, aes(x = Year, ymin = Lo95, ymax = Hi95, fill = .model), alpha = 0.2) + 
  geom_ribbon(data = plot_fc, aes(x = Year, ymin = Lo80, ymax = Hi80, fill = .model), alpha = 0.4) + 
  geom_line(data = plot_actuals, aes(x = Year, y = Qty), color = "black", size = 0.8) +
  
  # Forecast Lines
  geom_line(data = plot_lines_connected, aes(x = Year, y = Qty, color = .model), size = 1) +
  
  # Jump-off Anchor
  geom_point(data = jump_off_point, aes(x = Year, y = Qty), 
             color = "#D55E00", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  geom_text(data = jump_off_point, aes(x = Year, y = Qty, label = paste("", Year)), 
            hjust = 1.1, vjust = -0.5, fontface = "bold", size = 3.5, color = "#D55E00") +
  
  labs(
    title = paste("Bottom-Up Forecast Analysis:", interesting_country),
    subtitle = paste0("Model Comparison (Lower MAPE is better):\n",
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