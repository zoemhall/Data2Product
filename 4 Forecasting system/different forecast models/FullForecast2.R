library(dplyr)
library(tsibble)
library(fpp3)
library(readr)
library(tidyr)
library(zoo)

# ==============================================================================
# 1. DATA PREPARATION
# ==============================================================================
message("Step 1: Loading, Cleaning Outliers, and Imputing...")

raw_data <- read.csv("Core data/FOR FORECASTING.csv", stringsAsFactors = FALSE)

# --- Reshape Trade Data ---
imports <- raw_data %>%
  select(Country, Year, Qty = Imports, PrimaryValue = Import_Value) %>%
  mutate(flowDesc = "Import")

exports <- raw_data %>%
  select(Country, Year, Qty = Exports, PrimaryValue = Export_Value) %>%
  mutate(flowDesc = "Export")

trade <- bind_rows(imports, exports)
trade$Year <- as.integer(trade$Year)

# --- OUTLIER REMOVAL (The Benin Fix) ---
# If unit price is absurd (> $5/kg) or quantity is tiny (< 1 tonne),
# we treat that record as missing so the imputation can replace it.
trade <- trade %>%
  mutate(
    Raw_Unit_Price = ifelse(Qty > 0, PrimaryValue / (Qty * 1e9), 0), # Price per Kg
    is_outlier = Raw_Unit_Price > 5.00 | (Qty * 1e9) < 1000,
    Qty         = ifelse(is_outlier, NA, Qty),
    PrimaryValue = ifelse(is_outlier, NA, PrimaryValue)
  )

# --- Imputation Logic ---
price_ref <- trade %>%
  filter(!is.na(Qty), Qty > 0, PrimaryValue > 0) %>%
  mutate(unit_price = PrimaryValue / Qty) %>%
  group_by(flowDesc) %>%
  summarise(global_unit_price = median(unit_price, na.rm = TRUE),
            .groups = "drop")

df_imputed <- trade %>%
  left_join(price_ref, by = "flowDesc") %>%
  group_by(Country, flowDesc) %>%
  arrange(Year) %>%
  mutate(
    unit_price_raw = if_else(Qty > 0 & PrimaryValue > 0,
                             PrimaryValue / Qty, NA_real_),
    
    # Interpolate price over time (with outliers removed)
    unit_price_interp = if (sum(!is.na(unit_price_raw)) >= 2) {
      zoo::na.approx(unit_price_raw, x = Year, na.rm = FALSE)
    } else {
      unit_price_raw
    },
    unit_price_interp = zoo::na.locf(unit_price_interp, na.rm = FALSE),
    unit_price_interp = zoo::na.locf(unit_price_interp,
                                     fromLast = TRUE, na.rm = FALSE),
    unit_price_final  = coalesce(unit_price_interp, global_unit_price),
    
    # Fill missing quantity from value and unit price
    Qty_filled = if_else(
      (is.na(Qty) | (Qty == 0 & PrimaryValue > 0)) &
        !is.na(unit_price_final) & unit_price_final > 0,
      PrimaryValue / unit_price_final,
      Qty
    )
  ) %>%
  ungroup()

# Reconstruct Wide Dataset from imputed trade data
df_imputed_wide <- df_imputed %>%
  select(Country, Year, flowDesc, Qty_filled, PrimaryValue) %>%
  pivot_wider(
    names_from  = flowDesc,
    values_from = c(Qty_filled, PrimaryValue)
  ) %>%
  rename(
    Imports      = Qty_filled_Import,
    Exports      = Qty_filled_Export,
    Import_Value = PrimaryValue_Import,
    Export_Value = PrimaryValue_Export
  )

# Final Historical Data
history_data <- raw_data %>%
  select(-Imports, -Exports, -Import_Value, -Export_Value) %>%
  left_join(df_imputed_wide, by = c("Country", "Year")) %>%
  mutate(
    Imports = replace_na(Imports, 0),
    Exports = replace_na(Exports, 0),
    
    Est_Recycled_Amount_Kg = Est_Recycled_Amount * 1e9,
    Est_Waste_Produced_Kg  = Est_Waste_Produced * 1e9,
    Exports_Kg             = Exports * 1e9,
    Imports_Kg             = Imports * 1e9,
    
    Import_Value = replace_na(Import_Value, 0),
    Export_Value = replace_na(Export_Value, 0),
    
    Regional_Recycling_Rate = replace_na(Regional_Recycling_Rate, 0)
  )

# ==============================================================================
# 2. SIMPLE IMPROVED FORECASTING ENGINE
# ==============================================================================
message("Step 2: Training Models & Generating Simulated Paths...")

vars_to_forecast <- c(
  "Est_Waste_Produced_Kg", 
  "Est_Recycled_Amount_Kg", 
  "Imports_Kg", 
  "Exports_Kg",
  "Import_Value",
  "Export_Value",
  "Regional_Recycling_Rate"
)

# 2.1 Prepare tsibble with gap-filling for ARIMA
forecast_input <- history_data %>%
  select(Country, Year, all_of(vars_to_forecast)) %>%
  pivot_longer(
    cols      = all_of(vars_to_forecast),
    names_to  = "Variable",
    values_to = "Amount"
  ) %>%
  as_tsibble(key = c(Country, Variable), index = Year) %>%
  fill_gaps() %>%
  group_by(Country, Variable) %>%
  mutate(
    Amount = zoo::na.approx(Amount, x = Year, na.rm = FALSE),
    Amount = zoo::na.locf(Amount, na.rm = FALSE),
    Amount = zoo::na.locf(Amount, fromLast = TRUE, na.rm = FALSE),
    Amount = replace_na(Amount, 0)
  ) %>%
  ungroup()

# 2.2 Historical variability per country-variable (for extra noise)
hist_stats <- forecast_input %>%
  as_tibble() %>%
  group_by(Country, Variable) %>%
  summarise(
    hist_sd = sd(Amount, na.rm = TRUE),
    .groups = "drop"
  )

# 2.3 Train ARIMA and generate forecasts with a bit of extra randomness
set.seed(123)  # for reproducibility

fc_output <- forecast_input %>%
  model(ARIMA(Amount)) %>%
  generate(h = 20, times = 1, bootstrap = TRUE) %>%
  as_tibble() %>%  # drop tsibble structure for easier joins
  left_join(hist_stats, by = c("Country", "Variable")) %>%
  mutate(
    # If series is flat, hist_sd may be 0 or NA, so give a small default
    hist_sd  = if_else(is.na(hist_sd) | hist_sd == 0, 0.001, hist_sd),
    noise_sd = hist_sd * 0.10,                    # 10% of historical variation
    Amount   = .sim + rnorm(n(), 0, noise_sd),    # add extra noise
    Amount   = pmax(Amount, 0)                    # no negative values
  )

# ==============================================================================
# 3. RECONSTRUCT FUTURE DATA & METRICS
# ==============================================================================
message("Step 3: Calculating Robust Prices & Metrics...")

future_data <- fc_output %>%
  select(Country, Year, Variable, Amount) %>%
  # Ensure we have a row for every Country–Year–Variable; fill missing with 0
  tidyr::complete(Country, Year, Variable, fill = list(Amount = 0)) %>%
  # Clamp recycling rate forecasts to [0, 100]
  mutate(
    Amount = if_else(
      Variable == "Regional_Recycling_Rate",
      pmin(pmax(Amount, 0), 100),
      Amount
    )
  ) %>%
  # Go back to wide format
  pivot_wider(names_from = Variable, values_from = Amount) %>%
  # Derived forecast metrics
  mutate(
    Processed_Own_Waste    = Est_Recycled_Amount_Kg - Exports_Kg,
    Processed_Others_Waste = Imports_Kg,
    Percent_Own_Processed  = if_else(
      Est_Waste_Produced_Kg > 0,
      (Processed_Own_Waste / Est_Waste_Produced_Kg) * 100,
      0
    )
  )

# ==============================================================================
# 4. COMBINE HISTORY + FORECAST, CALCULATE PRICES & INDUSTRY VALUE
# ==============================================================================
full_dataset <- bind_rows(
  history_data %>%
    mutate(
      Processed_Own_Waste    = Est_Recycled_Amount_Kg - Exports_Kg,
      Processed_Others_Waste = Imports_Kg,
      Percent_Own_Processed  = if_else(
        Est_Waste_Produced_Kg > 0,
        (Processed_Own_Waste / Est_Waste_Produced_Kg) * 100,
        0
      ),
      Type = "History"
    ),
  future_data %>% mutate(Type = "Forecast")
) %>%
  arrange(Country, Year)

# --- ROBUST PRICE CALCULATION (Using Cleaned Data) ---
global_prices <- full_dataset %>%
  filter(Imports_Kg > 1000) %>%  # Only trust shipments > 1 Tonne
  group_by(Year) %>%
  summarise(
    Global_Avg_Price = median(Import_Value / Imports_Kg, na.rm = TRUE),
    .groups          = "drop"
  )

final_output <- full_dataset %>%
  left_join(global_prices, by = "Year") %>%
  group_by(Country) %>%
  mutate(
    # 1. Raw price, only where we have enough import volume
    Raw_Price = if_else(Imports_Kg > 1000,
                        Import_Value / Imports_Kg, NA_real_),
    
    # 2. Interpolate price over time
    Interp_Price = zoo::na.approx(Raw_Price, na.rm = FALSE),
    Interp_Price = zoo::na.locf(Interp_Price, na.rm = FALSE),
    Interp_Price = zoo::na.locf(Interp_Price,
                                fromLast = TRUE, na.rm = FALSE),
    
    # 3. Fallback to global average if still missing
    Final_Price = coalesce(Interp_Price, Global_Avg_Price),
    
    # 4. Cap at $2.50/kg
    Final_Price = pmin(Final_Price, 2.50),
    
    # 5. Smooth price with 3-year rolling mean
    Smoothed_Price = zoo::rollapply(
      Final_Price, width = 3, FUN = mean,
      align = "right", fill = "extend"
    ),
    
    # 6. Local Industry Value
    Local_Industry_Value =
      (Processed_Own_Waste + Processed_Others_Waste) * Smoothed_Price
  ) %>%
  ungroup() %>%
  select(
    Country, Year, Type,
    Est_Waste_Produced_Kg,
    Est_Recycled_Amount_Kg,
    Regional_Recycling_Rate,
    Imports_Kg,
    Exports_Kg,
    Import_Value,
    Export_Value,
    Processed_Own_Waste,
    Processed_Others_Waste,
    Percent_Own_Processed,
    Local_Industry_Value
  )

write.csv(
  final_output,
  "Final_Waste_Forecast_20Years_2.1.csv",
  row.names = FALSE
)

message("Done! Saved with Improved Forecasting, Outlier Handling, and Price Caps.")
