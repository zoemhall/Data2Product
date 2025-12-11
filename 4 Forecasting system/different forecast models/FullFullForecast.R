library(dplyr)
library(tsibble)
library(fpp3)
library(readr)
library(tidyr)
library(zoo)

# ==============================================================================
# 1. DATA PREPARATION (shared)
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
trade <- trade %>%
  mutate(
    Raw_Unit_Price = ifelse(Qty > 0, PrimaryValue / (Qty * 1e9), 0),  # USD/kg
    is_outlier      = Raw_Unit_Price > 5.00 | (Qty * 1e9) < 1000,     # >$5/kg or <1 tonne
    Qty             = ifelse(is_outlier, NA, Qty),
    PrimaryValue    = ifelse(is_outlier, NA, PrimaryValue)
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
    unit_price_interp = if (sum(!is.na(unit_price_raw)) >= 2) {
      zoo::na.approx(unit_price_raw, x = Year, na.rm = FALSE)
    } else {
      unit_price_raw
    },
    unit_price_interp = zoo::na.locf(unit_price_interp, na.rm = FALSE),
    unit_price_interp = zoo::na.locf(unit_price_interp,
                                     fromLast = TRUE, na.rm = FALSE),
    unit_price_final  = coalesce(unit_price_interp, global_unit_price),
    Qty_filled = if_else(
      (is.na(Qty) | (Qty == 0 & PrimaryValue > 0)) &
        !is.na(unit_price_final) & unit_price_final > 0,
      PrimaryValue / unit_price_final,
      Qty
    )
  ) %>%
  ungroup()

# Reconstruct Wide Dataset
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
    Import_Value           = replace_na(Import_Value, 0),
    Export_Value           = replace_na(Export_Value, 0),
    Regional_Recycling_Rate = replace_na(Regional_Recycling_Rate, 0)
  )

# ==============================================================================
# 2. COMMON TSIBBLE FOR FORECASTING
# ==============================================================================
message("Step 2: Building common forecast input...")

vars_to_forecast <- c(
  "Est_Waste_Produced_Kg",
  "Est_Recycled_Amount_Kg",
  "Imports_Kg",
  "Exports_Kg",
  "Import_Value",
  "Export_Value",
  "Regional_Recycling_Rate"
)

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

# ==============================================================================
# 3A. SELF-SUFFICIENCY BRANCH (simple ARIMA + bootstrap)
# ==============================================================================
message("Step 3A: Forecasting for self-sufficiency (simple model)...")

set.seed(123)  # reproducible

fc_self <- forecast_input %>%
  model(ARIMA(Amount)) %>%
  generate(h = 20, times = 1, bootstrap = TRUE)

future_self <- fc_self %>%
  as_tibble() %>%
  select(Country, Year, Variable, Amount = .sim) %>%
  mutate(Amount = pmax(Amount, 0)) %>%
  pivot_wider(
    names_from  = Variable,
    values_from = Amount
  ) %>%
  mutate(
    Processed_Own_Waste = Est_Recycled_Amount_Kg - Exports_Kg,
    Percent_Own_Processed = if_else(
      Est_Waste_Produced_Kg > 0,
      (Processed_Own_Waste / Est_Waste_Produced_Kg) * 100,
      0
    ),
    Type = "Forecast"
  ) %>%
  select(Country, Year, Type, Percent_Own_Processed)

self_history <- history_data %>%
  mutate(
    Processed_Own_Waste = Est_Recycled_Amount_Kg - Exports_Kg,
    Percent_Own_Processed = if_else(
      Est_Waste_Produced_Kg > 0,
      (Processed_Own_Waste / Est_Waste_Produced_Kg) * 100,
      0
    ),
    Type = "History"
  ) %>%
  select(Country, Year, Type, Percent_Own_Processed)

self_df <- bind_rows(self_history, future_self) %>%
  rename(Self_Sufficiency = Percent_Own_Processed)

# ==============================================================================
# 3B. INDUSTRY VALUE BRANCH (improved engine + controlled noise)
# ==============================================================================
message("Step 3B: Forecasting for industry value (improved model)...")

# Historical variability for controlled noise
hist_stats <- forecast_input %>%
  as_tibble() %>%
  group_by(Country, Variable) %>%
  summarise(
    hist_sd = sd(Amount, na.rm = TRUE),
    .groups = "drop"
  )

# Improved ARIMA + extra noise
fc_ind <- forecast_input %>%
  model(ARIMA(Amount)) %>%
  generate(h = 20, times = 1, bootstrap = TRUE) %>%
  as_tibble() %>%
  left_join(hist_stats, by = c("Country", "Variable")) %>%
  mutate(
    hist_sd  = if_else(is.na(hist_sd) | hist_sd == 0, 0.001, hist_sd),
    noise_sd = hist_sd * 0.10,
    Amount   = .sim + rnorm(n(), 0, noise_sd),
    Amount   = pmax(Amount, 0)
  )

future_ind <- fc_ind %>%
  select(Country, Year, Variable, Amount) %>%
  tidyr::complete(Country, Year, Variable, fill = list(Amount = 0)) %>%
  mutate(
    Amount = if_else(
      Variable == "Regional_Recycling_Rate",
      pmin(pmax(Amount, 0), 100),
      Amount
    )
  ) %>%
  pivot_wider(
    names_from  = Variable,
    values_from = Amount
  ) %>%
  mutate(
    Processed_Own_Waste    = Est_Recycled_Amount_Kg - Exports_Kg,
    Processed_Others_Waste = Imports_Kg,
    Percent_Own_Processed  = if_else(
      Est_Waste_Produced_Kg > 0,
      (Processed_Own_Waste / Est_Waste_Produced_Kg) * 100,
      0
    ),
    Type = "Forecast"
  )

full_dataset_ind <- bind_rows(
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
  future_ind
) %>%
  arrange(Country, Year)

# ==============================================================================
# 4. PRICE CALCULATION & LOCAL INDUSTRY VALUE
# ==============================================================================
message("Step 4: Calculating prices and Local Industry Value...")

global_prices <- full_dataset_ind %>%
  filter(Imports_Kg > 1000) %>%  # Only trust shipments > 1 tonne
  group_by(Year) %>%
  summarise(
    Global_Avg_Price = median(Import_Value / Imports_Kg, na.rm = TRUE),
    .groups          = "drop"
  )

final_industry <- full_dataset_ind %>%
  left_join(global_prices, by = "Year") %>%
  group_by(Country) %>%
  mutate(
    Raw_Price = if_else(
      Imports_Kg > 1000,
      Import_Value / Imports_Kg,
      NA_real_
    ),
    Interp_Price = zoo::na.approx(Raw_Price, na.rm = FALSE),
    Interp_Price = zoo::na.locf(Interp_Price, na.rm = FALSE),
    Interp_Price = zoo::na.locf(Interp_Price, fromLast = TRUE, na.rm = FALSE),
    Final_Price  = coalesce(Interp_Price, Global_Avg_Price),
    Final_Price  = pmin(Final_Price, 2.50),
    Smoothed_Price = zoo::rollapply(
      Final_Price,
      width = 3,
      FUN = mean,
      align = "right",
      fill = "extend"
    ),
    Local_Industry_Value =
      (Processed_Own_Waste + Processed_Others_Waste) * Smoothed_Price
  ) %>%
  ungroup()

# ==============================================================================
# 5. MERGE: SELF-SUFFICIENCY (simple) + INDUSTRY VALUE (improved)
# ==============================================================================
message("Step 5: Merging branches...")

final_output <- final_industry %>%
  left_join(self_df, by = c("Country", "Year", "Type")) %>%
  select(
    Country, Year, Type,
    Est_Waste_Produced_Kg,
    Est_Recycled_Amount_Kg,
    Regional_Recycling_Rate,
    Imports_Kg, Exports_Kg,
    Import_Value, Export_Value,
    Processed_Own_Waste,
    Processed_Others_Waste,
    Percent_Own_Processed,   # from improved branch
    Self_Sufficiency,        # from simple branch (your preferred self-suff)
    Local_Industry_Value
  )

# ==============================================================================
# 6. ADD VOLATILITY COLUMNS (per country)
# ==============================================================================
message("Step 6: Computing volatility columns...")

vol_by_country <- final_output %>%
  group_by(Country) %>%
  summarise(
    Vol_Industry_Value   = sd(Local_Industry_Value, na.rm = TRUE),
    Vol_Self_Sufficiency = sd(Self_Sufficiency, na.rm = TRUE),
    .groups = "drop"
  )

final_output <- final_output %>%
  left_join(vol_by_country, by = "Country")

# ==============================================================================
# 7. SAVE FINAL SPREADSHEET
# ==============================================================================
message("Step 7: Saving final spreadsheet...")

write.csv(
  final_output,
  "AFullFullForecast.csv",
  row.names = FALSE
)

message("Done! Saved combined forecasts with volatility columns.")
