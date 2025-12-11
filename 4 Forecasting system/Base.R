library(dplyr)
library(ggplot2)
library(zoo)
library(tidyr)
library(readr)

# ==============================================================================
# 1. SETUP: LOAD AND RESHAPE
# ==============================================================================
# Load the new dataset
raw_data <- read.csv("Core data/FOR FORECASTING.csv", stringsAsFactors = FALSE)

# Reshape "Wide" data (Columns) into "Long" data (Rows) so the original logic works
# We create the specific columns your script expects: Qty, PrimaryValue, flowDesc
imports <- raw_data %>%
  select(Country, Year, Qty = Imports, PrimaryValue = Import_Value) %>%
  mutate(flowDesc = "Import")

exports <- raw_data %>%
  select(Country, Year, Qty = Exports, PrimaryValue = Export_Value) %>%
  mutate(flowDesc = "Export")

# Combine them into the 'trade' dataframe your script looks for
trade <- bind_rows(imports, exports)
trade$Year <- as.integer(trade$Year)

# ==============================================================================
# 2. ORIGINAL IMPUTATION LOGIC (UNCHANGED)
# ==============================================================================

# Option B: Manually set the country to Spain (Make sure this country exists in your file)
target_country <- "Spain" 
message("Selected country for plotting: ", target_country)

# Calculate Global Reference Prices (Fallback)
price_ref <- trade %>%
  filter(Qty > 0, PrimaryValue > 0) %>%
  mutate(unit_price = PrimaryValue / Qty) %>%
  group_by(flowDesc) %>%
  summarise(global_unit_price = median(unit_price, na.rm = TRUE), .groups = "drop")

# Process Data for ALL Countries (Global Imputation)
df_global_imputed <- trade %>%
  left_join(price_ref, by = "flowDesc") %>%
  group_by(Country, flowDesc) %>% 
  arrange(Year) %>%
  mutate(
    # Calculate observed unit price where possible
    unit_price_raw = if_else(Qty > 0 & PrimaryValue > 0, PrimaryValue / Qty, NA_real_),
    
    # Interpolate prices over time
    unit_price_interp = if (sum(!is.na(unit_price_raw)) >= 2) {
      zoo::na.approx(unit_price_raw, x = Year, na.rm = FALSE)
    } else {
      unit_price_raw
    },
    # Fill remaining edges (forward/backward fill)
    unit_price_interp = zoo::na.locf(unit_price_interp, na.rm = FALSE),
    unit_price_interp = zoo::na.locf(unit_price_interp, fromLast = TRUE, na.rm = FALSE),
    
    # Select final price
    unit_price_final = case_when(
      !is.na(unit_price_interp) ~ unit_price_interp,
      !is.na(global_unit_price) ~ global_unit_price,
      TRUE ~ NA_real_
    ),
    
    # Fill Quantity using Value / Price
    Qty_filled = if_else(
      (is.na(Qty) | (Qty == 0 & PrimaryValue > 0)) & !is.na(unit_price_final) & unit_price_final > 0,
      PrimaryValue / unit_price_final,
      Qty
    )
  ) %>%
  ungroup()