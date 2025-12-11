library(dplyr)
library(ggplot2)
library(zoo)

trade <- read.csv("UN_Trimmed_1988-2024.csv", stringsAsFactors = FALSE)
trade$Year <- as.integer(trade$Year)

# Select the Target Country for Plotting
# This variable controls which country's graph is generated, 
# but the full imputation runs on all data.
# Option A: Find the country with the most missing Qty but known Value (UAE)
# target_country_df <- trade %>%
#    filter((is.na(Qty) | Qty == 0) & (!is.na(PrimaryValue) & PrimaryValue > 0)) %>%
#    count(Country, sort = TRUE) %>%
#    slice(1)
# target_country <- target_country_df$Country[1]

# Option B: Manually set the country to Spain
target_country <- "Spain"

message("Selected country for plotting: ", target_country)

# Calculate Global Reference Prices (Fallback)
price_ref <- trade %>%
  filter(Qty > 0, PrimaryValue > 0) %>%
  mutate(unit_price = PrimaryValue / Qty) %>%
  group_by(flowDesc) %>%
  summarise(global_unit_price = median(unit_price, na.rm = TRUE), .groups = "drop")

# Process Data for ALL Countries (Global Imputation)
# The full imputation logic is applied here, grouped by Country and Flow.
df_global_imputed <- trade %>%
  left_join(price_ref, by = "flowDesc") %>%
  group_by(Country, flowDesc) %>% # Group by both Country and Flow for unit price calculation
  arrange(Year) %>%
  mutate(
    # Calculate observed unit price where possible
    unit_price_raw = if_else(Qty > 0 & PrimaryValue > 0, PrimaryValue / Qty, NA_real_),
    
    # Interpolate prices over time
    unit_price_interp = if (sum(!is.na(unit_price_raw)) >= 2) {
      # na.approx performs time-series interpolation
      zoo::na.approx(unit_price_raw, x = Year, na.rm = FALSE)
    } else {
      unit_price_raw
    },
    # Fill remaining edges (forward/backward fill)
    unit_price_interp = zoo::na.locf(unit_price_interp, na.rm = FALSE),
    unit_price_interp = zoo::na.locf(unit_price_interp, fromLast = TRUE, na.rm = FALSE),
    
    # Select final price (interpolated > global fallback)
    unit_price_final = case_when(
      !is.na(unit_price_interp) ~ unit_price_interp,
      !is.na(global_unit_price) ~ global_unit_price,
      TRUE ~ NA_real_
    ),
    
    # Fill Quantity
    Qty_filled = if_else(
      (is.na(Qty) | (Qty == 0 & PrimaryValue > 0)) & !is.na(unit_price_final) & unit_price_final > 0,
      PrimaryValue / unit_price_final,
      Qty
    )
  ) %>%
  ungroup()

# Filter the Global Data for Plotting the Target Country
df_country_plot <- df_global_imputed %>%
  filter(Country == target_country)

# Prepare Plot Data (Before vs After) - Using the filtered data
df_before <- df_country_plot %>%
  mutate(
    # Use original Qty for the "Before" plot, setting 0s/NAs to NA for clean gaps
    Plot_Qty = if_else(Qty > 0, Qty, NA_real_),
    State = "Before Imputation"
  )

df_after <- df_country_plot %>%
  mutate(
    Plot_Qty = Qty_filled,
    State = "After Imputation",
    # Flag rows where imputation occurred
    Is_Imputed = (is.na(Qty) | Qty == 0) & !is.na(Qty_filled)
  )

plot_data <- bind_rows(df_before, df_after)

# Force "Before" to appear on the left
plot_data$State <- factor(plot_data$State, levels = c("Before Imputation", "After Imputation"))

# Generate Plot
ggplot(plot_data, aes(x = Year, y = Plot_Qty)) +
  # Line and Points
  geom_line(aes(color = flowDesc), size = 0.8, na.rm = TRUE) +
  geom_point(aes(color = flowDesc), size = 1.5, na.rm = TRUE) +
  
  # Highlight Imputed points (only in 'After' panel)
  geom_point(data = filter(plot_data, State == "After Imputation" & Is_Imputed == TRUE),
             aes(y = Qty_filled), color = "red", shape = 4, size = 3, stroke = 1.5) +
  
  # Facet: Rows=Flow, Cols=State (Ensures Before is Left, After is Right)
  facet_grid(flowDesc ~ State, scales = "free_y") +
  
  # Labels and Theme
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = paste("Data Imputation: ", target_country),
    y = "Quantity (kg)",
    x = "Year",
    color = "Trade Flow"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )


# Clean up the global data to be ready for forecasting
df_forecast_global <- df_global_imputed %>%
  mutate(Qty = Qty_filled) %>%
  select(Year, CountryCode, Country, flowCode, flowDesc, Qty, PrimaryValue)

# Export the entire imputed dataset
write.csv(df_forecast_global, "raw_data_for_forecasting_ALL.csv", row.names = FALSE)