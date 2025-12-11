library(tidyverse)
library(ggrepel)

PlasticTrade <- read.csv("UN_Trimmed_1988-2024.csv", header=TRUE)

# Looking at the data for china
# PlasticTrade %>%
#   filter(Country == "China") %>%
#   mutate(Qty_Billion = Qty / 1000000000) %>%  # Create the scaled column
#   ggplot(aes(x = Year, y = Qty_Billion, color = flowDesc)) +
#   geom_line(linewidth = 1) +
#   geom_point() +
#   scale_color_manual(values = c("Import" = "blue", "Export" = "red")) +
#   scale_y_continuous(labels = scales::comma) +
#   labs(title = "Import and Export Quantity for China Over Time",
#        x = "Year",
#        y = "Quantity (Billions)",  # Update the label
#        color = "Flow Type") +
#   theme_minimal()

# Looking at value of import and export plastic waste for china

# PlasticTrade %>%
#   filter(Country == "China") %>%
#   mutate(PrimaryValue_Billion = PrimaryValue / 1000000000) %>%  # Create the scaled column
#   ggplot(aes(x = Year, y = PrimaryValue_Billion, color = flowDesc)) +
#   geom_line(linewidth = 1) +
#   geom_point() +
#   scale_color_manual(values = c("Import" = "blue", "Export" = "red")) +
#   scale_y_continuous(labels = scales::comma) +
#   labs(title = "Import and Export Value for China Over Time",
#        x = "Year",
#        y = "Value (USD Billions)",  # Update the label
#        color = "Flow Type") +
#   theme_minimal()

# Looking at the value per kg for imports and exports in china

# PlasticTrade %>%
#   filter(Country == "China") %>%
#   mutate(ValuePerKg = PrimaryValue / Qty) %>%
#   ggplot(aes(x = Year, y = ValuePerKg, color = flowDesc)) +
#   geom_line(linewidth = 1) +
#   geom_point() +
#   scale_color_manual(values = c("Import" = "blue", "Export" = "red")) +
#   scale_y_continuous(labels = scales::comma) +
#   labs(title = "Value per Kg for China Over Time",
#        x = "Year",
#        y = "Value per Kg (USD)",  # Update the label
#        color = "Flow Type") +
#   theme_minimal()

# Looking at which countries import the most plastic waste in 2024

# PlasticTrade %>%
#   filter(Year == 2024, flowDesc == "Import") %>%
#   filter(Qty > 0, PrimaryValue > 0) %>%
#   ggplot(aes(x = PrimaryValue, y = Qty)) +
#   geom_point(alpha = 0.6, color = "blue") +
#   
#   # Set log scales
#   scale_x_log10(labels = scales::comma) +
#   scale_y_log10(labels = scales::comma) +
#   
#   # Add the log tick marks on the outside of the axes
#   annotation_logticks() + 
#   
#   labs(title = "Import Quantity vs Value for All Countries (2024)",
#        x = "Primary Value (USD)",
#        y = "Quantity") +
#   
#   theme_minimal() +
#   
#   # Explicitly style the grid lines to make the 'log lines' visible
#   theme(
#     panel.grid.major = element_line(color = "grey80"), # Major lines (powers of 10)
#     panel.grid.minor = element_line(color = "grey90")  # Minor lines (subdivisions)
#   )

# Comparing value per kg for exports to value per kg for imports in 2024

# plot_data <- PlasticTrade %>%
#   filter(Year == 2004) %>%
#   select(Country, flowDesc, Qty, PrimaryValue) %>%
#   pivot_wider(names_from = flowDesc, values_from = c(Qty, PrimaryValue)) %>%
#   filter(Qty_Import > 0, Qty_Export > 0, 
#          PrimaryValue_Import > 0, PrimaryValue_Export > 0) %>%
#   mutate(
#     Import_UnitValue = PrimaryValue_Import / Qty_Import,
#     Export_UnitValue = PrimaryValue_Export / Qty_Export,
#     Ratio = Export_UnitValue / Import_UnitValue
#   )
# 
# # Identify Extreme Values (Same as before)
# top_ratio <- plot_data %>% slice_max(order_by = Ratio, n = 3) %>% pull(Country)
# bottom_ratio <- plot_data %>% slice_min(order_by = Ratio, n = 3) %>% pull(Country)
# top_export <- plot_data %>% slice_max(order_by = Export_UnitValue, n = 3) %>% pull(Country)
# top_import <- plot_data %>% slice_max(order_by = Import_UnitValue, n = 3) %>% pull(Country)
# countries_to_label <- unique(c(top_ratio, bottom_ratio, top_export, top_import))
# 
# # Plot 
# plot_data %>%
#   ggplot(aes(x = Import_UnitValue, y = Export_UnitValue)) +
#   geom_point(alpha = 0.6, color = "steelblue", size = 2) +
#   
#   # --- THE KEY CHANGE IS HERE ---
#   geom_text_repel(
#     data = filter(plot_data, Country %in% countries_to_label),
#     aes(label = Country),
#     size = 3.5,
#     fontface = "bold",
#     box.padding = 0.5,   # Space around the label
#     point.padding = 0.3, # Space around the data point
#     force = 2            # Strength of the repulsion
#   ) +
#   # ------------------------------
# 
# geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
#   
#   scale_x_log10(labels = scales::dollar) +
#   scale_y_log10(labels = scales::dollar) +
#   annotation_logticks() +
#   
#   labs(title = "Export vs. Import Value per Kg (2004)",
#        x = "Import Value per Kg (USD)",
#        y = "Export Value per Kg (USD)") +
#   theme_minimal()

# # Comparing import and export value to define trade value in 2024
# plot_data <- PlasticTrade %>%
#   filter(Year == 2004) %>%
#   select(Country, flowDesc, PrimaryValue) %>%
#   pivot_wider(names_from = flowDesc, values_from = PrimaryValue) %>%
#   filter(Import > 0, Export > 0) %>%
#   mutate(
#     NetValue = Import - Export,
#     Status = ifelse(NetValue > 0, "Profit (Net Importer)", "Pay to remove (Net Exporter)"),
#     LogDistance = abs(log10(Import) - log10(Export))
#   )
# 
# # Identify 5 Extremes for EACH group
# extreme_countries <- plot_data %>%
#   group_by(Status) %>%                # Group by the side of the line
#   slice_max(order_by = LogDistance, n = 5) %>% # Take top 5 from each group
#   pull(Country)
# 
# # Plot
# ggplot(plot_data, aes(x = Export, y = Import, color = Status)) +
#   geom_point(alpha = 0.7, size = 3) +
#   
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
#   
#   # Label the specific countries identified above
#   geom_text_repel(
#     data = filter(plot_data, Country %in% extreme_countries),
#     aes(label = Country),
#     size = 3.5,
#     fontface = "bold",
#     box.padding = 0.5,
#     force = 2,
#     color = "black"
#   ) +
#   
#   scale_x_log10(labels = scales::dollar) +
#   scale_y_log10(labels = scales::dollar) +
#   annotation_logticks() +
#   
#   scale_color_manual(values = c("Profit (Net Importer)" = "green", 
#                                 "Pay to remove (Net Exporter)" = "red")) +
#   
#   labs(title = "Import vs. Export Value (2004)",
#        x = "Total Export Value (USD)",
#        y = "Total Import Value (USD)",
#        color = "Status") +
#   theme_minimal()

# # Plotting the most volatile countries, i.e. ones with the biggest fluctuations
# 
# top_volatile_countries <- PlasticTrade %>%
#   group_by(Country) %>%
#   summarise(volatility = sd(Qty, na.rm = TRUE)) %>%
#   arrange(desc(volatility)) %>%
#   head(6) %>% 
#   pull(Country)
# 
# # Plot (Overlaid)
# PlasticTrade %>%
#   filter(Country %in% top_volatile_countries) %>%
#   ggplot(aes(x = Year, y = Qty/1000000000, color = Country, linetype = flowDesc)) +
#   geom_line(linewidth = 1) +
#   
#   # Use comma formatting for the Y axis
#   scale_y_continuous(labels = scales::comma) +
#   
#   labs(title = "Plastic Waste Trade: Top 6 Volatile Countries",
#        y = "Quantity (billion kg)",
#        x = "Year",
#        color = "Country",
#        linetype = "Flow") +
#   theme_minimal()

# ----
# # Plotting price per unit variations for Cyprus!
# PlasticTrade %>%
#   filter(Country == "Cyprus", flowDesc == "Import") %>%
#   filter(Qty > 0, PrimaryValue > 0) %>%
#   mutate(UnitPrice = PrimaryValue / Qty) %>%
#   
#   ggplot(aes(x = Year, y = UnitPrice)) +
#   
#   # Line shows the trend
#   geom_line(color = "#2E86C1", alpha = 0.6, linewidth = 1) +
#   
#   geom_point(color = "#2E86C1", size = 2, alpha = 0.8) +
#   
#   scale_y_continuous(labels = scales::dollar) +
#   
#   labs(title = "Unit Price of Plastic Waste Imports: Cyprus",
#        x = "Year",
#        y = "Price per Unit (USD/kg)") +
#   
#   theme_minimal()

# Finding the most variable year for unit prices
most_volatile_year_row <- PlasticTrade %>%
  filter(Year >= 2004, Qty > 0, PrimaryValue > 0) %>%
  mutate(UnitPrice = PrimaryValue / Qty) %>%
  group_by(Year) %>%
  summarise(
    SD_Price = sd(UnitPrice, na.rm = TRUE)
  ) %>%
  arrange(desc(SD_Price)) %>% # Sort descending to find the highest SD
  slice(1)                    # Take the top 1

# Extract the actual year and the SD value for the label
target_year <- most_volatile_year_row$Year
sd_value <- round(most_volatile_year_row$SD_Price, 1)

print(paste("The most volatile year was", target_year, "with an SD of", sd_value))


# 2. Filter data for that specific year (Imports only)
plot_data_import <- PlasticTrade %>%
  filter(Year == target_year, Qty > 0, PrimaryValue > 0, flowDesc == "Import") %>%
  mutate(UnitPrice = PrimaryValue / Qty)


# 3. Identify the Top 5 Extreme Locations (The "Volatile" outliers)
top_5_outliers <- plot_data_import %>% 
  slice_max(order_by = UnitPrice, n = 3)


# 4. Plot
ggplot(plot_data_import, aes(x = 1, y = UnitPrice)) + 
  
  # Jitter points show the full distribution of prices
  geom_jitter(width = 0.1, size = 2.5, alpha = 0.6, color = "#2E86C1") +
  
  # Label the top 5 locations
  geom_text_repel(
    data = top_5_outliers,
    aes(label = paste(Country, "-", scales::dollar(UnitPrice))),
    size = 3.5,
    fontface = "bold",
    force = 15,
    box.padding = 0.6,
    segment.color = 'grey50',
    min.segment.length = 0
  ) +
  
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(limits = c(0.8, 1.2), breaks = NULL) +
  
  labs(title = paste("Import Price per Unit Distribution in", target_year),
       x = NULL,
       y = "Unit Price (USD/kg)") +
  
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )