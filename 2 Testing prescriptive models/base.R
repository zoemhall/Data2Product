library(tidyverse)
library(ggplot2)
library(broom)
library(wbstats)

# Combining import, export, total waste product, and recycling rates
# together to create the prescriptive analytics

RecyclingData <- read.csv("OECD_recycling_data.csv", header=TRUE)
TradeData <- read.csv("UN_Trimmed_1988-2024.csv", header=TRUE) %>%
  mutate(Qty = Qty / 1e9) # 1 Billion Kg = 1 Million Tonnes
code_mapping <- read_csv("country_code_bridge.csv")

# GDP data to split OECD regions into countries
gdp_data <- wb_data("NY.GDP.MKTP.KD", start_date = 1990, end_date = 2020) %>%
  select(iso3c, country, date, NY.GDP.MKTP.KD) %>% 
  rename(
    WB_Code = iso3c, 
    Country = country, 
    Year = date, 
    GDP = NY.GDP.MKTP.KD
  )

# Linking countries to OECD region for TradeData
TradeData_Clean <- TradeData %>%
  left_join(code_mapping, by = "Country")
  

# Total waste produced per region
# Total column from OECD data :)
WasteProduction <- RecyclingData %>%
  filter(PLASTICS_CODES == "TOTAL") %>%
  select(LOCATION_CODES, TIME_PERIOD, OBS_VALUE)

RecyclingProduction <- RecyclingData %>%
  filter(PLASTICS_CODES %in% c("REC", "TOTAL")) %>%
  pivot_wider(
    id_cols = c(Location, LOCATION_CODES, TIME_PERIOD), 
    names_from = PLASTICS_CODES, 
    values_from = OBS_VALUE
  )

# RecyclingRates <- RecyclingData %>%
#   filter(PLASTICS_CODES %in% c("REC", "TOTAL")) %>%
#   pivot_wider(
#     id_cols = c(Location, LOCATION_CODES, TIME_PERIOD), 
#     names_from = PLASTICS_CODES, 
#     values_from = OBS_VALUE
#   ) %>%
# 
#   mutate(RecyclingRate = REC / TOTAL) %>%
#   select(Location, LOCATION_CODES, TIME_PERIOD, RecyclingRate)

# Comparing recycling rates for each region over the three years on a plot
# ggplot(RecyclingRates, aes(x = TIME_PERIOD, y = RecyclingRate, color = LOCATION_CODES)) +
#   geom_line(size = 1) +
#   geom_point() +
#   labs(
#     title = "Recycling Rate by OECD Region",
#     x = "Year",
#     y = "Recycling Rate (%)",
#     color = "OECD Region"
#   ) +
#   theme_minimal()

# Rate of growth of recycling rates for each region
# RecyclingGrowthRates <- RecyclingRates %>%
#   group_by(LOCATION_CODES, Location) %>%
#   do(tidy(lm(RecyclingRate ~ TIME_PERIOD, data = .))) %>%
#   filter(term == "TIME_PERIOD") %>%
#   select(LOCATION_CODES, Location, estimate) %>%
#   arrange(desc(estimate)) %>%
#   rename(Gradient = estimate)
# write.csv(RecyclingGrowthRates, "recycling_rate_growth.csv", row.names = FALSE)

# Comparing to volume of plastic waste recycled per region
# ggplot(RecyclingProduction, aes(x = TIME_PERIOD, y = REC, color = LOCATION_CODES)) +
#   geom_line(size = 1) +
#   geom_point() +
#   labs(
#     title = "Recycling Production by Region",
#     x = "Year",
#     y = "Recycling Production (million tons)",
#     color = "OECD Region"
#   ) +
#   theme_minimal()

# ==========================================
# Processing their own waste = (waste produced * recycling rate) - exports
# Processing others waste = imports
# We're assuming imports are fully recyclable in the imported region
# ==========================================

# Processing their own waste first!
# OECD is in million tons, while UN is in kgs!
# We need to split the OECD regions waste production into estimates for countries
# Doing this using the GDP proportions of each country in the region

# Joining the bridge to the GDP data
GDP_Clean <- gdp_data %>%
  inner_join(code_mapping, by = "WB_Code", relationship = "many-to-many") %>%
  select(Country.y, WB_Code, OECD_Code, Year, GDP) %>%
  rename(Country = Country.y) 

# Total GDP for OECD regions
Region_GDP_Totals <- GDP_Clean %>%
  group_by(OECD_Code, Year) %>%
  summarise(Region_Total_GDP = sum(GDP, na.rm = TRUE), .groups = 'drop')

# Countries proportion
Country_GDP_Shares <- GDP_Clean %>%
  left_join(Region_GDP_Totals, by = c("OECD_Code", "Year")) %>%
  mutate(GDP_Share = GDP / Region_Total_GDP) %>%
  select(Country, OECD_Code, Year, GDP_Share)

# Putting OECD total and reycling rate together
Regional_Data <- RecyclingData %>%
  filter(PLASTICS_CODES %in% c("TOTAL", "REC")) %>%
  select(LOCATION_CODES, TIME_PERIOD, PLASTICS_CODES, OBS_VALUE) %>%
  pivot_wider(names_from = PLASTICS_CODES, values_from = OBS_VALUE) %>%
  rename(
    Region_Waste_Produced = TOTAL,
    Region_Recycled_Amount = REC,
    Year = TIME_PERIOD, 
    OECD_Code = LOCATION_CODES
  ) %>%
  mutate(Regional_Recycling_Rate = Region_Recycled_Amount / Region_Waste_Produced)

# Prescriptive!! Country waste production and recycling amounts
# Doing this way so that its the same methodology when we forecast :)
# I.e. This way meaning using recycling rate instead of amount recycled
Prescriptive_Country_Data <- Country_GDP_Shares %>%
  inner_join(Regional_Data, by = c("OECD_Code", "Year")) %>%
  mutate(
    Est_Waste_Produced = Region_Waste_Produced * GDP_Share,
    Est_Recycled_Amount = Est_Waste_Produced * Regional_Recycling_Rate
  ) %>%
  select(Country, Year, Est_Waste_Produced, Est_Recycled_Amount, Regional_Recycling_Rate)

# Making one big dataset!
# Merging with UN data (imports and export data)

# Splitting the trade quantities so that exp and imp are in different columns
# Splitting the trade quantities so that exp and imp are in different columns
Trade_Aggregated <- TradeData %>%
  group_by(Country, Year, flowCode) %>%
  summarise(
    Total_Qty = sum(Qty, na.rm = TRUE), 
    # ONLY FOR FORECASTING FILE
    Total_Value = sum(PrimaryValue, na.rm = TRUE), # <--- NEW LINE
    .groups = 'drop'
  ) %>% 
  pivot_wider(
    id_cols = c(Country, Year),
    names_from = flowCode,
    # values_from = Total_Qty,
    values_from = c(Total_Qty, Total_Value),
    values_fill = 0
  ) %>%
  rename(
    Imports = Total_Qty_M, 
    Exports = Total_Qty_X,
    # ONLY FOR FORECASTING FILE
    Import_Value = Total_Value_M, 
    Export_Value = Total_Value_X  
  )

Waste_Processing_Analysis <- Prescriptive_Country_Data %>%
  left_join(Trade_Aggregated, by = c("Country", "Year")) %>%
  mutate(
    Imports = replace_na(Imports, 0),
    Exports = replace_na(Exports, 0),
    # ONLY FOR FORECASTING FILE
    Import_Value = replace_na(Import_Value, 0), 
    Export_Value = replace_na(Export_Value, 0), 
    
    # amount a country processes its own waste
    Processed_Own_Waste = Est_Recycled_Amount - Exports,
    # and others!
    Processed_Others_Waste = Imports,
    
    # % of Own Waste Processed
    Percent_Own_Processed = (Processed_Own_Waste / Est_Waste_Produced) * 100
  )

write.csv(Waste_Processing_Analysis, "Final_Country_Waste_Analysis.csv", row.names = FALSE)
