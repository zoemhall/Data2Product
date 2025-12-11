library(dplyr)
library(tsibble)
library(fpp3)
library(readr)
library(tidyr)
library(knitr)
library(zoo)

# ==============================================================================
# 1. SETUP: LOAD DATASETS & DEFINE SPLIT
# ==============================================================================
country_name <- "Spain" # Change to your target country
message("Running full validation for: ", country_name)

# DATASET A: RAW (Minimal gap filling so ARIMA works)
df_raw <- read.csv("Core data/FOR FORECASTING.csv") %>%
  filter(Country == country_name) %>%
  mutate(
    Est_Recycled_Amount = Est_Recycled_Amount * 1e9,
    Est_Waste_Produced = Est_Waste_Produced * 1e9
  ) %>%
  as_tsibble(index = Year) %>%
  fill_gaps() %>%
  mutate(across(where(is.numeric), ~zoo::na.approx(., na.rm=FALSE))) %>%
  mutate(across(where(is.numeric), ~zoo::na.locf(., na.rm=FALSE))) %>%
  mutate(across(where(is.numeric), ~zoo::na.locf(., fromLast=TRUE)))

# DATASET B: IMPUTED (High Quality)
df_imputed <- read.csv("Final_Country_Waste_Analysis_IMPUTED.csv") %>%
  filter(Country == country_name) %>%
  mutate(
    Est_Recycled_Amount = Est_Recycled_Amount * 1e9,
    Est_Waste_Produced = Est_Waste_Produced * 1e9
  ) %>%
  as_tsibble(index = Year)

# SPLIT (Train <= 2014, Test > 2014)
train_raw <- df_raw %>% filter(Year <= 2014)
train_imp <- df_imputed %>% filter(Year <= 2014)
test_data <- df_imputed %>% filter(Year > 2014) # Ground Truth

# ==============================================================================
# APPROACH 1: DIRECT FORECAST (Imputed Data)
# ==============================================================================
fit_direct <- train_imp %>%
  model(
    ARIMA = ARIMA(Est_Recycled_Amount),
    ETS   = ETS(Est_Recycled_Amount),
    Mean  = MEAN(Est_Recycled_Amount)
  )

fc_direct <- fit_direct %>% forecast(h = 5) %>% as_tibble()

res_direct <- fc_direct %>%
  select(Year, Model = .model, Derived_Recycled = .mean) %>%
  mutate(Approach = "1. Direct Forecast (Imputed)")

# ==============================================================================
# APPROACH 2: DYNAMIC BOTTOM-UP (Raw Data)
# Forecast Waste(Raw) * Rate(Raw)
# ==============================================================================
# Fit separate models
fit_raw_waste <- train_raw %>%
  model(
    W_ARIMA = ARIMA(Est_Waste_Produced),
    W_ETS   = ETS(Est_Waste_Produced),
    W_Mean  = MEAN(Est_Waste_Produced)
  )

fit_raw_rate <- train_raw %>%
  model(
    R_ARIMA = ARIMA(Regional_Recycling_Rate),
    R_ETS   = ETS(Regional_Recycling_Rate),
    R_Mean  = MEAN(Regional_Recycling_Rate)
  )

# Forecast
fc_raw_waste <- fit_raw_waste %>% forecast(h = 5) %>% as_tibble()
fc_raw_rate  <- fit_raw_rate %>% forecast(h = 5) %>% as_tibble()

# Clean & Combine
clean_raw_waste <- fc_raw_waste %>%
  mutate(Model = gsub("W_", "", .model)) %>%
  select(Year, Model, Waste_Val = .mean)

clean_raw_rate <- fc_raw_rate %>%
  mutate(Model = gsub("R_", "", .model)) %>%
  select(Year, Model, Rate_Val = .mean)

res_raw_dynamic <- clean_raw_waste %>%
  left_join(clean_raw_rate, by = c("Year", "Model")) %>%
  mutate(
    Derived_Recycled = Waste_Val * Rate_Val,
    Approach = "2. Dynamic Bottom-Up (Raw)"
  ) %>%
  select(Year, Model, Derived_Recycled, Approach)

# ==============================================================================
# APPROACH 3: CONTROLLED BOTTOM-UP (Imputed Data)
# Forecast Waste(Imputed) * Fixed Rate(Naive)
# ==============================================================================
# Fit separate models
fit_imp_waste <- train_imp %>%
  model(
    W_ARIMA = ARIMA(Est_Waste_Produced),
    W_ETS   = ETS(Est_Waste_Produced),
    W_Mean  = MEAN(Est_Waste_Produced)
  )

fit_imp_rate <- train_imp %>%
  model(R_Naive = NAIVE(Regional_Recycling_Rate))

# Forecast
fc_imp_waste <- fit_imp_waste %>% forecast(h = 5) %>% as_tibble()
fc_imp_rate  <- fit_imp_rate %>% forecast(h = 5) %>% as_tibble()

# Clean & Combine (Join every waste model to the single Naive rate)
rate_val_df <- fc_imp_rate %>% select(Year, Rate_Val = .mean)

res_imp_controlled <- fc_imp_waste %>%
  mutate(Model = gsub("W_", "", .model)) %>%
  left_join(rate_val_df, by = "Year") %>%
  mutate(
    Derived_Recycled = .mean * Rate_Val,
    Approach = "3. Controlled Bottom-Up (Imputed)"
  ) %>%
  select(Year, Model, Derived_Recycled, Approach)

# ==============================================================================
# FINAL COMPARISON TABLE
# ==============================================================================
all_results <- bind_rows(res_direct, res_raw_dynamic, res_imp_controlled)

accuracy_table <- all_results %>%
  left_join(test_data %>% select(Year, Actual = Est_Recycled_Amount), by = "Year") %>%
  group_by(Approach, Model) %>%
  summarise(
    RMSE = sqrt(mean((Derived_Recycled - Actual)^2, na.rm=TRUE)),
    MAE  = mean(abs(Derived_Recycled - Actual), na.rm=TRUE),
    MAPE = mean(abs((Derived_Recycled - Actual)/Actual), na.rm=TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(RMSE)

print(accuracy_table)
kable(accuracy_table, digits = 0, caption = "Full Model Comparison: Direct vs Raw vs Controlled")
write.csv(accuracy_table, "Full_Forecasting_Comparison.csv", row.names = FALSE)