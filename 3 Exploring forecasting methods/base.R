library(dplyr)
library(tsibble)
library(fpp3)
library(readr)
library(tidyr)
library(ggplot2)
library(zoo)


# Preparing the data!
OwnWaste <- read.csv("prescriptive data/Final_Country_Waste_Analysis copy.csv", header=TRUE) %>% 
  select(Country, Year, Processed_Own_Waste) %>%
  # filter(Country == "Canada") %>% # Canada is the most volatile
  rename(
    Qty = Processed_Own_Waste
  ) %>%
  mutate(Qty = Qty * 1e9) %>% # from 1 Million Tonnes to Kg
  as_tsibble(key = Country, index = Year) %>%
  fill_gaps() %>%
  # Using linear interpolation to fill the gaps!! Should fix Arima issues, ETS was able to run already
  mutate(Qty = zoo::na.approx(Qty, rule = 2)) 
  # group_by_key() %>%
  # filter(n() >= 5) %>%
  # ungroup()



# Displaying the first few rows
head(OwnWaste)


# Showing Historical data
plot1 <- OwnWaste %>%
  pivot_longer(cols = c(Qty), 
               names_to = "Waste_Type", values_to = "Amount") %>%
  autoplot(Amount) +
  labs(title = "Historical Waste Processing Trends (All Countries)",
       y = "Amount Processed (Kg)",
       x = "Year") +
  theme_minimal() +
  theme(legend.position = "none") 
print(plot1)


fit <- OwnWaste %>%
  model(
    # Model 1: ETS (Exponential Smoothing)
    ETS_Own = ETS(Qty),
    
    # Model 2: ARIMA
    # Note: ARIMA can be slower on large datasets with many keys. 
    # If it's too slow, comment out the ARIMA lines below.
    ARIMA_Own = ARIMA(Qty)
  )

print("Models fitted successfully.")

fc_own <- fit %>%
  select(ETS_Own, ARIMA_Own) %>%
  forecast(h = 10)

print("Plotting forecast for example country: USA")

plot2 <- fc_own %>%
  filter(Country == "Afghanistan") %>% 
  autoplot(OwnWaste) +
  labs(title = "10-Year Forecast: Processed Own Waste (Japan)",
       subtitle = "Comparison of ETS and ARIMA models",
       y = "Amount (Kg)") +
  theme_minimal()

print(plot2)

all_forecasts <- fc_own %>% 
  hilo(level = c(80, 95)) %>% 
  unpack_hilo(c("80%", "95%"))

# Show the first few rows of the result
print(head(all_forecasts))
