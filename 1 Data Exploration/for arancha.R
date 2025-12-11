library(tidyverse)

# Read CSV (it's semicolon-separated)
data <- read.csv("UN_Trimmed_1988-2024.csv")

# Filter for China
china_data <- data %>% filter(Country == "China")

# Plot import values (blue)
plot(
  china_data$Year, china_data$ImpValue,
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Year",
  ylab = "Trade Value (USD)",
  main = "China Import (blue) and Export (red) Values"
)

# Add export values (red)
lines(
  china_data$Year, china_data$ExpValue,
  col = "red",
  lwd = 2
)

# Add legend
legend(
  "topleft",
  legend = c("Import", "Export"),
  col = c("blue", "red"),
  lwd = 2
)