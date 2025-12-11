library(dplyr)
library(ggplot2)
library(readr)
library(ggtext) 

# Load Data
df <- read.csv("THIS ONE WORKS.csv")

# Calculate Volatility (Based on Industry Value now)
volatility_rank <- df %>%
  group_by(Country) %>%
  summarise(
    Volatility = sd(Local_Industry_Value, na.rm = TRUE) 
  ) %>%
  arrange(desc(Volatility)) %>%
  slice(1:5) # Pick Top 5

top_volatile_countries <- volatility_rank$Country

plot_data <- df %>%
  filter(Country %in% top_volatile_countries) %>%
  left_join(volatility_rank, by = "Country") %>%
  mutate(Label = paste0(
    "**", Country, "**<br>", 
    "<span style='font-size:8pt; color:#666666;'>SD: $", format(round(Volatility, 0), big.mark=","), "</span>"
  ))

# Generate Plot with Side Legend
ggplot(plot_data, aes(x = Year, y = Local_Industry_Value, color = Label)) +
  # Forecast Area Highlighting
  geom_rect(aes(xmin = 2019, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = "grey95", alpha = 0.01, color = NA) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "grey50") +
  
  # Lines & Points
  geom_line(size = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  
  labs(
    title = "Top 5 Most Volatile Countries: Industry Value",
    y = "Local Industry Value ($)",
    x = "Year",
    color = NULL 
  ) +
  theme_minimal() +
  theme(
    # --- PLACEMENT: RIGHT SIDE ---
    legend.position = "right", 
    legend.justification = "top", 
    
    # --- TEXT STYLING ---
    legend.text = element_markdown(size = 10, lineheight = 1.2),
    legend.key.height = unit(1.2, "cm"), 
    
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  guides(color = guide_legend(ncol = 1, override.aes = list(size = 3)))