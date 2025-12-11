library(tidyverse)
library(scales)

# Plotting the Country Waste Analysis!
waste_data <- read_csv("Final_Country_Waste_Analysis.csv")

target_countries <- c(
  "Germany", "United Kingdom", "Japan", 
  "Canada", "Australia"
)

volatile_countries <-c(
  "China", "Mexico", "Canada", "USA"
)

# Mexico??

plot_data <- waste_data %>%
  filter(Country %in% volatile_countries) %>%
  filter(Year >= 1990 & Year <= 2019)

ggplot(plot_data, aes(x = Year, y = Percent_Own_Processed, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") + # The "Net Zero" line
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(scale = 1)) + # Format axis as %
  labs(
    title = "Plastic Waste Self-Sufficiency (1990-2019)",
    y = "% of Own Waste Processed Domestically",
    x = "Year",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

volume_data <- plot_data %>%
  select(Country, Year, Processed_Own_Waste, Processed_Others_Waste) %>%
  pivot_longer(
    cols = c("Processed_Own_Waste", "Processed_Others_Waste"),
    names_to = "Source",
    values_to = "Volume_Million_Tonnes"
  ) %>%
  mutate(
    Source_Label = case_when(
      Source == "Processed_Own_Waste" ~ "Domestic Waste",
      Source == "Processed_Others_Waste" ~ "Imported Waste"
    )
  )


ggplot(volume_data %>% filter(Country %in% volatile_countries),
       aes(x = Year, y = Volume_Million_Tonnes, fill = Source_Label)) +
  geom_area(alpha = 0.8, color = "white") + # Area chart shows volume accumulation
  facet_wrap(~Country, scales = "free_y") + # Separate panel for each country
  scale_fill_manual(values = c("Domestic Waste" = "#2E8B57", "Imported Waste" = "#4682B4")) +
  labs(
    title = "Composition of Processed Plastic Waste",
    y = "Million Tonnes",
    x = "Year",
    fill = "Source of Waste"
  ) +
  theme_light() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )