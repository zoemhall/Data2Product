library(wbstats)

RecyclingData <- read.csv("OECD_recycling_data.csv", header=TRUE)
TradeData <-  read.csv("UN_Trimmed_1988-2024.csv", header=TRUE)
oecd_mapping <- read_csv("oecd_mapping.csv")

# Matching GDP library country codes to OECD regions and UN country codes
# From the oecd_mapping.csv file

wb_countries <- wb_cachelist$countries %>%
  select(iso3c, country) %>%
  rename(WB_Name = country, WB_Code = iso3c) %>%
  mutate(Join_Name = WB_Name)

#Mismatched dictionary!
name_fixes <- c(
  "USA" = "United States",
  "Viet Nam" = "Vietnam",
  "Fed. Rep. of Germany (...1990)" = "Germany",
  "Belgium-Luxembourg (...1998)" = "Belgium", 
  "Bosnia Herzegovina" = "Bosnia and Herzegovina",
  "Rep. of Moldova" = "Moldova",
  "Serbia and Montenegro (...2005)" = "Serbia", 
  "Faroe Isds" = "Faroe Islands",
  "State of Palestine" = "West Bank and Gaza", 
  "China, Macao SAR" = "Macao SAR, China",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "China, Hong Kong SAR" = "Hong Kong SAR, China",
  "Wallis and Futuna Isds" = "Wallis and Futuna", # Doesn't match!
  "Cook Isds" = "Cook Islands", # Doesn't match!
  "Solomon Isds" = "Solomon Islands",
  "FS Micronesia" = "Micronesia, Fed. Sts.",
  "Congo" = "Congo, Rep.", 
  "Dem. Rep. of the Congo" = "Congo, Dem. Rep.", 
  "Gambia" = "Gambia, The",
  "Côte d'Ivoire" = "Cote d'Ivoire",
  "Sudan (...2011)" = "Sudan",
  "United Rep. of Tanzania" = "Tanzania",
  "Central African Rep." = "Central African Republic",
  "Saint Lucia" = "St. Lucia",
  "Saint Kitts and Nevis" = "St. Kitts and Nevis",
  "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
  "Dominican Rep." = "Dominican Republic",
  "Bahamas" = "Bahamas, The",
  "Turks and Caicos Isds" = "Turks and Caicos Islands",
  "Curaçao" = "Curacao",
  "Southern African Customs Union (...1999)" = "South Africa",
  "Mayotte (Overseas France)" = "France",
  "Guadeloupe (Overseas France)" = "France",
  "Martinique (Overseas France)" = "France",
  "Netherlands Antilles (...2010)" = "Netherlands Antilles", # Doesnt match!
  "Cayman Isds" = "Cayman Islands",
  "Korea, Rep." = "Korea, Rep.",
  "Rep. of Korea" = "Korea, Rep.",
  "South Korea" = "Korea, Rep.",
  "Slovakia" = "Slovak Republic",
  "Slovak Republic" = "Slovak Republic",
  "Czechia" = "Czech Republic",
  "Czech Republic" = "Czech Republic",
  "Russia" = "Russian Federation",
  "Russian Federation" = "Russian Federation",
  "Türkiye" = "Turkey",
  "Egypt" = "Egypt, Arab Rep.",
  "Iran" = "Iran, Islamic Rep.",
  "Venezuela" = "Venezuela, RB",
  "Yemen" = "Yemen, Rep.",
  "Kyrgyzstan" = "Kyrgyz Republic",
  "Lao People's Dem. Rep." = "Lao PDR",
  "Syria" = "Syrian Arab Republic",
  "Viet Nam" = "Vietnam"
)

Country_Bridge <- oecd_mapping %>%
  mutate(
    Match_Attempt_Name = case_when(
      Country %in% names(name_fixes) ~ name_fixes[Country],
      TRUE ~ Country
    )
  ) %>%
  left_join(wb_countries, by = c("Match_Attempt_Name" = "Join_Name")) %>%
  select(Country, OECD_Code, WB_Code, WB_Name)

Mismatches <- Country_Bridge %>%
  filter(is.na(WB_Code))

print(paste("Total Countries:", nrow(Country_Bridge)))
print(paste("Successfully Matched:", nrow(Country_Bridge) - nrow(Mismatches)))
print(paste("Failed Matches:", nrow(Mismatches)))

if(nrow(Mismatches) > 0) {
  print("The following countries don't have a match:")
  print(Mismatches$Country)
}

# write.csv(Country_Bridge, "country_code_bridge.csv", row.names = FALSE)