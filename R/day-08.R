# Bella Conrad
# February 21, 2025

# Question 1

# Step 1- Read into the COVID-19 Data
library(tidyverse)
library(readr)
covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# Step 2- Create a new data.frame
df <- data.frame(region = state.region,
                 state = state.name,
                 abbr = state.abb)
head(df)
# region      state abbr
# 1  South    Alabama   AL
# 2   West     Alaska   AK
# 3   West    Arizona   AZ
# 4  South   Arkansas   AR
# 5   West California   CA
# 6   West   Colorado   CO

# Step 3- Join new data.frame to raw COVID data
# Step 4- Split-apply the joined data to determine the daily, cumulative, cases and deaths for each region
# Step 5- Pivot your data from wide format to long
# Step 6- Plot your data
plot_8 <- inner_join(df, covid_data, by = "state") %>% 
  group_by(region, date) %>% 
  summarize(cases = sum(cases),
            deaths = sum(deaths)) %>% 
  pivot_longer(cols = c(cases, deaths),
               names_to = "type",
               values_to = "count") %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_grid(type~region, scales = "free_y") +
  theme_minimal()
print(plot_8)
# Save image
ggsave("Covid_19_US_Region.png", plot = plot_8, width = 10, height = 6, dpi = 300)

