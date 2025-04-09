# Load necessary libraries
library(tidyverse)

# Read the CSV file
greenspace_data <- read.csv("greenspace_data_share.csv", encoding = "latin1")
# Define South Asian countries
south_asia <- c("Afghanistan", "Bangladesh", "Bhutan", "India", 
                "Maldives", "Nepal", "Pakistan", "Sri Lanka")

# Filter to South Asian countries only
greenspace_sa <- greenspace_data %>%
  filter(Country %in% south_asia)

# Remove any rows with missing values in key columns
greenspace_sa <- greenspace_sa %>%
  filter(!is.na(annual_avg_2010) & !is.na(annual_avg_2021))

# Add a new column showing change over time
top_change <- greenspace_sa %>%
  arrange(change_ndvi) %>%
  slice(c(1:10, (n() - 9):n()))  # 10 lowest and 10 highest changes

# Plot bar chart
ggplot(top_change, aes(x = reorder(City, change_ndvi), y = change_ndvi, fill = Country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Cities with Greatest Change in Urban Greenspace (2010–2021)",
    x = "City",
    y = "NDVI Change (2021 - 2010)"
  ) +
  theme_minimal()

# Paired t-test to compare 2010 and 2021 greenness
t.test(greenspace_sa$annual_avg_2021, greenspace_sa$annual_avg_2010, paired = TRUE)

# Bar chart of mean greenspace by country (2021)
greenspace_sa %>%
  group_by(Country) %>%
  summarise(mean_greenspace = mean(annual_avg_2021, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Country, -mean_greenspace), y = mean_greenspace, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Urban Greenspace by Country (2021)",
    x = "Country",
    y = "Mean NDVI"
  ) +
  theme_minimal()
    
  # Scatterplot of greenspace vs. HDI level (as categorical or numeric)
  ggplot(greenspace_sa, aes(x = HDI_level, y = annual_avg_2021, fill = HDI_level)) +
      geom_boxplot() +
      labs(
        title = "Urban Greenspace by HDI Level (2021)",
        x = "HDI Level",
        y = "NDVI (2021)"
      ) +
      theme_minimal()
    
