# Load packages
library(tidyverse)

# Read the CSV file
greenspace_data <- read.csv("greenspace_data_share.csv", encoding = "latin1")

# Define South Asian countries
south_asia <- c("Afghanistan", "Bangladesh", "Bhutan", "India", 
                "Maldives", "Nepal", "Pakistan", "Sri Lanka")

# Filter to South Asian countries only
greenspace_sa <- greenspace_data %>%
  filter(Country %in% south_asia)

# Remove rows with missing 2021 NDVI values
greenspace_sa <- greenspace_sa %>%
  filter(!is.na(annual_avg_2021))

# Calculate average NDVI for each country (2021)
country_avg <- greenspace_sa %>%
  group_by(Country) %>%
  summarise(mean_ndvi_2021 = mean(annual_avg_2021, na.rm = TRUE)) %>%
  arrange(desc(mean_ndvi_2021))

# Plot: Average NDVI in 2021 by country
ggplot(country_avg, aes(x = reorder(Country, -mean_ndvi_2021), 
                        y = mean_ndvi_2021, 
                        fill = mean_ndvi_2021)) + 
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
  labs(
    title = "Average Urban Greenspace by Country in South Asia (2021)",
    x = "Country",
    y = "Mean NDVI (2021)"
  ) +
  scale_fill_gradient(low = "#c7e9c0", high = "#006d2c") +  # Light to dark green
  expand_limits(y = 0) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

ggplot(greenspace_sa, aes(x = HDI_level, y = annual_avg_2021, fill = HDI_level)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(
    values = c(
      "Low" = "#d9f0d3",      # light green
      "Medium" = "#74c476",   # medium green
      "High" = "#006d2c"      # dark green
    )
  ) +
  labs(
    title = "Urban Greenspace by HDI Level (2021)",
    x = "HDI Level",
    y = "NDVI Score (2021)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Run ANOVA test
anova_result <- aov(annual_avg_2021 ~ HDI_level, data = greenspace_sa)
summary(anova_result)
