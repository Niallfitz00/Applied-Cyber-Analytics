# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)




# Reading the dataset with the correct file path and extension
cyberthreat <- read_csv('C:/Cybersecurity Masters/Blair Applied Analytics/Assignment 3/Assignment3Project/cyberthreat.csv')
countries <- read_csv('C:/Cybersecurity Masters/Blair Applied Analytics/Assignment 3/Assignment3Project/countries.csv')
databreaches <- read_csv('C:/Cybersecurity Masters/Blair Applied Analytics/Assignment 3/Assignment3Project/databreaches.csv')
world_shape <- st_read("C:/Cybersecurity Masters/Blair Applied Analytics/Assignment 3/Assignment3Project/world-administrative-boundaries.shp")
head(world_shape)


# Inspect the first few rows and the structure of the dataset
head(cyberthreat)
head(databreaches)
head(countries)
str(cyberthreat)
str(countries)
str(databreaches)

#Data Preperation
cyberthreat <- na.omit(cyberthreat)
databreaches <- na.omit(databreaches)
countries <- na.omit(countries)


#Dataset 1: cyberthreats---------------------------------------------------

# Calculate the top 3 most common Attacktypes (or more if there are ties)
top_three_attacktypes <- cyberthreat %>%
  count(Attacktype, sort = TRUE) %>%
  top_n(n = 3, wt = n)

# Generate a rainbow color scheme based on the number of unique attack types included
num_of_attack_types <- nrow(top_three_attacktypes)
colors <- rainbow(num_of_attack_types)

# Plotting with Plotly
plot <- plot_ly(data = top_three_attacktypes, x = ~Attacktype, y = ~n, type = 'bar',
                marker = list(color = colors),
                text = ~paste('Count: ', n)) %>%
  layout(title = "Top Attack Types",
         xaxis = list(title = "Attack Type", tickangle = -45, tickfont = list(size = 10)),
         yaxis = list(title = "Frequency"))

# Render the plot
plot

# Get only rows with top 3 Attacktypes
top_attacktypes_data <- cyberthreat %>%
  filter(Attacktype %in% top_three_attacktypes$Attacktype)



# Calculate the percentage split of 'Protocol' within each 'Attacktype'
protocol_percentage <- top_attacktypes_data %>%
  group_by(Attacktype, Protocol) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Total = sum(Count), Percentage = (Count / Total) * 100) %>%
  select(Attacktype, Protocol, Percentage) %>%
  ungroup()

# Print the results
print(protocol_percentage)

ggplot(protocol_percentage, aes(x = Attacktype, y = Percentage, fill = Protocol)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = 0:12, limits = c(0, 12)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6, face = "bold"), # Adjusting x-axis text
        axis.title.x = element_text(size = 10, face = "bold"), # Adjusting x-axis title
        axis.title.y = element_text(size = 10, face = "bold")) + # Adjusting y-axis title
  labs(title = "Protocol Usage by Attack Type", x = "Attack Type", y = "Percentage")



#Dataset 2: DataBreaches---------------------------------------------------
# Analysis 1: Frequency of Data Breaches Over Years
# Ensuring 'Year' is treated as numeric
databreaches$Year <- as.numeric(as.character(databreaches$Year))

# Calculating and printing breaches per year
breaches_per_year <- databreaches %>%
  group_by(Year) %>%
  summarise(Count = n())

print(breaches_per_year)
# Frequency of Data Breaches Over Years
ggplot(breaches_per_year, aes(x = Year, y = Count)) +
  geom_line() + geom_point() +
  labs(title = "Frequency of Data Breaches Over Years", x = "Year", y = "Count") +
  theme_minimal()



# Analysis 2: Sectors Most Affected by Data Breaches

# Calculating and printing breaches by sector
breaches_by_sector <- databreaches %>%
  group_by(Sector) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(breaches_by_sector)




# Analysis 3: Most Common Methods of Data Breaches

# Removing complete duplicate rows
databreaches_unique <- databreaches %>%
  distinct()

# Calculating and printing common methods of breaches on the unique dataset
common_methods <- databreaches_unique %>%
  group_by(Method) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(common_methods)

# Analysis 4: Average Number of Records Affected by Breaches Per Sector

# Calculating and printing average records affected by breaches per sector
average_records_per_sector <- databreaches %>%
  group_by(Sector) %>%
  summarise(AverageRecords = mean(Records)) %>%
  arrange(desc(AverageRecords))

print(average_records_per_sector)




# Analysis 5: Average Number of Records Affected by Breaches Per Method

# Calculating and printing average records affected by breaches per method
average_records_per_method <- databreaches %>%
  group_by(Method) %>%
  summarise(AverageRecords = mean(Records)) %>%
  arrange(desc(AverageRecords))

print(average_records_per_method)

average_records_per_method <- databreaches %>%
  group_by(Method) %>%
  summarise(AverageRecords = mean(Records)) %>%
  arrange(desc(AverageRecords))

ggplot(average_records_per_method, aes(x = reorder(Method, AverageRecords), y = AverageRecords)) +
  geom_col() +
  theme_minimal() +
  coord_flip() + # Flip coordinates for better readability
  scale_y_continuous(labels = scales::comma) + # Use comma formatting for large numbers
  labs(title = "Average Records Affected by Breach Method", x = "Method", y = "Average Records")

#____________________________________________________________________________________________________
countries <- countries %>%
  mutate_at(vars(CEI, GCI, NCSI, DDL), as.numeric) %>% # Ensure numeric
  rowwise() %>%
  mutate(ThreatLevel = round(mean(c(CEI, GCI, NCSI, DDL), na.rm = TRUE), 2), # Aggregate and round scores
         Classification = case_when(
           ThreatLevel > 50 ~ "red",
           ThreatLevel > 25~ "yellow",
           TRUE ~ "green"
         )) %>%
  ungroup()


# Merge this data with the spatial data from 'world_shape'
world_data <- merge(world_shape, countries, by.x = "name", by.y = "CountryName", all.x = TRUE)
# Assigning a specific color for missing data
world_data$Classification[is.na(world_data$Classification)] <- "no data"
# Visualization with Leaflet, including "No Data" handling
leaflet(world_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~case_when(
    Classification == "red" ~ "red",
    Classification == "yellow" ~ "yellow",
    Classification == "green" ~ "green",
    TRUE ~ "grey"  # Grey color for no data
  ), fillOpacity = 0.8, color = "grey", weight = 0.5,
  popup = ~paste(name, "<br>Threat Level: ", ifelse(is.na(ThreatLevel), "No Data", ThreatLevel))) %>%
  addLegend(position = "bottomright",
            title = "Security Threat Classification",
            labels = c("High (Red)" = "Red", "Medium (Yellow)" = "Yellow", "Low (Green)" = "Green", "No Data" = "No Data"),
            colors = c("red", "yellow", "green", "grey"),
            opacity = 1)

