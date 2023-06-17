#Upload Olympic Data
olympic_data <- read.csv("athlete_events.csv")
olympic_data

#Data Analysis
sum(olympic_data)
head(olympic_data)

#Install Packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tidyverse")
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)

#Heights Analysis (Summer Olympic Games)
olympic_data_not_na <- olympic_data[complete.cases(olympic_data),]
head(olympic_data_not_na)
season <- olympic_data_not_na$Season 
season
summer <- filter(olympic_data_not_na, olympic_data_not_na$Season == "Summer") #Filter Summer Games 
gold <- filter(summer, summer$Medal == "Gold") #Filter Gold Medalists

#Average Heights (Gold medalists over the years)
avg_height_women <- aggregate(Height ~ Sport, data = gold[gold$Sex == "F",], FUN = mean)
avg_height_women

avg_height_men <- aggregate(Height ~ Sport, data = gold[gold$Sex == "M",], FUN = mean)
avg_height_men

#Order the Average Heights
avg_height_women_ord <- avg_height_women[order(avg_height_women$Height), ] 
avg_height_women_ord

avg_height_men_ord <- avg_height_men[order(avg_height_men$Height), ]
avg_height_men_ord

#Top 5 Greater and Lower Average Heights
greater_avg_heights_women <- tail(avg_height_women_ord, 5)
greater_avg_heights_women

lower_avg_heights_women <- head(avg_height_women_ord, 5)
lower_avg_heights_women

greater_avg_heights_men <- tail(avg_height_men_ord, 5)
greater_avg_heights_men

lower_avg_heights_men <- head(avg_height_men_ord, 5)
lower_avg_heights_men

#Aggregate Height and Sport Through the Years
avg_height_years_women <- aggregate(Height ~ Sport + Year, data = gold[gold$Sex == "F",], FUN = mean)
avg_height_years_women

avg_height_years_men <- aggregate(Height ~ Sport + Year, data = gold[gold$Sex == "M",], FUN = mean)
avg_height_years_men

#Organize DataFrame to do the graphs
#Women
avg_height_per_year_women <- avg_height_years_women %>%
  pivot_wider(names_from = Year, values_from = Height)
avg_height_per_year_women

greater_avg_heights_women_per_year <- avg_height_per_year_women %>%
  filter(Sport %in% greater_avg_heights_women$Sport)
greater_avg_heights_women_per_year

lower_avg_heights_women_per_year <- avg_height_per_year_women %>%
  filter(Sport %in% lower_avg_heights_women$Sport)
lower_avg_heights_women_per_year

#Men
avg_height_per_year_men <- avg_height_years_men %>%
  pivot_wider(names_from = Year, values_from = Height)
avg_height_per_year_men

greater_avg_heights_men_per_year <- avg_height_per_year_men %>%
  filter(Sport %in% greater_avg_heights_men$Sport)
greater_avg_heights_men_per_year

lower_avg_heights_men_per_year <- avg_height_per_year_men %>%
  filter(Sport %in% lower_avg_heights_men$Sport)
lower_avg_heights_men_per_year

# Reorganizing Data to do the Graphs
reorganized_data_greater_w <- greater_avg_heights_women_per_year %>%
  pivot_longer(cols = -Sport, names_to = "Year", values_to = "Height") %>%
  mutate(Year = as.integer(gsub("`", "", Year)))  # Convert year to numeric format

# Graph Creation
colors <- c("#FE4C00", "#FFDF2B", "#0050F5", "#000000", "#2eb8ac")
graph <- ggplot(reorganized_data_greater_w, aes(x = Year, y = Height, color = Sport)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Year", y = "Height", title = "Highest Average Gold Medalist Heights per Sport - Women") +
  scale_color_manual(values = colors)   # Use personalized color pallet

# Show Graph
print(graph)

# Reorganizing Data to do the Graphs
reorganized_data_lower_w <- lower_avg_heights_women_per_year %>%
  pivot_longer(cols = -Sport, names_to = "Year", values_to = "Height") %>%
  mutate(Year = as.integer(gsub("`", "", Year)))  

# Graph Creation
colors <- c("#FE4C00", "#FFDF2B", "#0050F5", "#000000", "#2eb8ac")
graph <- ggplot(reorganized_data_lower_w, aes(x = Year, y = Height, color = Sport)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Year", y = "Height", title = "Lower Average Gold Medalist Heights per Sport - Women") +
  scale_color_manual(values = colors)   

# Show Graph
print(graph)

# Reorganizing Data to do the Graphs
reorganized_data_greater_m <- greater_avg_heights_men_per_year %>%
  pivot_longer(cols = -Sport, names_to = "Year", values_to = "Height") %>%
  mutate(Year = as.integer(gsub("`", "", Year)))  

# Graph Creation
colors <- c("#FE4C00", "#FFDF2B", "#0050F5", "#000000", "#2eb8ac")
graph <- ggplot(reorganized_data_greater_m, aes(x = Year, y = Height, color = Sport)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Year", y = "Height", title = "Highest Average Gold Medalist Heights per Sport - Men") +
  scale_color_manual(values = colors)   

# Show Graph
print(graph)

# Reorganizing Data to do the Graphs
reorganized_data_lower_m <- lower_avg_heights_men_per_year %>%
  pivot_longer(cols = -Sport, names_to = "Year", values_to = "Height") %>%
  mutate(Year = as.integer(gsub("`", "", Year)))  

# Graph Creation
colors <- c("#FE4C00", "#FFDF2B", "#0050F5", "#000000", "#2eb8ac")
graph <- ggplot(reorganized_data_lower_m, aes(x = Year, y = Height, color = Sport)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Year", y = "Height", title = "Lower Average Gold Medalist Heights per Sport - Men") +
  scale_color_manual(values = colors)  

# Show Graph
print(graph)

#Average Ages (Gold medalists over the years)
avg_age_women <- aggregate(Age ~ Sport, data = gold[gold$Sex == "F",], FUN = mean)
avg_age_women

avg_age_men <- aggregate(Age ~ Sport, data = gold[gold$Sex == "M",], FUN = mean)
avg_age_men

#Order the Average Ages
avg_age_women_ord <- avg_age_women[order(avg_age_women$Age), ] 
avg_age_women_ord

avg_age_men_ord <- avg_age_men[order(avg_age_men$Age), ]
avg_age_men_ord

#Top 5 Greater and Lower Average Ages
greater_avg_ages_women <- tail(avg_age_women_ord, 5)
greater_avg_ages_women

lower_avg_ages_women <- head(avg_age_women_ord, 5)
lower_avg_ages_women

greater_avg_ages_men <- tail(avg_age_men_ord, 5)
greater_avg_ages_men

lower_avg_ages_men <- head(avg_age_men_ord, 5)
lower_avg_ages_men

#Aggregate Age and Sport Through the Years
avg_age_years_women <- aggregate(Age ~ Sport + Year, data = gold[gold$Sex == "F",], FUN = mean)
avg_age_years_women

avg_age_years_men <- aggregate(Age ~ Sport + Year, data = gold[gold$Sex == "M",], FUN = mean)
avg_age_years_men

#Organize DataFrame to do the graphs
#Women
avg_age_per_year_women <- avg_age_years_women %>%
  pivot_wider(names_from = Year, values_from = Age)
avg_age_per_year_women

greater_avg_ages_women_per_year <- avg_age_per_year_women %>%
  filter(Sport %in% greater_avg_ages_women$Sport)
greater_avg_ages_women_per_year

lower_avg_ages_women_per_year <- avg_age_per_year_women %>%
  filter(Sport %in% lower_avg_ages_women$Sport)
lower_avg_ages_women_per_year

#Men
avg_age_per_year_men <- avg_age_years_men %>%
  pivot_wider(names_from = Year, values_from = Age)
avg_age_per_year_men

greater_avg_ages_men_per_year <- avg_age_per_year_men %>%
  filter(Sport %in% greater_avg_ages_men$Sport)
greater_avg_ages_men_per_year

lower_avg_ages_men_per_year <- avg_age_per_year_men %>%
  filter(Sport %in% lower_avg_ages_men$Sport)
lower_avg_ages_men_per_year

# Reorganizing Data to do the Graphs
reorganized_data_greater_w <- greater_avg_ages_women_per_year %>%
  pivot_longer(cols = -Sport, names_to = "Year", values_to = "Age") %>%
  mutate(Year = as.integer(gsub("`", "", Year)))  # Convert year to numeric format

# Graph Creation
colors <- c("#FE4C00", "#FFDF2B", "#0050F5", "#000000", "#2eb8ac")
graph <- ggplot(reorganized_data_greater_w, aes(x = Year, y = Age, color = Sport)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Year", y = "Age", title = "Highest Average Gold Medalist Ages per Sport - Women") +
  scale_color_manual(values = colors)   # Use personalized color pallet

# Show Graph
print(graph)

# Reorganizing Data to do the Graphs
reorganized_data_lower_w <- lower_avg_ages_women_per_year %>%
  pivot_longer(cols = -Sport, names_to = "Year", values_to = "Age") %>%
  mutate(Year = as.integer(gsub("`", "", Year)))  

# Graph Creation
colors <- c("#FE4C00", "#FFDF2B", "#0050F5", "#000000", "#2eb8ac")
graph <- ggplot(reorganized_data_lower_w, aes(x = Year, y = Age, color = Sport)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Year", y = "Age", title = "Lower Average Gold Medalist Heights per Sport - Women") +
  scale_color_manual(values = colors)   

# Show Graph
print(graph)

# Reorganizing Data to do the Graphs
reorganized_data_greater_m <- greater_avg_ages_men_per_year %>%
  pivot_longer(cols = -Sport, names_to = "Year", values_to = "Age") %>%
  mutate(Year = as.integer(gsub("`", "", Year)))  

# Graph Creation
colors <- c("#FE4C00", "#FFDF2B", "#0050F5", "#000000", "#2eb8ac")
graph <- ggplot(reorganized_data_greater_m, aes(x = Year, y = Age, color = Sport)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Year", y = "Age", title = "Highest Average Gold Medalist Ages per Sport - Men") +
  scale_color_manual(values = colors)   

# Show Graph
print(graph)

# Reorganizing Data to do the Graphs
reorganized_data_lower_m <- lower_avg_ages_men_per_year %>%
  pivot_longer(cols = -Sport, names_to = "Year", values_to = "Age") %>%
  mutate(Year = as.integer(gsub("`", "", Year)))  

# Graph Creation
colors <- c("#FE4C00", "#FFDF2B", "#0050F5", "#000000", "#2eb8ac")
graph <- ggplot(reorganized_data_lower_m, aes(x = Year, y = Age, color = Sport)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Year", y = "Age", title = "Lower Average Gold Medalist Ages per Sport - Men") +
  scale_color_manual(values = colors)  

# Show Graph
print(graph)

