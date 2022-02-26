library(tidyverse)
library(ggplot2)
library(scales)
library(maps)
library(mapproj)

# Incarceration Data
df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Scatter plot with trend line
capacity_vs_admitted <- df %>%
  filter(year >= 2008) %>%
  filter(jail_rated_capacity > 1) %>%
  filter(!is.na(jail_rated_capacity)) %>%
  filter(total_jail_adm > 1) %>%
  filter(!is.na(total_jail_adm)) %>%
  filter(state == "WA") %>%
  mutate(admitted_vs_capacity_diff = abs(total_jail_adm - jail_rated_capacity))

cap_vs_adm_plot <- ggplot(data = capacity_vs_admitted, mapping = aes(x = jail_rated_capacity, y = total_jail_adm)) + 
  geom_point() +
  geom_smooth() +
  geom_point(mapping = aes(y = admitted_vs_capacity_diff), color = "red", alpha = .3) +
  labs(title = "Jail Capacity Compared to Jail Admitted",
       x = "Jail Rated Capacity", y = "Total Inmates Admitted")
cap_vs_adm_plot

# Line Graph
ethnicty_compare_by_year <- df %>%
  filter(state == "WA") %>%
  filter(!is.na(aapi_jail_pop) | !is.na(black_jail_pop) | !is.na(white_jail_pop) |
         !is.na(latinx_jail_pop) | !is.na(native_jail_pop)) %>%
  filter(year >= 2008) %>%
  group_by(year) %>%
  summarise(aapi_jail_pop = sum(aapi_jail_pop), black_jail_pop = sum(black_jail_pop),
            white_jail_pop = sum(white_jail_pop), latinx_jail_pop = sum(latinx_jail_pop),
            native_jail_pop = sum(native_jail_pop))

# Chose to create the variable colors to act as a sort of dictionary to link a
# ethnicity to a color.
colors <- c("Asian and Pacific Islander Population" = "steelblue", "Black Population" = "red",
            "Latinx Population" = "purple", "White Population" = "orange", "Native American Population" = 
              "green")

line_plot_pop_by_ethnicity <- ggplot(ethnicty_compare_by_year, aes(x = year)) +
  geom_line(aes(y = aapi_jail_pop, color = "Asian and Pacific Islander Population")) +
  geom_line(aes(y = black_jail_pop, color = "Black Population")) +
  geom_line(aes(y = latinx_jail_pop, color = "Latinx Population")) +
  geom_line(aes(y = white_jail_pop, color = "White Population")) +
  geom_line(aes(y = native_jail_pop, color = "Native American Population")) +
  labs(title = "Inmate Population by Ethnicity", color = 'Ethnicity') +
  scale_color_manual(values = colors) +
  scale_x_continuous(name = "Years", limits = c(2008, 2018), expand = c(0,0)) +
  scale_y_continuous(name = "Population of Inmates", labels = comma)
line_plot_pop_by_ethnicity

# Map, Jail population in WA by county in 2018
# This map is useful to understand where people are in jail in the state of 
# Washington and gives a good understanding of where jails are located. Something
# I didn't realize until completing this map was that it really will also represent
# where the population is the densest.

jail_data_2018_WA <- df %>%
  filter(year == 2018) 

# Ran into a problem with pierce and san juan counties having multiple different
# polynames so had to come up with a way to join that to my map_data.
unique_fips <- (county.fips) %>%
  distinct(fips, .keep_all = TRUE) 
  
unique_fips$polyname <- sapply(strsplit(unique_fips$polyname, ":"), '[', 1)

county_shapes_US <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(unique_fips, by = "polyname")

map_data <- county_shapes_US %>%
  left_join(jail_data_2018_WA, by = "fips") %>%
  filter(state == "WA")

# Blank theme that is shown in the book to limit clutter.
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),     
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank() 
  )

jail_pop_in_WA_2018_map <- ggplot(map_data) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop), color = "black",
               size = 0.4) +
  coord_map() +
  scale_fill_continuous(name = "Population of People in Prison in WA 2018", limits = 
                          c(0, max(map_data$total_jail_pop)), low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Population of People in Prison by County in WA 2018")
jail_pop_in_WA_2018_map

  
# Summary Values

# This value shows how jail populations have increased by almost 1 million in a 
# 10 year time span. 
change_in_jail_pop_2008to2018 <- df %>%
  filter(state == "WA") %>%
  filter(year >= 2008) %>%
  group_by(year) %>%
  summarise(total_pop = sum(total_pop)) %>%
  pull(total_pop)
# Change in jail population in WA from 2018 to 2008 has increased by 973360
change_in_jail_pop_2008to2018 <- max(change_in_jail_pop_2008to2018) - min(change_in_jail_pop_2008to2018)

# The following values in the summary show the comparison of the amount of people in 
# Washington by ethnicity and the the amount of people in jail by ethnicity. I then chose
# to find the proportion of people of these two numbers to determine if there are
# more percentage of people in jail for there given ethnicity compared to the
# white population in Washington. This calculation was interesting because it 
# showed that Native Americans and Black people had a much higher chance of being 
# in jail than white people but Asian Americans/Pacific Islanders and the Latinx
# population had a lower chance than white people to be in jail based on the
# amount for each ethnicity in Washingtons population.

# White population compared to native population.
white_jail_pop_diff_to_native_2018 <- ethnicty_compare_by_year %>%
  filter(year == 2018) %>%
  summarise(diff = native_jail_pop / white_jail_pop) %>%
  pull(diff)

white_pop_diff_to_native_2018 <- df %>%
  filter(year == 2018) %>%
  filter(state == "WA") %>%
  group_by(year) %>%
  summarise(white_pop_15to64 = sum(white_pop_15to64), native_pop_15to64 = sum(native_pop_15to64)) %>%
  summarise(white_pop_compared_to_native_pop = native_pop_15to64/white_pop_15to64) %>%
  pull(white_pop_compared_to_native_pop)

# This shows the difference between Native Americans population in Washington to 
# white peoples population and how they have a much higher portion of people in 
# jail compared to the white population.
jail_vs_pop_native_to_white = white_jail_pop_diff_to_native_2018 - white_pop_diff_to_native_2018

# Same as above but Black population compared to White Population  
white_jail_pop_diff_to_black_2018 <- ethnicty_compare_by_year %>%
  filter(year == 2018) %>%
  summarise(diff = black_jail_pop / white_jail_pop) %>%
  pull(diff)

white_pop_diff_to_black_2018 <- df %>%
  filter(year == 2018) %>%
  filter(state == "WA") %>%
  group_by(year) %>%
  summarise(white_pop_15to64 = sum(white_pop_15to64), black_pop_15to64 = sum(black_pop_15to64)) %>%
  summarise(white_pop_compared_to_black_pop = black_pop_15to64/white_pop_15to64) %>%
  pull(white_pop_compared_to_black_pop)

# This shows the difference between the Black population in Washington to 
# white peoples population and how they have a much higher portion of people in 
# jail compared to the white population.
jail_vs_pop_black_to_white = white_jail_pop_diff_to_black_2018 - white_pop_diff_to_black_2018

# Same as above but Latinx Population compared to White population
white_jail_pop_diff_to_latinx_2018 <- ethnicty_compare_by_year %>%
  filter(year == 2018) %>%
  summarise(diff = latinx_jail_pop / white_jail_pop) %>%
  pull(diff)

white_pop_diff_to_latinx_2018 <- df %>%
  filter(year == 2018) %>%
  filter(state == "WA") %>%
  group_by(year) %>%
  summarise(white_pop_15to64 = sum(white_pop_15to64), latinx_pop_15to64 = sum(latinx_pop_15to64)) %>%
  summarise(white_pop_compared_to_latinx_pop = latinx_pop_15to64/white_pop_15to64) %>%
  pull(white_pop_compared_to_latinx_pop)

# This shows the difference between the LatinX population in Washington to 
# white peoples population and how they have a slightly lower portion of people in
# jail compared to the white population. 
jail_vs_pop_latinx_to_white = white_jail_pop_diff_to_latinx_2018 - white_pop_diff_to_latinx_2018

# Same as above but with the Asian American/Pacific Islander Population
white_jail_pop_diff_to_aapi_2018 <- ethnicty_compare_by_year %>%
  filter(year == 2018) %>%
  summarise(diff = aapi_jail_pop / white_jail_pop) %>%
  pull(diff)

white_pop_diff_to_aapi_2018 <- df %>%
  filter(year == 2018) %>%
  filter(state == "WA") %>%
  group_by(year) %>%
  summarise(white_pop_15to64 = sum(white_pop_15to64), aapi_pop_15to64 = sum(aapi_pop_15to64)) %>%
  summarise(white_pop_compared_to_aapi_pop = aapi_pop_15to64/white_pop_15to64) %>%
  pull(white_pop_compared_to_aapi_pop)

# This shows the difference between the Asian American/Pacific Islander population in Washington to 
# white peoples population and how they have a much lower portion of people in
# jail compared to the white population. 
jail_vs_pop_aapi_to_white = white_jail_pop_diff_to_aapi_2018 - white_pop_diff_to_aapi_2018



