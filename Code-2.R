#Global Terrorism Database(GTD) visualization 

#Loading the libraries used
library(readr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggrepel)
library(stringr)
library(gsubfn)
library(proto)
library(ggplot2)
library(CGPfunctions)
library(terra)
library(spData)
library(spDataLarge)
library(sf)
library(tidyverse)
library(magrittr)
library(tidyr)
library(tmap)
library(viridis) # for a visually appealing color palette
library(forcats) # for reordering factor levels
library(tmaptools)
library(lubridate)
library(RColorBrewer)
library(readxl)
library(sp)
library(ggmap)
library(ggthemes)


#DATA CLEANING AND PREPARATION

#loading the global terrorism database
GTD <- read_csv("globalterrorismdb.csv")

#chcking the structure and examining the variables
View(GTD)
str(GTD)
summary(GTD)

#dropping the non useful columns because the columns are too many (very large dataset)
GTD <- subset(GTD, select = -c (extended, resolution,location, alternative,
                                attacktype2, attacktype2_txt, attacktype3,
                                attacktype3_txt,targtype2, targtype2_txt,
                                targsubtype2, targsubtype2_txt, corp2, target2,
                                natlty2, natlty2_txt, targtype3, targtype3_txt,
                                targsubtype3, targsubtype3_txt, corp3, target3, 
                                natlty3,natlty3_txt, gsubname,gname2, gsubname2,
                                gname3, gsubname3, motive,guncertain2, 
                                guncertain3, claimmode, claimmode_txt, claim2, 
                                claimmode2, claimmode2_txt, claim3, claimmode3, 
                                claimmode3_txt, compclaim, weaptype2, 
                                weaptype2_txt, weapsubtype2,weapsubtype2_txt, 
                                weaptype3, weaptype3_txt, weapsubtype3, 
                                weapsubtype3_txt, weaptype4, weaptype4_txt, 
                                propextent, propextent_txt,propvalue, 
                                propcomment, nhostkid, nhostkidus, nhours, 
                                ndays, divert, kidhijcountry, ransom, ransomamt,
                                ransomamtus, ransompaid, ransompaidus, 
                                ransomnote, hostkidoutcome, hostkidoutcome_txt, 
                                nreleased, addnotes, scite2, scite3, related,
                                weapsubtype4, weapsubtype4_txt, weapdetail,
                                approxdate, alternative_txt, corp2, 
                                attacktype2_txt, attacktype3, attacktype3_txt )) 
#viewing the dataset
View(GTD)

#renaming entries for the month variable
GTD$imonth = as.factor(GTD$imonth)
GTD$imonth <- recode(GTD$imonth, 
                             "1" = 'Jan', 
                             "2" = 'Feb',
                             "3" = 'Mar',
                             "4" = 'Apr',
                             "5 "= 'May',
                             "6 "= 'Jun')

#renaming the columns
GTD = rename(GTD, country_no = country )
GTD = rename(GTD, country = country_txt )
GTD = rename(GTD, month = imonth )
GTD = rename(GTD, day = iday )
GTD = rename(GTD, year = iyear )
GTD = rename(GTD, region_no = region )
GTD = rename(GTD, region = region_txt )
GTD = rename(GTD, attacktype = attacktype1_txt )
GTD = rename(GTD, targettype = targtype1_txt )
GTD = rename(GTD, targetsubtype = targsubtype1_txt )
GTD = rename(GTD, weapontype = weaptype1_txt )

str(GTD)

#declaring the categorical variables
GTD$country = as.factor(GTD$country)
GTD$region = as.factor(GTD$region)
GTD$attacktype = as.factor(GTD$attacktype)
GTD$targettype = as.factor(GTD$targettype)
GTD$targetsubtype = as.factor(GTD$targetsubtype)
GTD$weapontype = as.factor(GTD$weapontype)

str(GTD)


#extracting the date from the summary column
#the summary column was a mix of text and numbers (date)
GTD <- GTD %>%
  mutate(date = str_extract(summary, "([0-9]{1,2}/){2}[0-9]{4}"))

View(GTD)

# Convert the "date" column to a Date format
GTD$date <- as.Date(GTD$date, format = "%m/%d/%Y")


#VISUALIZATIONS 

# Visualizing the most common weapons usage over time
#subseting the GTD dataset 
time_weapon_plot <- GTD %>%
  group_by(year, weapontype) %>%
  #counting the number of attacks
  summarise(num_attacks = n()) %>%
  ggplot(aes(x = year, y = num_attacks, color = weapontype)) +
  geom_line(size = 1.2) +
  scale_color_viridis_d(option = "plasma") + # changing the color palette
  labs(title = "Most Common Weapons Usage Over Time",
       x = "Year",
       y = "Number of Attacks",
       color = "Weapon Type") +
  theme_ipsum() + # change theme
  theme(
    legend.position = "bottom", 
    legend.box.background = element_rect(color = "black"),
    legend.title = element_text(face = "bold", size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 12, color = "black")
  ) +
  # customizing x-axis labels
  scale_x_continuous(breaks = seq(min(GTD$year), max(GTD$year), by = 5)) + 
  #using the geom_text_repel function, which displays the weapon type names for those years
  geom_text_repel(
    data = . %>% filter(year %in% c(1998, 2001, 2004, 2007, 2010, 
                                    2013, 2016, 2019)), # adding labels for years
    aes(label = weapontype),
    hjust = 0,
    direction = "y",
    fontface = "bold",
    color = "black"
  )
#showing the plot
time_weapon_plot



# Creating a bar chart that shows the number of terrorist attacks 
#by year from 1970 to 2020

# Selecting relevant variables and grouping by year
attacks_by_year <- GTD %>% 
  select(year) %>% 
  group_by(year) %>% 
  summarize(num_attacks = n())

# creating a vector of color codes that will be used to fill the bars in the bar chart. 
#It ranges from light red to dark red
color_palette <- c("#f8c4c4", "#f59b9b", "#ee6e73", "#e53935", "#b71c1c")

# Ploting the bar chart
#The data frame attacks_by_year is mapped to the x-axis (year) 
#and y-axis (num_attacks). The fill of the bars is also set to num_attacks.
ggplot(attacks_by_year, aes(x = year, y = num_attacks, fill = num_attacks)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#f8c4c4", high = "#b71c1c", 
                      name = "Number of Attacks (K)", guide = "colorbar", 
                      breaks = seq(0, 15000, by = 2500), 
                      labels = c(" 0 ", " 2.5 ", " 5 ", " 7.5 ", "10", "12.5", "15")) +
  scale_y_continuous(expand = c(0,0), name = "Number of Attacks ") +
  scale_x_continuous(expand = c(0,0), name = "Year") +
  ggtitle("Global Terrorism Database: Number of Attacks by Year (1970-2020)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  #adding text labels to the top of each bar
  geom_text(aes(label = num_attacks, vjust = -0.5), size = 2, fontface = "bold", color = "black") 


#Creating a stacked bar chart showing the number of terrorist attacks by year 
#and region.

#choosing color palette
display.brewer.all()
display.brewer.all(colorblindFriendly = TRUE)
# Viewing a single RColorBrewer palette by specifying its name
display.brewer.pal(n = 8, name = 'Blues')
# To extract the hexadecimal color specification 
brewer.pal(n = 8, name = "Reds")
brewer.pal(n = 8, name = "Oranges")

# Selecting relevant variables and group by year and region
attacks_by_year_region <- GTD %>% 
  select(year, region) %>% 
  group_by(year, region) %>% 
  summarize(num_attacks = n())

# Creating a color palette
color_palette <- c("#FFF5F0", "#FEE0D2" ,"#FCBBA1", "#FC9272", "#FB6A4A",
                            "#EF3B2C" ,"#CB181D", "#99000D", 
                            "#FD8D3C", "#F16913", "#D94801", "#8C2D04")

# Plotting the bar chart
ggplot(attacks_by_year_region, aes(x = year, y = num_attacks, fill = region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = color_palette, name = "Region") +
  #scaling the x and y axis
  scale_y_continuous(expand = c(0,0), name = "Number of Attacks") +
  scale_x_continuous(expand = c(0,0), name = "Year") +
  ggtitle("Global Terrorism Database: Number of Attacks by Year 
          and Region (1970-2020)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  geom_text(aes(label = num_attacks, vjust = -0.5), size = 2, 
            fontface = "bold", color = "white") 

#Creating a heatmap showing the number of terrorist attacks by 
#region and month 

# Creating a data frame that contains counts of attacks by region and month
GTD_counts <- GTD %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(region) %>%
  #adding a new column to the data frame that contains the month extracted 
  #from the date column
  mutate(region_count = n()) %>%
  group_by(region, month) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  #converting the region column to a factor variable and ordering it by the total 
  #number of attacks in descending order
  mutate(region = factor(region, levels = names(sort(table(GTD$region), 
                                                     decreasing = TRUE))))
#creating a variable to store the order of the regions by the total 
#number of attacks.
region_order <- GTD_counts %>% 
  group_by(region) %>% 
  summarize(total_attacks = sum(n)) %>% 
  arrange(desc(total_attacks)) %>% 
  pull(region)

# creating the plot 
ggplot(GTD_counts, aes(x = month, y = region, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Number of terrorist attacks by region and month",
       x = "Month", y = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  geom_text(aes(label = n), size = 3, fontface = "bold")



#Creating a line chart that shows the number of terrorist attacks over time
# with different lines for different types of attacks:

# Create a data frame of counts by year and attack type
GTD_counts <- GTD %>%
  group_by(year, attacktype) %>%
  summarise(n = n())

# Creating the plot
ggplot(GTD_counts, aes(x = year, y = n, color = attacktype)) +
  #drawing a line connecting the data points for each attack type
  geom_line(size = 1.5) +
  #adding error bands to the plot using the n variable and the sqrt function 
  #to calculate the standard error
  geom_ribbon(aes(ymin = n - 1.96 * sqrt(n), ymax = n + 1.96 * sqrt(n)), alpha = 0.2) +
  labs(title = "Number of terrorist attacks over time by attack type",
       x = "Year", y = "Number of attacks",
       color = "") +
  scale_color_discrete(guide = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.x = unit(0.1, "cm"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  annotate("text", x = 2001, y = 2000, label = "9/11 Attacks") +
  annotate("segment", x = 2001, xend = 2001, y = 0, yend = 2500, linetype = "dashed") +
  annotate("text", x = 2004, y = 10000, label = "War in Iraq Begins") +
  annotate("segment", x = 2003, xend = 2003, y = 0, yend = 25000, linetype = "dashed") +
  annotate("text", x = 2011, y = 8000, label = "Arab Spring") +
  annotate("segment", x = 2011, xend = 2011, y = 0, yend = 25000, linetype = "dashed") +
  annotate("text", x = 2014, y = 10000, label = "Rise of ISIS") +
  annotate("segment", x = 2014, xend = 2014, y = 0, yend = 25000, linetype = "dashed")



#Scatterplot that shows the number of fatalities and injuries 
#for each terrorist attack:

# Selecting relevant columns and removing rows with missing values
GTD_counts <- GTD %>%
  select(eventid, nkill, nwound, attacktype, region) %>%
  filter(nkill > 0 | nwound > 0) %>%
  na.omit()

# creating a color palette for the regions
palette <- brewer.pal(n=12, "Paired")

# Creating the plot
ggplot(GTD_counts, aes(x = nkill, y = nwound, color = region, shape = attacktype)) +
  geom_point(alpha = 0.5, size = 3) +
  #setting the x-axis and y-axis to a logarithmic scale
  #When plotted on a linear scale, the points representing attacks with fewer
  #casualties may appear to cluster around the origin
  scale_x_log10() + scale_y_log10() +
  labs(title = "Number of fatalities and injuries by terrorist attack, 
       with region and attack type (1970 - 2020)",
       x = "Number of fatalities (log scale)",
       y = "Number of injuries (log scale)",
       color = "Region", shape = "Attack type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_color_manual(values = palette) +
  guides(shape = guide_legend(override.aes = list(size = 3), ncol = 2))


#Creating a stacked bar chart that shows the number of terrorist attacks by 
#target type and attack type:

# creating a data frame of counts by target type and attack type
GTD_counts <- GTD %>%
  group_by(targettype, attacktype) %>%
  summarise(n = n()) %>%
  arrange(targettype, desc(n))

# creating the plot
ggplot(GTD_counts, aes(x = targettype, y = n, fill = attacktype)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Number of terrorist attacks by target and attack type",
       x = "Target", y = "Number of attacks",
       fill = "Attack type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold")) 

#Creating a heatmap that shows the number of terrorist attacks by 
#country and attack type:

# Creating a data frame of counts by region and attack type
GTD_counts <- GTD %>%
  group_by(region, attacktype) %>%
  summarise(n = n()) %>%
  filter(n > 0) # remove rows with zero counts

# Creating the plot
ggplot(GTD_counts, aes(x = region, y = attacktype, fill = n)) +
  geom_tile() +
  #The log scale used in the color scale allows for better visualization of 
  #the regions with a high number of attacks
  scale_fill_viridis_c(
    option = "magma", 
    trans = "log10", 
    guide = guide_colorbar(barwidth = 10, 
                           barheight = 0.5, 
                           title.position = "top", 
                           title.vjust = 1, title.hjust = 0.5, 
                           ticks = FALSE, nbin = 20)) +
  labs(title = "Number of terrorist attacks by region and attack type",
       x = "Region", y = "Attack type",
       fill = "Number of attacks") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1, 
    size = 12, 
    margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.text.y = element_text(size = 12, 
                                   margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 16, 
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.title.x = element_text(size = 14, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)))



#Creating a bubble chart that shows the distribution of casualities by region and year with
#the size of the bubbles representing the number of casualities:

# Create a data frame of casualties by region and year
GTD_casualties <- GTD %>%
  group_by(region, year) %>%
  summarise(nkill = sum(nkill), nwound = sum(nwound)) %>%
  filter(nkill > 0 | nwound > 0) # remove rows with zero counts

# Create the plot
ggplot(GTD_casualties, aes(x = year, y = region, size = nkill + nwound, 
                           fill = nkill)) +
  geom_point(alpha = 0.8, shape = 21, color = "black") +
  scale_size(range = c(2, 20)) +
  scale_fill_gradient(low = "white", high = "#FF7F0E") +
  labs(title = "Distribution of casualties by Region and year",
       x = "Year", y = "Region",
       size = "Number of casualties", fill = "Number of fatalities") +
  theme_minimal() +
  #setting the theme
  theme(axis.text.y = element_text(size = 8, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 1, size = 8, color = "grey50"))



#SPATIAL ANALYSIS

#importing dataset with GDP and population info for the countries
GDP <- read_csv("GDP.csv")
View(GDP)
#importing the dataset containing the population
POP <- read_csv("POP.csv")
View(POP)
#merging the population and the GDP dataset
merge_pop_gdp <- merge(POP, GDP, by = "country")
View(merge_pop_gdp)

#importing the dataset containing the coordinates of countries in the world
world_coord <- read_csv("world_coord.csv")
View(world_coord)

#merging the coordinates with the population and GDP dataset
merge_pop_gdp_coord <- merge(merge_pop_gdp, world_coord, by = "country")
View(merge_pop_gdp_coord)
#converting the country column to factor
merge_pop_gdp_coord$country <- as.factor(merge_pop_gdp_coord$country)


#transforming the GTD (Global Terrorism Dataset) to simple features file for 
#spatial analysis

#google keys to use ggmaps (this is my personal key)
ggmap::register_google(key = "AIzaSyDmQ9__SP7iI1DLfzXNYZ_PqVNm-6C7VW4")

# Filtering out rows with missing coordinates
GTD_points <- GTD[complete.cases(GTD[c("longitude", "latitude")]),]

# Converting to simple features file/ data frame
GTD_sf <- st_as_sf(GTD_points, coords = c("longitude", "latitude"), crs = 4326)

#to confirm that the dataset was properly converted to a simple features file 
class(GTD_sf)
#to chack the coordinate reference system
st_crs(GTD_sf)

#NO NEED TO RUN THIS LINE BELOW (DATA ALREADY EXISTS IN FOLDER)
# Saving the shapefile to my system
st_write(GTD_sf, "GTD.shp")

#transforming the merged GDP AND POP file to sf
# Filter out rows with missing coordinates
POP_GDP_points <- merge_pop_gdp_coord[complete.cases(
  merge_pop_gdp_coord[c("longitude", "latitude")]),]
#transform to sf
POP_GDP_sf <- st_as_sf(merge_pop_gdp_coord, 
                       coords = c("longitude", "latitude"), crs = 4326)

#to confirm that the dataset was properly converted to a simple features file 
class(POP_GDP_sf)
#to check the coordinate reference system
st_crs(POP_GDP_sf)

#both datasets have the same coordinate reference systems

#exploring both files
head(GTD_sf)
head(POP_GDP_sf)

#plotting the geometry of both files
plot(st_geometry(GTD_sf))
plot(st_geometry(POP_GDP_sf))

#premilinary plots (exploring ggmap and tmap using the dataset)
ggplot(data = GTD_sf, aes(fill = attacktype))+
  geom_sf()


# Get map background using ggmap
map <- get_map(location = c(lon = mean(st_coordinates(GTD_sf)[, 1]), 
                            lat = mean(st_coordinates(GTD_sf)[, 2])),
               zoom = 2, maptype = "satellite")



ggplot(GTD_sf) + 
  geom_sf(aes(fill = attacktype))

ggmap(map) +
  geom_point(aes(longitude, latitude, color = attacktype, size = nkill), 
             data = GTD)

?ggmap
tm_shape(GTD_sf)+
  tm_dots(col = "attacktype")


#visualizing the number of wounds sustained 
tmap_mode("view")
tm_basemap("Stamen.Watercolor") +
  tm_shape(GTD_sf) + tm_dots(size = "nwound", col = "red") +
  tm_tiles("Stamen.TonerLabels")

?tm_tiles


#Showing Attacks by country with the number of kills
# Create the map
GTD_map <- tm_shape(GTD_sf) +
  #adding points and representing the number of kills 
  tm_dots(size = "nkill", col = "nkill", popup.vars = c("country", "nkill"),
          palette = "Reds") +
  #adding legend
  tm_legend(legend.position = c("left", "bottom"), 
            legend.title.size = 1,
            legend.title.color = "#000000",
            title.position = c("center", "center")) +
  tm_scale_bar(breaks = c(0, 1000, 2000, 3000), position = c("left", "bottom")) +
  #adding a compas
  tm_compass(type = "rose", position = c("right", "bottom"), size = 0.7) +
  tm_layout(title = "Global Terrorism Database: Attacks by Region and Year",
            title.position = c("center", "top"),
            frame = FALSE,
            inner.margins = c(0.1, 0.1, 0.1, 0.1),
            outer.margins = c(0, 0, 0, 0),
            legend.bg.color = "white")


# Display the map
GTD_map


#Showing Attacks by weapon type
# Create the map
tm_shape(GTD_sf) +
  #adding points and representing the number of kills and setting color to weapon type
  tm_dots(size = "nkill", col = "weapontype", popup.vars = 
            c("country", "nkill","weapontype"),
          palette = "Reds") +
  #setting the legend
  tm_legend(legend.position = c("left", "bottom"), 
            legend.title.size = 1,
            legend.title.color = "#000000",
            title.position = c("center", "center")) +
  tm_scale_bar(breaks = c(0, 1000, 2000, 3000), position = c("left", "bottom")) +
  tm_compass(type = "rose", position = c("right", "bottom"), size = 0.7) +
  tm_layout(title = "Global Terrorism Database: Attacks by weapon type",
            title.position = c("center", "top"),
            frame = FALSE,
            inner.margins = c(0.1, 0.1, 0.1, 0.1),
            outer.margins = c(0, 0, 0, 0),
            legend.bg.color = "white")



#Showing Attacks by attack type
# Create the map
tm_shape(GTD_sf) +
  tm_dots(size = "nkill", col = "attacktype", popup.vars = 
            c("country", "nkill","attacktype"),
          palette = "Reds") +
  tm_legend(legend.position = c("left", "bottom"), 
            legend.title.size = 1,
            legend.title.color = "#000000",
            title.position = c("center", "center")) +
  tm_scale_bar(breaks = c(0, 1000, 2000, 3000), position = c("left", "bottom")) +
  tm_compass(type = "rose", position = c("right", "bottom"), size = 0.7) +
  tm_layout(title = "Global Terrorism Database: Attacks by attack type",
            title.position = c("center", "top"),
            frame = FALSE,
            inner.margins = c(0.1, 0.1, 0.1, 0.1),
            outer.margins = c(0, 0, 0, 0),
            legend.bg.color = "white")


#showing number of attacks by country
# Selecting and cleaning the relevant columns
GTD_sel <- GTD %>%
  select(year, country, region, attacktype, weapontype, 
         nkill, property, gname, nwound) %>%
  #capitalizing the first letter of each word in any character columns
  mutate_if(is.character, str_to_title)

# Load the world map shapefile
world_map <- world
#rename column to match
GTD_sel$name_long <- GTD_sel$country
View(world_map)

# Join the GTD data to the world map shapefile
world_map_GTD <- world_map %>% left_join(GTD_sel)
?left_join
class(world_map$name_long)
class(GTD_sel$name_long)
GTD_sel$name_long <- as.character(GTD_sel$name_long)
# Aggregating the number of attacks by country
GTD_agg <- GTD_sel %>%
  group_by(country) %>%
  summarize(n_attacks = n()) %>%
  mutate(n_attacks = ifelse(n_attacks == 0, NA, n_attacks))

GTD_agg$name_long <- GTD_agg$country

# Joining the aggregated data to the world map shapefile
world_map_GTD_agg <- left_join(world_map_GTD, GTD_agg)

# Creating the choropleth map showing the number of attacks by country
attacks = tm_shape(world_map_GTD_agg) +
  tm_polygons("n_attacks", style = "quantile", title = "Number of Attacks",
              palette = "Reds", n = 7, border.col = "grey50",
              legend.hist = TRUE, legend.is.portrait = TRUE) +
  tm_compass(type = "radar", size = 0.5, position = c("left", "top"), 
             color.dark = "white", color.light = "black") +
  tm_scale_bar(breaks = c(0, 1000, 2000, 3000), position = c("left", "bottom"), 
               text.size = 2, text.color = "black") +
  tm_layout(main.title = "Global Terrorism Database: Number of Attacks by Country",
            title.position = c("center", "top"),
            title.size = 2,
            bg.color = "white",
            main.title.size = 1,
            legend.position = c("right", "bottom"), 
            legend.outside = TRUE, 
            legend.bg.color = "white", 
            legend.title.size = 2,
            legend.text.size = 1,
            legend.text.fontface = "bold",
            legend.width = 1,
            legend.hist.width = 1,
            legend.outside.size = 0.3,
            frame = TRUE)
attacks

#style = "quantile" ensures the same number of observations fall into each 
#category (with the potential downside that bin ranges can vary widely);

#5. Creating a  choropleth map showing the number of wounds by country
breaks = c(0, 2, 4, 6,8,10,20) * 100
wound = tm_shape(world_map_GTD_agg) +
  tm_polygons("nwound", title = "Number of Wounds",
              palette = "BuGn", n = 5, border.col = "grey50",
              legend.hist = FALSE, legend.is.portrait = TRUE) +
  tm_compass(type = "radar", size = 0.5, position = c("left", "top"), 
             color.dark = "white", color.light = "black") +
  tm_scale_bar(breaks = breaks, position = c("left", "bottom"), 
               text.size = 2, text.color = "black") +
  tm_layout(main.title = "Global Terrorism Database: Wounds sustained by Country",
            title.position = c("center", "top"),
            title.size = 2,
            bg.color = "white",
            main.title.size = 1,
            legend.position = c("right", "bottom"), 
            legend.outside = TRUE, 
            legend.bg.color = "white", 
            legend.title.size = 2,
            legend.text.size = 1,
            legend.text.fontface = "bold",
            legend.width = 1,
            legend.hist.width = 1,
            legend.outside.size = 0.3,
            frame = TRUE)
wound


#6. Creating a  choropleth map showing the number of deaths by country
kill = tm_shape(world_map_GTD_agg) +
  tm_polygons("nkill", title = "Number of deaths",
              palette = "BuGn", n = 6, border.col = "grey50",
              legend.hist = TRUE, legend.is.portrait = TRUE, breaks = breaks) +
  tm_compass(type = "radar", size = 0.5, position = c("left", "top"), 
             color.dark = "white", color.light = "black") +
  tm_scale_bar(breaks = c(0, 1000, 2000, 3000), position = c("left", "bottom"), 
               text.size = 2, text.color = "black") +
  tm_layout(main.title = "Global Terrorism Database: Deaths by Country",
            title.position = c("center", "top"),
            title.size = 2,
            bg.color = "white",
            main.title.size = 1,
            legend.position = c("right", "bottom"), 
            legend.outside = TRUE, 
            legend.bg.color = "white", 
            legend.title.size = 2,
            legend.text.size = 1,
            legend.text.fontface = "bold",
            legend.width = 1,
            legend.hist.width = 1,
            legend.outside.size = 0.3,
            frame = TRUE)

kill







