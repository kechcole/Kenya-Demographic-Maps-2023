               
               # Set working directory 
setwd('D:/Programs/R/Kenya_demographics')


                # Install packages 
install.packages("ggplot2")
install.packages("sf")
install.packages("dplyr")
install.packages("viridis")
install.packages("ggspatial")

                # Load packages
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(ggspatial)



                # Load data 

# Kenyan counties shapefile
kenyan_counties <- st_read("kenyan_counties.shp") 

# Demographic csv data 
demographics <- read.csv("Combined_data.csv", header=TRUE)

            # EXPLORE KENYAN COUNTY SHAPEFILE 

# examine the data , class type, attribute information, 
str(kenyan_counties)

# Plot 
kenyan_counties %>% 
  ggplot() +
  geom_sf(color = "black", fill = "lightblue")


        # Plot names for each polygon on the centre.

# Pipe in spatial data into ggplot
county_map <- kenyan_counties %>% 
  ggplot() +
  
  # Fill polygons with grey and borders white
  geom_sf(color = "white", fill = "grey") + 
  
  # Label each county 
  geom_sf_text(aes(label = COUNTIES), 
               size = 2, 
               color = "black", 
               fontface = "bold", 
               check_overlap = TRUE) + 
  
  # Remove Labels in X and Y axis  
  xlab("") + ylab("") +
  
  # Title of map
  ggtitle("MAP OF KENYAN COUNTIES") +

  # Add north arrow at top left corner
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)  + 
  
  # Theme elements 
  theme(
    # Add grids 
    panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        # Background colour
        panel.background = element_rect(fill = "aliceblue"),  
        # centre title and bold 
        plot.title = element_text(hjust = 0.5, face="bold")  
    )  

# View map
county_map


          

            # EXPLORE DEMOGRAPHIC DATA  
    
# First rows 
head(demographics)


# Column names 
names(demographics)

# Rename columns using a dplry function and assign new variable
demography <- demographics %>% 
  rename(Health = Sum.of.Percentage.healthinsurance,
         Lack_Food = Sum.of.Percentage.lacking.food,
         School_Attendance = Sum.of.primary_school.attendance,
         Wealth_Inequality = Sum.of.gini_coefficient)

# Structure of data 
str(demography)

# Any missing values 
colSums(is.na(demography))

# Compute summary statistics 
summary(demography)

# Change columns to hold decimal percentage values for each variable 
demography <- demography %>%
  select(County, Health, Lack_Food, School_Attendance, Wealth_Inequality) %>%
  mutate(Health_Perc = log(Health), 
         Lack_Food_Perc = log(Lack_Food),
         School_Attendance_Perc = log(School_Attendance))

# Summary of demographic data 
glimpse(demography) 



#              SPATIAL JOIN.
# Merge shapefile with csv data 
county_merged <- merge(kenyan_counties, demography, 
                        by.x="COUNTIES", by.y="County")

str(county_merged)

# Coordinate reference system
st_crs(county_merged)



#              PLOT VARIABLES  
# 1. Health 
health_cover <- county_merged %>%
  ggplot() +
  
  # Colour according to health column and remove county borderline colour
  geom_sf(aes(fill = Health_Perc),
          color = NA) +              
  
  # Title of the map 
  labs(title = "MAP OF HEALTH INSURANCE COVERAGE") + 
  
  # Label legend  
  labs(fill='Health') + 
  
  # Visualization type
  scale_fill_viridis_c(option = "plasma") + 
  
  # Legend, axis label and title features 
  theme(
    # Make title bold, centre, increase size
    plot.title = element_text(hjust = 0.5, face="bold", size=20),
        # Remove x and y labels,  
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        # add map borderline
        plot.background = element_rect(color = "black", linewidth = 1.5),
        # Legend position
        legend.position=c(0.9, 0.13),legend.title=element_text(size=18))

# Display map
health_cover


# 2. Wealth inequality
wealth_inequality <- county_merged %>%
  ggplot() +
  geom_sf(aes(fill = Wealth_Inequality), 
          color = NA) +
  labs(title = "MAP OF WEALTH INEQUALITY") + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(fill='Gini') +
  theme(
    plot.title = element_text(hjust = 0.5, face="bold", size=23),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(), 
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    plot.background = element_rect(color = "black", linewidth = 1.5),
    legend.position=c(0.9, 0.15),legend.title=element_text(size=20)
    )
  
# Display map
wealth_inequality


# 3. School attendance 
school_attendance <- county_merged %>%
  ggplot() +
  geom_sf(aes(fill = School_Attendance_Perc), color = NA) +
  labs(title = "MAP OF PRIMARY SCHOOL ATTENDANCE") + 
  scale_fill_viridis_c(option = "plasma") +
  labs(fill='Schooling') +
  theme(
    plot.title = element_text(hjust = 0.5, face="bold", size=20),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(), 
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    plot.background = element_rect(color = "black", linewidth = 1.5),
    legend.position=c(0.9, 0.13),legend.title=element_text(size=14)
  )

# Display map
school_attendance



# 4. Food insufficient
food_insufficient <- county_merged %>%
  ggplot() +
  geom_sf(aes(fill = Lack_Food_Perc), color = NA) +
  labs(title = "MAP OF FOOD INSUFFICIENCY") + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(fill='Insecure') +
  theme(
    plot.title = element_text(hjust = 0.5, face="bold", size=23),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(), 
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    plot.background = element_rect(color = "black", linewidth = 1.5),
    legend.position=c(0.90, 0.13),legend.title=element_text(size=18)
  )

# Display map
food_insufficient


                      # SAVE MAPS 
# county map 
ggsave("counties1.png", plot = county_map, 
       width = 30, height = 20, units = "cm")

# Health
ggsave("health.png", plot = health_cover, 
       width = 30, height = 20, units = "cm")

# Wealth Inequality
ggsave("wealth.png", plot = wealth_inequality, 
       width = 30, height = 20, units = "cm")

# School attendance 
ggsave("school.png", plot = school_attendance, 
       width = 30, height = 20, units = "cm")

# Food Insecure
ggsave("food.png", plot = food_insufficient, 
       width = 30, height = 20, units = "cm")



"
                  References.
1. Colour brewer, scale_fill_vridis - https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
2. CRS UTM kENYA - 32637

"
  
  

















