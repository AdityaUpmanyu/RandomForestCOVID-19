# Women in Labor Force

install.packages("tidyverse")
install.packages("maps")
install.packages("virdis")
install.packages("scales")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
require(viridis)
library(scales)
library(GGally)

# Reading data from local files into dataframes
lfprf<-read.csv("/Users/Desktop/Sheffield/Academics/DataVisualization/LFPRF.csv",header = T)
rfmlfpr<-read.csv("/Users/Desktop/Sheffield/Academics/DataVisualization/RFMLFPR.csv",header = T)
epr<-read.csv("/Users/Desktop/Sheffield/Academics/DataVisualization/EPR.csv",header = T)

# Merging dataframes
womens_employment <- merge(merge(
  lfprf,
  rfmlfpr, by=c('Entity','Code','Year'),all.x = TRUE),
  epr, by=c('Entity','Code','Year'),all.x = TRUE)
write.csv(womens_employment, "womens_employment1.csv")

#Excel Filtering is done externally

# Reading the filtered data
we<-read.csv("/Users/Desktop/Sheffield/Academics/DataVisualization/womens_employment1.csv",header = T)

# Visualizations

# Q1: Which countries had the highest average number of working women in the chosen time period?

# Selecting OECD counties with data
some_countries <- c("Austria",
                    "Belgium",
                    "Denmark",
                    "France",
                    "Germany",
                    "Greece",
                    "Ireland",
                    "Italy",
                    "Luxembourg",
                    "Netherlands",
                    "Norway",
                    "Portugal",
                    "Spain",
                    "Switzerland",
                    "United Kingdom"
)

# Extracting a world map
world_map <- map_data("world", region=some_countries)
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

# Calculating mean of LFPRF for the counties
we1 <- we %>%
  group_by(Entity) %>%
  summarise(mean_lfprf = mean(LFPRF))

# Renaming for readability
we1 <- rename(we1, region = Entity)

# Joining the map and data dataframes
map_data <- left_join(we1, world_map, by = "region")

# Plotting the choropleth
ggplot(map_data, aes(long, lat, group = group))+
  geom_polygon(aes(fill = mean_lfprf ),  color = "white")+
  scale_fill_viridis_c(option = "C", name = "FEMALE LABOR \nFORCE \nPARTICIPATION \nRATE (%)", direction = -1) +
  labs(title = "Country-wise average Female Labor Force \nParticipation Rate", 
       x = "Longitude", y = "Latitude", subtitle = "From 1990 to 2019", caption = "From OWID based on World Bank and ILOSTAT data")


# Q2: How does the ratio of female to male participation rate change between 1999, 2009 and 2019 in all chosen countries?

# Filering the data for 1999, 2009, 2019 years
we_9 <-filter(we, Year == "1999" |  Year=="2009" | Year == "2019")

# Plotting the Lollipop Plot
ggplot(we_9, aes(x=Entity, y=RFMLFPR) ) +
  geom_segment( aes(x=Entity ,xend=Entity, y=40, yend=RFMLFPR), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip()+ facet_grid(. ~ Year)+
  labs(title = "Change in ratio of female to male \nlabor force participation rate", 
       x = "Country", y = "Ratio of Female to Male Labor Force Participation Rate (%)", subtitle = "In 1990, 2009 and 2019", 
       caption = "From OWID based on World Bank and ILOSTAT data")

# Q3: How are women labor force participation rate and the ratio of women LF Participation rate with men related?

# Plotting the scatter plot for correlation
ggplot(we, aes(x=LFPRF, y=EPR)) + 
  geom_point()+
geom_smooth(method=loess , color="#22A884FF", fill="#69b3a2", se=TRUE)+
  labs(title = "Correlation between Female Labor Force Participation Rate \nand Employment to Population Ratio", 
       x = "Female Labor Force Participation Rate (%)", y = "Employment to Population Ratio (%)", subtitle = "From 1990 to 2019", 
       caption = "From OWID based on World Bank and ILOSTAT data")

# Q4: What are the changes in women's labor force participation rate, % of women population that are employed and 
# ratio of female to male participation rate from 1990 to 2019?

# Filtering the data for 1990 and 2019 years
we_9019<-filter(we, Year == "1990" | Year == "2019")

# Plotting the Parallel Plot
ggparcoord(we_9019,
           columns = c(5,7,6), groupColumn = 2, scale="globalminmax", showPoints = TRUE
) + 
scale_color_viridis_d(name = "Country") + facet_grid(. ~ Year) +
labs(title = "Female Labor Force Participation Profile ", x = "Indicator", y = "Value (%)", subtitle = "In 1990 and 2019", 
caption = "From OWID based on World Bank and ILOSTAT data \nLFPRF = Female Labor Force Participation Rate 
\nRFMLFPR = Ratio of Female to Male Labor Force Participation Rate 
\nEPR = Employment to Population Ratio")
