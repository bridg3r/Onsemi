# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library(tidyverse)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(htmlwidgets)

#fix the cities
onsemi <- read_csv('/Users/bridg3r/Documents/GitHub/Onsemi/streamlitTest/onsemi')

world <- ne_countries(scale = "medium", returnclass = "sf")

#specify shapes to use
# add in annotations
master <- ggplot(data = world) +
  geom_sf(color = "black", fill = rgb(144, 238, 144, maxColorValue = 255)) +
  geom_point(data = onsemi, aes(Longitude, Latitude, color = site_type, shape=cgroup, 
                                text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
             fill = 'black', size = 3) +
  labs(shape="Group", color = "Site Function") +
  theme(panel.background = element_rect(fill = rgb(137, 207, 240, maxColorValue = 255)),
        panel.grid.major = element_line(linetype = 'dashed', size = .3),
        plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.position = "none", text = element_text(family = "Comic Sans MS")) + 
  scale_color_manual(values = c(rgb(255, 172, 28, maxColorValue = 255), rgb(238, 75, 43, maxColorValue = 255), 
                                rgb(34, 139, 34, maxColorValue = 255)))
master

# add in clickable link
#make my own legend in R Shiny
masteri <- ggplotly(master, tooltip = "text" )
masteri

# use a for loop to make a map for every city with an arrow pointing or some annotation
# put all of these into a dictionary

# make r shiny dashboard to show different plots

# save the widget as .html format
saveWidget(masteri, file="OnsemiFootPrint.html")