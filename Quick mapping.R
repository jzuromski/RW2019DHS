library(rdhs)
library(dplyr)
library(haven)
library(here)
library(labelled)
library(devtools)
library(microbenchmark)
library(survey)
library(ggplot2)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(terra)
library(stars)
library(spdep)
library(gstat)




#quick map to see distribution of malaria positive DBS
#mapping it out

world <- ne_countries(scale = "medium", returnclass = "sf") 
rwanda <-  subset(world, admin == "Rwanda")

# Download a shapefile for CORRECT administrative level of RW

admin10 <- ne_download(scale= "large", type = "admin_1_states_provinces_lines",
                       category = "cultural", returnclass = "sf")
rivers10 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', 
                        category = 'physical', returnclass = "sf")
lakes10 <- ne_download(scale = "large", type = 'lakes', 
                       category = 'physical', returnclass = "sf")
sov110 <- ne_download(scale= "medium", type = "sovereignty",
                      category = "cultural", returnclass = "sf")
admin110 <- ne_download(scale= "large", type = "populated_places",
                        category = "cultural", returnclass = "sf")
roads10 <- ne_download(scale= "large", type = "roads",
                       category = "cultural", returnclass = "sf")

#------------------------------------------------------------------#
#         THIS CODE DOES NOT WORK              #
#----------------------------------------------------------------#
ggplot(rwanda) +
  geom_sf(data=rivers10, color="cyan4", size=0.5, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=admin10, color="grey80", size= 0.4) +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
           color="grey60", size=5 , fontface="italic") +
  geom_point(data = map_mal_pos_DBS, aes(x = LONGNUM, y = LATNUM, 
                 fill = "lightblue", 
                 color = "darkblue", 
                 shape = 17), size = 2) +
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1,-2.9), expand = TRUE)+
  theme_void()

#--------------------------------------------------#
####### THIS CODE WORKS ##############
#--------------------------------------------------#
ggplot(rwanda) +
  geom_sf(fill = "blanchedalmond") +
  geom_sf(data=admin10, color="grey10", size= 0.9) +
  geom_sf(data=rivers10, color="cyan4", size=0.9, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  geom_sf(data=roads10, color="grey70",fill= "ivory", size= 0.4) +
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanzania", 
           color="grey60", size=5 , fontface="italic") +
  geom_point(data = Malaria_positive_DBS, aes(x = LONGNUM, y = LATNUM), 
             fill = "lightblue", 
             color = "darkblue", 
             shape = 17, 
             size = 2) +
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void()



ggplot(data = rwanda) +
  geom_sf(fill = "blanchedalmond") +
  geom_sf(data=lakes10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=admin10, color="grey80", size= 0.4) +
  geom_point(data = mal_tested_DBS, aes(x = LONGNUM, y = LATNUM), 
             fill = "lightblue", 
             color = "darkblue", 
             shape = 17, 
             size = 1) +
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1, -2.9), expand = TRUE) +
  theme_void()
