#Morgan RAI maps 

library(readr)
library(tidyverse)
library(sf)
library(spatstat)
library(spatstat.geom)
library(ggplot2)
library(ggspatial)
library(mapedit)

#basemaps packages
install.packages("basemaps")
devtools::install_github("16EAGLE/basemaps")
library(basemaps)


#load in RAI data
rai_by_loc <- read_csv("RAIByLocation_20250710Data_csv.csv")

#select only vicuna & correct RAI column 
rai_filtered <- rai_by_loc %>%
  filter(Species == "Vicugna vicugna") %>%
  select(Location, Species, RAI_independent_30m)



#load in points
latrine_pts <- st_read("D:\\Briana Roberson\\atrinepoints_nodup_102925.shp")
st_geometry_type(latrine_pts)
summary(latrine_pts)
plot(st_geometry(latrine_pts))

#extract coordinates
latrine_coords <- st_coordinates(latrine_pts)
class(latrine_coords) #should be function
head(latrine_coords) #should be lat & long

#project
latrine_pts_utm <- st_transform(latrine_pts, crs = 32719) #UTM zone 19S
latrine_coords_utm <- st_coordinates(latrine_pts_utm)

#convert to SF object 
latrine_pts_sf <- st_as_sf(latrine_pts, coords = latrine_coords_utm, crs = 32719)


#define extent 
ext <- draw_ext() #draw desired rectangle around latrine points 
ext_prj <- st_transform(ext, 32719) #fix prj to UTM zone 19S

#pull basemap (ESRI World Imagery)
set_defaults(map_service = "esri", map_type = "world_imagery")
basemap <- basemap_magick(ext_prj)

#convert magick basemap raster
basemap_raster <- as.raster(basemap)


#define bounding box for basemap 
bbox <- st_bbox(ext_prj)
#manual way



#plot with latrine points 
latrines_plot <- ggplot() +
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  geom_sf(data = latrine_pts_sf, color = "red", size = 2) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  theme_minimal()

latrines_plot
