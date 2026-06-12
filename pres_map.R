library(readr)
library(tidyverse)
library(sf)
library(spatstat)
library(spatstat.geom)
library(ggplot2)
library(ggspatial)
library(mapedit)
library(basemaps)
library(stars)
library(raster)
library(grid)
library(gridExtra)
library(metR)
library(isoband)
library(ggsflabel)
library(rnaturalearth)
library(cowplot)
library(ggspatial)
library(geodata)
library(terra)
library(prettymapr)


#load in points
latrine_pts <- st_read("latrinepoints_nodup_102925.shp")
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

# Save extent to an RDS file
saveRDS(ext_prj, "latrine_extent.rds")
ext_prj <- readRDS("latrine_extent.rds")

#plot with latrine points 
latrines_plot <- ggplot() +
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  geom_sf(data = latrine_pts_sf, color = "cyan3", size = 2) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  theme_minimal()

latrines_plot

#add isolines 
isolines_raster <- raster("isolines.tif")
plot(isolines_raster)
#reproject 
isolines_prj <- projectRaster(
  isolines_raster,
  crs = "EPSG:32719")  
plot(isolines_prj)
isolines_prj


isolines_stars <- st_as_stars(isolines_prj)
isolines_stars


latrines_plot <- ggplot() +
  # Basemap underneath
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  # Latrine points on top
  geom_sf(data = soil_points, 
          color = "black",        
          size = 0.5, 
          shape = 21,          
          fill = "white",      
          stroke = 1.5)         +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  geom_sf_label_repel(data = soil_points, aes(label = Name), size = 1.5) + 
  theme_minimal()
latrines_plot


#colors
isolines_rgb <- brick("isolines.tif")
isolines_prj <- projectRaster(isolines_rgb, crs = "EPSG:32719", method = "ngb")

#process
r <- as.array(isolines_prj) / 255  # scale 0-1

# Separate RGB and Alpha
RGB <- r[,,1:3]
Alpha <- r[,,4]

# Replace NA in Alpha with 1 (opaque)
Alpha[is.na(Alpha)] <- 1

# Replace NA in RGB with background color (white)
for (i in 1:3) {
  RGB[,,i][is.na(RGB[,,i])] <- 1
}

# Apply alpha blending with white background
RGB_blend <- array(1, dim = dim(RGB))  # white background
for (i in 1:3) {
  RGB_blend[,,i] <- RGB[,,i] * Alpha + (1 - Alpha) * 1  # blend with white
}

# Ensure values are within [0,1]
RGB_blend[RGB_blend < 0] <- 0
RGB_blend[RGB_blend > 1] <- 1

# Create grob
img_grob <- rasterGrob(RGB_blend,
                       width = unit(1,"npc"),
                       height = unit(1,"npc"),
                       interpolate = TRUE)



# Add to ggplot
latrines_plot_isolines <- ggplot() +
  annotation_custom(img_grob,
                    xmin = xmin(isolines_prj), xmax = xmax(isolines_prj),
                    ymin = ymin(isolines_prj), ymax = ymax(isolines_prj)) +
  geom_sf(data = soil_points, 
          color = "black",        
          size = 0.25, 
          shape = 21,          
          fill = "green",      
          stroke = 1)         +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  labs(title = "Seimon isolines (prj)") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
  ) 


latrines_plot_isolines

library(gridExtra)

grid.arrange(latrines_plot, latrines_plot_isolines, ncol = 2)


#Dr. Tang's isolines 
glacier_melting <- raster("Glacier_Melting_1985_2024.tif")
glacier_melting_prj <- projectRaster(glacier_melting, crs = "EPSG:32719", method = "ngb")
glacier_melting_prj
plot(glacier_melting_prj)

# Crop 
glacier_crop <- crop(
  glacier_melting_prj,
  extent(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
)

#convert
glacier_df <- as.data.frame(glacier_crop, xy = TRUE)
colnames(glacier_df)[3] <- "value"

#mask zeros 
glacier_df <- glacier_df %>%
  dplyr::mutate(value = ifelse(value == 0, NA, value))

#check it looks right 
ggplot(glacier_df) +
  geom_tile(aes(x = x, y = y, fill = value)) +
  coord_equal() +
  scale_fill_viridis_c()




#this plot works 
ggplot() +
  # Basemap
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  annotation_custom(img_grob,
                    xmin = xmin(isolines_prj), xmax = xmax(isolines_prj),
                    ymin = ymin(isolines_prj), ymax = ymax(isolines_prj)) +
  geom_tile(data = glacier_df,
            aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = "H", na.value = "transparent") +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  )  +
  
  # Latrine points
  geom_sf(data = plant_points, 
          color = "black",        
          size = 1.5, 
          shape = 21,          
          fill = "green",      
          stroke = 0.5) +
  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) 


#add second raster
glacier_melting2 <- raster("Glacier_Melting_1974_1984.tif")
glacier_melting2_prj <- projectRaster(glacier_melting2, crs = "EPSG:32719", method = "ngb")
# Crop 
glacier_crop2 <- crop(
  glacier_melting2_prj,
  extent(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
)
#convert
glacier_df2 <- as.data.frame(glacier_crop2, xy = TRUE)
colnames(glacier_df2)[3] <- "value"

#mask zeros, assign 1974 to else 
glacier_df2 <- glacier_df2 %>%
  dplyr::mutate(
    value = ifelse(value == 0, NA,
                   ifelse(value == 1, 1974, value))
  )
#check it looks right 
ggplot(glacier_df2) +
  geom_tile(aes(x = x, y = y, fill = value)) +
  coord_equal() +
  scale_fill_viridis_c()


#plot with both 
glacier_both_plot <- ggplot() +
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  
  geom_tile(data = glacier_df,
            aes(x = x, y = y, fill = value)) +
  
  geom_tile(data = glacier_df2,
            aes(x = x, y = y, fill = value)) +
  
  scale_fill_viridis_c(option = "H", na.value = "transparent" , name = NULL) +
  
  geom_sf(data = soil_points, 
          color = "white",        
          size = 0.25, 
          shape = 21,          
          fill = "white",      
          stroke = 1)         +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) + 
  labs(title = "Tang year of first melting") +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(10, "cm"),   
      barheight = unit(0.2, "cm")
    )
  ) +   
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  )
glacier_both_plot

###plot with both and isolines


#new isoline grob w transparency
RGBA <- array(NA, dim = c(dim(RGB)[1], dim(RGB)[2], 4))

RGBA[,,1:3] <- RGB
RGBA[,,4] <- Alpha * 0.45

img_grob_2 <- rasterGrob(RGBA,
                         width = unit(1,"npc"),
                         height = unit(1,"npc"),
                         interpolate = TRUE)


#plot


glacier_both_plot2 <- ggplot() +
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  
  # First layer (original)
  geom_tile(data = glacier_df,
            aes(x = x, y = y, fill = value)) +
  
  # Second layer (modified)
  geom_tile(data = glacier_df2,
            aes(x = x, y = y, fill = value)) +
  
  scale_fill_viridis_c(option = "H", na.value = "transparent") +
  annotation_custom(img_grob_2,
                    xmin = xmin(isolines_prj), xmax = xmax(isolines_prj),
                    ymin = ymin(isolines_prj), ymax = ymax(isolines_prj)) +
  geom_sf(data = soil_points, 
          color = "black",        
          size = 0.75, 
          shape = 21,          
          fill = "white",      
          stroke = 1.5) +
  labs(title = "Year of first melting") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
  ) +
  theme(legend.position = "none") +
  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  )
glacier_both_plot2

grid.arrange(latrines_plot_isolines, glacier_both_plot, glacier_both_plot2, ncol = 3, nrow = 1)

#comparing isolines 
glacier_both_plot3 <- ggplot() +
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  annotation_custom(img_grob,
                    xmin = xmin(isolines_prj), xmax = xmax(isolines_prj),
                    ymin = ymin(isolines_prj), ymax = ymax(isolines_prj)) +
  
  # First layer (original)
  geom_tile(data = glacier_df,
            aes(x = x, y = y, fill = value)) +
  
  # Second layer (modified)
  geom_tile(data = glacier_df2,
            aes(x = x, y = y, fill = value)) +
  
  scale_fill_viridis_c(option = "H", na.value = "transparent") +
  geom_sf(data = soil_points, 
          color = "black",        
          size = 0.25, 
          shape = 21,          
          fill = "green",      
          stroke = 1)         +
  labs(title = "both (seimon under)") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
  ) +
  theme(legend.position = "none") +
  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  )
glacier_both_plot3

grid.arrange(glacier_both_plot, glacier_both_plot2, glacier_both_plot3,ncol = 3, nrow = 1)


#export crops 
writeRaster(glacier_crop,'glacier_crop.tif',options=c('TFW=YES'))
writeRaster(glacier_crop,'glacier_crop2.tif',options=c('TFW=YES'))

#plot with tang's rasters and proposed age classes 


glacier_both_plot_prop <- ggplot() +
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  annotation_custom(img_grob,
                    xmin = xmin(isolines_prj), xmax = xmax(isolines_prj),
                    ymin = ymin(isolines_prj), ymax = ymax(isolines_prj)) +
  
  geom_tile(data = glacier_df,
            aes(x = x, y = y, fill = value)) +
  
  scale_fill_viridis_c(option = "H", na.value = "transparent") +
  geom_contour(
    data = glacier_df,
    aes(x = x, y = y, z = value),
    breaks = c(1990),
    color = "black",
    size = 0.8
  ) +
  geom_text_contour(
    data = glacier_df,
    aes(x = x, y = y, z = value),
    breaks = c(1991),
    size = 4,
    fontface = "bold",
    color = "white",
    check_overlap = FALSE,  # allow overlapping if necessary
    skip = 0                # try to place labels on all segments
  ) +
  geom_sf(data = soil_points, 
          color = "black",        
          size = 0.75, 
          shape = 21,          
          fill = "purple",      
          stroke = 0.5) +
  labs(title = "proposed isolines") +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(5, "cm"),   
      barheight = unit(0.2, "cm")
    )
  ) +   
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  )
glacier_both_plot_prop



grid.arrange(glacier_both_plot, glacier_both_plot2, glacier_both_plot3, glacier_both_plot_prop, ncol = 4, nrow = 1)


###investigate latrine coverage 
latrine_coverage <- read_csv("latrine_coverage.csv")
plant_points <- latrine_pts_sf %>%
  inner_join(latrine_coverage, by = c("Name" = "plants"))


#microbes: 

latrine_coverage$soil <- sub("^L", "L23 ", latrine_coverage$soil)
latrine_coverage[23, 1] <- "L111."
latrine_coverage[27, 1] <- "L112."


soil_points <- latrine_pts_sf %>%
  inner_join(latrine_coverage, by = c("Name" = "soil"))
soil_points <- soil_points %>%
  filter(Name != "L23 62")

#plot with soil points 
soil_plot <- ggplot() +
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  geom_sf(data = soil_points, color = "cyan3", size = 2) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  theme_minimal()

soil_plot





#plot with plant points 
plant_plot <- ggplot() +
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  geom_sf(data = plant_points, color = "cyan3", size = 2) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  theme_minimal()

plant_plot

##interpolate values from tang raster 1 
library(terra)
glacier_rast <- rast(glacier_melting_prj)
plant_points <- st_transform(plant_points, crs(glacier_rast))
soil_points  <- st_transform(soil_points, crs(glacier_rast))
plant_points$year <- terra::extract(glacier_rast, vect(plant_points))[,2]
soil_points$year  <- terra::extract(glacier_rast, vect(soil_points))[,2]

plant_points$type <- "Plant"
soil_points$type  <- "Soil"
all_points <- bind_rows(plant_points, soil_points) 


ggplot(all_points, aes(x = year, fill = type)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "dodge") +
  scale_x_continuous(
    breaks = seq(1980, 2024, by = 1),
    limits = c(1980, NA)
  ) +
  theme_classic() +
  labs(title = "Tang year of first melting",
       x = "Year",
       y = "# latrines")

#select post-1984 latrines 
all_points_old <- all_points %>%
  filter(year > 0)

all_points_in <- all_points[all_points$year != 0, ]


##SOIL MANUSCRIPT PLOT

#make the inset 

peru <- ne_countries(country = "Peru", returnclass = "sf")
#Centroid of latrine points
study_site <- st_centroid(st_union(soil_points))
study_site <- st_transform(study_site, 4326)
basemap2 <- get_tiles(
  peru,
  provider = "Esri.OceanBasemap",
  zoom = 5)
basemap_masked <- mask(basemap, peru_v)
bm <- as.data.frame(basemap_masked, xy = TRUE, na.rm = TRUE)
bm$col <- rgb(bm[[3]], bm[[4]], bm[[5]], maxColorValue = 255)

peru_inset <- ggplot() +
  geom_raster(
    data = bm,
    aes(x = x, y = y, fill = col)
  ) +
  scale_fill_identity() +
  geom_sf(data = study_site, color = "red", size = 2) +
  coord_sf(
    xlim = range(bm$x),
    ylim = range(bm$y),
    expand = FALSE
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(
      fill = "#80b1d3",
      color = "black",
      linewidth = 0.5))
  peru_inset



#Main plot 
manu_plot <- ggplot() +
  annotation_raster(
    basemap_raster,
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"]
  ) +
  geom_sf(data = latrine_pts_sf, color = "white", size = 1.5) +
  geom_sf(data = soil_points, color = "#b3de69", size = 1.5) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE, label_graticule = "SW") +
  annotation_scale(
    location = "bl",      # bottom left
    width_hint = 0.25, text_col = "white",
    line_col = "white" , text_face = "bold") +
  annotation_north_arrow(
    location = "br",      # top right
    which_north = "true",
    style = north_arrow_orienteering(text_col = "white", line_col = "white")) +
  labs(x = "Longitude", y = "Latitude") + 
  theme_minimal()
manu_plot

manu_plot_combo <- ggdraw() +
  draw_plot(manu_plot) +
  draw_plot(
    peru_inset,
    x = 0.52, y = 0.72,  
    width = 0.25,
    height = 0.25)
manu_plot_combo

