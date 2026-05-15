###DEC 24 shadow identification  #############################################

library(raster)
library(glcm)
library(terra)
library(sp)


# L57AL56 test #########

L57AL56_rgb <- brick("D:/Briana Roberson/arcpro_exports/L57AL56/L57AL56_rgb_prj.tif")
L57AL56_rgb[L57AL56_rgb == 0] <- NA
plotRGB(L57AL56_rgb, 1, 2, 3, stretch = "lin")
crs(L57AL56_rgb)

#greyscale
L57AL56_greyscale <- mean(L57AL56_rgb)
plot(L57AL56_greyscale)
#fix projection
L57AL56_rgb2 <- brick("D:/Briana Roberson/arcpro_exports/L57AL56/L57AL56_rgb_prj.tif")
crs(L57AL56_greyscale) <- crs(L57AL56_rgb2)
crs(L57AL56_greyscale)

#test crop
ext <- drawExtent()
L57aL56_greyscale_cropped <- crop(L57AL56_greyscale, ext)
plot(L57aL56_greyscale_cropped)


L57AL56_textures_cropped <- glcm(L57aL56_greyscale_cropped)
plot(L57AL56_textures_cropped)
crs(L57AL56_textures_cropped)

writeRaster(L57AL56_textures_cropped, "L57AL56_textures_cropped.tif", overwrite = TRUE)
L57AL56_rgb_cropped <- crop(L57AL56_rgb, ext)
plot(L57AL56_rgb_cropped)


#CLASSIFICATION

#bring in bands 
L57AL56_red <- rast("F:/arcpro_exports/L57AL56/L57AL56_red_prj_norm.tif")
L57AL56_green <- rast("F:/arcpro_exports/L57AL56/L57AL56_green_prj_norm.tif")
L57AL56_re <- rast("F:/arcpro_exports/L57AL56/L57AL56_re_prj_norm.tif")
L57AL56_nir <- rast("F:/arcpro_exports/L57AL56/L57AL56_NIR_prj_norm.tif")
L57AL56_ndvi <- rast("F:/arcpro_exports/L57AL56/L57AL56_NDVI_prj.tif")
L57AL56_msavi <- rast("F:/arcpro_exports/L57AL56/L57AL56_MSAVI.tif")
L57AL56_ci <- rast("F:/arcpro_exports/L57AL56/L57AL57_CI.tif")
L57AL56_rgb_cropped_sr <- rast(L57AL56_rgb_cropped) #convert to terra format

#fix projection
compareGeom(L57AL56_red, L57AL56_green, L57AL56_re, L57AL56_nir, L57AL56_ndvi, L57AL56_msavi,L57AL56_rgb_cropped_sr)
crs(L57AL56_rgb_cropped_sr)
L57AL56_rgb_cropped_sr <- project(L57AL56_rgb_cropped_sr, L57AL56_red)

#fix extent - rgb as template (NOT FOR CROPS) 
L57AL56_red_ext <- resample(L57AL56_red, L57AL56_rgb_sr, method = "bilinear")
L57AL56_green_ext <- resample(L57AL56_green, L57AL56_rgb_sr,  method = "bilinear")
L57AL56_re_ext <- resample(L57AL56_re, L57AL56_rgb_sr,  method = "bilinear")
L57AL56_nir_ext <- resample(L57AL56_nir, L57AL56_rgb_sr,  method = "bilinear")
L57AL56_ndvi_ext <- resample(L57AL56_ndvi, L57AL56_rgb_sr,  method = "bilinear")
L57AL56_msavi_ext <- resample(L57AL56_msavi, L57AL56_rgb_sr,  method = "bilinear")
L57AL56_ci_ext <- resample(L57AL56_ci, L57AL56_rgb_sr,  method = "bilinear")

compareGeom(L57AL56_red_ext, L57AL56_green_ext, L57AL56_re_ext, L57AL56_nir_ext, L57AL56_ndvi_ext, L57AL56_msavi_ext, L57AL56_ci_ext, L57AL56_rgb_sr)


#crop for test 
L57AL56_red_cropped <- crop(L57AL56_red_ext, ext)
L57AL56_green_cropped <- crop(L57AL56_green_ext, ext)
L57AL56_re_cropped <- crop(L57AL56_re_ext, ext)
L57AL56_nir_cropped <- crop(L57AL56_nir_ext, ext)
L57AL56_ndvi_cropped <- crop(L57AL56_ndvi_ext, ext)
L57AL56_msavi_cropped <- crop(L57AL56_msavi_ext, ext)
L57AL56_ci_cropped <- crop(L57AL56_ci_ext, ext)

#fix extent for crops
L57AL56_red_ext <- resample(L57AL56_red_cropped, L57AL56_rgb_cropped_sr, method = "bilinear")
L57AL56_green_ext <- resample(L57AL56_green_cropped, L57AL56_rgb_cropped_sr,  method = "bilinear")
L57AL56_re_ext <- resample(L57AL56_re_cropped, L57AL56_rgb_cropped_sr,  method = "bilinear")
L57AL56_nir_ext <- resample(L57AL56_nir_cropped, L57AL56_rgb_cropped_sr,  method = "bilinear")
L57AL56_ndvi_ext <- resample(L57AL56_ndvi_cropped, L57AL56_rgb_cropped_sr,  method = "bilinear")
L57AL56_msavi_ext <- resample(L57AL56_msavi_cropped, L57AL56_rgb_cropped_sr,  method = "bilinear")
L57AL56_ci_ext <- resample(L57AL56_ci_cropped, L57AL56_rgb_cropped_sr,  method = "bilinear")


#now textures 
L57AL56_textures_sr <- rast(L57AL56_textures_cropped)
L57AL56_textures_ext <- project(L57AL56_textures_sr, L57AL56_red)
L57AL56_textures_ext <- resample(L57AL56_textures_ext, L57AL56_rgb_cropped_sr,  method = "bilinear")

compareGeom(L57AL56_red_ext, L57AL56_green_ext, L57AL56_re_ext, L57AL56_nir_ext, L57AL56_ndvi_ext, L57AL56_msavi_ext, L57AL56_ci_ext, L57AL56_rgb_cropped_sr, L57AL56_textures_ext)
#should say TRUE


#master stack - trial *removed RGB*
L57AL56_rstack <- c(L57AL56_red_ext, L57AL56_green_ext, L57AL56_re_ext, L57AL56_nir_ext, L57AL56_ndvi_ext, L57AL56_msavi_ext, L57AL56_ci_ext, L57AL56_textures_ext)
names(L57AL56_rstack) <- c("Red", "Green", "RedEdge", "NIR", "NDVI", "MSAVI", "CI", names(L57AL56_textures_ext)) 
plot(L57AL56_rstack)


L57AL56_stackvals <- values(L57AL56_rstack)
str(L57AL56_stackvals)

#or a df
rstack_df <- as.data.frame(L57AL56_rstack)
rstack_df_clean <- rstack_df[apply(rstack_df, 1, function(x) all(is.finite(x) & !is.na(x))), ]
saveRDS(df, "rstack_df_clean")
#to read in 
rstack_df_clean <- readRDS("rstack_df_clean")


#perform k-means clustering !
set.seed(4)
L57AL56_kmncluster <- kmeans(na.omit(rstack_df_clean), centers = 20, iter.max = 5000)
str(L57AL56_kmncluster)




#apply to new raster 
cluster_raster <- raster(L57AL56_rstack[[1]])
cluster_raster[valid_rows] <- L57AL56_kmncluster$cluster
plot(cluster_raster, col = rainbow(k))




