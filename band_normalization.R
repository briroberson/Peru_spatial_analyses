library(terra)

#set wd to flight folder
#change input raster name out output name for each flight 

ms_raster <- rast("L57AL56_multispectral_nocalib_070126.tif")
ext <- rast("l57Al56_ext_rast.tif")
ext_poly <- vect("L57AL56_extent.shp")
ext_poly_UTM <- vect("L57AL56_extent_UTM.shp")

#crop rgb to correct extent
rgb_crop <- crop(rgb, ext_poly)
rgb_masked <- mask(rgb_crop, ext_poly)
plot(rgb_masked)
writeRaster(rgb_masked, "L57AL56_RGB_maked.tif", overwrite = TRUE, datatype = "INT1U") 

#crop ms to correct extent
ms_crop <- crop(ms_raster, ext_poly_UTM)
ms_masked <- mask(ms_crop, ext_poly_UTM)
plot(ms_masked)
writeRaster(ms_masked, "L57AL56_MS_masked.tif", overwrite = TRUE, datatype = "INT2U") 

#check bands
names(ms_raster)
hist(ms_raster)
crs(ms_raster)

#to read in masked layers 
ms_masked <- rast("L57AL56_MS_masked.tif")

#re-assign no data value to NA
ms_masked[ms_masked == 0] <- NA
hist(ms_masked)

#extract min & max values for each band (exluding the extent band, #5)
ms_masked <- ms_masked[[1:4]] #drop alpha channel
min_vals <- global(ms_masked, "min", na.rm = TRUE)[,1]
max_vals <- global(ms_masked, "max", na.rm = TRUE)[,1]

print(min_vals)
print(max_vals)

#normalize each band using min/max normalization, scale to 1000, and round  
ms_1000 <- ms_masked

for (i in 1:nlyr(ms_masked)) {
  ms_1000[[i]] <- round(
    ((ms_masked[[i]] - min_vals[i]) / (max_vals[i] - min_vals[i])) *1000
  )
}

#export 
writeRaster(ms_1000, "L57AL56_MS_masked_norm.tif", datatype = "INT2U", overwrite = TRUE)

#Calculate scaled MSAVI 
red <- ms_masked[[2]]
nir <- ms_masked[[4]]

msavi <- (2 * nir + 1 - sqrt((2 * nir + 1)^2 - 8 * (nir - red))) / 2
msavi_1000 <- round(((msavi+1)/2) * 1000)
# Save the MSAVI raster
writeRaster(msavi_1000, "L57AL56_MSAVI_masked_norm.tif", datatype = "INT2U", overwrite = TRUE)

#Calculate scaled NDWI
green <- ms_masked[[1]]
ndwi <- (green - nir) / (green + nir)
ndwi_1000 <- round(((ndwi+1)/2) * 1000)
# Save the NDWI raster
writeRaster(ndwi_1000, "L57AL56_NDWI_masked_norm.tif", datatype = "INT2U", overwrite = TRUE)

#Calculate reflectance / albedo 
re <- ms_masked[[3]]
albedo <- green + red + re + nir
min_alb <- min(values(albedo), na.rm = TRUE)
max_alb <- max(values(albedo), na.rm = TRUE)
albedo_1000 <- round(((albedo - min_alb) / (max_alb - min_alb)) *1000)
# Save the albedo raster
writeRaster(albedo_1000, "L57AL56_albedo_masked_norm.tif", datatype = "INT2U", overwrite = TRUE)

#to crop other layers 
dsm_aligned <- project(dsm, ms_masked, method = "bilinear")
compareGeom(dsm_aligned, ms_masked)

res(dsm)
res(dsm_aligned)

dsm_crop2 <- crop(dsm_aligned, ext_poly_UTM)
dsm_masked <- mask(dsm_crop2, ext_poly_UTM)
#plot(ms_masked)
writeRaster(dsm_masked, "L57AL56_dsm_masked.tif", overwrite = TRUE) 

