#Covariate calculations to use in eCognition segmentation

library(terra)
library(raster)
library(glcm)

#set wd to target mosaic folder 

#load in files 
nir <- rast("Dec25_L57AL56_MS_NIR.tif")

#convert to raster
nir_rast <- raster(nir)
crs(nir_rast)


Dec25_L57AL56_glcm <- glcm(nir_rast, window = c(11, 11), shift = c(1, 1),
  statistics = c(
    "mean",
    "variance",
    "homogeneity",
    "contrast",
    "dissimilarity",
    "entropy",
    "second_moment"))

names(Dec25_L57AL56_glcm)
crs(Dec25_L57AL56_glcm)
crs(Dec25_L57AL56_glcm) <- crs(nir_rast)

#export 
writeRaster(
  stack(Dec25_L57AL56_glcm),
  filename = names(Dec25_L57AL56_glcm),
  format = "GTiff",
  bylayer = TRUE,
  suffix = "names",
  overwrite = TRUE)

#export 
for (i in 1:nlayers(Dec25_L57AL56_glcm)) { 
  writeRaster(Dec25_L57AL56_glcm[[i]], filename = paste0(names(Dec25_L57AL56_glcm)[i], ".tif"), 
              format = "GTiff", overwrite = TRUE)}


#export 
metrics <- c(
  "mean",
  "variance",
  "homogeneity",
  "contrast",
  "dissimilarity",
  "entropy",
  "second_moment"
)

for (i in seq_along(metrics)) {
  
  r <- rast(Dec25_L57AL56_glcm[,,i])
  
  writeRaster(
    r,
    filename = paste0("prjDec25_L7AL56_glcm_", metrics[i], ".tif"),
    overwrite = TRUE
  )
}



