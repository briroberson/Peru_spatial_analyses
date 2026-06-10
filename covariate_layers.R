#Covariate calculations to use in eCognition segmentation

library(terra)
library(glcm)

#set wd to target mosaic folder 

#load in files 
rgb <- rast("result.tif")
re  <- rast("result_RedEdge.tif")
nir <- rast("result_NIR.tif")

#glcm textures 
nir_mat <- as.matrix(nir, wide = TRUE)
nir_mat[is.na(nir_mat)] <- 0
nir_mat <- round(nir_mat)

Dec25_L57AL56_glcm <- glcm(nir_mat, window = c(5, 5), shift = c(1, 1),
  statistics = c(
    "mean",
    "variance",
    "homogeneity",
    "contrast",
    "dissimilarity",
    "entropy",
    "second_moment"))

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
    filename = paste0("Dec25_L7AL56_glcm_", metrics[i], ".tif"),
    overwrite = TRUE
  )
}



