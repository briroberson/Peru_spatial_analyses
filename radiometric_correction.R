library(terra)
library(sf)
library(dplyr)
library(purrr)
library(mapview)

#Set working directory to target flight folder 

#1: Read in multispectral rasters 
red <- rast("flight_.tif")
green <- rast()
nir <-rast()
re <- rast()

#2: Digitize tarp polygon & create samples
mapview(red)
pt <- terra::click(red, n = 1, xy = TRUE) 
pt_v <- vect(pt) #convert to vector 
tarp_v <- buffer(pt_v, width = 0.5) #buffer width in meters, 40% of tarp width
tarp_samps <- spatSample(tarp_v, size = 30) #30 samples

#3: Extract pixel values from tarp samples across bands 
red_tarp <- extract(red_tarp, tarp_samps) [,2]
green_tarp <- extract(red_tarp, tarp_samps) [,2]
nir_tarp <- extract(red_tarp, tarp_samps) [,2]
re_tarp <- extract(red_tarp, tarp_samps) [,2]

#3a: FOR REFERENCE FLIGHT ONLY - Save RDS 
base <- data.frame(red_base = red_tarp, green_base = green_tarp, nir_base = nir_tarp, re_base = re_tarp)
saveRDS(base, "base_Dec25.rds")

#3b: Save means as RDS 
df < data.frame(red_flt = red_tarp, green_flt = green_tarp, nir_flt = nir_tarp, re_flt = re_tarp)
saveRDS(df, "flight_tarp_means_Dec25.rds")
#to read back in 
flight_tarp_means <- readRDS("flight_tarp_means_Dec25.rds")

#4: Load baseline flight values
base <- readRDS("DPonds_tarp_means_Dec25.rds")
base_red <- base$red
base_green <- base$green
base_nir <- base$nir
base_re <- base$re

#5: Fit regressions

df <- data.frame(red_flt = red_tarp, green_flt = green_tarp, nir_flt = nir_tarp, re_flt = re_tarp)

red_mod <- lm(red_base ~ red_tarp)

stack_flt <- c(red, green, re, nir)
names(stack_flt) <- c("red", "green", "re", "nir")

flight_red_radcor <- predict(stack_flt, model_red)




