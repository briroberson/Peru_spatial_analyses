library(rhdf5)
library(reshape2)
library(terra)
library(plyr)
library(ggplot2)
library(grDevices)
library(raster)


L57AL56_rgb <- brick("D:\\arcpro_exports\\L57AL56\\L57AL56_rgb_prj.tif")
plotRGB(L57AL56_rgb, 1, 2, 3, stretch = "lin")
crs(L57AL56_rgb)
extent(L57AL56_rgb)
e <- drawExtent()
L57AL56_rgb_crop <- crop(L57AL56_rgb, e)
plotRGB(L57AL56_rgb_crop, 1, 2, 3, stretch = "lin")

e2 <- drawExtent()
L57AL56_rgb_crop2 <- crop(L57AL56_rgb, e2)
plotRGB(L57AL56_rgb_crop2, 1, 2, 3, stretch = "lin")


#spectral signatures ____________________________________________________________________________________________

# run in console

#dev.new(noRStudioGD = TRUE)  
#par(col="red", cex=2)
#plotRGB(L57AL56_rgb_crop2, r=1, g=2, b=3, scale=300, stretch = "hist") 
#c <- click(L57AL56_rgb_crop2, n = 6, id=TRUE, xy=TRUE, cell=TRUE, type="p", pch=16, col="red", col.lab="red")

dev.off()
c$row <- c$cell%/%nrow(L57AL56_rgb_crop2)+1 
c$col <- c$cell%%ncol(L57AL56_rgb_crop2)

point_coords <- c[, c("x", "y")]
spectral_values <- extract(L57AL56_rgb_crop2, point_coords)
spectral_values_clean <- spectral_values[, 1:3]
bands <- c("Red", "Green", "Blue")


matplot(t(spectral_values_clean), type='l', lty=1, lwd=2, col=rainbow(nrow(spectral_values_clean)),
        xaxt='n', xlab='Band', ylab='DN Value', main='Spectral Signatures')

axis(side=1, at=1:3, labels=bands, tick=TRUE)

legend("topright", legend=paste("Point", 1:nrow(spectral_values_clean)),
       col=rainbow(nrow(spectral_values_clean)), lty=1, lwd=2)

#now with more bands ____________________________________________________________________________________________
L57AL56_red <- brick("D:/arcpro_exports/L57AL56/L57AL56_red_prj_norm.tif")
L57AL56_green <- brick("D:/arcpro_exports/L57AL56/L57AL56_green_prj_norm.tif")
L57AL56_re <- brick("D:/arcpro_exports/L57AL56/L57AL56_re_prj_norm.tif")
L57AL56_nir <- brick("D:/arcpro_exports/L57AL56/L57AL56_NIR_prj_norm.tif")
L57AL56_ndvi <- brick("D:/arcpro_exports/L57AL56/L57AL56_NDVI_prj.tif")
L57AL56_msavi <- brick("D:/arcpro_exports/L57AL56/L57AL56_MSAVI.tif")
L57AL56_ci <- brick("D:/arcpro_exports/L57AL56/L57AL57_CI.tif")

L57AL56_red_cropped <- crop(L57AL56_red, e)
L57AL56_green_cropped <- crop(L57AL56_green, e)
L57AL56_re_cropped <- crop(L57AL56_re, e)
L57AL56_nir_cropped <- crop(L57AL56_nir, e)
L57AL56_ndvi_cropped <- crop(L57AL56_ndvi, e)
L57AL56_msavi_cropped <- crop(L57AL56_msavi, e)
L57AL56_ci_cropped <- crop(L57AL56_ci, e)

L57AL56_red_ext <- resample(L57AL56_red_cropped, L57AL56_rgb_crop, method = "bilinear")
L57AL56_green_ext <- resample(L57AL56_green_cropped, L57AL56_rgb_crop,  method = "bilinear")
L57AL56_re_ext <- resample(L57AL56_re_cropped, L57AL56_rgb_crop,  method = "bilinear")
L57AL56_nir_ext <- resample(L57AL56_nir_cropped, L57AL56_rgb_crop,  method = "bilinear")
L57AL56_ndvi_ext <- resample(L57AL56_ndvi_cropped, L57AL56_rgb_crop,  method = "bilinear")
L57AL56_msavi_ext <- resample(L57AL56_msavi_cropped, L57AL56_rgb_crop,  method = "bilinear")
L57AL56_ci_ext <- resample(L57AL56_ci_cropped, L57AL56_rgb_crop,  method = "bilinear")

L57AL56_stack <- stack(L57AL56_red_ext, L57AL56_green_ext, L57AL56_re_ext, L57AL56_nir_ext, L57AL56_ndvi_ext, L57AL56_msavi_ext, L57AL56_ci_ext)

names(L57AL56_stack) <- c("Red", "Green", "RedEdge", "NIR", "NDVI", "MSAVI", "CI") 

#if rerunning w diff points, start here 
spectral_values2 <- extract(L57AL56_stack, point_coords)
bands <- c("Red", "Green", "RedEdge", "NIR", "NDVI", "MSAVI", "CI") 


matplot(t(spectral_values2), type='l', lty=1, lwd=2, col=viridis(nrow(spectral_values2)),
        xaxt='n', xlab='Band', ylab='DN Value', main='Spectral Signatures')

axis(side=1, at=1:7, labels=bands, tick=TRUE)

legend("topleft", legend = class_labels,
       col = viridis(6), lty = 1, lwd = 2)

class_labels <- c("1 = light rock",
                  "2 = mixed rock",
                  "3 = green plant",
                  "4 = brown/grassy plant",
                  "5 = small rock shadow",
                  "6 = large rock shadow")



#JUST SHADOWS 
# run in console each line 

#dev.new(noRStudioGD = TRUE)  
#par(col="red", cex=1)
#plotRGB(L57AL56_rgb_crop2, r=1, g=2, b=3, scale=300, stretch = "hist") 
#c <- click(L57AL56_rgb_crop2, n = 6, id=TRUE, xy=TRUE, cell=TRUE, type="p", pch=16, col="red", col.lab="red")
dev.off()

par(col = "red", cex = 2)

# Plot RGB image
plotRGB(L57AL56_rgb_crop2, r = 1, g = 2, b = 3, scale = 300, stretch = "hist")

# ---- Cover Type 1 ----
message("Select 10 points for COVER TYPE 1")
cover1_points <- click(L57AL56_rgb_crop2, n = 10, id = TRUE, xy = TRUE, cell = TRUE,
                       type = "p", pch = 16, col = "red", col.lab = "red")

# ---- Cover Type 2 ----
message("Select 10 points for COVER TYPE 2")
cover2_points <- click(L57AL56_rgb_crop2, n = 10, id = TRUE, xy = TRUE, cell = TRUE,
                       type = "p", pch = 16, col = "blue", col.lab = "blue")


spectra_cover1 <- extract(L57AL56_stack, cover1_points[, c("x", "y")])
spectra_cover2 <- extract(L57AL56_stack, cover2_points[, c("x", "y")])

spectra_cover1 <- as.data.frame(spectra_cover1)
spectra_cover2 <- as.data.frame(spectra_cover2)

colnames(spectra_cover1) <- bands
colnames(spectra_cover2) <- bands

spectra_cover1$CoverType <- "Big rock shadows"
spectra_cover2$CoverType <- "Small rock shadows"

spectral_values2 <- rbind(spectra_cover1, spectra_cover2)
spectral_matrix <- as.matrix(spectral_values2[, bands])

# Plot all point spectra as separate lines
matplot(
  t(spectral_matrix),
  type = 'l', lty = 1, lwd = 2,
  col = c(rep("cyan4", nrow(spectra_cover1)), rep("lightgreen", nrow(spectra_cover2))),
  xaxt = 'n',
  xlab = 'Band',
  ylab = 'Reflectance (DN Value)'
)

# Add x-axis labels (band names)
axis(1, at = 1:length(bands), labels = bands)


# Optionally overlay mean spectra for each cover type
cover1_mean <- colMeans(spectral_matrix[spectral_values2$CoverType == "Big rock shadows", ])
cover2_mean <- colMeans(spectral_matrix[spectral_values2$CoverType == "Small rock shadows", ])

lines(1:length(bands), cover1_mean, col = "blue4", lwd = 3)
lines(1:length(bands), cover2_mean, col = "forestgreen", lwd = 3)
legend("topright",
       legend = c("mean big shadows", "mean small shadows"),
       col = c("blue4", "forestgreen"), lwd = 3)




