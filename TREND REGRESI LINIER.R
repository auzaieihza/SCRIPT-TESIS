install.packages(c("raster", "Kendall", "Rtsne"))
install.packages("Kendall")
install.packages("hydroTSM")

library(raster)
library(Kendall)
library(Rtsne)
library(hydroTSM)
library(trend)
library(zoo)
library(zyp)
library(terra)
library(spatialEco)
library(ggplot2)
library(sp)

"raster_2013 <- raster("D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/asset bandung/rsei/RSEI_KOTA_BANDUNG_2013_KEMARAU.tif")
raster_2018 <- raster("D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/asset bandung/rsei/RSEI_KOTA_BANDUNG_2018_KEMARAU.tif")"

ras"ter_2013 <- raster("D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/asset bandung/rsei/RSEI_KOTA_BANDUNG_2014_HUJAN.tif")
raster_2018 <- raster("D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/asset bandung/rsei/RSEI_KOTA_BANDUNG_2018_HUJAN.tif")"

"raster_2013 <- raster("D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/asset semarang/rsei/RSEI_KOTA_SEMARANG_2013_KEMARAU.tif")
raster_2018 <- raster("D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/asset semarang/rsei/RSEI_KOTA_SEMARANG_2018_KEMARAU.tif")"

raster_2013 <- raster("D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/asset semarang/rsei/RSEI_KOTA_SEMARANG_2013_HUJAN.tif")
raster_2018 <- raster("D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/asset semarang/rsei/RSEI_KOTA_SEMARANG_2018_HUJAN.tif")


raster_stack <- stack(raster_2013, raster_2018)

print(raster_stack)

plot(raster_stack)

#REGRESI LINEAR (kernel diubah menjadi 3x3 px; 1:3 menunjukkan datanya 3)
# https://gis.stackexchange.com/questions/400718/perform-a-thiel-sen-regression-on-a-raster-stack-in-r
rs = lapply(1:2, function(i){raster(matrix(runif(9),3,3))})
time = 1:2

fun <- function(x){if(is.na(x[1])){NA}else{m=lm(x ~ time); summary(m)$coefficients[2]}}
slope_raster <- calc(raster_stack, fun)

plot(slope_raster, main = "Trend RSEI 2013 - 2018 Kota Semarang Musim Kemarau")

writeRaster(slope_raster, filename = "D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/HASIL NEW/Gradient_Semarang_Hujan.tif", format="GTiff", overwrite=TRUE)


#coefficient and intercept value
fun <- function(x){
  if (is.na(x[1])){
    c(NA, NA)  # Return NA for both intercept and coefficient if there are NA values
  } else {
    m <- lm(x ~ time)
    coef <- summary(m)$coefficients
    c(intercept = coef[1], coefficient = coef[2])
  }
}

intercept_coefficient_raster <- calc(raster_stack, fun)


# Plotting Intercept Raster
plot(intercept_coefficient_raster[[1]], main = "Intercept Values")

# Plotting Coefficient Raster
#plot(intercept_coefficient_raster[[2]], main = "Coefficient Values")

# Menyimpan hasil perhitungan intercept dalam format TIFF
writeRaster(intercept_coefficient_raster[[1]], filename = "D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/NEW TREND FIX/HASIL NEW/Intercept_Semarang_Hujan.tif", format = "GTiff", overwrite = TRUE)

# Menyimpan hasil perhitungan koefisien dalam format TIFF
#writeRaster(intercept_coefficient_raster[[2]], filename = "D:/KULIAH ZAI MAGISTER PENGINDERAAN JAUH/BISMILLAH THESIS/SCRIPT R TREND/HASIL/Coefficient_BANDUNG_KEMARAU.tif", format = "GTiff", overwrite = TRUE)


-----------------------------------------------------------------------------------------------------------------------------------

# Calculate RMSE and R-squared values
fun_rmse_rsquare <- function(x) {
  if (is.na(x[1])) {
    c(NA, NA)
  } else {
    m <- lm(x ~ time)
    pred <- predict(m)
    rmse <- sqrt(mean((x - pred)^2))
    rsquared <- summary(m)$r.squared
    c(RMSE = rmse, R_squared = rsquared)
  }
}

rmse_rsquare_raster <- calc(raster_stack, fun_rmse_rsquare)

rmse_rsquare_raster


# Plot RMSE and R-squared rasters
plot(rmse_rsquare_raster[[1]], main = "RMSE Raster")
plot(rmse_rsquare_raster[[2]], main = "R-squared Raster")



