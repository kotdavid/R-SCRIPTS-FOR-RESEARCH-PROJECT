
        #### THE IMPACT OF CLIMATE CHANGE IN RWANDA   ###################


## This R script reads in and creates municipality level data of the Standardised Precipitation-Evapotranspiration Index (SPEI)
# https://spei.csic.es/home.html

# the data comes in netCDF format
# R has the capability of reading and writing netCDF files using ncdf4 packages and through other packages like raster


#clear the environment
remove(list = ls())

# Insert your working directory
        
setwd("")   ### I HAVE REMOVED THE WORKING DIRECTORY (DATA SOURCE)  ##########

# Load all necessary packages###
pkgs <- c("rgdal", "maptools", "gridExtra", "rgeos", "dplyr", "haven", "sf", "brickr", "raster", "tidyverse", "ggplot2", "cowplot", "ncdf4", "RNetCDF")
lapply(pkgs, require, character.only = TRUE)

# same way to install packages
# pkgs <- c("rgdal", "maptools", "gridExtra", "rgeos", "dplyr", "haven", "sf", "brickr", "raster", "tidyverse", "ggplot2", "cowplot", "ncdf4", "RNetCDF")
# install.packages(pkgs)

library(ncdf4)
nc_spei <- nc_open("spei01.nc")  #1-month time scale
print(nc_spei) #this gives us the metadata

# Save meta info to a text file: 
{
    sink('spei__metadata.txt')
    print(nc_spei)
    sink()
}

lon <- ncvar_get(nc_spei, "lon") 
lat <- ncvar_get(nc_spei, "lat", verbose = F)
t <- ncvar_get(nc_spei, "time")

#units: days since Jan 1 1901
tunits <- ncatt_get(nc_spei,"time","units")
tunits
#The object tunits has two components hasatt (a logical variable), and 
# tunits$value, string variable "days since 1900-1-1"

# Read in the data from the precip variable ####
spei.array <- ncvar_get(nc_spei, "spei") # store the data in a 3-dimensional array

dlname <- ncatt_get(nc_spei,"spei","long_name") #monthly mean of surface temperature
dunits <- ncatt_get(nc_spei,"spei","units")
fillvalue <- ncatt_get(nc_spei, "spei", "_FillValue") #missing data on website says -9.96921e+36f

# verify the dimensions of the array.
dim(spei.array)

nc_close(nc_spei) 
# 2) Reshaping from raster to rectangular data ####
# NetCDF files or data sets are naturally 2-D raster slabs (e.g. longitude by latitude "slices"), 
# 3-D bricks (e.g. longitude by latitude by time), or 4-D arrays (e.g. longitude by latitude by height by time) 
# most data analysis routines in R expect 2-D variable-by-observation "tidy" data frames. 

# A) Time variable:
# NetCDF time is usually stored as the CF (Climate Forecast) "time since" format that is not usually human-readable.
# The time variable, in "time-since" units can be converted into "real" (or more easily readable) time values by splitting 
# the time tunits$value string into its component parts, and then using the chron() function to determine the absolute value of each time value from the time origin.

# Read time coordinate and attributes:
time_coord2 <-t
time_unit2 <- tunits$value

library("RNetCDF")
time_posixct2 <- utcal.nc(time_unit2, time_coord2, "c")
time_posixct2 

dim(spei.array)

spei.brick <- brick(spei.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

rm(spei.array)

spei.brick <- t(flip(spei.brick, direction='x'))

save(spei.brick, time_posixct2, file = "speibrick.RData")


# Upload rwd adm3 shapefile
setwd("")  ### I HAVE REMOVED THE WORKING DIRECTORY (DATA SOURCE)  ##########


rd_adm3 <- read_sf("rwa_adm3_2006_NISR_WGS1984_20181002.shp")

# rda_adm4 <- read_sf("rwa_adm4_2006_NISR_WGS1984_20181002.shp")



# load SPEI data in raster format

# This file contains data of SPEI indicators from 01Jan1900 to 01Jan2020

setwd("")  ### I HAVE REMOVED THE DATA SOURCE ##########

load("speibrick.RData")

# Select specific layers of the rasterfile
r1 <- spei.brick[[1]] # 01Jan1900
p1 <- plot(r1)

r2 <- spei.brick[[1200]] # 01Jan2000
p2 <- plot(r2)

r3 <- spei.brick[[1440]] #01Jan2020
p3 <- plot(r3)

# Cut the rasterfile using rwanda adm03 extents

bbox <- st_bbox(rd_adm3); bbox

# With this line I check if the CRS of the two files is the same BEFORE cropping the raster file
st_crs(rd_adm3) == st_crs(spei.brick[[1]])

ntc_spei<- crop(spei.brick, bbox)

save(ntc_spei, file = "speibrick.RData")

# Extract average SPEI indicator values at adm03 level for rwanda

library(exactextractr)
rd_spei <- exact_extract(ntc_spei, rd_adm3, "mean")
rd_spei$admin3<-rd_adm3$ADM3_PCODE


rd_spei <- rd_spei%>%
    relocate(admin3) %>%
    pivot_longer(2:1441, #columns (month = column)
                 names_to = "month",
                 names_prefix = "mean.layer.", #take away that prefix
                 values_to = "spei_mean")

month <- rep(time_posixct2,times= 701) #we take our dates and replicate it for each adm3 (701 in total)

# rd_spei$date <- as.Date(month[1:nrow(rd_spei)]) # Subset the month vector to match the number of rows in rd_spei:

month_subset <- month[1:599040] # Subset 'month' to have 599040 elements
rd_spei$date <- as.Date(month_subset)  # default format, %Y-%m-%d

# Formatting data variable
rd_spei$year <- strftime(rd_spei$date, format="%Y")
rd_spei$month_str <- strftime(rd_spei$date, format="%b")
rd_spei$month <- strftime(rd_spei$date, format="%m")

# Selecting years from 2000 onwards, getting yearly values of SPEI
rd_spei_adm3 <- rd_spei %>%
    filter(year >= 2000)%>%
    group_by(year, admin3)%>%
    summarize(spei = mean(spei_mean), .groups = "keep")%>% # you can remove .group = "keep" or use .group = "drop". 
    ungroup()

#Plot time series data

#Calculate yearly average
rwanda <- rd_spei_adm3%>%
    group_by(year)%>%
    summarize(spei = mean(spei, na.rm = T))

#Identify peaks
max_peak <- mali[which.max(rwanda$spei), ]
min_peak <- mali[which.min(rwanda$spei), ]

plot <- ggplot(data = rwanda, aes(x = as.numeric(year), y = spei)) +
    geom_line() +
    geom_point(size = 5) +
    geom_point(size = 5) +
    labs(x = "Year", y = "SPEI", title = "The Standardized Precipitation Evapotranspiration index - SPEI
of Rwanda 2000 - 2020") +
    theme_minimal()

plot


plot <- ggplot(data = rwanda, aes(x = as.numeric(year), y = spei)) +
    geom_line() +
    geom_point(data = max_peak, aes(color = "Maximum"), size = 5) +
    geom_point(data = min_peak, aes(color = "Minimum"), size = 5) +
    labs(x = "Year", y = "SPEI", title = "SPEI (Rwanda, 2000-2020)") +
    theme_minimal() + 
    scale_color_manual(values = c("Minimum" = "red", "Maximum" = "blue"),
                       guide = guide_legend(title = "Peaks"))


plot


#Select the admin3 registering the maximum and minimum peak
rd_admin3 <- rd_spei_adm3%>%
    group_by(year, admin3)%>%
    summarize(spei = mean(spei, na.rm = T))%>%
    ungroup()

adm3max <- max(rd_admin3$spei, na.rm = T)
adm3min <- min(rd_admin3$spei, na.rm = T)

names <- rd_admin3%>%
    mutate(maxmin = case_when(spei == adm3max ~ "max", 
                              spei == adm3min ~ "min"))%>%
    filter(!is.na(maxmin))

rd_admin3 <- rd_admin3%>%
    filter(admin3 %in% names$admin3)

plot <- ggplot(data = rd_admin3, aes(x = as.numeric(year), y = spei, color = admin3))+
    geom_line()+
    theme_minimal()

plot

# What's going on here?


rd_plot <- rd_adm3%>%
    filter(ADM3_PCODE %in% names$admin3)%>%
    mutate(admin3 = ADM3_PCODE)

rd_plot <- left_join(rd_plot, names)

ggplot()+
    geom_sf(data = rd_plot, aes(fill = as.factor(maxmin)))+
    geom_sf(data = rd_adm3, fill = NA) 

#Two admin3 are within the same SPEI cell


# Add a geometry to the data
rd_adm3 <- rd_adm3 %>%
    mutate(admin3 = ADM3_PCODE)

rd_spei_fin_adm3 <- left_join(rd_spei_adm3, rd_adm3
                                %>%dplyr::select(admin3, geometry))

rd_spei_fin_adm3 <- st_as_sf(rd_spei_fin_adm3, crs = st_crs(rd_adm3))

p_2000 <- ggplot() + 
    geom_sf(data = rd_spei_fin_adm3 %>% filter(year == 2000), aes(fill = spei), color = NA) +
    scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0,
                         breaks = c(-0.4, 0, 0.3),  
                         labels = c("Dry", "Neutral", "Wet"), 
                         name = "Weather Shocks") +  
    labs(title = "Weather conditions in Rwanda - SPEI (2000)") + 
    theme_minimal()


p_2010 <- ggplot() + 
    geom_sf(data = rd_spei_fin_adm3 %>% filter(year == 2010), aes(fill = spei), color = NA) +
    scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0,
                         breaks = c(-0.6, 0, 0.4),  # define the point for "dry", "neutral", "wet"
                         labels = c("Dry", "Neutral", "Wet"),  # assign the labels
                         name = "Weather Shocks") +  # personal the name of the legend
    labs(title = "Weather conditions in Rwanda - SPEI (2010)") +  # add the title 
    theme_minimal()


p_2015 <- ggplot() + 
    geom_sf(data = rd_spei_fin_adm3 %>% filter(year == 2015), aes(fill = spei), color = NA) +
    scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0,
                         breaks = c(-0.6, 0, 0.2),  
                         labels = c("Dry", "Neutral", "Wet"),  
                         name = "Weather Shocks") +  
    labs(title = "Weather conditions in Rwanda - SPEI (2015)") + 
    theme_minimal()

p_2020 <- ggplot() + 
    geom_sf(data = rd_spei_fin_adm3 %>% filter(year == 2020), aes(fill = spei), color = NA) +
    scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0,
                         breaks = c(-0.6, 0, 0.5),  
                         labels = c("Dry", "Neutral", "Wet"), 
                         name = "Weather Shocks") +  
    labs(title = "Weather conditions in Rwanda - SPEI (2020)") +  
    theme_minimal()


# To plot the map

library(patchwork)
p_combined <- p_2000+ p_2010 + p_2015 + p_2020 + 
    plot_layout(ncol = 2, nrow = 2) 

p_combined


## Identify climate shocks

# For the given SPEI thresholds, it counts the number of shocks in each admin3
shock_adm3 <- rd_spei%>%
    filter(year >= 2000)%>%
    mutate(year = as.numeric(year))%>%
    mutate(dry = ifelse(spei_mean <= -1, 1, 0), 
           wet = ifelse(spei_mean >= +1, 1, 0))%>%
    group_by(year, admin3)%>%
    summarize(dry = sum(dry), 
              wet = sum(wet))%>%
    ungroup()%>%
    mutate(shock = dry + wet)

summary(shock_adm3$shock)
summary(shock_adm3$dry)
summary(shock_adm3$wet)


# save the map

save(shock_adm3, file = "") ### I HAVE REMOVED THE LOCATION DIRECTORY ##########



##################### THE END ########################






