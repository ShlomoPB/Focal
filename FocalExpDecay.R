library(raster)
library(rgdal)
library(tidyverse)
library(sp)
library(rgeos)
library(ggplot2)

#Upload the raster you want to work with
r <- raster(r"(D:\Golan Wolves GIS\0_1_Rasters\Open Spaces 0_1 TenKM_1.tif)")

#Replace NA cells with 0
r[is.na(r[])] <- 0
writeRaster(r,r"(D:\Golan Wolves GIS\0_1_Rasters\Shootings_10YR_TimeDecay_0_1.tif)")

plot(r)

#Define the raster, distance and shape you want to work with
d1 <- focalWeight(r, 7000, "circle")

#Define the exponential decay function
middle_row <- function(matrix){
  return(ceiling(dim(matrix)[1]/2))
}

middle_col <- function(matrix){
  return(ceiling(dim(matrix)[2]/2))
}

d1[middle_row(d1), middle_col(d1)] <- 1

d1[d1 != 0] <- 1

#Define the dimensions based on your raster pixel size (mine were 25m)
d1_new_matrix <- outer(seq(-280,280,1),seq(-280,280,1),function(x,y) 0.01*sqrt(x^2 + y^2))

d1_new_matrix <- exp(-d1_new_matrix)

my_circle <- (d1 * d1_new_matrix)

# visualize focal circle with chosen equation:

(test_plot <- my_circle %>%
    as_tibble() %>%
    rownames_to_column() %>%
    pivot_longer(2:length(.)) %>%
    mutate(name = parse_number(name)) %>%
    mutate(rowname = as.numeric(rowname)) %>%
    ggplot()+
    aes(x = name, y = rowname, fill = value)+
    geom_tile())

# The focal function itself now that we have defined everything:
the_focus <- focal(r,d1,sum,na.rm=FALSE,pad=TRUE,padValue=0)

writeRaster(the_focus,r"(D:\Golan Wolves GIS\Focal rasters\Shootings_10YR_TimeDecay_Focal1.tif)")

