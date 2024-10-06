getwd()
# Create a vector of package names
packages <- c("raster", "maptools","gtools","sp","spdep","rgdal",
              "ggplot2","tiff","leaflet","tmap","RColorBrewer",
              "dplyr","OpenStreetMap","maptiles")

# Install the packages
install.packages(packages)

install.packages('rgdal')
install.packages("sf")
install.packages("terra")
install.packages("Iwgeom")
install.packages("rtools")
install.packages("spatsurv")
install.packages("tinytex")
install.packages("webshot2")

install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")

install.packages('rgdal', type = 'source')

install.packages('rgdal', type = "source",configure.args = c('--with-proj-
                  include=/usr/local/include','--with-proj-lib=/usr/local/lib'))
R.version.string

getwd()
setwd('C:/Users/Brain Computers/Desktop/R-files')
getwd()

library(raster)
library(maptools)
library(gtools)
library(sp)
library(spdep)
library(rgdal)
library(ggplot2)
library(tiff)
library(leaflet)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(dplyr)
library(spatsurv)
library(maptiles)
library(tinytex)
library(webshot2)
getwd()

fe<-readOGR("gadm40_GHA_shp","gadm40_GHA_1")
class(fe)
plot(fe)
plot(fe, axes=T)

head(fe@data)

data<-fe@data
class(data)

head(data)
####Hard approach
var<- c("ID_1", "NAME_1")
data<-data [,var]
names(data)
write.csv(data, "data.csv")

feg <- read.csv("data.csv")
summary(feg)
feg$ID
head(feg)
feg_data<- merge(fe, feg, by= "NAME_1")


#The simple format
#fe@data
#fe@data$malaria_p<-c(15.8, 35.4, 29.9, 26.4, 1.0, 18.7, 30.6,22.6,33.3,31.2)
#head(fe)

feg_data <- st_as_sf(feg_data)
class(feg_data)

femap<- tm_shape(feg_data)+
  tm_polygons("malaria_p")+
  tm_text("NAME_1")
femap

femap1<- tmap_leaflet(femap)
femap1

feg_map<-tm_shape(feg_data)+
  tm_polygons("malaria_p",title="Under-five malaria \n prevalence (%)",
              style = "fixed",textNA="",
              breaks = c(1,5,10,15,20,25,30,35.4),
              legend.hist = F) +
  tm_layout(legend.outside = TRUE)+
  tm_scale_bar(position=c("right", "bottom"))

feg_map

feg_map_web <- tmap_leaflet(feg_map)
feg_map_web

###Open street

feg_map1<-tm_basemap(leaflet::providers$OpenStreetMap) +
  tm_shape(feg_data)+
  tm_polygons("malaria_p",title="Under-five malaria \n prevalence (%)",
              style = "fixed",textNA="",
              breaks = c(1,5,10,15,20,25,30,35.4),
              legend.hist = F) +
  tm_layout(legend.outside = TRUE)+
  tm_scale_bar(position=c("right", "bottom"))

feg_map1


feg_map_web <- tmap_leaflet(feg_map)
feg_map_web

feg_map1<-tm_basemap(leaflet::providers$OpenStreetMap) +
  tm_shape(feg_data)+
  tm_polygons("malaria_p", alpha=0.4,title="Under-five malaria \n prevalence (%)",
              style = "fixed",textNA="",
              breaks = c(1,5,10,15,20,25,30,35.4),
              legend.hist = F) +
  tm_layout(legend.outside = TRUE)+
  tm_scale_bar(position=c("right", "bottom"))

feg_map1

feg_map_web <- tmap_leaflet(feg_map1)
feg_map_web
#insert the values directly in the polygons

feg_map1<-tm_basemap(leaflet::providers$OpenStreetMap) +
  tm_shape(feg_data)+
  tm_polygons("malaria_p", alpha=0.4,title="Under-five malaria \n prevalence (%)",
              style = "fixed",textNA="",
              breaks = c(1,5,10,15,20,25,30,35.4),
              legend.hist = F) +
  tm_text("malaria_p")+
  tm_layout(legend.outside = TRUE)+
  tm_scale_bar(position=c("right", "bottom"))

feg_map1

feg_map_web <- tmap_leaflet(feg_map1)
feg_map_web

#cntl+sft+r
# Example 2 ---------------------------------------------------------------
mal_dat<-readOGR("shps","sdr_subnational_data_mis_2019") 
names(mal_dat) 
#from sf package
mal_dat2<-st_read("shps/sdr_subnational_data_mis_2019.shp") 
names(mal_dat2)
#######################################
mal_dat2<-tm_shape(mal_dat)+
  tm_polygons("MLCMLTCANM",title="Percentage of <5s \n tested for malaria",
              style = "fixed",
              breaks = c(92.1,94.0,97.0,100.0),
              legend.hist = F) +
  tm_layout(legend.outside = TRUE)+
  tm_scale_bar(position=c("right", "bottom"))
mal_dat2

mal_map_web <- tmap_leaflet(mal_dat2)
mal_map_web

summary(mal_dat$MLCMLTCANM)


# pal=brewer.pal(3,"RdYlGn")
# mal_test2<-tm_shape(mal_dat)+
#   tm_polygons("MLCMLTCANM",palette=pal, title="Under-fives tested \n for malaria (%)",
#               style = "fixed",textNA="",
#               breaks = c(92.1,94.0,97.0,100.0),
#               legend.hist = F) +
#   tm_layout(legend.outside = TRUE)+
#   tm_scalebar(position=c("right", "bottom"))

mal_d <- read.csv("mal_d.csv")
summary(mal_d)

mal_d$mal_prev_per<- mal_d$mal_prev*100
summary(mal_d)

pal<- colorBin("viridis", 0,25,50,75,100)

dist<- readOGR("gadm40_GHA_shp","gadm40_GHA_2")
Nothern<- subset(dist, NAME_1 == "Northern")
district_colors <- c("red", "green", "blue")  # Add more colors as needed
plot(Nothern,col=district_colors)
tamale_district <- subset(Nothern, NAME_2 == "Tamale")
plot(tamale_district, col = "purple", add = TRUE)  # Overlay Tamale district



#plot(tamale_district, col = "green", main = "Tamale District")

# Assignment --------------------------------------------------------------
fe_1<-readOGR("gadm40_GHA_shp","gadm40_GHA_1")
class(fe_1)
plot(fe_1)
plot(fe_1, axes=T)

head(fe_1@data)

data_1<-fe_1@data
class(data_1)

head(data_1)
####Hard approach
var<- c("ID_1", "NAME_1")
data_1<-data_1 [,var]
names(data_1)
write.csv(data_1, "data_1.csv")

feg_1 <- read.csv("data_1.csv")
summary(feg_1)
feg_1$ID
head(feg_1)
feg_1_data<- merge(fe_1, feg_1, by= "NAME_1")
head(feg_1_data)
class(feg_1_data)
#############################################
feg_1_data <- st_as_sf(feg_1_data)
class(feg_1_data)

femap_1<- tm_shape(feg_1_data)+
  tm_polygons("malaria_m")+
  tm_text("NAME_1")
femap_1

femap1<- tmap_leaflet(femap_1)
femap1

feg_2_map<-tm_shape(feg_1_data)+
  tm_polygons("malaria_m",title="Under-five malaria \n prevalence (%) by microscopy",
              style = "fixed",textNA="",
              breaks = c(1,5,10,15,20,25,30,35.4),
              legend.hist = F) +
  tm_layout(legend.outside = TRUE)+
  tm_scale_bar(position=c("right", "bottom"))

feg_2_map

feg_1_map_web <- tmap_leaflet(feg_2_map)
feg_1_map_web

getwd()
# Geostatistical Modelling and Mapping ------------------------------------
install.packages("INLA", repos = c(getOption("C:/Users/Brain Computers/Desktop/R-files"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)

#Load devtools
library(devtools)
library(usethis)
# Install INLA from GitHub
remotes::install_version("INLA", version="24.05.10",repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

url::download.file(url = "https://github.com/hrue/r-inla/archive/refs/heads/stable.zip", destfile = "INLA.zip")
install.packages("INLA.zip", repos = NULL, type = "source", lib = .libPaths()[1])

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

remotes::install_version("INLA", version="24.05.10",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)


install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dependencies = TRUE)

library(INLA)
