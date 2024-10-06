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
##################################################################
# Read shapefile data for Nigerian states
state <- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
View(state)
# Define study sites
study_sites <- c("Anambra", "Sokoto", "Oyo")
# Subset the state data for the study sites
study_sites_data <- subset(state, NAME_1 %in% study_sites)
# Plot the Nigerian states
plot(state, col = "grey")
# Overlay study sites in orange
plot(study_sites_data, col = "orange", add = TRUE)


# Map plotting 2 ----------------------------------------------------------

# Read shapefile data for Nigerian states
state <- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
View(state)
# Define study sites
study_sites <- c("Anambra", "Sokoto", "Oyo")
# Subset the state data for the study sites
study_sites_data <- subset(state, NAME_1 %in% study_sites)

# Subset the state data for the study sites
study_sites_data <- subset(state, NAME_1 %in% study_sites)

# Plot the Nigerian states
plot(state, col = "grey")

# Overlay study sites in different colors
colors <- c("magenta", "red", "orange")
for (i in 1:length(study_sites)) {
  plot(study_sites_data[i, ], col = colors[i], add = TRUE)
  # Add labels with site names and numbers
  text(study_sites_data[i, ], labels = paste(i, ". ", study_sites[i]), pos = 1)
}
###################
# Read shapefile data for Nigerian states
state <- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
# Define study sites
study_sites <- c("Anambra", "Oyo", "Sokoto")
# Subset the state data for the study sites
study_sites_data <- subset(state, NAME_1 %in% study_sites)
# Plot the Nigerian states
plot(state, col = "grey")
# Overlay study sites in different colors
colors <- c("orange", "blue", "green")
for (i in 1:length(study_sites)) {
  plot(study_sites_data[i, ], col = colors[i], add = TRUE)
  # Add labels with site names and numbers
  text(study_sites_data[i, ], labels = paste(i, ". ", study_sites[i]), pos = 1)
}
###################################################################
# Read shapefile data for Nigerian states
state <- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
# Define study sites
study_sites <- c("Anambra", "Oyo", "Sokoto")
# Subset the state data for the study sites
study_sites_data <- subset(state, NAME_1 %in% study_sites)
# Plot the Nigerian states
plot(state, col = "grey", axes=T)
# Overlay study sites in different colors
colors <- c("magenta", "orange", "red")
site_numbers <- c(5.4, 20.9, 35.9)  # Arbitrary numbers for each site
for (i in 1:length(study_sites)) {
  plot(study_sites_data[i, ], col = colors[i], add = TRUE)
  text(study_sites_data[i, ], labels = study_sites[i], pos = 3, cex = 0.8, adj = c(0.1, 0.1))  # Site names
  text(study_sites_data[i, ], labels = site_numbers[i], pos = 1, cex = 0.8, adj = c(0.1, 0.1))  # Numbers
}
# Add a legend for temperature levels
legend("bottomright", inset = c(0.2, -0.0005), legend = c("Hypo-endemic", "Meso-endemic", "Hyper-endemic"), fill = colors, title = "Malaria endemicity", cex = 0.5, adj = c(0.1, 0.1))
# Add compass (north arrow)
arrows(x0 = 0.9, y0 = 0.5, x1 = 0.9, y1 = 0.2, length = 0.1, col = "black", lwd = 2)
text(x = 0.9, y = 0.2, labels = "N", col = "black", cex = 0.5)
# Add scale bar (optional)
scale_bar(position = c("left", "bottom"), text.size = 0.8)
###########################tm_map version#############################
library(tmap)

# Load the state data
state <- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")

# Define study sites
study_sites <- c("Anambra", "Oyo", "Sokoto")

# Subset the state data for the study sites
study_sites_data <- subset(state, NAME_1 %in% study_sites)

# Set the mode to "view"
tmap_mode("view")

# Create the map with state outlines
tm_shape(state) + tm_fill(col = "grey") + tm_borders(col = "black")

# Overlay study sites in different colors
colors <- c("magenta", "orange", "red")
site_numbers <- c(5.4, 20.9, 35.9)  # Arbitrary numbers for each site

for (i in 1:length(study_sites)) {
  tm_borders(study_sites_data[i, ], col = colors[i], lwd = 2)
  tm_text(study_sites_data[i, ], labels = study_sites[i], size = 0.8)
  tm_text(study_sites_data[i, ], labels = site_numbers[i], size = 0.8)
}

# Add a legend for malaria levels
tm_legend(position = c("bottom"), title = "Malaria endemicity",
          text.size = 0.5)

# Add compass (north arrow) below the legends
tm_compass(x = 0.9, y = 0.1, size = 0.1, col = "black")

# Add scale bar
tm_scale_bar(position = c("left", "bottom"), text.size = 0.8)
###################################################################
library(ggplot2)
library(sf)

# Load the state data
state <- st_read("gadm41_NGA_shp", layer = "gadm41_NGA_1")

# Define study sites
study_sites <- c("Anambra", "Oyo", "Sokoto")

# Subset the state data for the study sites
study_sites_data <- state[state$NAME_1 %in% study_sites, ]

# Calculate centroids for study sites
study_sites_data$centroid_x <- rowMeans(study_sites_data[, c("long", "lat")])
study_sites_data$centroid_y <- rowMeans(study_sites_data[, c("lat", "long")])

# Create the map
ggplot() +
  geom_sf(data = state, fill = "grey", color = "black") +
  geom_sf(data = study_sites_data, aes(fill = NAME_1), color = "black") +
  geom_text(data = study_sites_data, aes(x = centroid_x, y = centroid_y, label = NAME_1), size = 3, vjust = 0.5) +
  geom_text(data = study_sites_data, aes(x = centroid_x, y = centroid_y, label = site_numbers), size = 3, vjust = -0.5) +
  scale_fill_manual(values = c("Anambra" = "magenta", "Oyo" = "orange", "Sokoto" = "red")) +
  theme_minimal()

###################################################################

nig<-readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
class(nig)
plot(nig)
plot(nig, axes=T)

head(nig@data)

nig_data<-nig@data
class(nig_data)
head(nig_data)
####Hard approach
var1<- c("GID_1", "NAME_1")
nig_data<-nig_data [,var1]
names(nig_data)
write.csv(nig_data, "nig_new.csv")

nig_new <- read.csv("nig_new.csv")
summary(nig_new)
nig_new$GID_1
head(nig_new)
nig_newdata<- merge(nig, nig_new, by= "NAME_1")
class(nig_newdata)

#The simple format
#fe@data
#fe@data$malaria_p<-c(15.8, 35.4, 29.9, 26.4, 1.0, 18.7, 30.6,22.6,33.3,31.2)
#head(fe)

nig_newdata <- st_as_sf(nig_newdata)
class(nig_newdata)


nigmap<- tm_shape(nig_newdata)+
  tm_polygons("Prevalence")+
  tm_text("NAME_1")
nigmap

nigmap1<- tmap_leaflet(nigmap)
nigmap1
###############################################
# Assuming you've loaded the shapefile as 'nig_states'
nigmap2 <- tm_shape(nig_newdata) +
  tm_borders() +  # Add state boundaries
  tm_polygons("Prevalence", title = "Under-five malaria \n prevalence (%) \n by RDT",
              style = "fixed", textNA = "",
              breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80),
              legend.hist = FALSE) +
  #tm_shape(NAME_1) +  # Separate layer for state names
  tm_text("NAME_1", size = 0.5, col = "black") +  # Add state names
  tm_compass(type = "rose", position = c("left","top"), show.labels = 2, size = 1) +
  #tm_compass(type = "8-star", x = 0, y = 15, show.labels = 1) +
  tm_layout(legend.outside = TRUE) +


# Replace 'state_name' with the actual column name containing state names in your shapefile
nigmap2
# # Create a separate compass layout element
# compass_layout <- tm_layout(legend.outside = TRUE) +
#   tm_compass(type = "arrow", size = 1.5, show.labels = 3)
# arrow, 4-star, 8-star, radar, or rose
# # Combine the main map and compass layout
# nigmap2_with_compass <- nigmap2 + compass_layout
# 
# # Display the final map
# tmap_mode("plot")
# plot(nigmap2_with_compass)


nigmap2_web <- tmap_leaflet(nigmap)
nigmap2_web


# vector abundance map ----------------------------------------------------

vec_abundace<- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
class(vec_abundace)
plot(vec_abundace)
plot(vec_abundace, axes=T)

head(vec_abundace@data)

data<-vec_abundace@data
class(data)

head(data)
####Hard approach
var<- c("GID_1", "NAME_1")
data<-data [,var]
names(data)
set.seed(123)
write.csv(data, "datav.csv")

vec_csv <- read.csv("datav.csv")
summary(vec_csv)
vec_csv$GID_1
head(vec_csv)
vec_data<- merge(vec_abundace, vec_csv, by= "NAME_1")

vec_data <- st_as_sf(vec_data)
class(vec_data)

vecmap<- tm_shape(vec_data)+
  tm_polygons("vec_abundace")+
  tm_text("NAME_1")
vecmap

vecmap1<- tmap_leaflet(vecmap)
vecmap1
###############################################################################
# Create a new column with additional text
# vec_data$NAME_1_with_text <- ifelse(vec_data$NAME_1 %in% c("State1", "State2"),
#                                     paste(vec_data$NAME_1, " - Additional Text"),
#                                     vec_data$NAME_1)

# Create a new column with specific text for each state
vec_data$NAME_1_with_text <- vec_data$NAME_1
vec_data$NAME_1_with_text[vec_data$NAME_1 == "Oyo"] <- "Oyo\nAn.g.f"
vec_data$NAME_1_with_text[vec_data$NAME_1 == "Sokoto"] <- "Sokoto\nAn.g.f.a"
vec_data$NAME_1_with_text[vec_data$NAME_1 == "Ebonyi"] <- "Ebonyi\nAn.g.f"
# Add more states as needed
############################
vec_map <- tm_shape(vec_data) +
  tm_polygons("vec_abundace", title = "Major Anopheles spp\nabundance and composition",
              style = "fixed", textNA = "",
              breaks = c(1, 200, 400, 600, 800, 1000, 1200, 1400),
              legend.hist = FALSE) +
  tm_text("NAME_1_with_text") +
  tm_layout(legend.outside = TRUE) +
  tm_scale_bar(position = c("right", "bottom"))

vec_map

vec_map_web <- tmap_leaflet(vec_map)
vec_map_web
############################################################################
############################################################################
############################################################################
# Prevalence 2010 ---------------------------------------------------------
pre_2010<- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
class(pre_2010)
plot(pre_2010)
plot(pre_2010, axes=T)

head(pre_2010@data)

data<-pre_2010@data
class(data)
head(data)
####Hard approach
var<- c("GID_1", "NAME_1")
data<-data [,var]
names(data)
set.seed(123)
write.csv(data, "pre2010.csv")

preten_csv <- read.csv("pre2010.csv")
summary(preten_csv)
preten_csv$GID_1
head(preten_csv)
preten_data<- merge(pre_2010, preten_csv, by= "NAME_1")

preten_data <- st_as_sf(preten_data)
class(preten_data)

premap<- tm_shape(preten_data)+
  tm_polygons("prev_mpm")+
  tm_text("NAME_1")
premap

premap1<- tmap_leaflet(premap)
premap1
#####################################################
preten_data$NAME_1 <- as.character(preten_data$NAME_1)
preten_data$NAME_1[preten_data$NAME_1 == "Anambra"] <- "Anambra★"  # Red star
preten_data$NAME_1[preten_data$NAME_1 == "Oyo"] <- "Oyo★"
preten_data$NAME_1[preten_data$NAME_1 == "Sokoto"] <- "Sokoto★" #●Thick red dot
################################################################
pre_map <- tm_shape(preten_data) +
  tm_polygons("prev_mpm", title = "2010 Malaria prevalence \nby microscopy",
              style = "fixed", textNA = "",
              breaks = c(25, 30, 35, 40, 45, 50, 55),
              legend.hist = FALSE) +
  tm_shape(preten_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = F) +
  tm_shape(preten_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = F, ymod = -0.2) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0))+
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_compass(position = c("left", "top")) #, inner.margins = c(0.1, 0.1, 0, 0))
  #tm_compass(type = "8star", position = c("left", "top"), size = 2)
pre_map

pre_map_web <- tmap_leaflet(pre_map)
pre_map_web
##########################################################################
# Convert NAME_1 to character
preten_data$NAME_1 <- as.character(preten_data$NAME_1)

# Assign special labels to the states
preten_data$NAME_1[preten_data$NAME_1 == "Anambra"] <- "Anambra★"
preten_data$NAME_1[preten_data$NAME_1 == "Oyo"] <- "Oyo★"
preten_data$NAME_1[preten_data$NAME_1 == "Sokoto"] <- "Sokoto★"

# Add prevalence data for each state
prevalence_data <- data.frame(
  state = c("Abia", "Adamawa", "Akwa Ibom", "Bauchi", "Bayelsa", "Anambra★", "Oyo★", "Sokoto★"),
  prevalence = c(36.1, 55.0, 22.0, 59.8, 18.4, 41.5, 45.6, 38.9)
)

# Merge prevalence data with preten_data
preten_data <- merge(preten_data, prevalence_data, by.x = "NAME_1", by.y = "state", all.x = TRUE)

# Set tmap options to show all levels
tmap_options(max.categories = 37)

# Plot the map
pre_map <- tm_shape(preten_data) +
  tm_polygons("prev_mpm", title = "2010 Malaria prevalence \nby microscopy",
              style = "fixed", textNA = "",
              breaks = c(25, 30, 35, 40, 45, 50, 55),
              legend.hist = FALSE) +
  tm_shape(preten_data) + 
  tm_fill(col = "prevalence", palette = "viridis", legend.show = TRUE) +
  tm_shape(preten_data) + 
  tm_fill(col = "NAME_1", palette = state_colors, legend.show = FALSE) +
  tm_shape(preten_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = FALSE) +
  tm_shape(preten_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = FALSE, ymod = -1) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0)) +
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(position = c("left", "top"))

# Display the map
pre_map

# Convert to leaflet map
pre_map_web <- tmap_leaflet(pre_map)
pre_map_web


# Prevalence 2013 ---------------------------------------------------------

pre_2013<- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
class(pre_2013)
plot(pre_2013)
plot(pre_2013, axes=T)

head(pre_2013@data)

data<-pre_2013@data
class(data)
head(data)
####Hard approach
var<- c("GID_1", "NAME_1")
data<-data [,var]
names(data)
set.seed(123)
write.csv(data, "pre2013.csv")

prethirteen_csv <- read.csv("pre2013.csv")
summary(prethirteen_csv)
prethirteen_csv$GID_1
head(prethirteen_csv)
prethirteen_data<- merge(pre_2013, prethirteen_csv, by= "NAME_1")

prethirteen_data <- st_as_sf(prethirteen_data)
class(prethirteen_data)

premap13<- tm_shape(prethirteen_data)+
  tm_polygons("prev_mpm")+
  tm_text("NAME_1")
premap13

premap131<- tmap_leaflet(premap13)
premap131
#####################################################
prethirteen_data$NAME_1 <- as.character(preten_data$NAME_1)
prethirteen_data$NAME_1[prethirteen_data$NAME_1 == "Anambra"] <- "Anambra★"  # Red star
prethirteen_data$NAME_1[prethirteen_data$NAME_1 == "Oyo"] <- "Oyo★"
prethirteen_data$NAME_1[prethirteen_data$NAME_1 == "Sokoto"] <- "Sokoto★" #●Thick red dot
################################################################
pre_map13 <- tm_shape(prethirteen_data) +
  tm_polygons("prev_mpm", title = "2013 Malaria prevalence \nby microscopy",
              style = "fixed", textNA = "",
              breaks = c(10, 20, 30, 40,50, 60),
              legend.hist = FALSE) +
  tm_shape(prethirteen_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = F) +
  tm_shape(prethirteen_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = F, ymod = -0.2) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0))+
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_compass(position = c("left", "top")) #, inner.margins = c(0.1, 0.1, 0, 0))
#tm_compass(type = "8star", position = c("left", "top"), size = 2)
pre_map13

pre_map_web13 <- tmap_leaflet(pre_map13)
pre_map_web13


# Prevalence 2015 ---------------------------------------------------------
pre_2015<- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
class(pre_2015)
plot(pre_2015)
plot(pre_2015, axes=T)

head(pre_2015@data)

data<-pre_2015@data
class(data)
head(data)
####Hard approach
var<- c("GID_1", "NAME_1")
data<-data [,var]
names(data)
set.seed(123)
write.csv(data, "pre2015.csv")

prefifteen_csv <- read.csv("pre2015.csv")
summary(prefifteen_csv)
prefifteen_csv$GID_1
head(prefifteen_csv)
prefifteen_data<- merge(pre_2015, prefifteen_csv, by= "NAME_1")

prefifteen_data <- st_as_sf(prefifteen_data)
class(prefifteen_data)

premap15<- tm_shape(prefifteen_data)+
  tm_polygons("prev_mpm")+
  tm_text("NAME_1")
premap15

premap151<- tmap_leaflet(premap15)
premap151
#####################################################
prefifteen_data$NAME_1 <- as.character(prefifteen_data$NAME_1)
prefifteen_data$NAME_1[prefifteen_data$NAME_1 == "Anambra"] <- "Anambra★"  # Red star
prefifteen_data$NAME_1[prefifteen_data$NAME_1 == "Oyo"] <- "Oyo★"
prefifteen_data$NAME_1[prefifteen_data$NAME_1 == "Sokoto"] <- "Sokoto★" #●Thick red dot
################################################################
pre_map15 <- tm_shape(prefifteen_data) +
  tm_polygons("prev_mpm", title = "2015 Malaria prevalence \nby microscopy",
              style = "fixed", textNA = "",
              breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70),
              legend.hist = FALSE) +
  tm_shape(prefifteen_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = F) +
  tm_shape(prefifteen_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = F, ymod = -0.2) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0))+
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_compass(position = c("left", "top")) #, inner.margins = c(0.1, 0.1, 0, 0))

pre_map15

pre_map_web15 <- tmap_leaflet(pre_map15)
pre_map_web15

# Prevalence 2018 ---------------------------------------------------------
pre_2018<- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
class(pre_2018)
plot(pre_2018)
plot(pre_2018, axes=T)

head(pre_2018@data)

data<-pre_2018@data
class(data)
head(data)
####Hard approach
var<- c("GID_1", "NAME_1")
data<-data [,var]
names(data)
set.seed(123)
write.csv(data, "pre2018.csv")

preeighteen_csv <- read.csv("pre2018.csv")
summary(preeighteen_csv)
preeighteen_csv$GID_1
head(preeighteen_csv)
preeighteen_data<- merge(pre_2018, preeighteen_csv, by= "NAME_1")

preeighteen_data <- st_as_sf(preeighteen_data)
class(preeighteen_data)

premap18<- tm_shape(preeighteen_data)+
  tm_polygons("prev_mpm")+
  tm_text("NAME_1")
premap18

premap181<- tmap_leaflet(premap18)
premap181
#####################################################
preeighteen_data$NAME_1 <- as.character(preeighteen_data$NAME_1)
preeighteen_data$NAME_1[preeighteen_data$NAME_1 == "Anambra"] <- "Anambra★"  # Red star
preeighteen_data$NAME_1[preeighteen_data$NAME_1 == "Oyo"] <- "Oyo★"
preeighteen_data$NAME_1[preeighteen_data$NAME_1 == "Sokoto"] <- "Sokoto★" #●Thick red dot
################################################################
pre_map18 <- tm_shape(preeighteen_data) +
  tm_polygons("prev_mpm", title = "2018 Malaria prevalence \nby microscopy",
              style = "fixed", textNA = "",
              breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60),
              legend.hist = FALSE) +
  tm_shape(preeighteen_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = F) +
  tm_shape(preeighteen_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = F, ymod = -0.2) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0))+
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_compass(position = c("left", "top")) #, inner.margins = c(0.1, 0.1, 0, 0))
pre_map18

pre_map_web18 <- tmap_leaflet(pre_map18)
pre_map_web18
##################
preeighteen_data$NAME_1 <- as.character(preeighteen_data$NAME_1)
preeighteen_data$NAME_1[preeighteen_data$NAME_1 == "Anambra"] <- "Anambra★"  # Red star
preeighteen_data$NAME_1[preeighteen_data$NAME_1 == "Oyo"] <- "Oyo★"
preeighteen_data$NAME_1[preeighteen_data$NAME_1 == "Sokoto"] <- "Sokoto★" #●Thick red dot

# Plot the map with viridis color scheme
pre_map18 <- tm_shape(preeighteen_data) +
  tm_polygons("prev_mpm", title = "2018 Malaria prevalence \nby microscopy",
              style = "fixed", textNA = "",
              breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60),
              palette = "viridis", direction = -1,legend.hist = FALSE) +
  tm_shape(preeighteen_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = FALSE) +
  tm_shape(preeighteen_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = FALSE, ymod = -1) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0)) +
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(position = c("left", "top"))

pre_map18

pre_map_web18 <- tmap_leaflet(pre_map18)
pre_map_web18
#############################
# Subset Anambra data
anambra_data <- subset(preeighteen_data, NAME_1 == "Anambra★")

# Plot the map for Anambra
anambra_map <- tm_shape(anambra_data) +
  tm_polygons("prev_mpm", title = "2018 Malaria prevalence in Anambra\nby microscopy",
              style = "fixed", textNA = "")+
              #breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60),
              #palette = "viridis", direction = -1, legend.hist = FALSE) +
  tm_text("NAME_1", size = 0.7, col = "black", shadow = FALSE) +
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = FALSE, ymod = -1) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0)) +
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(position = c("left", "top"))

anambra_map

anambra_map_web <- tmap_leaflet(anambra_map)
anambra_map_web


# Prevalence 2021 ---------------------------------------------------------
pre_2021<- readOGR("gadm41_NGA_shp", "gadm41_NGA_1")
class(pre_2021)
plot(pre_2021)
plot(pre_2021, axes=T)

head(pre_2021@data)

data<-pre_2021@data
class(data)
head(data)
####Hard approach
var<- c("GID_1", "NAME_1")
data<-data [,var]
names(data)
set.seed(123)
write.csv(data, "pre2021.csv")

pretwentyone_csv <- read.csv("pre2021.csv")
summary(pretwentyone_csv)
pretwentyone_csv$GID_1
head(pretwentyone_csv)
pretwentyone_data<- merge(pre_2021, pretwentyone_csv, by= "NAME_1")

pretwentyone_data <- st_as_sf(pretwentyone_data)
class(pretwentyone_data)

premap21<- tm_shape(pretwentyone_data)+
  tm_polygons("prev_mpm")+
  tm_text("NAME_1")
premap21

premap211<- tmap_leaflet(premap21)
premap211
#####################################################
pretwentyone_data$NAME_1 <- as.character(pretwentyone_data$NAME_1)
pretwentyone_data$NAME_1[pretwentyone_data$NAME_1 == "Anambra"] <- "Anambra★"  # Red star
pretwentyone_data$NAME_1[pretwentyone_data$NAME_1 == "Oyo"] <- "Oyo★"
pretwentyone_data$NAME_1[pretwentyone_data$NAME_1 == "Sokoto"] <- "Sokoto★" #●Thick red dot
################################################################
pre_map21 <- tm_shape(pretwentyone_data) +
  tm_polygons("prev_mpm", title = "2021 Malaria prevalence \nby microscopy",
              style = "fixed", textNA = "",
              breaks = c(0, 5,10, 15, 20, 25, 30, 35, 40, 45, 50, 55),
              legend.hist = FALSE) +
  tm_shape(pretwentyone_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = F) +
  tm_shape(pretwentyone_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = F, ymod = -1) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0))+
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_compass(position = c("left", "top")) #, inner.margins = c(0.1, 0.1, 0, 0))
pre_map21

pre_map_web21 <- tmap_leaflet(pre_map21)
pre_map_web21
#####################################################
# Subset Anambra data
anambra_data <- subset(pretwentyone_data, NAME_1 == "Anambra★")

# Plot the map for Anambra
anambra_map <- tm_shape(anambra_data) +
  tm_polygons("prev_mpm", title = "2021 Malaria prevalence in Anambra\nby microscopy:Hypo-endemic",
              style = "fixed", textNA = "",
              breaks = c(0, 5,10, 15, 20, 25, 30, 35, 40, 45, 50, 55),
              legend.hist = FALSE) +
  tm_shape(anambra_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = FALSE) +
  tm_shape(anambra_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = FALSE, ymod = -1) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0)) +
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(position = c("left", "top"))

anambra_map

anambra_map_web <- tmap_leaflet(anambra_map)
anambra_map_web
###########################################################
# Subset Anambra data
oyo_data <- subset(pretwentyone_data, NAME_1 == "Oyo★")

# Plot the map for Anambra
oyo_map <- tm_shape(oyo_data) +
  tm_polygons("prev_mpm", title = "2021 Malaria prevalence in Oyo\nby microscopy:Meso-endemic",
              style = "fixed", textNA = "",
              breaks = c(0, 5,10, 15, 20, 25, 30, 35, 40, 45, 50, 55),
              legend.hist = FALSE) +
  tm_shape(oyo_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = FALSE) +
  tm_shape(oyo_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = FALSE, ymod = -1) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0)) +
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(position = c("left", "top"))

oyo_map
############################
# Subset Anambra data
sokoto_data <- subset(pretwentyone_data, NAME_1 == "Sokoto★")

# Plot the map for Anambra
sokoto_map <- tm_shape(sokoto_data) +
  tm_polygons("prev_mpm", title = "2021 Malaria prevalence in Sokoto\nby microscopy:Hyper-endemic",
              style = "fixed", textNA = "",
              breaks = c(0, 5,10, 15, 20, 25, 30, 35, 40, 45, 50, 55),
              legend.hist = FALSE) +
  tm_shape(sokoto_data) + 
  tm_text("NAME_1", size = 0.7, col = "black", shadow = FALSE) +
  tm_shape(sokoto_data) + 
  tm_text("prev_mpm", size = 0.7, col = "black", shadow = FALSE, ymod = -1) +
  tm_layout(legend.outside = TRUE, outer.margins = c(0, 0, 0, 0)) +
  tm_view(text.size.variable = TRUE) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(position = c("left", "top"))

sokoto_map
