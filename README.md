# DataTas plot-a-thon entry for Charlie Verdel

![alt text][final_plot]

[final_plot]: https://github.com/cverdel/DataTas_plot_a_thon/blob/main/final_plot.png?raw=true

The idea behind this is to combine a simple bar chart with a shaded relief map. I've added two external datasets: digital elevation data from the Himalaya region, and a table with latitude and longitude coordinates for many of the peaks in the Himalayan database.

```
library(tidyverse)
library(raster)
library(rayshader)
library(rgl)
library(wesanderson)
library(palr)

### NOTE: the rayshader package needs to be installed from the Github version devtools::install_github("tylermorganwall/rayshader")

# Read tidytuesday data
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

#Load elevation data
DEM_url<-"https://github.com/cverdel/DataTas_plot_a_thon/raw/main/dem.tif"
temp1<-tempfile()
download.file(DEM_url, temp1, mode="wb")

elevation1 = raster::raster(temp1)
elevation=elevation1+0 #Creates a LargeRasterLayer
res(elevation1)

#Load peak locations
locations <- readr::read_csv('https://github.com/cverdel/DataTas_plot_a_thon/raw/main/peaks_coords_clean.csv')

#Find the sum of all members for each expedition for each peak
peaksum <- aggregate(members~peak_id,expeditions,sum)

#Add peaksum as a column to peak locations dataframe
locations<-merge(locations, peaksum, by.x="peak_id")

#Change peak locations into a spatial points df
coordinates(locations) <- ~long+lat

#Rasterize peak location point data
r <- raster(extent(elevation)) #Creates a raster r with the same extent as elevation
res(r)<-raster::res(elevation) #Sets resolution of new raster to the same as the elevation dataset

new_raster<-rasterize(locations, r, locations$members, background=0, fun = mean) #Creates a raster wherein values are the total number of expedition members who have attempted that peak

height_shade(raster_to_matrix(new_raster)) %>%
  plot_map()

#Aggregate data (ie, grid data into large cells)
agg <- aggregate(x = new_raster, fact = 50, fun = sum)
disagg<-disaggregate(x = agg, fact = 50) #Have to disaggregate by the same amount to end up with a raster that matches the elevation data

#Do some raster arithmetic
final<-(6*log(elevation))+(2*(disagg+1)^0.5) #Add the elevation data to the attempts data. Trial and error to find a good combination.
plot(final)

#Add a colour scale
pal <- wes_palette("Zissou1", 256, type = "continuous")
image_map <- image_raster(final, col=pal, xaxt='n', yaxt='n', ann=FALSE)

#Splits image up into rgb
names(image_map) = c("r","g","b")
image_map_r = rayshader::raster_to_matrix(image_map$r)
image_map_g = rayshader::raster_to_matrix(image_map$g)
image_map_b = rayshader::raster_to_matrix(image_map$b)

image_map_array = array(0,dim=c(nrow(image_map_r),ncol(image_map_r),3))

image_map_array[,,1] = image_map_r/255 #Red 
image_map_array[,,2] = image_map_g/255 #Blue 
image_map_array[,,3] = image_map_b/255 #Green 
image_map_array = aperm(image_map_array, c(2,1,3))

#Rayshader part
el_matrix = rayshader::raster_to_matrix(final) #Raster to matrix conversion of elevation data
small_el_matrix = resize_matrix(el_matrix, scale = 0.6) #Reduce the size of the elevation data, for speed

#Render. This part can take a little while to process
zscale1=1
ambient_layer = ambient_shade(small_el_matrix, zscale = zscale1, multicore = TRUE, maxsearch = 200)
ray_layer = ray_shade(small_el_matrix, zscale = zscale1, sunaltitude=40, sunangle=210, multicore= TRUE)

zscale2=1
#Plot in 3D
rgl::rgl.close() #Closes the rgl window in case it's open
(image_map_array) %>% #
  # sphere_shade(texture = "desert") %>%
  add_shadow(ray_layer,0.3) %>%
  add_shadow(ambient_layer,0.1) %>%
  plot_3d(small_el_matrix, zscale=zscale2, asp=1, fov = 90, theta = 20, zoom = 0.6, phi = 60, 
          windowsize = c(1000, 800), solid=TRUE, background='#f5f5f5')
Sys.sleep(0.2)
bgplot3d({
  plot.new()
  title(main = 'Himalaya climbing attempts', cex.main=4, font.main=1, line = -3)
  title(main = 'Attempts extruded as elevation', cex.main=3, font.main=1, line = -6)
})
Sys.sleep(0.2)

#Save image
w=2000
h=2400
rgl::par3d(windowRect = c(0,0,w,h))
render_snapshot("final_plot", width=w, height=h, software_render = FALSE)
