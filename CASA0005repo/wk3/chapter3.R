#3.5.1 Changing projections
#3.5.1.1 Vector
#we will need an outline of Australia to explore gridded temperature in Australia later

#Once we’ve downloaded the .gpkg 
#let’s see what is inside it with st_layers()…
library(sf)
library(here)
st_layers(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "gadm36_AUS_gpkg", "gadm36_AUS.gpkg"))

#then read in the GeoPackage layer for the whole of Australia (layer ending in 0)
library(sf)
Ausoutline <- st_read(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "gadm36_AUS_gpkg", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')
#you could check the coordinate reference systems of 'sf' or 'sp' objects using the print function:
print(Ausoutline)
#another alternative to identify the CRS of our layer
#is to find the 'proj4' string rather than using 'print' 
#A 'proj4' string is meant to be a compact way of identifying a coordinate reference system
library(sf)
st_crs(Ausoutline)$proj4string

#if downloaded data doesn’t have a CRS,added afterwards
#by adding the proj4string to the file or just assigning an EPSG code.
#3.5.1.3 EPSG
#if our Australian outline didn’t have a SRS(CRS)
#we could have just set it using 'st_set_crs()'
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)
#Or, more concisely
Ausoutline <- st_read(here("wk3", "gadm36_AUS_gpkg", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0') %>% 
  st_set_crs(4326)
#Remember this is ONLY useful if no CRS in the data set

#3.5.1.4 Reprojecting your spatial data
#So once your data has a coordinates system to work with,
#we can re-project or transform to anything we like.
#For SF objects, like our outline of Australia it’s 
#carried out using st_transform.
#Here we are changing from WGS84 to GDA94, which is a 
#local CRS for Australia and has the EPSG code 3112….
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

print(AusoutlinePROJECTED)
#You might also encounter an 'SP' object from the sp package.
#transforming the 'sp' object to 'sf' and changing the projection.

#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")
#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()


#3.5.2 WorldClim data
#So far we’ve only really considered vector data.
#Within the rest of this practical we will explore 
#some raster data sources and processing techniques.
#read the raster data:
library(raster)
jan<-raster(here("CASA", "modules", "gis", "CASA0005repo", "wk3","wc2.1_5m_tavg", "wc2.1_5m_tavg_01.tif"))
# have a look at the raster layer jan
jan

#then have a quick look at the data
plot(jan)
#we can see it’s again in the geographic projection of WGS84.

#To reproject a raster the whole grid must be recomputed
#to the new grid.
#to do this, use 'projectRaster()' from the Raster package. 
#However, it only accepts 'PROJ4' strings.

# set the proj 4 to a new object
newproj<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#right hand side are cheetsheet, you could research for it

# get the jan raster and give it the new proj4
pr1 <- jan %>%
  projectRaster(., crs=newproj)
plot(pr1)

#if we want from Mollweide to WGS84 
#we can simply set the crs to "+init=epsg:4326"
pr1 <- pr1 %>%
  projectRaster(., crs="+init=epsg:4326")
plot(pr1)

#projections part over
#3.5.3 Data loading
# look in our folder, find the files that end with .tif 
library(fs)
dir_info("CASA/modules/gis/CASA0005repo/wk3/wc2.1_5m_tavg/") 

#to search for filenames containing ".tif"
library(tidyverse)
listfiles<-dir_info("CASA/modules/gis/CASA0005repo/wk3/wc2.1_5m_tavg/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()
#have a look at the file names 
listfiles

#Then load all of the data into a raster stack. 
worldclimtemp <- listfiles %>%
  stack()

#have a look at the raster stack
worldclimtemp
#as you can see, there are 12 layers (nlayers). 
#The stack has loaded the 12 months of average 
#temperature data for us in order.

#To access single layers within the stack:
# access the january layer
worldclimtemp[[1]]

#can rename our layers within the stack:
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month
#p.s.rename() from the dplyr package isn’t available for raster data ☹️

#use our new layer name to get data for just January 
worldclimtemp$Jan


#Finally just select the paths.
#3.5.4 Raster location
#Using a raster stack we can extract data with a 
#single command.
#let’s make a dataframe of some sample sites 
#--Australian cities/towns.
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all city points 
AUcitytemp<- raster::extract(worldclimtemp, samples)
#then the city name diappear

#Add the city names to the rows of AUcitytemp:
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")



#3.6 Part 2 descriptive statistics

#3.6.1 Data preparation

#subset our data using the row name:
#Perth city
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
#Or subset our data using the row location:
Perthtemp <- Aucitytemp2[3,]

#3.6.2 Histogram(frequency of distribution of our data)
#Make a histogram of Perth’s temperature
hist(as.numeric(Perthtemp))
#x axis is the temperature and 
#the y is the frequency of occurrence.

#to improve the aesthetics of simple histogram above:
#define where you want the breaks in the historgram(x values)
userbreak<-c(8,10,12,14,16,18,20,22,24,26)
hist(as.numeric(Perthtemp), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature",   #title
     xlab="Temperature", 
     ylab="Frequency")


#Check out the histogram information R generated
histinfo <- Perthtemp %>%
  as.numeric()%>%
  hist(.)

histinfo


#3.6.3 Using more data
#to see the distribution of temperatures for the whole 
#of Australia in Jan (from averaged WorldClim data)

#Here, we will use the outline of Australia we loaded earlier
#Check the layer by plotting the geometry(don't run this)
plot(Ausoutline$geom)  #may crash your PC
#however,.shp is quite complex (i.e. lots of points)
#below is a qucker way:

#load the rmapshaper package
library(rmapshaper)
#simplify the shapefile
#to keep specifies the % of points
AusoutSIMPLE<-Ausoutline %>%
  ms_simplify(.,keep=0.05)

plot(AusoutSIMPLE$geom)  #the outline of Australia
#This should load quicker, but for ‘publication’ or
#‘best’ analysis. i’d recommend using the real file.



#Next
#set our map extent (where we want to clip the data to)
#to the outline of Australia then crop our WorldClim 
#dataset to it.

#to make sure that both of our layers are in the same 
#CRS when we combine them
print(Ausoutline)
#and this works nicely for rasters
crs(worldclimtemp)

#Now start:
Austemp <- Ausoutline %>%
  # to crop our temp data to the extent
  crop(worldclimtemp,.)

# plot the output
plot(Austemp)
#as you can see, the raster hasn’t been perfectly 
#clipped to the exact outline.
#if want raster data within the outline of the shape:
exactAus <- Austemp %>%
  mask(.,Ausoutline, na.rm=TRUE)
#You could also run this using the original 
#worldclimtemp raster, however, it may take some time.
#I’d recommend cropping to the extent first.

#Now Both our Austemp and exactAus are raster bricks.

#Let’s re-compute our histogram for Australia in March
#subset using the known location of the raster
#we know March is third in the RasterBrick.
hist(exactAus[[3]], col="red", main ="March temperature")

#OR
#We can also subset based on the name of the Brick
#subset with the word Mar
hist(raster::subset(exactAus, "Mar"), col="red", main ="March temperature")


#3.6.4 Histogram with ggplot
#need to make our raster into a data.frame to be 
#compatible with ggplot2, using a dataframe or tibble:
exactAusdf <- exactAus %>%
  as.data.frame()


library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))





#Plotting multiple months of temperature data on the 
#same histogram:
#we need to put our variable (months) into a one 
#column using pivot_longer()
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,   #select columns 1-12 (all the months) 
    names_to = "Month",   #place them in a new column called Month
    values_to = "Temp"   #and their values in another column called Temp
  )

#Then subset the data, selecting two months
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")

#Then to Get the mean for each month we selected
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths

#Select the colour and fill based on the variable (which is our month).
#The intercept is the mean we just calculated, with 
#the lines also based on the coloumn variable.
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


#Annoying error message? 
#Bin size defaults to 30 in ggplot2
#non-finate values is referring to lots of NAs (no data) 

#Therefore, In the code below i’ve:
#1.dropped all the NAs with drop_na()
#2.made sure that the Month column has the levels 
#specified, which will map in descending order 
#(e.g. Jan, Feb, March..).
#3.selected a bin width of 5 and produced a faceted plot…
data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))




#Next
#An interactive histogram using plotly:
library(plotly)
# split the data for plotly based on month

jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")

jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)

# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist










#Enough of histograms
#Now,
# descriptive statistics you might want to use:

# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))

# print the top 1
head(meanofall, n=1)

# standard deviation per month
sdofall <- squishdata %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))

# maximum per month
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))

# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))

# Interquartlie range per month
IQRofall <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))

# perhaps you want to store multiple outputs in one list..
lotsofstats <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE), 
            max=max(Temp, na.rm=T))

# or you want to know the mean (or some other stat) 
#for the whole year as opposed to each month...

meanwholeyear=squishdata %>%
  summarize(meanyear = mean(Temp, na.rm=TRUE))
