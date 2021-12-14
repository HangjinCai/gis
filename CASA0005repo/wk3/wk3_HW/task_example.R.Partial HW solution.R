#Task:
#to manipulate some raster data and produce some 
#descriptivec statistics.

#Questions:
#For any country in the World produce descriptive statistics
#that show the difference in maximum annual temperature for 
#key cities between SSP1 and SSP5 for the years 2081-2100,
#using any model and resolution.
#our data is monthly rather than annually here(don't misunderstand it)

#the city we choose is Spain

library(sf)
library(here)
library(janitor)
library(tidyverse)
library(raster)
library(ggplot2)

#server = livecode::serve_file() #idk do not run this

#To read our data:

#read geopkg
spain <- st_read(here("Data", "gadm36_ESP.gpkg"), 
                      layer='gadm36_ESP_0')

#read .shp
world_cities <- st_read(here("Data", "World_Cities", "World_cities.shp"))

#just want to get the Spain .shp out from world_cities:
spanish_cities <- world_cities %>%
  clean_names()%>%
  filter(cntry_name=="Spain")


#Read raster datas

#if you read in 'raster(here())',type 'ssp1'(don't run this)
## ssp1 <-raster(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "wk3_HW", "WorldClim future projections", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp126_2081-2100.tif"))

## ssp5 <-raster(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "wk3_HW", "WorldClim future projections", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp585_2081-2100.tif"))
## ssp1
#it shows:  band: 1  (of  12  bands)
#this telling you that the source had four bands but
#irs only read one,so use 'stack' instead:

ssp1 <-stack(here("Data", "ssp1", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp126_2081-2100.tif"))

ssp5 <-stack(here("Data", "ssp5", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp585_2081-2100.tif"))
#now we have all 12 bands.
# could also read this in as a brick...brick(here(...))
#(if read in brick:change 'stack'to'brick',that would do)
# 'brick' is similar to a RasterStack (that can be 
#created with stack), but shorter processing time with 
#less flexibility as they can only point to a single file.
#stack is individual layers,brick is a file


#Idk what is is following code,I didn't run this
ssp1brick <- brick(ssp1)

#last_model <- raster(here("last_model", "ssp126", "wc2.1_2.5m_tmax_MRI-ESM2-0_ssp126_2081-2100.tif"))
#create a differencing(subtract them)
ssp_diff <- ssp5-ssp1

####ssp1

spain_diff <- ssp1 %>%
  # now crop our temp data to the extent
  crop(.,spain)

exact_spain <- spain_diff %>%
  mask(.,spain, na.rm=TRUE)

###ssp5

spain_diff5 <- ssp5 %>%
  # now crop our temp data to the extent
  crop(.,spain)

exact_spain5 <- spain_diff5 %>%
  mask(.,spain, na.rm=TRUE)

#### subtraction

diff_climate_model <- exact_spain5 - exact_spain 

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(diff_climate_model) <- month

spain_city_diff<- raster::extract(diff_climate_model, spanish_cities)
#now we have 12 columns in 'diff_climate_model'
#there are NA in our data set, it's your decision to
#determine whether to include or not
#here,we say it's not a major city,we just ignore it.

#generate col
#add_column called 'spain_city_diffJan' from the'=data'
spain_city_diff2 <- spanish_cities %>% 
  add_column(spain_city_diffJan=spain_city_diff[,1])%>%
  add_column(spain_city_diffFeb=spain_city_diff[,2])%>%
  add_column(spain_city_diffMar=spain_city_diff[,3])%>%
  add_column(spain_city_diffApril=spain_city_diff[,4])%>%
  add_column(spain_city_diffMay=spain_city_diff[,5])%>%
  add_column(spain_city_diffJune=spain_city_diff[,6])%>%
  add_column(spain_city_diffJuly=spain_city_diff[,7])%>%
  add_column(spain_city_diffAug=spain_city_diff[,8])%>%
  add_column(spain_city_diffSept=spain_city_diff[,9])%>%
  add_column(spain_city_diffOct=spain_city_diff[,10])%>%
  add_column(spain_city_diffNov=spain_city_diff[,11])%>%
  add_column(spain_city_diffDec=spain_city_diff[,12])
  

# visualise all cities in spain where there is data

city_climate_diff <- spain_city_diff2 %>% 
  dplyr::select(contains("spain_city_diff"))%>%   
  #去掉%>%,run,you'll see there is a geometry col 
  #drop the geometry column that we don't need
  st_drop_geometry(.)%>%
  #not dropping the na now,why?
  as_tibble()%>%   #what is this?why adding this?
  dplyr::rename(.,Jan=spain_city_diffJan)%>%
  dplyr::rename(.,Feb=spain_city_diffFeb)%>%
  dplyr::rename(.,Mar=spain_city_diffMar)%>%
  dplyr::rename(.,Apr=spain_city_diffApril)%>%
  dplyr::rename(.,May=spain_city_diffMay)%>%
  dplyr::rename(.,Jun=spain_city_diffJune)%>%
  dplyr::rename(.,Jul=spain_city_diffJuly)%>%
  dplyr::rename(.,Aug=spain_city_diffAug)%>%
  dplyr::rename(.,Sep=spain_city_diffSept)%>%
  dplyr::rename(.,Oct=spain_city_diffOct)%>%
  dplyr::rename(.,Nov=spain_city_diffNov)%>%
  dplyr::rename(.,Dec=spain_city_diffDec)

#shifted to a very long data set(tidy the data)
tidy_city_diff <- city_climate_diff %>%
  pivot_longer(everything(), 
               names_to="Months",  
               values_to="temp_diff")

#to plot a faceted histogram
facet_plot <- tidy_city_diff %>%
  mutate(Months = factor(Months, levels = c("Jan","Feb","Mar",
                                            "Apr","May","Jun",
                                            "Jul","Aug","Sep",
                                            "Oct","Nov","Dec")))
# Plot faceted histogram   
ggplot(facet_plot, aes(x=temp_diff, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 1)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Months ~ .)+
  theme(plot.title = element_text(hjust = 0.5))
#if the graph looks odd,change the binwindth in line2
#here we changed from 5 to 1. It looks better.







####### extra analysis####when i assumed it was just a single year not months

ssp2 <-raster(here("Data", "ssp2", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp245_2081-2100.tif"))

ssp3 <-raster(here("Data", "ssp3", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp370_2081-2100.tif"))


spain_city_ssp1<- raster::extract(ssp1, spanish_cities)
spain_city_ssp2<- raster::extract(ssp2, spanish_cities)
spain_city_ssp3<- raster::extract(ssp3, spanish_cities)
spain_city_ssp5<- raster::extract(ssp5, spanish_cities)

spain_city3 <- spanish_cities %>% 
  add_column(ssp1=spain_city_ssp1)%>%
  add_column(ssp2=spain_city_ssp2)%>%
  add_column(ssp3=spain_city_ssp3)%>%
  add_column(ssp5=spain_city_ssp5)

spain_city4 <- spain_city3 %>%
  as_tibble()%>%
  dplyr::select(city_name, ssp1, ssp2, ssp3, ssp5)%>%
  pivot_longer(cols = 2:5,
              names_to = "model",
              values_to = "max_temp"
  )


violin <- ggplot(spain_city4, aes(x=model, y=max_temp, fill=model)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()

box <- ggplot(spain_city4, aes(x=model, y=max_temp, fill=model)) + # fill=name allow to automatically dedicate a color for each group
  geom_boxplot()+
  theme_minimal()+
  labs(
    x="Model", 
    y="Climate projection max tempearture")+
  theme(legend.position = "none")

violin <- ggplot(spain_city4, aes(x=model, y=max_temp, fill=model)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()

# we could also create a violin plot for the whole of spain
# per model or use metro areas. 