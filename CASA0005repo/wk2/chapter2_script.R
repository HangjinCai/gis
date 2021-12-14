A <- 1
B <- 2
C <- A+B
C
ls()
rm(A)
#Functions can be thought of as single or multiple 
#calculations that you apply to objects.
#   function(object, argument1, argument2, argument3)
#You could save the output to a new object using something like…
#   X<-function(data, argument1, argument2, argument3)

#create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")

#just for fun, create some more, this time some normally distributed
#vectors of 100 numbers
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")

#help
?plot

#Data structure
df <- data.frame(Data1, Data2)
plot(df, col="green")

#to see a selection of a large data frame
library(tidyverse)
#show the first 10 and then last 10 rows of data in df...
df %>%
  head()   #or
df %>%
  tail()

#refer specifically to ranges or elements of rows and colunms
# function:     data.frame[row,column], e.g...
df[1,2]
df[1:10, 1]
df[5:15,]
df[c(2,3,6),2]
df[,1]

#to change the column headings
library(dplyr)
df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)

#to select or refer to columns directly by name(3 ways to do this):
df %>% 
  dplyr::select(column1)

df$column1

df[["column1"]]



#2.5 Reading data into R
#to read the .csv data file(in old way)(clean is in Excel first
# and then read as following)
LondonDataOSK<- read.csv("CASA0005repo/wk2/LondonDataOSK.csv", 
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")
#to see more parameters could be included when read csv
?read.csv

#Here(basically remove the problem of / or \)
#More straightforward way to read in files(developed in 2017)
install.packages("here")
library(here)
here::here()
LondonDataOSK<- read.csv(here::here("CASA0005repo","wk2","LondonDataOSK.csv"), 
                         header = TRUE, sep = ",",  
                         encoding = "latin1")

#New skool cleaning(use the csv downloaded from web directly)
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
#notes about this on my notebook

#Examining your new data
#to check the data type
class(LondonData)
class(LondonDataOSK)
#to check that our data has been read in correctly
Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

#Try reading in LondonData again, 
#but this time without excluding the ‘n/a’ values in the file
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                       locale = locale(encoding = "latin1"))
#then run the Datatypelist code again
Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist
#you should see that some of the columns (those the n/a values in) 
#have been read in as something other than numeric. 
#run the previous one to replace the incorret LondonData


#to quickly edit data (edit() function)
LondonData <- edit(LondonData)
#summarise the data
summary(df)
#to look at the column headers
LondonData%>%
  colnames()%>%
  # just look at the head, top5
  head()


#2.5.5 Data manipulation in R
#2.5.5.1 Selecting rows
#now to create a subset by selecting these rows into a new data frame
LondonBoroughs<-LondonData[626:658,]
#then reducing that data frame to just four columns
#There are a few ways of doing this:
LondonBoroughs<-LondonData%>%
  slice(626:658)
#in dplyr package, has a function called filter()
#filter()that let’s you subset rows based on conditions
#here we extracting all the wards where female life expextancy > 90.
Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)
#However, our 'New code' column that holds Borough codes 
#can’t be filtered like this as it’s in a character format
#in cases like this we can combine the function str_detect() 
#from the stringr package to filter()
#they are all in tidyverse package though
#more ‘data sciency way’
LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))

#Check it worked(tidyverse is more user-friendly):
LondonBoroughs$`Ward name`
#this is the same to check it worked:
LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()

#you will have two rows for 'the City of London'in LondonBorough
#to extract only unique rows with distinct()
LondonBoroughs<-LondonBoroughs %>%
  distinct()
#to check
LondonBoroughs$`Ward name`

#2.5.5.2 Selecting columns
#select columns 1,19,20 and 21
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]
#We can also replicate this with dplyr with select()
#select columns 1,19,20 and 21
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))
#we also have a more ‘data sciency way’ to
#select the columns that contain certain words
LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name")) 

#2.5.5.3 Renaming columns
library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%
  clean_names()
#by default(removes all capitals and uses an underscore wherever there is a space)
#However, you can change to al capitals by following code(but don't do it now)
LondonBoroughs <- LondonBoroughs %>%
  #here the ., means all data
  clean_names(., case="big_camel")



#2.5.5.4 More dplyr verbs
# mutate() function:add new variables based on existing ones
Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))

#to show us the top and bottom number of rows instead of head or tail
#top of data
slice_head(Life_expectancy, n=5)
#bottom of data
slice_tail(Life_expectancy,n=5)


#2.5.5.5 Levelling up withdplyr
#how does the life expectancy of the London Boroughs 
#compare the the UK average of 81.16?
Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2
#this doesn't tell much about the data itself
#if we wanted to know the range of life expectancies 
#for London Boroughs that are above the national average….
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))
Life_expectancy2_group

#if want more information based on the distribution 
#of the Boroughs compared to the national average:
Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())
Life_expectancy3

#information we could use and make into a plot or map
Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(is.numeric, round, 3))%>%
  mutate(across(UKdiff, round, 0))
#useless part?didn't explain what is this

#2.5.6 Plotting
#For a simple and quick plot, plot() function
plot(LondonBoroughs$male_life_expectancy_2009_13,
     LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)
#2.5.7 Pimp my graph!
install.packages("plotly")
library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy_2009_13, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")

#2.5.8 Spatial Data in R
install.packages("maptools")

install.packages(c("classInt", "tmap"))
# might also need these ones
install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", "rgdal", 
                   "geojsonio"))
#Load Packages (ignore any error messages about being built under a 
#different R version):
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
#2.5.8.2 Making some choropleth maps
#read spatial data in straight from the internet(Geojson file)
EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")
#but we use another way to read, as following
# this will take a few minutes
# Geojson in local folder
EW <- st_read(here::here("CASA",
                         "modules",
                         "gis",
                         "CASA0005repo",
                         "wk2",
                         "Local_Authority_Districts_(December_2015)_Boundaries.geojson"))
#Shapefile in local folder
EW <- st_read(here::here("CASA",
                         "modules",
                         "gis",
                         "CASA0005repo",
                         "wk2",
                         "Local_Authority_Districts_(December_2015)_Boundaries",
                         "Local_Authority_Districts_(December_2015)_Boundaries.shp"))
#if not working(could find st_read), because here() starts at /Users/tsoiwaisin

#then Pull out London
LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))
#plot it using the qtm function
qtm(LondonMap)


#2.5.8.3 Attribute data
LondonData <- clean_names(LondonData)
#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,      #our data set (EW)
        LondonData,      # merg with this data set
        by.x="lad15cd",      #in x data set which is our EW,the column you merge is the "lad15cd"
        by.y="new_code",     #in y data set which is our EW,the column you merge is the "new_code"
        no.dups = TRUE)%>%    #don't want duplications in my data set
  distinct(.,lad15cd,
           .keep_all = TRUE)
#usually use left join,e.g.:
BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))

#2.5.9 Simple mapping
#create a choropleth map very quickly
library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")
#But that’s a bit basic! How about adding a basemap
#to get a basemap we need to extract it from OpenStreetMap(OSM) 
#using the read_osm() function from the tmaptools package.
library(OpenStreetMap)
tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>%     #extract bonding box form your map
  tmaptools::read_osm(., type = "osm", zoom = NULL)

#Now we have the basemap (tmaplondon),
#add the basemap, add the shape (our London layer)
#tell it which attribute to map (job seekers)
tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))
#to see more palette options
palette_explorer()   #or
tmaptools::palette_explorer()


#To map Life_expectancy4 data:
#Merge our Life_expectancy4map with the spatial data EW
Life_expectancy4map <- EW %>%
  merge(.,
        Life_expectancy4, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)
#Map our merge with tmap
tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Life_expectancy4map) + 
  tm_polygons("UKdiff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))


#2.6 Tidying data
#to force the columns to the appopraite data types(e.g. text, numberic)
flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv", 
                        col_types = cols(
                          code = col_character(),
                          area = col_character(),
                          year = col_character(),
                          total_incidents = col_number(),
                          total_action_taken = col_number(),
                          warning_letters = col_number(),
                          fixed_penalty_notices = col_number(),
                          statutory_notices = col_number(),
                          formal_cautions = col_number(),
                          injunctions = col_number(),
                          prosecutions = col_number()
                        ))
# view the data
view(flytipping1)


#to make sure that each observation has its own row
#convert the tibble into a tidy tibble
flytipping_long <- flytipping1 %>% 
  pivot_longer(
    cols = 4:11,
    names_to = "tipping_type",
    values_to = "count"
  )
# view the data
view(flytipping_long)
#an alternative which just pulls everything out into a single table
flytipping2 <- flytipping1[,1:4]


#pivot the tidy tibble into one that is suitable for mapping
flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,   #only columns 1&2, keep remains the same
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )

view(flytipping_wide)

#if you want a specific varaible and wanted the coloums 
#to be each year of the data
widefly <- flytipping2 %>% 
  pivot_wider(
    names_from = year, 
    values_from = total_incidents)


#Q after class:
#1. st_bbox():it makes the bonding box(x_min,x_max,y_min,y_max)
#e.g.(Andy gave this code)
#BoroughDataMap %>%
#  st_bbox()
#2. $:an access operator,use e.g.to print a particular column
