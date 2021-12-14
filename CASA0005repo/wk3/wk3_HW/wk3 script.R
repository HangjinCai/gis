

#create a differencing(subtract them)
ssp_diff <- ssp5-ssp1
# to crop our temp data to the extent (and then plot)
spain_diff <- ssp_diff %>%
  crop(.,spain)
# plot the output
plot(spain_diff)
#as you can see, the raster hasn’t been perfectly 
#clipped to the exact outline.

#if want raster data within the outline of the shape:
exact_spain <- spain_diff %>%
  mask(.,spain, na.rm=TRUE)
#run this to see the new specific shape of spain data
plot(exact_spain)

#change the name to 12 month names

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(exact_spain) <- month
plot(exact_spain)  #you'll see the name changes

#Now from the exact_spain,we extract the 
spain_city_diff<- raster::extract(exact_spain, spanish_cities)







#to plot a faceted histogram
facet_plot <- tidy_city_diff %>%
  mutate(Months = factor(Months, levels = c("spain_city_diffJan","spain_city_diffFeb","spain_city_diffMar",
                                            "spain_city_diffApril","spain_city_diffMay","spain_city_diffJune",
                                            "spain_city_diffJuly","spain_city_diffAug","spain_city_diffSept",
                                            "spain_city_diffOct","spain_city_diffNov","spain_city_diffDec")))




data_complete_cases <- data %>%
  drop_na()%>%
  mutate(Month = factor(Month, levels=c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))