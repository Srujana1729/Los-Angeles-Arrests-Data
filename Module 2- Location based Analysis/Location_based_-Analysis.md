---
title: "Location based Analysis"
author: "Srujana"
date: "1st December 2019"
output: 
  html_document: 
    keep_md: yes
    theme: readable
---
<style>
body {
text-align: justify}
</style>


```r
knitr::opts_chunk$set(echo=TRUE)
```

This module is about calculating distances using the location data (provided as (latitude, longitude)) in this dataset. The module is organized into two parts. The first part aims to calculate the number of arrests that occur in certain radius. The second part tries to estimate the number of arrests per km on a particular road. The length of the road is calculated based on the addresses of the arrests and their respective location coordinates.

### Part I: Arrests within a radius

In this first part of the module , we use two different methods to calculate distances between the arrest locations, and a certain point, say the Bradbury Building. We use (34.050536, -118.247861) for the coordinates of the Bradbury Building. The results from these methods are compared. Subsequently, we calculate the number of arrests that occured in a radius of 2 km from the Bradbury Building.

The steps to load and preprocess the data into a dataframe are the same as that in the first module (Section 1). An additional preprocessing step is required here to extract the latitude and longitude from the given location data. 



```r
library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(geosphere)
```


#### 1. Loading and preprocessing the data

1.1 Load data into a data frame


```r
data <- fread("https://data.lacity.org/api/views/yru6-6re4/rows.csv?accessType=DOWNLOAD")
summary(data)
```

1.2 Remove spaces in headers


```r
names(data)<-str_replace_all(names(data), c(" " = "_")) 
```

1.3 Convert Arrest_Date from character to date class


```r
data$Arrest_Date <- as.Date(data$Arrest_Date, format = "%m/%d/%Y") 
```

1.4 Check if class was changed correctly


```r
class(data$Arrest_Date) 
```

```
## [1] "Date"
```

1.5 Create a subset with data prior to January 1, 2019, as the 2019 data is incomplete as of today


```r
df <- subset(data, data$Arrest_Date < as.Date("2019-01-01")) 
summary(df)
```

1.6 Create subset without empty locations, and split latitude and longitude into separate columns


```r
dfbrad <- subset(df, df$Location != "" & df$Location != "(0.0, 0.0)") 

dfbrad$Latitude <- gsub('.*\\((.*),.*', '\\1', dfbrad$Location)
dfbrad$Longitude <- gsub('.*,(.*)\\).*', '\\1',dfbrad$Location)

# Add in the location of Bradbury Building as two separate columns for latitude and longitude
dfbrad <- cbind(dfbrad, Brad_Latitude = 34.050536, Brad_Longitude = -118.247861)
```


#### 2. Calculating distance between the arrest locations and Bradbury Building using two different methods

2.1 The first method calculates the distances using the Spherical Earth projected to a plane formula. The distances are calculated in km, assuming the radius of the earth as 6371 km.


```r
# Create a function representing the formula
distance_this <- function(lat, long) {
    6371 * sqrt((0.01745329*(lat - 34.050536))^2 + 
        (cos((0.01745329*(lat + 34.050536))/2)*(0.01745329*(long - (-118.247861)))^2))
   }
dfbrad$Latitude <- as.numeric((dfbrad$Latitude))
dfbrad$Longitude <- as.numeric((dfbrad$Longitude))

# Apply the function to the respective columns in the dataframe
answer_distance <- mapply(distance_this, dfbrad$Latitude, dfbrad$Longitude)

# Add the distance column to the dataframe
dfbrad <- cbind(dfbrad, answer_distance)
```


2.2 In the second method, we calculate distances using the geosphere package. This package has several options for calculating distances that use either the spheroid or the ellipsoid model of the earth. Here, we use the commonly used Haversine formula which assumes a spheroid earth.


```r
dfbrad <- dfbrad %>% 
  mutate(dhav= distHaversine(cbind(dfbrad$Longitude, dfbrad$Latitude), cbind(dfbrad$Brad_Longitude, dfbrad$Brad_Latitude), r= 6371))
```


2.3 Here, we compare the results of the two models (planar versus spherical earth) to see how they change with increasing distance. Due to the size of the data, using ggplot takes a lot of time to create a scatter plot. Since we are primarily interested in the broad pattern of variation, we can use a smoothed color density representation of a scatterplot. This can be done with the smoothScatter function.


```r
dfbrad <- dfbrad %>% mutate(diff= abs(answer_distance-dhav))

smoothScatter(dfbrad$answer_distance, dfbrad$diff,
              xlab = "Distance", ylab = "Difference", 
              main = "Comparison of the Two Models") 
```

![](Location_based_-Analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


We see that although at smaller distances, the results from the planar and spheroid earth models are similar, as the distance increases, they start to increasingly diverge. This is because, as distances increase, the impact of the curvature of the earth is greater, and the planar projection model becomes more inaccurate. For the most accuracy, functions that use the ellispsoid model of the earth are better suited.These are available in the geosphere package. Next we move on to calculating number of arrests in a given radius.


#### 3. Find arrest incidents occurred within 2 km from the Bradbury Building


```r
# Using the planar model
dfwithin2 <- subset(dfbrad, dfbrad$answer_distance < 2)
arrestswithin2 = count(dfwithin2)

# Using the spherical model
dfwithin2hav <- subset(dfbrad, dfbrad$dhav < 2)
arrestswithin2hav = count(dfwithin2)

# Compare both models
if (arrestswithin2 == arrestswithin2hav) print("The values are same") else print(abs(arrestswithin2-arrestswithin2hav))
```

```
## [1] "The values are same"
```


### Part II: Arrests per km

In this second part of the module, we estimate the length of Pico Boulevard using the addresses of arrests in the dataset. We first generate a susbset containing arrests that occured on Pico Boulevard, and remove the outliers. Next, we calculate the distance between the most western and eastern coordinate points in this dataframe and use this to arrive at the arrests per km.

#### 1. Generate a susbset of arrests that occured on Pico Boulevard

Here, we generate a logical vector that contains TRUE or FALSE based on whether the word "PICO" is present in the address or not. We then use the logical vector to subset the location dataframe created in Part I, thereby giving us a dataframe of only those arrests that contain "PICO". We remove any outliers by filtering out locations where either the latitude or longitude is 2 standard deviations beyond the mean of the subset of identified points.


```r
# Logically check for presence of PICO
dflogic <- grepl("PICO", dfbrad$Address)

# Subset using TRUEs
dfpico <- dfbrad[c(dflogic),]

# Remove outliers
SDLat = sd(dfpico$Latitude)
MeanLat= mean(dfpico$Latitude)
SDLong = sd(dfpico$Longitude)
MeanLong= mean(dfpico$Longitude)

dfpico <- filter(dfpico, dfpico$Latitude < (MeanLat + 2 * SDLat) & 
                   dfpico$Latitude > (MeanLat - 2 * SDLat) &
                   dfpico$Longitude < (MeanLong + 2 * SDLong) &
                   dfpico$Longitude > (MeanLong - 2 * SDLong))
```

#### 2. Find length of Pico Boulevard 

Here, we identify the most western and eastern coordinate points in the dataframe by determining the smallest longitude and its corresponding latitude, and the greatest latitude and its corresponding longitude. The distance between these two points gives us the estimate of the length of Pico Boulevard. As discussed in part I, distance between two points can be calculated using either the planar, or spherical or ellipsoid model of the earth. Here we try all three methods.


```r
# Find the most western and eastern coordinate points
WLong = min(dfpico$Longitude)
WLatrow = dfpico[which.min(dfpico$Longitude), ]
WLat = WLatrow$Latitude
ELong = max(dfpico$Longitude)
ELatrow =  dfpico[which.max(dfpico$Longitude),]
ELat = ELatrow$Latitude

# Calculate the distance using the multiple meathods discussed in Part I
EWDist = 6371 * sqrt((0.01745329*(WLat - ELat))^2 + (cos((0.01745329*(WLat + ELat))/2)*(0.01745329*(WLong - ELong))^2)) # Planar model of earth

EWDistHav = distHaversine(c(WLong, WLat), c(ELong, ELat), r= 6371) # Spherical model of earth

EWDistGeo = distGeo(c(WLong, WLat), c(ELong, ELat))/1000 # Ellipsoid model of earth

cat("The estimated lengths of Pico Boulevard using the planar, spherical and ellipsoid model of earth are", EWDist, ",", EWDistGeo, "and", EWDistHav, "respectively")
```

```
## The estimated lengths of Pico Boulevard using the planar, spherical and ellipsoid model of earth are 23.51558 , 21.4527 and 21.40623 respectively
```

The actual length of Pico Boulevard is around 25 km. We see that the estimate from the planar model of earth is closest to this. This could be because both distHaversine and distGeo yield the shortest distance between two points, or the distance "as the crow flies", thereby not always providing the real-world distance.

#### 3. Arrests per km on Pico Boulevard

The arrests per km is calculated by dividing the total number of arrests with addresses that contain "PICO" with the length of Pico Boulevard.


```r
arrestsperkm = unlist(count(dfpico) / EWDist)

cat("The number of arrests per km on Pico Boulevard is", arrestsperkm)
```

```
## The number of arrests per km on Pico Boulevard is 306.265
```





