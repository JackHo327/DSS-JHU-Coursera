---
title: "leaftlet"
author: "JackHo"
date: "July 11, 2017"
output: 
 html_document:
  self_contained: no
---




```r
library(leaflet)
library(dplyr)
my_map <- leaflet() %>% addTiles() %>% addMarkers(lat = 31.2228559, lng = 121.4729522, popup = "where I am")
my_map
```

```
## Error in file(con, "rb"): cannot open the connection
```


```r
set.seed(123)
df <- data.frame(lat=runif(20,min=39.2,max=39.3), lng=runif(20,min=-76.6, max=-76.5))
df %>% leaflet() %>% addTiles() %>% addMarkers()
```

```
## Error in file(con, "rb"): cannot open the connection
```


```r
myIcon <- makeIcon(iconUrl = "https://jackho327.github.io/NOWHERE/images/avatar.jpg", iconWidth = 31*215/230, iconHeight = 31, iconAnchorX = 31*215/230/2, iconAnchorY = 31/2)
df <- data.frame(lat=runif(20,min=31.00,max=31.99), lng=runif(20,min=121.40, max=121.49), link = rep("<a href='https://jackho327.github.io/NOWHERE/'> my personal blog </a>", times=20))
df %>% leaflet() %>% addTiles() %>% addMarkers(lat=~lat, lng= ~lng,icon = myIcon, popup = ~link)
```

```
## Error in file(con, "rb"): cannot open the connection
```


```r
set.seed(123)
df <- data.frame(lat=runif(500,min=39.2,max=39.3), lng=runif(500,min=-76.6, max=-76.5))
df %>% leaflet() %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
```

```
## Error in file(con, "rb"): cannot open the connection
```

```r
df %>% leaflet() %>% addTiles() %>% addCircleMarkers(clusterOptions = markerClusterOptions())
```

```
## Error in file(con, "rb"): cannot open the connection
```


```r
library(ggmap)
geoCodes <- geocode(c("Baltimore, the United States","Frederick, the United States","Rockville, the United States","Gaithersburgh, the United States","Bowie, the United States","Hangerstown, the United States","Annapolis, the United States","College Park, the United States"))

my_df <- data.frame(name = c("Baltimore, the United States","Frederick, the United States","Rockville, the United States","Gaithersburgh, the United States","Bowie, the United States","Hangerstown, the United States","Annapolis, the United States","College Park, the United States"), pop = c(619493,66169,62334,61045,55232,39890,38880,30587), lat= geoCodes$lat, lng = geoCodes$lon, col=sample(c("red","blue","salmon","green","brown","pink","yellow","magenta"),size = 8,replace = T))

my_df %>% leaflet() %>% addTiles() %>% addCircles(weight= 1, radius = sqrt(my_df$pop) * 30)
```

```
## Error in file(con, "rb"): cannot open the connection
```

```r
my_df %>% leaflet() %>% addTiles() %>% addCircleMarkers(lat = ~lat, lng = ~lng, color = ~col) %>% addLegend(labels = ~name, colors = ~col)
```

```
## Error in file(con, "rb"): cannot open the connection
```
