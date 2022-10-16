---
title: "Project"
author: "Keyan Chen"
date: "2021-12-19"
output: 
  html_document:
    keep_md: yes
---




```r
sessionInfo()
```

```
## R version 4.1.0 (2021-05-18)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS 12.1
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRblas.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
##  [1] compiler_4.1.0  magrittr_2.0.1  fastmap_1.1.0   tools_4.1.0    
##  [5] htmltools_0.5.2 yaml_2.2.1      jquerylib_0.1.4 stringi_1.7.6  
##  [9] rmarkdown_2.11  knitr_1.36      stringr_1.4.0   xfun_0.28      
## [13] digest_0.6.29   rlang_0.4.12    evaluate_0.14
```


```r
library(tidyverse)
library(lubridate)
library(factoextra)
library(gridExtra)
library(mltools)
library(tmaptools)

set.seed(729)
```


```r
my.data <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908.csv") %>% 
  tibble()
```

# Data Wrangle


```r
head(my.data)
```

```
## # A tibble: 6 × 13
##   Date       Time  Location Operator `Flight #` Route Type  Registration `cn/In`
##   <chr>      <chr> <chr>    <chr>    <chr>      <chr> <chr> <chr>        <chr>  
## 1 09/17/1908 17:18 Fort My… Militar… <NA>       Demo… Wrig… <NA>         1      
## 2 07/12/1912 06:30 Atlanti… Militar… <NA>       Test… Diri… <NA>         <NA>   
## 3 08/06/1913 <NA>  Victori… Private  -          <NA>  Curt… <NA>         <NA>   
## 4 09/09/1913 18:30 Over th… Militar… <NA>       <NA>  Zepp… <NA>         <NA>   
## 5 10/17/1913 10:30 Near Jo… Militar… <NA>       <NA>  Zepp… <NA>         <NA>   
## 6 03/05/1915 01:00 Tienen,… Militar… <NA>       <NA>  Zepp… <NA>         <NA>   
## # … with 4 more variables: Aboard <dbl>, Fatalities <dbl>, Ground <dbl>,
## #   Summary <chr>
```

```r
tail(my.data)
```

```
## # A tibble: 6 × 13
##   Date       Time  Location Operator `Flight #` Route Type  Registration `cn/In`
##   <chr>      <chr> <chr>    <chr>    <chr>      <chr> <chr> <chr>        <chr>  
## 1 05/03/2009 12:00 Near El… Militar… <NA>       Patr… Mi-35 EV08114      <NA>   
## 2 05/20/2009 06:30 Near Ma… Militar… <NA>       Jaka… Lock… A-1325       1982   
## 3 05/26/2009 <NA>  Near Is… Service… <NA>       Goma… Anto… 9Q-CSA       5005   
## 4 06/01/2009 00:15 Atlanti… Air Fra… 447        Rio … Airb… F-GZCP       660    
## 5 06/07/2009 08:30 Near Po… Strait … <NA>       Lour… Brit… C-FJJR       424    
## 6 06/08/2009 <NA>  State o… Militar… <NA>       Mech… Anto… <NA>         <NA>   
## # … with 4 more variables: Aboard <dbl>, Fatalities <dbl>, Ground <dbl>,
## #   Summary <chr>
```

First, we need to deal with the missing values. 


```r
my.data$Time[is.na(my.data$Time)] <- '00:00'
my.data$Time <- str_replace_all(my.data$Time, 'c: ', '')
my.data$Time <- str_replace_all(my.data$Time, 'c:', '')
my.data$Time <- str_replace_all(my.data$Time, 'c', '')
my.data$Time <- str_replace_all(my.data$Time, '\'', ':')
my.data$Time <- str_replace_all(my.data$Time, '\\.', ':')
my.data$Time[!is.na(str_match(my.data$Time, '[0-9]{4}'))]
```

```
## [1] "0943"
```

Modify the time `0943`. 


```r
my.data$Time <- str_replace_all(my.data$Time, '0943', '09:43')
```


```r
my.data$Time[!is.na(str_match(my.data$Time, '[0-9]{3}'))]
```

```
## [1] "114:20"
```

```r
my.data$Time <- str_replace_all(my.data$Time, '114:20', '00:00')
```

Modify `114:20`. 


```r
my.data$Time <- paste(my.data$Date, my.data$Time)
my.data$Time <- mdy_hm(my.data$Time)
my.data <- my.data %>%
  select(-c(`Flight #`, Route, Registration, `cn/In`, Summary)) %>%
  na.omit()
```

# Visualizing


```r
my.data %>%
  group_by(year(Time)) %>%
  summarise(count = n()) %>%
  ggplot(aes(`year(Time)`, count)) +
  geom_line(color = "gray52") +
  geom_point(color = "sienna1") +
  theme_light() +
  xlab("Year") +
  ylab("Counts")
```

![](Final_files/figure-html/time_flow-1.png)<!-- -->

The number of air crashes was higher from 1940 to 2005. Next, let's see if the crash is related to a specific number of months, days of the week, or a specific time period. 


```r
p.week <- my.data %>%
  group_by(wday(Time, week_start = 1, label = TRUE)) %>%
  summarise(count = n()) %>%
  ggplot(aes(`wday(Time, week_start = 1, label = TRUE)`, count)) +
  geom_col(fill = "#90AFC5", width = 0.8) +
  theme_light() +
  xlab("Day of Week") +
  ylab("Counts")
```


```r
p.month <- my.data %>%
  group_by(month(Time, label = TRUE)) %>%
  summarise(count = n()) %>%
  ggplot(aes(`month(Time, label = TRUE)`, count)) +
  geom_col(fill = "#336B87", width = 0.8) +
  theme_light() +
  xlab("Month") +
  ylab("Counts")
```


```r
p.day <- my.data %>%
  filter(hour(Time) != 0 | minute(Time) != 0) %>%
  group_by(hour(Time)) %>%
  summarise(count = n()) %>%
  ggplot(aes(`hour(Time)`, count)) +
  geom_col(fill = "#2A3132", width = 0.8) +
  theme_light() +
  xlab("Hour") +
  ylab("Counts") +
  scale_x_continuous(breaks = seq(0, 24, 4))
```

A significant increase in the number of air crashes can be seen during certain time periods. But we don't know the exact reason for this increase. 

It is possible that some factors during these specific periods of time have led to a higher probability of air crashes. It could also be that the number of flights increased during the specific time period, which led to an increase in the number of crashes.



```r
# library(Rmisc)
```



```r
# multiplot(p.week, p.month, p.day, layout = matrix(c(1, 2, 3, 3),
#                                                   nrow = 2, byrow = TRUE))
```


```r
my.data <- my.data %>%
  mutate(Survivors = Aboard - Fatalities,
         SurvivalRate = Survivors / Aboard) %>% 
  na.omit()
```


```r
summary(my.data)
```

```
##      Date                Time                       Location        
##  Length:5179        Min.   :1908-09-17 17:18:00   Length:5179       
##  Class :character   1st Qu.:1954-12-20 18:30:00   Class :character  
##  Mode  :character   Median :1973-06-03 00:00:00   Mode  :character  
##                     Mean   :1972-01-31 13:13:04                     
##                     3rd Qu.:1990-08-28 00:47:00                     
##                     Max.   :2009-06-08 00:00:00                     
##    Operator             Type               Aboard         Fatalities    
##  Length:5179        Length:5179        Min.   :  1.00   Min.   :  0.00  
##  Class :character   Class :character   1st Qu.:  5.00   1st Qu.:  3.00  
##  Mode  :character   Mode  :character   Median : 13.00   Median :  9.00  
##                                        Mean   : 27.76   Mean   : 20.21  
##                                        3rd Qu.: 30.00   3rd Qu.: 23.00  
##                                        Max.   :644.00   Max.   :583.00  
##      Ground         Survivors        SurvivalRate  
##  Min.   :   0.0   Min.   :  0.000   Min.   :0.000  
##  1st Qu.:   0.0   1st Qu.:  0.000   1st Qu.:0.000  
##  Median :   0.0   Median :  0.000   Median :0.000  
##  Mean   :   1.6   Mean   :  7.553   Mean   :0.166  
##  3rd Qu.:   0.0   3rd Qu.:  2.000   3rd Qu.:0.200  
##  Max.   :2750.0   Max.   :516.000   Max.   :1.000
```

```r
top_n(count(my.data, Location, sort = TRUE), 10)
```

```
## Selecting by n
```

```
## # A tibble: 11 × 2
##    Location                   n
##    <chr>                  <int>
##  1 Sao Paulo, Brazil         15
##  2 Moscow, Russia            14
##  3 Anchorage, Alaska         13
##  4 Bogota, Colombia          13
##  5 Manila, Philippines       13
##  6 Cairo, Egypt              12
##  7 Chicago, Illinois         11
##  8 New York, New York        11
##  9 Rio de Janeiro, Brazil    11
## 10 AtlantiOcean               9
## 11 Tehran, Iran               9
```

```r
top_n(count(my.data, Operator, sort = TRUE), 10)
```

```
## Selecting by n
```

```
## # A tibble: 11 × 2
##    Operator                            n
##    <chr>                           <int>
##  1 Aeroflot                          176
##  2 Military - U.S. Air Force         174
##  3 Air France                         67
##  4 Deutsche Lufthansa                 62
##  5 Air Taxi                           44
##  6 Military - U.S. Army Air Forces    43
##  7 United Air Lines                   43
##  8 Pan American World Airways         40
##  9 American Airlines                  36
## 10 Military - Royal Air Force         36
## 11 Military - U.S. Navy               36
```

```r
top_n(count(my.data, Type, sort = TRUE), 10)
```

```
## Selecting by n
```

```
## # A tibble: 11 × 2
##    Type                                         n
##    <chr>                                    <int>
##  1 Douglas DC-3                               331
##  2 de Havilland Canada DHC-6 Twin Otter 300    81
##  3 Douglas C-47A                               73
##  4 Douglas C-47                                60
##  5 Douglas DC-4                                40
##  6 Yakovlev YAK-40                             37
##  7 Antonov AN-26                               36
##  8 Junkers JU-52/3m                            31
##  9 Douglas C-47B                               29
## 10 De Havilland DH-4                           27
## 11 Douglas DC-6B                               27
```

```r
temp.data <- my.data %>%
  group_by(year(Time)) %>%
  summarise(Aboard = sum(Aboard),
            Fatalities = sum(Fatalities)) %>%
  ungroup() %>%
  mutate(Sur = Aboard - Fatalities,
         SurRate = Sur / Aboard) %>%
  na.omit()

temp.data %>%
  ggplot(aes(`year(Time)`, SurRate)) +
  geom_col(fill = 'gray60') +
  theme_light() +
  xlab('Years') +
  ylab('Survival Rate') +
  geom_hline(yintercept = 0.166) +
  geom_text(
    aes(x = 1908, y = 0.166),
    label = 'Average',
    vjust = 1.5,
    hjust = 0.5,
    color = 'indianred1'
  )
```

![](Final_files/figure-html/time_vs_surrate-1.png)<!-- -->



```r
cluster.data <- my.data %>%
  select(c(6:7, 9)) %>%
  scale()
```



```r
k2 <- kmeans(cluster.data, centers = 2, nstart = 25)
k3 <- kmeans(cluster.data, centers = 3, nstart = 25)
k4 <- kmeans(cluster.data, centers = 4, nstart = 25)
k5 <- kmeans(cluster.data, centers = 5, nstart = 25)
```



```r
p2 <-
  fviz_cluster(k2, geom = "point", data = cluster.data) + ggtitle("K-means k=2") +
  theme_light()
p3 <-
  fviz_cluster(k3, geom = "point", data = cluster.data) + ggtitle("K-means k=3") +
  theme_light()
p4 <-
  fviz_cluster(k4, geom = "point", data = cluster.data) + ggtitle("K-means k=4") +
  theme_light()
p5 <-
  fviz_cluster(k5, geom = "point", data = cluster.data) + ggtitle("K-means k=5") +
  theme_light()

grid.arrange(p2, p3, p4, p5)
```

![](Final_files/figure-html/k-means results-1.png)<!-- -->

```r
fviz_nbclust(cluster.data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  theme_light() +
  theme(title = element_blank())
```

![](Final_files/figure-html/elbow-1.png)<!-- -->



```r
clustered.data <- tibble(my.data, cluster = k3$cluster)
```



```r
(summary_cluster <- clustered.data %>%
  group_by(cluster) %>%
  summarise(
    Counts = n(),
    Mean_Aboard = mean(Aboard),
    Mean_Fatalities = mean(Fatalities),
    Mean_Sur_Rate = mean(SurvivalRate)
  ))
```

```
## # A tibble: 3 × 5
##   cluster Counts Mean_Aboard Mean_Fatalities Mean_Sur_Rate
##     <int>  <int>       <dbl>           <dbl>         <dbl>
## 1       1    111       178.             12.7        0.930 
## 2       2   4723        17.2            13.3        0.156 
## 3       3    345       125.            118.         0.0532
```


```r
or <- summary_cluster$Mean_Sur_Rate %>% 
  rank()
```



```r
clustered.data$cluster_id <-
  ifelse(
    clustered.data$cluster == or[3],
    "Large Passenger High Survival",
    ifelse(
      clustered.data$cluster == or[1],
      "Large Passenger High Fatality",
      "Small-Midsize Crashes"
    )
  )
```



```r
LgSurvive <-
  clustered.data[clustered.data$cluster_id == "Large Passenger High Survival", ]
LgFatal <-
  clustered.data[clustered.data$cluster_id == "Large Passenger High Fatality", ]

Top10TypeSurvive <-
  top_n(count(LgSurvive, Type, sort = TRUE), 10)
```

```
## Selecting by n
```

```r
Top10TypeFatal <-
  top_n(count(LgFatal, Type, sort = TRUE), 10)
```

```
## Selecting by n
```

```r
Top10OperSurvive <-
  top_n(count(LgSurvive, Operator, sort = TRUE), 10)
```

```
## Selecting by n
```

```r
Top10OperFatal <- top_n(count(LgFatal, Operator, sort = TRUE), 10)
```

```
## Selecting by n
```



```r
Top10TypeSurvive %>%
  ggplot(aes(x = reorder(Type, n), y = n)) +
  geom_col(fill = "mediumseagreen") +
  coord_flip() +
  # ggtitle("Top 10 Large Passenger High Survival Crashes by Model") +
  xlab("Plane Model") +
  ylab("Number of Crashes") +
  theme_light()
```

![](Final_files/figure-html/Type10_1-1.png)<!-- -->



```r
Top10TypeFatal %>%
  ggplot(aes(x = reorder(Type, n), y = n)) +
  geom_col(fill = "orangered3") +
  coord_flip() +
  # ggtitle("Top 10 Large Passenger High Fatality Crashes by Model") +
  xlab("Plane Model") +
  ylab("Number of Crashes") +
  theme_light()
```

![](Final_files/figure-html/Type10_2-1.png)<!-- -->



```r
ggplot(Top10OperSurvive, aes(x = reorder(Operator, n), y = n)) +
  geom_col(fill = "mediumseagreen") +
  coord_flip() +
  # ggtitle("Top Ten Large Passenger High Survival Crashes by Airline") +
  xlab("Operator") +
  ylab("Number of Crashes") +
  theme_light()
```

![](Final_files/figure-html/Op1-1.png)<!-- -->



```r
ggplot(Top10OperFatal, aes(x = reorder(Operator, n), y = n)) +
  geom_col(fill = "orangered3") +
  coord_flip() +
  # ggtitle("Top Ten Large Passenger High Fatality Crashes by Airline") +
  xlab("Operator") +
  ylab("Number of Crashes") +
  theme_light()
```

![](Final_files/figure-html/Op2-1.png)<!-- -->


```r
Locations <-
  as.character(clustered.data$Location[clustered.data$cluster != 2])
Locations <- gsub("Near ", "", Locations)
Locations <- gsub("Off ", "", Locations)
Locations <- gsub("off ", "", Locations)
Locations <- gsub("Czechoslovakia", "Czechia", Locations)
Locations <- gsub("AFB", "", Locations)
Locations <- gsub("Llandow Airport, ", "", Locations)
Locations <- gsub("La Guardia ", "LaGuardia ", Locations)
Locations <- gsub("Staten Island / ", "", Locations)
Locations <- gsub("Morrocco", "Morocco", Locations)
Locations <- gsub("Guadaloupe", "Guadeloupe", Locations)
Locations <- gsub("USSR", "Russia", Locations)
Locations <- gsub("Rio de Janerio", "Rio de Janeiro", Locations)
Locations <- gsub("Papanga", "Pampanga", Locations)
Locations <- gsub("Island of ", "", Locations)
Locations <- gsub(", Yugoslavia", "", Locations)
Locations <- gsub("near Roussillon, ", "", Locations)
Locations <- gsub("Covington/Hebron, ", "", Locations)
Locations <- gsub(" (Namibia)", "", Locations)
Locations <- gsub("Morioko", "Morioka", Locations)
Locations <- gsub("Mt. Fuji, ", "", Locations)
Locations <- gsub("Yuhnov", "Yukhnov", Locations)
Locations <- gsub("Ukraine, Russia", "Ukraine", Locations)
Locations <- gsub("Staines, Surrey, ", "", Locations)
Locations <- gsub("Massachusett", "Massachusetts", Locations)
Locations <- gsub("Corunda", "Coruna", Locations)
Locations <- gsub("Thirty-five miles west of ", "", Locations)
Locations <- gsub("Mountains ", "", Locations)
Locations <- gsub(" near Kampung Ladang, ", "", Locations)
Locations <- gsub("Elburz Mtns., near ", "", Locations)
Locations <- gsub("New York, New York", "New York", Locations)
Locations <- gsub("300 nm NW of ", "", Locations)
Locations <- gsub("950 nm S of  ", "", Locations)
Locations <- gsub("Western PacifiOcean, ", "", Locations)
Locations <- gsub("PacifiOcean", "Pacific Ocean", Locations)
Locations <- gsub("AtlantiOcean", "Atlantic Ocean", Locations)
Locations <- gsub("Over the ", "", Locations)
Locations <- gsub("110 miles SW of ", "", Locations)
Locations <-
  gsub("DemocratiRepubliCogo",
       "Democratic Republic of Congo",
       Locations)
Locations <- gsub("Calilfornia", "California", Locations)
Locations <-
  gsub(", 570 miles northeast of Natal, Brazil", "", Locations)
Locations <-
  gsub(", 100 miles W of Galway Bay, Ireland", "", Locations)
Locations <- gsub(", 110 miles West of Ireland", "", Locations)
Locations <-
  gsub(", 116 miles WSW of Annette Island, Alaska", "", Locations)
Locations <- gsub("Mariana Islands", "", Locations)
Locations <- gsub("Atalayasa", "Atalaya", Locations)
Locations <- gsub("Wusterausen", "Wusterhausen", Locations)
Locations <- gsub("Vrastsa", "Vratsa", Locations)
Locations <- gsub("Dneprodzerzhinsk", "Kamianske", Locations)
Locations <- gsub("At ", "", Locations)
Locations <- gsub("near Ajaccio, ", "", Locations)
Locations <- gsub("En route Miami, FL - ", "", Locations)
Locations <- gsub("near Ueno Village, ", "", Locations)
Locations <- gsub("French Alps, ", "", Locations)
Locations <-
  gsub("Kimpo Air Base", "Gimpo International Airport", Locations)
Locations <- gsub("Carpich", "Carpish", Locations)
Locations <- gsub(" (Namibia)", "", Locations)
Locations <-
  gsub("Al Fujayrah", "Fujairah International Airport", Locations)
Locations <- gsub(", Kefallinia, Greece", "", Locations)
Locations <- gsub("Karatepe Mountains", "Karatepe", Locations)
Locations <- gsub("Jeddah, ", "", Locations)
Locations <- gsub("Laskarak, ", "", Locations)
Locations <- gsub(", Ustica, Italy, ", "", Locations)
Locations <- gsub("Southern Belarus", "Belarus", Locations)
Locations <- gsub("Syktyvar", "Syktyvkar", Locations)
Locations <- gsub("Ko Phuket", "Phuket", Locations)
Locations <- gsub("Krosnovodsk", "Krasnovodsk", Locations)
Locations <- gsub("Combi, ", "", Locations)
Locations <- gsub("Persian Gulf, near ", "", Locations)
Locations <- gsub("Armenia, ", "", Locations)
Locations <- gsub("Mt. Saint-Odile, near ", "", Locations)
Locations <- gsub("Mt. Lalaboy,  ", "", Locations)
Locations <- gsub("Khorog", "", Locations)
Locations <- gsub("Tidjika", "Tidjikja", Locations)
Locations <- gsub("Kahengula, ", "", Locations)
Locations <- gsub("Domincan", "Dominican", Locations)
Locations <- gsub("Charkhidadri", "Charkhi Dadri", Locations)
Locations <- gsub("Massachusettss", "Massachusetts", Locations)
Locations <- gsub("Amritsar, India / ", "", Locations)
Locations <- gsub("Republiof", "Republic of", Locations)
Locations <- gsub("Khorramabed", "Khorramabad", Locations)

# coords <- geocode_OSM(Locations, keep.unfound = TRUE)
# 
# saveRDS(coords, file = "coords.RData")

coords <- readRDS(file = "coords.RData")
```


```r
map.data <- clustered.data[clustered.data$cluster != 2, ]

map.data$Latitude <- coords$lat
map.data$Longitude <- coords$lon

map.data <- na.omit(map.data)
```


```r
map_data("world") %>%
  ggplot() +
  coord_fixed() +
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "white",
               colour = "grey50") +
  geom_point(
    data = map.data,
    aes(
      x = Longitude,
      y = Latitude,
      color = cluster_id,
      size = 1 - SurvivalRate
    ),
    alpha = 0.2
  ) +
  theme_light() +
  # ggtitle("Large Passenger Plane Crash Locations 1933 - 2009") +
  scale_color_manual("Cluster", values = alpha(c("orangered3", "green"))) +
  theme(legend.position = "none") +
  scale_size(range = c(0, 2.5))
```

![](Final_files/figure-html/map-1.png)<!-- -->

