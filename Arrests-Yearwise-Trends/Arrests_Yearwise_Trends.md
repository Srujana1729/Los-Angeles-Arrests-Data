---
title: "Arrests: Yearwise Trends"
author: "Srujana"
date: "11/21/2019"
output: 
  html_document: 
    keep_md: yes
    theme: simplex
---
<style>
body {
text-align: justify}
</style>


```r
knitr::opts_chunk$set(echo = TRUE)
```

In this module, we try to get a sense of how the number of arrests has changed over the years. First, we load and preprocess the data into a dataframe (Section 1). Then we generate a quick plot of the number of arrests per year since 2010 (Section 2). Based on this initial plot, we use to next two sections to drill down deeper to see if the data can help us explain the observed trends.


```r
library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
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

#### 2. Understanding the trend in number of arrests per year

2.1 Generate plottable data by calculating arrests per year


```r
ArrestsByYear <- df %>% 
  group_by(year(df$Arrest_Date)) %>% 
  summarise(Arrests_Num = n()) 
names(ArrestsByYear) <- c("Year", "Arrests_Num")
```

2.2 Using the above data, we generate a quick plot to gauge the trend.


```r
plot(ArrestsByYear$Year, ArrestsByYear$Arrests_Num, 
     type = "l", 
     lwd = 2, 
     main = "Number of Arrests per Year", 
     xlab = "Year", 
     ylab = "Number of arrests",
     col = "red")
```

![](Arrests_Yearwise_Trends_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Interestingly, we find that there is a sharp decrease in arrests from 2012 onwards. This mandates further investigation by drilling down. In the data, there are five different types of arrests, namely, "Dependent", "Felony", "Infraction", "Misdemeanor", and "Other". In the next section, we try to see if any particular arrest type accounts for the decrease in the total number of arrests, or if the decrease is uniform across all types.

#### 3. Analyse trends by arrest type to see if this decrease is due to any particular types

3.1 Generate plottable data by calculating arrests per year by type


```r
TypesByYear <- df %>%
  group_by(Arrest_Type_Code, year(Arrest_Date)) %>%
  summarise(Arrests_Num = n())

names(TypesByYear) <- c("Type", "Year", "Arrests_Num")
```

3.2 Plot data- this time using ggplot to allow for more experimentation


```r
ggplot(TypesByYear, aes(Year, Arrests_Num, group= Type, color= Type)) + 
  geom_line(size = 1) +
  ggtitle("Number of Arrests per Year by Type") +
  scale_color_discrete(labels = c("Dependent", "Felony", "Infraction", "Misdemeanor", "Other")) +
  ylab("Number of Arrests") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour ="black"),
        panel.border = element_rect(linetype = "solid", fill = NA), legend.key =
        element_blank())
```

![](Arrests_Yearwise_Trends_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

3.3 Plot a stacked bar chart for an alternate visualization of the above data


```r
ggplot(TypesByYear, aes(Year, Arrests_Num, fill= Type)) + 
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Number of Arrests per Year by Type") +
  scale_fill_viridis_d(labels = c("Dependent", "Felony", "Infraction", "Misdemeanor", "Other")) +
  ylab("Number of Arrests") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(linetype = "solid", fill = NA), legend.key = element_blank())
```

![](Arrests_Yearwise_Trends_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

From both the above graphs, we see that a sharp decline in Misdemeanors is the main cause for decrease in arrests. There are several types of misdemeanors. So, in the next section, we further drill down by misdemeanor type to see if the decrease is uniform or if any particular types stand out and account for a major part of the decrease. However, since plotting all the types of misdemeanors clutters up the graph, we plot only the top 3 types, as shown below.

#### 4. Analyse trends by Misdemeanor type to see if this decrease is due to any particular types

4.1 Generate plottable data by calculating arrests per year by charge group description in the misdemeanor subset


```r
dfChargeDesc <- subset(df, df$Arrest_Type_Code == "M" & 
                         df$Charge_Group_Description != "")

ChargesByYearTotal <- dfChargeDesc %>%
  group_by(Charge_Group_Description) %>%
  summarise(Arrests_Num = n())
```

4.2 To select only the charge group descriptions that have higher number of arrests, first find the top 5 charge group descriptions


```r
top_n(ChargesByYearTotal,5)
```

```
## Selecting by Arrests_Num
```

```
## # A tibble: 5 x 2
##   Charge_Group_Description       Arrests_Num
##   <chr>                                <int>
## 1 Driving Under Influence             105907
## 2 Drunkeness                          105674
## 3 Larceny                              49390
## 4 Miscellaneous Other Violations      149253
## 5 Narcotic Drug Laws                   51135
```

4.3 There is a sharp decline in number of arrests for the fourth and fifth rank. So we consider only the top 3, namely, Driving Under Influence, Drunkeness and Miscellaneous Other Violations. Together, these three constitute more than half of all Misdemeanor arrests.


```r
ChargesByYear <- dfChargeDesc %>%
  group_by(Charge_Group_Description, year(Arrest_Date)) %>%
  summarise(Arrests_Num = n())

names(ChargesByYear) <- c("Charge_Group_Description", "Year", "Arrests_Num")

TopChargesByYear <- subset(ChargesByYear, 
                        Charge_Group_Description == "Driving Under Influence" |
                        Charge_Group_Description == "Drunkeness" |
                        Charge_Group_Description == "Miscellaneous Other Violations")
```

4.4 Plot data


```r
ggplot(TopChargesByYear, aes(Year, Arrests_Num, group= Charge_Group_Description, color= Charge_Group_Description)) + 
  geom_line(size = 1) +
  ggtitle("Number of Misdemeanor Arrests per Year by Charge Group Description") +
  ylab("Number of Arrests") +
  labs(color = "Charge Group Desc.") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour ="black"),
        panel.border = element_rect(linetype = "solid", fill = NA), legend.key =
        element_blank())
```

![](Arrests_Yearwise_Trends_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

4.5 Miscellaneous Other Violations is a bucket term for various different types of arrests. Here, we try to identify if there are certain types of Miscellaneous Other Violations that stand out.


```r
Misc <- subset(df, Charge_Group_Description == "Miscellaneous Other Violations" &
                 Arrest_Type_Code == "M")

SortedMisc <- Misc %>%
  group_by(Charge_Description) %>%
  summarise(Arrests_Num = n())
```

We learn that there are in fact 471 different types under the Miscellaneous Other Violations. The top three types are FTA after written promise, Los Angeles Municipal Code and Curfew - Juv Only.

Based on this analysis, we see that the decline in the number of arrests is due in large part to a decrease in Misdemeanor arrests related to Driving under influence and Drunkeness. They account for around 36% of Misdemeanor arrests, and around 21% of total arrests, across all years. 
