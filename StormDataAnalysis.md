Title
========================================================

### Synopsis

### Loading, Processing and Formatting the Dataset


```r
stormData <- read.csv("StormData.bz2")
```


Since we're going to be focused on analyzing the event type's (EVTYPE) impact against health and economic livelihood of the population, we can start by checking to make sure we have the columns properly named and typed.


```r
colnames(stormData)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```


Health impact would likely be measured best in number of fatalities and injuries (FATALITIES and INJURIES as the respective column names).

For the economic analysis we have PROPDMG and CROPDMG and their exp values in PROPDMGEXP and CROPDMGEXP


```r
unique(stormData$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```


Since the values have ranges from K, M, B to 1-9, we can assume K, M, B for thousand, million, billion and 1-9 as 10^n.  Now subset the columns we are taking to be relevant to the investigation, get rid of any zero values that we don't need.


```r
st <- subset(stormData, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 
    0, select = c("STATE__", "BGN_DATE", "END_DATE", "EVTYPE", "FATALITIES", 
    "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP"))
```


Let's total up the fatalities and injuries


```r
st$totalHealth <- st$FATALITIES + st$INJURIES
```


Now convert the dollar amounts for crop and property damage.


```r
st$propdamage <- st$PROPDMG
st$PROPDMGEXP <- as.character(st$PROPDMGEXP)
st$PROPDMGEXP[tolower(st$PROPDMGEXP) == "h"] <- "2"
st$PROPDMGEXP[tolower(st$PROPDMGEXP) == "k"] <- "3"
st$PROPDMGEXP[tolower(st$PROPDMGEXP) == "m"] <- "6"
st$PROPDMGEXP[tolower(st$PROPDMGEXP) == "b"] <- "9"
st$PROPDMGEXP[st$PROPDMGEXP == "" | st$PROPDMGEXP == "-" | st$PROPDMGEXP == 
    "?" | st$PROPDMGEXP == "+"] <- "0"
st$PROPDMGEXP <- as.numeric(st$PROPDMGEXP)
st$propdamage <- (10^st$PROPDMGEXP) * st$PROPDMG
```



```r
st$cropdamage <- st$CROPDMG
st$CROPDMGEXP <- as.character(st$CROPDMGEXP)
st$CROPDMGEXP[tolower(st$CROPDMGEXP) == "h"] <- "2"
st$CROPDMGEXP[tolower(st$CROPDMGEXP) == "k"] <- "3"
st$CROPDMGEXP[tolower(st$CROPDMGEXP) == "m"] <- "6"
st$CROPDMGEXP[tolower(st$CROPDMGEXP) == "b"] <- "9"
st$CROPDMGEXP[st$CROPDMGEXP == "" | st$CROPDMGEXP == "-" | st$CROPDMGEXP == 
    "?" | st$CROPDMGEXP == "+"] <- "0"
st$CROPDMGEXP <- as.numeric(st$CROPDMGEXP)
st$cropdamage <- (10^st$CROPDMGEXP) * st$CROPDMG
```


### Results

Now we can begin to look at the health impact by environmental disaster.  We can sum the total events by type, showing the top fifteen worst


```r
onlySums <- st[, c("EVTYPE", "FATALITIES", "INJURIES", "propdamage", "totalHealth")]
agged <- aggregate(. ~ EVTYPE, onlySums, sum)
head(agged[order(-agged$totalHealth), ], 15)
```

```
##                EVTYPE FATALITIES INJURIES propdamage totalHealth
## 407           TORNADO       5633    91346  5.695e+10       96979
## 61     EXCESSIVE HEAT       1903     6525  7.754e+06        8428
## 423         TSTM WIND        504     6957  4.485e+09        7461
## 86              FLOOD        470     6789  1.447e+11        7259
## 258         LIGHTNING        816     5230  9.304e+08        6046
## 151              HEAT        937     2100  1.797e+06        3037
## 73        FLASH FLOOD        978     1777  1.682e+10        2755
## 238         ICE STORM         89     1975  3.945e+09        2064
## 364 THUNDERSTORM WIND        133     1488  3.483e+09        1621
## 481      WINTER STORM        206     1321  6.688e+09        1527
## 200         HIGH WIND        248     1137  5.270e+09        1385
## 134              HAIL         15     1361  1.574e+10        1376
## 224 HURRICANE/TYPHOON         64     1275  6.931e+10        1339
## 170        HEAVY SNOW        127     1021  9.328e+08        1148
## 471          WILDFIRE         75      911  4.765e+09         986
```

