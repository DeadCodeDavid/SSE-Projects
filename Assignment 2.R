### Setup
setwd("C:/Users/OnetoFive/OneDrive/SSE/Courses/Introduction into R/Assignment 2 files")

#install.packages("tidyverse")
#install.packages("rio")
#install.packages("stringr")
#install.packages("magrittr")

## 1. loads packages
library("tidyverse")
library("rio")
library("stringr")
library("magrittr")

## 2. import data
Protests = read.csv("Protests Dec12.csv", sep=";", header= TRUE)

## 3. filter data
Protests = Protests[Protests$EVENT_TYPE == "Protests" | Protests$EVENT_TYPE == "Riots",]

## 4. change variable
Protests$SUB_EVENT_TYPE = as.factor(Protests$SUB_EVENT_TYPE)

## 5. creating new binary variables
Protests = mutate(Protests, BLM = str_detect(Protests$ASSOC_ACTOR_1, "BLM|Black Lives Matter" )) # searches vor "BLM" and "Black Lives Matter"
Protests = mutate(Protests, VIOL = str_detect(Protests$SUB_EVENT_TYPE, "Peaceful protest" )) # searches for "Peaceful protest"

## 6. creating a table
#convert into date format
Protests$EVENT_DATE = strptime(Protests$EVENT_DATE, format= "%d-%B-%Y")
# select by date
Protests = Protests[Protests$EVENT_DATE < "2020-11-03 00:00:00",]
# 
Summary = aggregate(VIOL ~ BLM, data = Protests, FUN = sum)


### Aggregating Data

## 1. look at data
glimpse(Protests$EVENT_DATE) #its already converted

## 2. convert into date format
Protests$EVENT_DATE = as.Date(Protests$EVENT_DATE, format= "%d-%B-%Y")

## 3. create a month date variable
Protests = mutate(Protests, MONTH = months(Protests$EVENT_DATE))

## 4. aggregating on the month & county level
Table =  Protests %>% group_by(MONTH, LOCATION) %>% filter(VIOL == "TRUE", BLM == "TRUE") %>% summarize(n())

## 5. exporting the dataset
write.csv(Table,"C:/Users/OnetoFive/OneDrive/SSE/Courses/Introduction into R/Assignment 2 files/Protests_Manipulated_Dataset.csv") #the last part of the directory is the set name


### Creating a Time Series plot

## 1.
by_blm =  Protests %>% filter(BLM == "TRUE") %>% group_by(EVENT_DATE,) %>% summarize(protests = n(),violentprotests = sum(VIOL == "TRUE"))

## 2. creatin time series plot
plot(x = by_blm$EVENT_DATE, y = by_blm$protests, type = "l")

## 3. add line
abline( v = "2020-05-26")


