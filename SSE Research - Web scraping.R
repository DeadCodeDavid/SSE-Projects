### Setup
setwd("C:/Users/OnetoFive/OneDrive/SSE/Courses/Introduction into R/Assignment 3/Assignment 3")

#install.packages("tidyverse")
#install.packages("rio")
#install.packages("magrittr")
#install.packages("plyr")
#install.packages("rvest")

## 1. loads packages
library(plyr)
library(tidyverse)
library(rio)
library(magrittr)
library(rvest)


### Part 1: Scraping voter registration data

## 2. specifying webpage
webpage <- paste0("https://elect.ky.gov/Resources/Pages/Registration-Statistics.aspx")

## 3. specifying CSS selector
css_selector <- "#ctl00_ctl00_PlaceHolderContentFromChild_PlaceHolderContent_ctl02__ControlWrapper_RichHtmlField p a"

## 4. load the webpage into R and extract the table
voters_2020 <- read_html(webpage) %>%
  html_nodes(css = css_selector) %>%
  html_attrs() %>%
  unlist() %>%
  tibble()

## 5. select the relevant links
colnames(voters_2020) = "link_ky"
# define which data to include and exlude by setting patterns
include = c("/Resources/Documents/voterstats-2020","/Resources/Documents/voterstats-201912")
exclude = c("202012")
# including 2020 & Dec.2019
voters_2020 = filter(voters_2020, grepl(paste(include, collapse="|"), voters_2020$link_ky))
# exluding the Dec. 2020 
voters_2020 = filter(voters_2020, !grepl(paste(exclude, collapse="|"), voters_2020$link_ky)) 
voters_2020_link = voters_2020$link_ky

## 6. adding missing a prefix. Add "https://elect.ky.gov" to each element of the vector.
voters_2020_link = paste("https://elect.ky.gov", voters_2020_link, sep="")

### Downloading the Excel files

## 7. creating the date_ky vector by extracting the first numbers/the date
date_ky = str_extract(voters_2020$link_ky, "[0-9]+")

## 8. downloading the data from the website
voters_2020_combined = map2(voters_2020_link, date_ky, ~ import(.x) %>% mutate(Date= .y))
#test=import(voters_2020_link[1]); to check what the output should look like 

## 9. turning the resulting list into a data frame
voters_2020_combined =  ldply(voters_2020_combined)

## 10. cleaning the data
# removing empty and state wide counties
voters_2020_combined = voters_2020_combined[!is.na(voters_2020_combined$County) & 
                                            voters_2020_combined$County != "Statewide totals",]
#summing up minor parties
voters_2020_combined$Side = rowSums( voters_2020_combined[,5:11] )
voters_2020_combined$County = str_extract(voters_2020_combined$County, "[:alpha:]+")
# removing all numbers(non-alphabetical) characters
voters_2020_combined = voters_2020_combined[,c("County", "Date", "Rep", "Dem", "Side")]
#keeping only the selected columns
voters_2020_combined = voters_2020_combined %>%
                       pivot_longer(!County & !Date, names_to = "Party", values_to = "Count")

## 11. creating a 2-letter state variable
voters_2020_combined$State = "KY"


### Part 2: Collect FIPS codes to be merged

## 12. setting webpage2 and CSS selector2
webpage2 <- paste0("https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697")
css_selector2 <- "#detail"

## 13. scraping the data from the website
FIPS = read_html(webpage2) %>%
  html_nodes(css = css_selector2) %>%
  html_table() %>%
  ldply()
# capitalizing counties/names
FIPS$Name = str_to_upper(FIPS$Name)

## 14. restoring the variable to its 5-digit format by adding a 0 at the front of all 4 digit values
FIPS$FIPS = str_pad(as.character(FIPS$FIPS), width = 5, side = "left", pad = "0")

## 15. joining the tables
voters_2020_combined = left_join(voters_2020_combined,FIPS, 
                                 by = c("County" = "Name", "State" = "State"))

## 16. saving the combined dataset
write.csv(voters_2020_combined, 
          "C:/Users/OnetoFive/OneDrive/SSE/Courses/Introduction into R/Assignment 3/Assignment 3/Voters_2020.csv") #the last part of the directory is the set name




