### Setup
setwd("C:/Users/OnetoFive/OneDrive/SSE/Courses/Introduction into R/Assignment 5/Assignment 5")

## loads packages
library(plyr) 
library(tidyverse)
library(rio)
library(magrittr)
library(broom)
library(stargazer)
library(lmtest)
library(sandwich)
library(ggthemes)


### Part 1, Figure 1 -------------------------------------

## 1. loading the data
CSWEP = import("CSWEPtrends_data.dta") 

## 2. keeping chairman columns
detect = colnames(CSWEP)
keep = str_detect(detect, paste(c("chairmans", "year"),collapse = '|'))
CSWEP = CSWEP[,keep == TRUE]

## 3. removing suffix
colnames(CSWEP) = gsub("_chairmans","",colnames(CSWEP))

## 4 & 5. removing missing observations
CSWEP = CSWEP[complete.cases(CSWEP), ]

## 6. creating the plot
plotdata = gather(CSWEP,"variable","value",2:ncol(CSWEP))
CSWEP_line_v1 = 
  ggplot(plotdata, aes(x=year, y=value, color = variable, linetype = variable, shape =variable)) +
  geom_line() + 
  geom_point() +
  theme_stata() + ## 7. changing theme
  scale_colour_stata() + ## 7. changing color
  scale_x_continuous(breaks = seq(min(CSWEP$year),max(CSWEP$year),4)) +  ## 8. setting ticks 4 years apart
  labs(title = "Representation of Women among First-Year PhD Students, New PhDs, and Faculty
by Rank for the Chairman's Group of Departments, 1998 - 2017", y = "% of total who are women")  + ## 8. adding labels to axis
  annotate("text", x = 1999, y = 36, label = "First year PhD students")  + ## 9. annotate flags
  annotate("segment", x = 1998.5, y = 35,
           xend = 1999, yend = 30,
           arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  annotate("text", x = 2017, y = 9, label = "Full professors")  + ## 9. annotate flags
  annotate("segment", x = 2017, y = 10,
           xend = 2016, yend = 12,
           arrow = arrow(length = unit(2, "mm"), type = "closed"))
CSWEP_line_v1


### Part 1, Figure 3 -------------------------------------

## 1. loading the data
Disci = import("discipline_data.dta") 

## 2. removing prefix and formatting data
colnames(Disci) = gsub("f_share_","",colnames(Disci))
plotdata2 = gather(Disci,"variable","value",3:ncol(Disci))
plotdata2 = plotdata2[plotdata2$variable != "Associate",]

## 3. plotting the data
Disci_line_v1 = 
  ggplot(plotdata2, aes(x=year, y=value, color = group, linetype = group, shape =group)) +
  geom_line() + 
  geom_point() +
  theme_stata() + 
  scale_colour_stata() + 
  scale_x_continuous(breaks = seq(min(Disci$year),max(Disci$year),4)) + 
  labs(title = "Representation of Women in Top-50 Departments, 2002 - 2012", y = "% of total who are women") +
  facet_wrap(~variable)
Disci_line_v1


### Part 2 -------------------------------------

## 1. loading the data
IHDS = import("IHDS_2012_Data.dta") 

## 2. linear regression
model1 <- lm(FP5 ~ EW8, data = IHDS)
summary(model1)
tidy(model1)

## 3. linear regression with robust std. errors
tidy(coeftest(model1, vcov = vcovHC(model1, type="HC1")))

## 4. advanced linear regression model
IHDS$STATEID = as.factor(IHDS$STATEID)
str(IHDS)

model2 = lm(FP5 ~ EW8 + STATEID + EW6 + SPRO5 + SPED6 + I(EW8^2), data = IHDS)
summary(model2)
tidy(model2) 

# failed to remove state dummy coefficients

## 5. running regressions for every state
states = unique(IHDS$STATEID)
reg_states = function(states) {
  subset = IHDS %>%
    filter(STATEID == states)
  
  tidy(lm(FP5 ~ EW8 + EW6 + SPRO5 + SPED6, data = subset), conf.int = T) %>%
    mutate(STATEID = states)
}

## 6. consolidating the regressions values
res_states = map_df(states, reg_states)
res_states = res_states[res_states$term == "EW8",]

## 7. plotting the coefficients
IHDS_plot_v1 = 
  ggplot(res_states) +
  geom_pointrange(aes(x = STATEID, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  expand_limits(y = 0) +
  theme_clean() +
  labs(x = "STATEID", y = "Coefficient estimates & CI of EW8")
IHDS_plot_v1