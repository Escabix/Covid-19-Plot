library(ggplot2)
library(tidyverse)  
library(reshape2)  
library(pracma)
library(gapminder)
library(dplyr)
library(tidylog)
library(scales)
library(zoo)
library(gghighlight)
library(utf8)

#Ronny Schlidt 7110951

#Import Data
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") 
attach(data)

#Transform Data
data$dateRep <- as.Date(data$dateRep, format = "%d/%m/%Y")

#Dates of the Lockdowns of the highlighted countries
countriesAndTerritories <- c("Australia", "Austria", "Brazil", "France", "Germany", "Japan", "India", "Italy", "Norway", "Russia", "South_Korea", "Turkey", "United_States_of_America", "United_Kingdom")
dateRep1 <- c("2020-03-23", "2020-03-16", "2020-02-17", "2020-03-17", "2020-03-23", "2020-02-27", "2020-03-25", "2020-03-09", "2020-03-12", "2020-03-28", "2020-02-20", "2020-04-23", "2020-03-20", "2020-03-23")
dateRep <- as.Date(dateRep1)
#Lockdown_week <- c(0, 0, 0, 1.7142857, 1.2857143, 0, 0, 1.8571429, 0, 0, 0, 0, 4.7142857, 1.8571429, 1.8571429)
#Lockdown_deaths <- c(0.0, 0.0, 0.0, 21.0, 27.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
Lockdown_week <- as.numeric(Lockdown_week)
Lockdown <- data.frame(countriesAndTerritories, dateRep)

data2 <- data %>% arrange(data$dateRep)


#DATA3 contatains the main Graph
data3 <- data2 %>% 
  group_by(countriesAndTerritories) %>%
  mutate(roll = rollmean(deaths, 15, na.pad = T)) %>%
  filter(countriesAndTerritories != "China")%>%
  filter(countriesAndTerritories != "Spain")%>%
  drop_na() %>%
  filter(!cumsum(roll >= 3) < 2)%>%
  mutate(anzahl = row_number()) %>%
  mutate(week = anzahl/7)

#all Lockdowns at Week 0
data4 <- data3 %>% slice(1) %>% filter(countriesAndTerritories %in% c("Australia", "Austria", "Brazil", "Japan", "India", "Norway", "Russia", "South_Korea"))
#all end-points of the highlighted countries
data5 <- data3 %>% slice(n()) %>% filter(countriesAndTerritories %in% c("Australia", "Austria", "Brazil", "France", "Germany", "Japan", "India", "Italy", "Norway", "Russia", "South_Korea", "Turkey", "United_States_of_America", "United_Kingdom"))  
data6 <- data3 %>% slice( n() ) %>%  filter(!countriesAndTerritories %in% c("Australia", "Austria", "Brazil", "France", "Germany", "Japan", "India", "Italy", "Norway", "Russia", "South_Korea", "Turkey", "United_States_of_America", "United_Kingdom"))

#Join, to find out the number of deaths during the lockdown
Lockdown1 <- right_join(data3, Lockdown)


#rename 
data3$countriesAndTerritories<-as.character(data3$countriesAndTerritories)
data3$countriesAndTerritories[data3$countriesAndTerritories =="United_States_of_America"]<-"USA"
data3$countriesAndTerritories[data3$countriesAndTerritories =="United_Kingdom"] <-"UK"
data3$countriesAndTerritories[data3$countriesAndTerritories =="South_Korea"] <-"S Korea"



#data5 <- select(data, cases, countriesAndTerritories, geoId) %>% group_by(countriesAndTerritories) %>% 
#  summarise(mean = mean(cases))

ggplot()+
  geom_point(data=Lockdown, aes(Lockdown_week, Lockdown_deaths), shape=18, size=, color="gray8", pch=21)

##########################################################################################################################################################################
a <- ggplot(data=data3, mapping = aes(x=week, y = roll, color = countriesAndTerritories))+
  geom_line()+
  
   
  
    labs(x = "Weeks since average daily deaths passed 3 \U27F6", y = "", title ="Daily death tolls are now at their peak of falling in many western countries", subtitle = "Daily deaths with coronavirus (7-day rolling average), by number of weeks since 3 daily deaths first recorded\n\U25B2 represent national lockdowns", caption = "Ronny Schlidt / @schlidt\nSource: European Centre for Disease Prevention and Control. Data updated June 06, 22:21 CEST")+ 
    gghighlight(countriesAndTerritories %in% c("USA","Australia", "China", "Austria", "Brazil", "France", "Germany", "Japan", "India", "Italy", "Norway", "Russia", "Spain", "S Korea", "Turkey", "UK"))+
  
    scale_y_continuous(trans = "pseudo_log", breaks = c(1,2,5,10,20,50,100,200,500,1000,2000), sec.axis = dup_axis(), limits = c(0, 2500), expand=c(0,0))+
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) , limits = c(0,15), expand=c(0,0))+
  
    theme(
    panel.background = element_rect(fill = "seashell", colour = "seashell", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"), 
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "seashell"),
    plot.subtitle = element_text(face = "bold", color = "azure4"),
    plot.caption = element_text(color = "azure4", hjust = 0),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold", color = "azure4"),
    axis.line.x = element_line(size = 0.5))+
    annotate(geom="text", x = 13.3, y = 1500, label = "Ronny Schlidt", color="lightgray", size=10)+
   
    #Add all Points
    geom_point(data=Lockdown1, aes(x=week, y=roll, color=countriesAndTerritories), shape=17, size=4)+
    geom_point(data=data4,     aes(x=week, y=roll, color=countriesAndTerritories), shape=17, size=4)+
    geom_point(data=data5,     aes(x=week, y=roll, color=countriesAndTerritories), shape=20, size=4)+
    geom_point(data=data6,     aes(x=week, y=roll), shape=20, size=2, color ="lightgray")
 
   
