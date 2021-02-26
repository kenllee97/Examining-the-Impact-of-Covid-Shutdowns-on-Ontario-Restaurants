#### Preamble ####
# Purpose: To compare between sales across all seasons.
# Author: Renjing Liu (Max)
# Email: renjing.liu@mail.utoronto.ca
# Date: 26 February 2021
# Prerequisites: -
# Issues: I want to see the sales differences across all time periods of the year.
# To do: 
# - Select 2 months that have the smallest difference.


#### Workspace set-up ####
# Libraries
library(ggplot2)
library(tidyverse)

#read inputs
Df<- read_csv(here::here("inputs/2110001901-noSymbol.csv"))
Df<- Df%>%
  filter(Geography=="Ontario")

#### Data cleaning ####
#Transpose row and column, remove missing value
Df<- t(Df)%>%
  na.omit()
Df<- as.data.frame(Df[-1,])
#Modify column name
names(Df)[1]<- "Total resturant Sales"
Df[,1]<-str_remove_all(Df$`Total resturant Sales`,pattern = ",")
Df<- as.data.frame(sapply(Df, as.numeric),row.names = row.names(Df))
#Calcuate percentage change
percen=0
for (i in 1:nrow(Df)){
  percen[i]= (Df[i+1,1]-Df[i,1])/Df[i,1]
  
}
percen<- na.omit(percen)
Df<- data.frame(Df[-1, ],row.names = row.names(Df)[-1])
Ontario_data<- mutate(Df, percentage_change= percen)
names(Ontario_data)[1]<- "Total sales"
Ontario_data<- mutate(Ontario_data, Date= row.names(Ontario_data))
Ontario_data<- separate(data= Ontario_data,
                        col = Date,
                        sep=" ",
                        remove = TRUE,
                        into= c("Month","Year"))

#### Data cleaning ####
Ontario_data$percentage_change= abs(Ontario_data$percentage_change*100)

Month_compare<- Ontario_data%>%
  group_by(Month)%>%
  summarise(Avg_change= mean(percentage_change))%>%
  arrange(.by_group = TRUE)

#### Saving Aggregated Data ####
Month_compare<- Month_compare[order(Month_compare$Avg_change),]
write.csv(Month_compare, "inputs/Month_performance_data.csv")
