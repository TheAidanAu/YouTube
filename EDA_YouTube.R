library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

df_raw=read.csv("US_youtube_trending_data.csv")
#head(df_raw,10) #too much data to display
View(df_raw)

str(df_raw) 
#check the datatypes of each column in the df

colnames(df_raw) #Look at just the column names

colSums(is.na(df_raw)) 
#check the number of NAs in each column in the df 

### Feature Engineering

## I. Convert the columns PublishedAt, trending_date from characters to datetime
df_raw$publishedAt=as.Date(as.POSIXct(df_raw$publishedAt,format='%Y-%m-%d',tz= "UTC"))
df_raw$trending_date=as.Date(as.POSIXct(df_raw$trending_date,format='%Y-%m-%d',tz= "UTC"))
str(df_raw$publishedAt)
str(df_raw$trending_date)
#check to make sure those 2 columns are now datetime

df_raw$days_from_published_to_trending=df_raw$trending_date-df_raw$publishedAt
df_raw$days_from_published_to_trending=as.integer(df_raw$days_from_published_to_trending)

#Create a new column of the number of days from published to trending
# by subtracting the two dates 
# and convert it from difftime datatype to integer 

sort(unique(df_raw$days_from_published_to_trending))
#check the unique values of number of days from published to trending

View(data.frame(df_raw$publishedAt,df_raw$trending_date,df_raw$days_from_published_to_trending))
#checking to see if the newly created column-"days_from_published_to_trending" looks good

sort(unique(df_raw$categoryId))

