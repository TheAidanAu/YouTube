library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

df_raw=read.csv("US_youtube_trending_data.csv")
#head(df_raw,10) #too much data to display
View(df_raw)

str(df_raw) 
#check the datatypes of each column in the df