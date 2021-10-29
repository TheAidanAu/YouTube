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

###II. add the category name column by matching with the category IDs

df_categoryName=read.csv("YouTube_categoryID.csv")
df_categoryName

df_raw=left_join(df_raw,df_categoryName,by="categoryId")

View(df_raw)

#Looks like there're 11 uniqle videos with zero view count. 
#I looked at each of them individually. They're all promotional videos of YouTube/Google
View(df_raw%>%filter(view_count==0))
zero_view_videos=df_raw%>%filter(view_count==0)
View(zero_view_videos)
unique(zero_view_videos$video_id)

#For our analysis purposes, these zero-viewed videos will be excluded 
df_raw=df_raw%>%filter(view_count!=0)

df_raw_copy=df_raw #saving a copy as backup

### Identify numerical and categorical variables

cols_list=colnames(df_raw)
cols_list

str(df_raw)

num_cols=c("view_count","likes","dislikes",
           "comment_count","days_from_published_to_trending")

cat_cols=c("video_id","title","channelId","channelTitle",
           "categoryId","tags","thumbnail_link","comments_disabled",
           "ratings_disabled","description","categoryName")

datetime_cols=c("publishedAt","trending_date")

summary(df_raw)
# check the descriptive statics of each column in the df 

###EDA

dim(df_raw)
#check the dimension/shape of the df

View(df_raw)

##If a video makes it to the trending video list on a particular day, 
##there is an entry about that video in the dataframe for that particular day. 
##The same videos can be a trending videos for consecutive days. 
##To avoid over-counting, let's make a dataframe which contains only unique videos, 
## and we are only keeping the most recent entry when that video on the trending video list


##In order to keep the most recent entry for each video, 
##that video is supposed to have the same video 
df_unique_video=df_raw%>%group_by(video_id)%>%
  summarise(title=last(title),publishedAt=last(publishedAt),channelId = last(channelId),
            last_trending_date = last(trending_date),
            channelTitle=last(channelTitle),
            categoryId=last(categoryId),
            categoryName=last(categoryName),
            tags=last(tags),
            view_count=last(view_count),
            likes=last(likes),
            dislikes=last(dislikes),
            comment_count=last(comment_count),
            thumbnail_link=last(thumbnail_link),
            comments_disabled=last(comments_disabled),
            ratings_disabled=last(ratings_disabled),
            description=last(description),
            days_from_published_to_trending=last(days_from_published_to_trending))
View(df_unique_video)

##checking to make sure the newly created df contains only unique video IDs
unique(duplicated(df_unique_video$video_id))
###_6XjfohyyRg dup entry

dim(df_raw)#
#There're 89146 entries of trending videos.
#Some entries were for the same videos because some videos were trending videos on consecutive days 
dim(df_unique_video)
#looks like there're only 15,488 unique videos in the dataset

###Correlation Matrix
###Correlation tells us how strong the relationship is

#Pick only the numeric columns from the unique video df for correlation analysis 
df_numeric=df_unique_video%>%
  select(view_count,likes,dislikes,
         comment_count,days_from_published_to_trending) 

#install.packages("corrplot")
library(corrplot)
df_numeric_corr = cor(df_numeric)

####What features correlate the most with the number of views or other features? 
corrplot(df_numeric_corr, method = 'number')
#Looks like there's a high correlation 
# between the number views and the number of likes (corr=0.86), 
# as well as the number of dislikes (corr=0.70)
# and just a little bit with the comment count (corr=0.62)

#Find the p-value for the correlation 
# between the above-mentioned features
cor.test(df_numeric$view_count,df_numeric$likes)
#p-value < 2.2e-16, strong certainty in the result
cor.test(df_numeric$view_count,df_numeric$dislikes)
#p-value < 2.2e-16, strong certainty in the result
cor.test(df_numeric$view_count,df_numeric$comment_count)
#p-value < 2.2e-16, strong certainty in the result

#Scatter plot 
ggplot(data=df_numeric,aes(x=likes,y=view_count))+geom_point()+geom_smooth(formula = y ~ x, method = "lm")+labs(title="Views vs Likes of Trending Videos \n r=0.86, p-value<2.2e-16 → Strong Certainty in the relationship", x ="Number of Likes", y = "Number of Views")
ggplot(data=df_numeric,aes(x=dislikes,y=view_count))+geom_point()+geom_smooth(formula = y ~ x, method = "lm")+labs(title="Views vs Dislikes of Trending Videos \n r=0.70, p-value<2.2e-16 → Strong Certainty in the relationship", x ="Number of Dislikes", y = "Number of Views")
ggplot(data=df_numeric,aes(x=dislikes,y=comment_count))+geom_point()+geom_smooth(formula = y ~ x, method = "lm")+labs(title="Views vs Comments of Trending Videos \n r=0.62, p-value<2.2e-16 → Strong Certainty in the relationship", x ="Number of Comments", y = "Number of Views")

##Histogram of number of days trended
ggplot(data=df_unique_video,aes(x=days_from_published_to_trending))+geom_bar(aes(y = (..count..)/sum(..count..)))+scale_y_continuous(labels = scales::percent)+coord_cartesian(xlim = c(0,20))+labs(title="Distribution of Duration of Videos Staying Trending \n How many days have videos stayed on the Trending List?", x ="Number of Consecutive Days Since First Trending", y = "Percentage of All Trending Videos")

df_videos_per_category=df_unique_video%>%group_by(categoryName)%>%summarise(num_videos=n())
View(df_videos_per_category)

df_videos_per_channel=df_raw%>%group_by(channelTitle)%>%distinct(video_id)%>%summarise(num_videos=n())
View(df_videos_per_channel)
#This chart below is for pure count, let's normalize it by the total count
#ggplot(data = df_videos_per_channel, aes(x=num_videos))+geom_histogram()

#Ryan: Half of all creators on the list only had one video make it in the 280 days covered by the data. Very few creators made the list more than 10 times in the 280 days covered.
ggplot(data = df_videos_per_channel, aes(x = num_videos))+geom_bar(aes(y = (..count..)/sum(..count..)))+scale_y_continuous(labels = scales::percent)+coord_cartesian(xlim = c(0,50))+ggtitle("Distribution of Videos Per Creator")+labs(title="Distrubtion of the Number of Trending Videos from the Same Creators \n How many trending videos do the same creators usually have?", x ="Number of Trending Videos from the Same Creators", y = "Percentage of Creators")

ggplot(data = df_unique_video, aes(x = categoryName))+geom_bar(aes(y = (..count..)/sum(..count..)))+scale_y_continuous(labels = scales::percent)+coord_flip()+labs(title="Trending Videos by Categories", x ="Categories", y = "Percentage of Trending Videos")
ggplot(data = df_unique_video, aes(x = categoryName, y = view_count/1000000))+geom_boxplot()+coord_flip(ylim=c(0,5))+labs(title="View Counts by Categories", x ="Categories", y = "View Counts (in Millions)")
ggplot(data = df_unique_video, aes(x = categoryName, y = likes/1000000))+geom_boxplot()+coord_flip(ylim=c(0,0.4))+labs(title="Number of Likes by Categories", x ="Categories", y = "Number of Likes (in Millions)")
ggplot(data = df_unique_video, aes(x = categoryName, y = dislikes))+geom_boxplot()+coord_flip(ylim=c(0,5000))+labs(title="Number of Dislikes by Categories", x ="Categories", y = "Number of Dislikes")
ggplot(data = df_unique_video, aes(x = categoryName, y = comment_count))+coord_flip()+geom_boxplot()+coord_flip(ylim=c(0,20000))+labs(title="Number of Comments by Categories", x ="Categories", y = "Number of Comments")
#Royce>>> Q2 and Q3, and same??
ggplot(data = df_unique_video, aes(x = categoryName, y = days_from_published_to_trending))+geom_boxplot()+coord_flip()

View(df_raw)
View(df_unique_video)

#Pareto Chart of Views vs Videos
View(df_unique_video%>%arrange(desc(view_count)))