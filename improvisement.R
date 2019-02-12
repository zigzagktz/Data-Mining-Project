library(ggplot2)
library(dplyr)
library(stringr)
library(corrplot)

setwd("C:/Users/ksiro/Desktop/data mining/week 3")
df <- read.csv("metadata.csv",sep=",",header=T)
head(df)
summary(df)


##removing duplicacy
df <- unique(df)


#if we create a new data frame only from values that have director facebook likes, 
#we realise a lot of other attirbutes also have missing values in that subset
# see the summary below
test <- df[which(is.na(df$director_facebook_likes)),]
colSums(is.na(df))
colSums(is.na(test))

#remove them
df <- df[-which(is.na(df$director_facebook_likes)),]

#imputation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df$actor_1_facebook_likes[which(is.na(df$actor_1_facebook_likes))] <- mean(df$actor_1_facebook_likes,na.rm = TRUE)
df$actor_2_facebook_likes[which(is.na(df$actor_2_facebook_likes))] <- mean(df$actor_2_facebook_likes,na.rm = TRUE)
df$actor_3_facebook_likes[which(is.na(df$actor_3_facebook_likes))] <- mean(df$actor_3_facebook_likes,na.rm = TRUE)

df$duration[which(is.na(df$duration))] <- mean(df$duration,na.rm=T)

df$num_critic_for_reviews[which(is.na(df$num_critic_for_reviews))] <- mean(df$num_critic_for_reviews,na.rm=TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Cleaning
## removing 4 rows that had missig values in title year because other also have NAs.
df <- df[-which(is.na(df$title_year)),]


##removing the whole row that has single big outlier in budget
summary(df$budget)
df <- df[-which.max(df$budget),]
##now imputing whats left in budget by mean since the oulier cannot skew the result now
df$budget[which(is.na(df$budget))]  <- mean(df$budget,na.rm=TRUE) #IMPUTING NAs


##two big outlier in face poster, see below
barplot(df$facenumber_in_poster)
t <- head(order(df$facenumber_in_poster,decreasing=TRUE),2)
df$facenumber_in_poster[t] <- mean(df$facenumber_in_poster,na.rm=TRUE) # impte outliers with mean
df$facenumber_in_poster[which(is.na(df$facenumber_in_poster))] <- mean(df$facenumber_in_poster,na.rm=TRUE) # impte NA with mean

##now looks different
barplot(df$facenumber_in_poster)

## impute number users for reviews
df$num_user_for_reviews[which(is.na(df$num_user_for_reviews))] <- mean(df$num_user_for_reviews,na.rm=TRUE)

## #impute median as missing value in gross value
df$gross[which(is.na(df1$gross))] <- median(df$gross,na.rm=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#missing values end




par(mfrow=c(1,3))
  boxplot(log(df$actor_1_facebook_likes))
  boxplot(log(df$actor_2_facebook_likes))
  boxplot(log(df$actor_3_facebook_likes))
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #most of the movies are drama, follwed by comdey, action and thriller
  df$genres <- as.character(df$genres)
  a <- strsplit(df$genres,"|",fixed=T)
  b <- unlist(a)
  t<-table(b)
  t <- as.data.frame(t)
  ggplot(t,aes(x=b,y=Freq))+ geom_histogram(stat="identity",aes()) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## cleaning the movie title names
  library(stringr)
  title <- gsub("Â","",df$movie_title)
  title <- str_trim(title)
  head(title)
  df$movie_title <- title
  
  
  #
  #
  #
  #

  
  #screen ratio vs imdb
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ggplot(df,aes(x=imdb_score,fill=factor(aspect_ratio)))+ geom_histogram(stat="count")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #taking top expensive movies out and compairng against screen ratio
  expensive <- budget[head(order(budget,decreasing=TRUE),400)]
  barplot(expensive,xlim = c(1,100)) ##I see only a few movies are too expensive
  expensive_df <- df[(df$budget>200) , ]
  table(expensive_df$aspect_ratio) 
  #we already knew that most of the movies are already in either 2.35 or 1.85 ration
  ## so no significance of screen ration on budegt of movie
  ## budget vs aspect ratio (no siginificance)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## countries vs imdb
  temp <- c("India", "USA","UK","Spain","Australia",
            "China","Canada","France","Germany","Italy","Japan","other")
  df$country <- factor(df$country)
  levels(df$country)[!(levels(df$country) %in% temp) ] <- "other"
  table(df$country)

  score <- df$imdb_score
  score[(score <= 1.6) & (score > 0.5)] <- 1
  score[(score <= 2.5) & (score > 1.5)] <- 2
  score[(score <= 3.5) & (score > 2.5)] <- 3
  score[(score <= 4.5) & (score > 3.5)] <- 4
  score[(score <= 5.5) & (score > 4.5)] <- 5
  score[(score <= 6.5) & (score > 5.5)] <- 6
  score[(score <= 7.5) & (score > 6.5)] <- 7
  score[(score <= 8.5) & (score > 7.5)] <- 8
  score[(score <= 9.5) & (score > 8.5)] <- 9
  score[(score <= 10)  &  (score > 9.5)] <- 10
  
  df["imdb_score_cat"] <- score
  
  ggplot(df,aes(x=country,fill=factor(imdb_score_cat))) + geom_histogram(stat = "count") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
 
   ## no country bias 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
  
  
  
  #year release of movie having 0 facebook likes
  dd <- df %>% select(title_year)  %>% filter(df$movie_facebook_likes==0)
  dd<- as.data.frame(dd)
  hist(dd$title_year,breaks=200)
  ##i see that even older movies does not have zero likes
  ##movies with zero likes are from 1980 to till date
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
  
  