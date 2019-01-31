setwd("C:/Users/ksiro/Desktop/data mining/week 3")
df <- read.csv("metadata.csv",sep=";",header=T)
head(df)
View(df)
df <- read.csv("metadata.csv",sep=",",header=T)
head(df)
summary(df)

a <- df$actor_3_facebook_likes
b <- df$actor_1_facebook_likes
c <- df$actor_2_facebook_likes
#all 3 actors have fb likes missing values in continous manner
likes <- cbind(b,c,a)
colnames(likes) <- c("fb_likes_actor_1","fb_likes_actor_2","fb_likes_actor_3")
summary(likes)
likes <- as.data.frame(likes)
t <- likes[which(is.na(likes$fb_likes_actor_3)),]
colSums(is.na(t))

#most of them are documentary that does not have actor facebook likes
x <- likes_test$genres
length(grep("Documentary",x))*100/23


#imputing likes
likes$fb_likes_actor_1[which(is.na(likes$fb_likes_actor_1))] <- mean(likes$fb_likes_actor_1,na.rm = TRUE)
likes$fb_likes_actor_2[which(is.na(likes$fb_likes_actor_2))] <- mean(likes$fb_likes_actor_2,na.rm = TRUE)
likes$fb_likes_actor_3[which(is.na(likes$fb_likes_actor_3))] <- mean(likes$fb_likes_actor_3,na.rm = TRUE)
summary(likes)




a <- df$actor_1_name
b<- df$actor_2_name
c <- df$actor_3_name
a <- as.character(a)
b<- as.character(b)
c <- as.character(c)
names <- cbind(a,b,c)
colnames(names) <- c("actor_1_name" , "actor_2_name","actor_3_name")

sum(is.na(names))
sum(is.na(likes))

which(is.na(likes))

par(mfrow=c(1,3))
for (i in 1:3){
  boxplot(likes[,i])
}

## removing likes and nams columns from data frame
n <- c("actor_3_facebook_likes","actor_2_facebook_likes","actor_1_facebook_likes","actor_1_name","actor_2_name","actor_3_name")
df <- df[,!(colnames(df) %in% n)]



colSums(is.na(df))

boxplot(df$gross)


hist(df$num_critic_for_reviews,breaks = 100)

hist(df$duration,breaks=100,xlim=c(50,200))


df$duration[which(is.na(df$duration))] <- mean(df$duration,na.rm=T)

a <- strsplit(genres,"|",fixed=T)
b <- unlist(a)
table(b)

summary(df$director_facebook_likes)
summary(df$title_year)

##removing an empty level from color
df$color <- as.character(df$color)
df$color[which(df$color=="")] <- "Color"

## all "director facebook likes" (104) missing values are within gross missing values (884)
## same goes with "movie title" (105) in test and (108) in df
test <- df[which(is.na(df$gross)),]
summary(test)[,5]
summary(df)[,5]
# missing gross, missing title, missing director facebook likes


## cleaning the movie title names
head(df$movie_title)
library(stringr)
title <- gsub("Â","",df$movie_title)
title <- str_trim(title)
head(title)
df$movie_title <- title

##imputing number of critics for review
df$num_critic_for_reviews[which(is.na(df$num_critic_for_reviews))] <- mean(df$num_critic_for_reviews,na.rm=TRUE)

summary(df)
## removing 108 rows that had missig values in title year because all 108 rows of budget column also ahd NA.
df1 <- df[-which(is.na(df$title_year)),]

## also remove them from likes and names table that we created above
likes <- likes[-which(is.na(df$title_year)),]
names <- names[-which(is.na(df$title_year)),]


##checking if screen ratio have any relation with imdb rating
ggplot(df1,aes(x=imdb_score,fill=factor(aspect_ratio)))+ geom_histogram(stat="count")
## i see that there seems to be no relation
## its just that most of the movies were either 2.2 or 1.85 ratio
## imdb score vs aspect ratio (no relation)


##convert money into millions
df1$budget <- df1$budget/1000000
options(scipen=999)## discables scientific notation

boxplot(df1$budget)
budget <- df1$budget
budget[head(order(df1$budget,decreasing=TRUE),1)]
budget <- budget[head(-(order(budget,decreasing = TRUE)),1)]
boxplot(budget)

## take top 50 expensive movie
expensive <- budget[head(order(budget,decreasing=TRUE),400)]
barplot(expensive)
## looking at the plot we see that there are only a few movies that were extremenly expensive
## let's extract top values out and analyse them
expensive[order(expensive,decreasing=TRUE)] 
## I am taking top 200 million dollars as therashold
## there are 46 moveis that are expensive than 200 million dollars
expensive_df <- df1[(df1$budget>200 & df1$budget!=12215.5 & !(is.na(df1$budget))) , ]

table(expensive_df$aspect_ratio) 
## we already knew that most of the movies are already in either 2.35 or 1.85 ration
## so no significance of screen ration on budegt of movie

## budget vs aspect ratio (no siginificance)






