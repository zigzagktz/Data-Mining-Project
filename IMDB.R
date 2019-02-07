library(ggplot2)
library(dplyr)
library(stringr)
library(corrplot)

setwd("C:/Users/ksiro/Desktop/data mining/week 3")
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

likes_test <- df[which(is.na(likes$fb_likes_actor_3)),]

#most of them are documentaries that does not have 3rd actor facebook likes
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
df1$budget[which(is.na(df1$budget))]  <- mean(df1$budget,na.rm=TRUE) #IMPUTING NAs

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## now countries vs the imdb
## lets first convert countries into factor
summary(df1$country)
temp <- c("India", "USA","UK","Spain","Australia",
          "China","Canada","France","Germany","Italy","Japan","other")
df1$country <- factor(df1$country)
levels(df1$country)[!(levels(df1$country) %in% temp) ] <- "other"
table(df1$country)



score <- df1$imdb_score
  score[(score < 1.5) & (score > 0.5)] <- 1
  score[(score <= 2.5) & (score > 1.5)] <- 2
  score[(score <= 3.5) & (score > 2.5)] <- 3
  score[(score <= 4.5) & (score > 3.5)] <- 4
  score[(score <= 5.5) & (score > 4.5)] <- 5
  score[(score <= 6.5) & (score > 5.5)] <- 6
  score[(score <= 7.5) & (score > 6.5)] <- 7
  score[(score <= 8.5) & (score > 7.5)] <- 8
  score[(score <= 9.5) & (score > 8.5)] <- 9
  score[(score <= 10)  &  (score > 9.5)] <- 10
  
  df1["imdb_score_cat"] <- score
  
  ggplot(df1,aes(x=country,fill=factor(imdb_score_cat))) + geom_histogram(stat = "count")
  ## no country bias 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#year release of movie having 0 facebook likes
  dd <- df1 %>% select(title_year)  %>% filter(df1$movie_facebook_likes==0)
  hist(dd$title_year,breaks=200)
##i see that even older movies does not have zero likes
##movies with zero likes are from 1980 to till date
  
## lets check older movies and their facebook likes
  ttyy <- df1 %>% select(title_year,movie_facebook_likes)
  cat <- seq(1910,2020,10)
  ttyy["cat_year"] <- ttyy$title_year  #copy to new column
  
  ttyy$cat_year[ttyy$cat_year <=1920] <- 1920
  ttyy$cat_year[ttyy$cat_year <=1930 & ttyy$cat_year >1920] <- 1930
  ttyy$cat_year[ttyy$cat_year <=1940 & ttyy$cat_year >1930] <- 1940
  ttyy$cat_year[ttyy$cat_year <=1950 & ttyy$cat_year >1940] <- 1950
  ttyy$cat_year[ttyy$cat_year <=1960 & ttyy$cat_year >1950] <- 1960
  ttyy$cat_year[ttyy$cat_year <=1970 & ttyy$cat_year >1960] <- 1970
  ttyy$cat_year[ttyy$cat_year <=1980 & ttyy$cat_year >1970] <- 1980
  ttyy$cat_year[ttyy$cat_year <=1990 & ttyy$cat_year >1980] <- 1990
  ttyy$cat_year[ttyy$cat_year <=2000 & ttyy$cat_year >1990] <- 2000
  ttyy$cat_year[ttyy$cat_year <=2010 & ttyy$cat_year >2000] <- 2010
  ttyy$cat_year[ttyy$cat_year <=2020 & ttyy$cat_year >2010] <- 2020
  
  jj <- ttyy%>% group_by(cat_year) %>% summarise(value=sum(movie_facebook_likes))
  ggplot(jj,aes(x=cat_year,y=log(value))) + geom_point() + theme_bw()
  
# facebook likes gradully increasing depend upon time, 
# eventhough most of the movies with zero likes are newer
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  summary(df1$f)
 ## calculate profit and classify if a movei is a success or a failure 
  
## number of critique, number votes, number reviews versus imdb  
  library(corrplot)
  ss <- df1$director_facebook_likes 
   dd <- df1$num_critic_for_reviews
   ff <- df1$num_user_for_reviews
   numb <- cbind(ss,dd,ff)
   ss <- df1$num_voted_users
   numb <- cbind(ss,dd,ff,df1$imdb_score)
   numb <- as.data.frame(numb)
   colnames(numb)<- c("count_votes","count_critics","count_reviews","IMDB rating")
   colSums(is.na(numb))
   numb$count_reviews[which(is.na(numb$count_reviews))] <- mean(numb$count_reviews,na.rm=TRUE)
   correlation <- cor(numb,method="s")
   corrplot(correlation,method="pie")
## imdb does not seems to have strong corealtion with these three attribtes
## number of reviews for a movie have high corelation with nummber of votes it gets
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## number of faces
   barplot(df1$facenumber_in_poster)
    ## two outliers, lets delete them
   head(df1$facenumber_in_poster[order(df1$facenumber_in_poster,decreasing=TRUE)],3)
   t <- head(order(df1$facenumber_in_poster,decreasing=TRUE),2)
   df1$facenumber_in_poster[t] <- mean(df1$facenumber_in_poster)
   ggplot(df1,aes(x=facenumber_in_poster,fill=factor(imdb_score_cat))) + geom_histogram(stat="count")
   ## the imdb ratings spreads evenly for each type of facenumber in poster
   
   
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #lets check how many genres of each category do we have
   genres <- as.character(df1$genres)
    gen <- strsplit(genres,"|",fixed=TRUE)   
      gen <- unlist(gen)    
          table(gen)    
    #most movies are comedy, second most is action
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        
    # conert gross from dollar to million
    df1$gross <- df1$gross/1000000
    plot(df1$gross)   
    #impute median as missing value in gross value
    df1$gross[which(is.na(df1$gross))] <- median(df1$gross,na.rm=TRUE)
    g<- ggplot(df1,aes(y=df1$gross,x=seq(1,nrow(df1))))+ geom_point() # not very big outliers
    g + facet_wrap(~df1$imdb_score_cat) + stat_smooth(method="lm")
    ## line keeps become steeper from 5-to-9
    ## which means as the rating would the average number of movies in that rating is also increases
    c <- cor(df1$imdb_score_cat,df1$gross)
    c
    ## however rating itself has very week corealtion with imdb rating
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    #remove the whole coulmn that has single outlier in budget
    df1<- df1[-which.max(df$budget),]
    #also remove from likes and numb dataframe
    likes <- likes[-which.max(df$budget),]
    numb<- numb[-which.max(df$budget),]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
    plot(df1$director_facebook_likes) 
    abline(h=10000,col="red")
    ## see a distinction between director likes above and below 10000 
    ## let' see what makes them differ
    
    index <- which(df1$director_facebook_likes >10000)
    tenthou <- df1[index,]    
    tennot <- df1[-index,]    
    par(mfrow=c(1,2))    
    ## unable to find distinction for now

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
    ## density plot of imdb based on content rated
    temp1 <- c("Approved","G","Not Rated","PG","PG-13","R","Unrated")
    levels(df1$content_rating)[!(levels(df1$content_rating)%in%temp1)]<-"other"        
    levels(df1$content_rating)
    f <- ggplot(df1,aes(imdb_score_cat,color=content_rating,fill=content_rating))
    f + geom_density(alpha=0.5)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
  #text analysis
   # install.packages("tm")
    library(tm)
    keywords <- as.character(df1$plot_keywords)
    keywords <- gsub("|"," ",keywords,fixed=TRUE)
    txt <- Corpus(VectorSource(keywords))
    txt <- tm_map(txt,tolower)
    txt <- tm_map(txt, stripWhitespace)    
    txt <- tm_map(txt,removeWords,stopwords("en"))    
    dtm <- DocumentTermMatrix(txt)    
    dtm <- as.matrix(dtm)    
    