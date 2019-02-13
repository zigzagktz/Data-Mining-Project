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
df$gross[which(is.na(df$gross))] <- median(df$gross,na.rm=TRUE)

##removing color
df<- df[,-1]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#cleaning values end
  
  #taking unique names of actors 1, 2 and 3 and comparing their facbook likes
  par(mfrow=c(1,3))
  like1 <- df[!(duplicated(df$actor_1_name)),]
  boxplot(log(like1$actor_1_facebook_likes))
  
  
  like2 <- df[!(duplicated(df$actor_2_name)),]
  boxplot(log(like1$actor_2_facebook_likes))
  
  
  like3 <- df[!(duplicated(df$actor_3_name)),]
  boxplot(log(like1$actor_3_facebook_likes))
  
  dev.off()
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
  #we can remove the aspect ration now
  df <- df[,!(colnames(df) %in% c("aspect_ratio"))]
  
  
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
 
  
  ##after carefully analysing the data, we realised that india & japan does not have bdget values
  #in dollars, so we are going to remove those rows since they can potentially be all wrong info
  #as far as other small contries are conceerned, they are not too many and does not affect the data that much.
  sum(df$country== "India" | df$country== "Japan") # removing 55 more rows
  df <- df[-which(df$country=="India"|df$country=="Japan"),]
  
  
   ## no country bias 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
  
  
  #year vs movie facebook likes
  #year release of movie having 0 facebook likes
  dd <- df %>% select(title_year)  %>% filter(df$movie_facebook_likes==0)
  dd<- as.data.frame(dd)
  hist(dd$title_year,breaks=200)
  ##i see that even older movies does not have zero likes
  ##movies with zero likes are from 1980 to till date
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
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
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  # imdb vs votes, critic and user
  dd <- df$num_critic_for_reviews
  ff <- df$num_user_for_reviews
  ss <- df$num_voted_users
  numb <- cbind(ss,dd,ff,df$imdb_score)
  numb <- as.data.frame(numb)
  colnames(numb)<- c("count_votes","count_critics","count_reviews","IMDB rating")
  colSums(is.na(numb))
  correlation <- cor(numb,method="s")
  corrplot(correlation,method="pie")
  ## imdb does not seems to have strong corealtion with these three attribtes
  ## number of reviews for a movie have high corelation with nummber of votes it gets
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # imdb vs content rating
  ## density plot of imdb based on content rated
  temp1 <- c("Approved","G","Not Rated","PG","PG-13","R","Unrated")
  levels(df$content_rating)[!(levels(df$content_rating)%in%temp1)]<-"other"        
  levels(df$content_rating)
  f <- ggplot(df,aes(imdb_score_cat,color=content_rating,fill=content_rating))
  f + geom_density(alpha=0.5)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
  
  #imdb vs gross 
  g<- ggplot(df,aes(y=(df$gross),x=seq(1,nrow(df))))+ geom_point() # not very big outliers
  g + facet_wrap(~df$imdb_score_cat) + stat_smooth(method="lm")
  ## line keeps become steeper from 5-to-9
  ## which means as the rating would the average number of movies in that rating is also increases
  
  
  
  #adding prfit from gross and budget
  df["profit"] <- df[,colnames(df) %in% c("gross")] - df[,colnames(df) %in% c("budget")]
  
  
  # remove director facebook likes and movie facebook likes becuase of uncertainity
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
  #text analysis
  # install.packages("tm")
  library(tm)
  library(class)
  keywords <- as.character(df$plot_keywords)
  keywords <- gsub("|"," ",keywords,fixed=TRUE)
  txt <- Corpus(VectorSource(keywords))
  txt <- tm_map(txt,tolower)
  txt <- tm_map(txt, stripWhitespace)    
  txt <- tm_map(txt,removeWords,stopwords("en"))    
  dtm <- DocumentTermMatrix(txt)    
  dtm <- as.matrix(dtm)    
  cs <- colSums(dtm)
  which.max(cs)
  dtm <- as.data.frame(dtm)
  
  classes <- df$imdb_score_cat
  classes <- ifelse(classes<=7,"7_or_lower","8_or_9")
  
  
  combined <- cbind(dtm,classes)  
  combined <- as.data.frame(combined)
  
  set.seed(123)
  combined$classes <- as.character(combined$classes)
  ran <- sample(nrow(combined),.9*nrow(combined))
  dtm.train <- combined[ran,]
  dtm.test <- combined[-ran,]
  
  dtm.train.value <- dtm.train[,1:(ncol(dtm.train)-1)]
  dtm.test.value <- dtm.test[,1:(ncol(dtm.test)-1)]
  
  a <- dtm.train.value
  b<- dtm.test.value
  c <- dtm.train$classes
  
  res <- knn(a,b,c)
  t <- table(res,dtm.test$classes)
  t
  (sum(diag(t))/sum(t))*100
  
  
  
  

 