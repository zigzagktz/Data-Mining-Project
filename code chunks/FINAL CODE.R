library(ggplot2)
library(dplyr)
library(stringr)
library(corrplot)
library(data.table)
library(randomForest)
library(plotly)
library(caret)
library(tibble)
library(tidyr)
library("gridExtra")
library(randomForest)
library(data.table)
library(tm)
library(class)



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
  g <- ggplot(df,aes(x=imdb_score,fill=factor(aspect_ratio))) 
    g <- g+geom_histogram(stat="count") 
     g <- g+ggtitle("Aspect Ratio Vs Imdb score") 
      g <- g+theme(legend.key.size =  unit(0.1, "in")) 
        g <- g+scale_fill_discrete(name = "Aspect Ratio") 
          g <- g+theme(axis.text.x = element_text(angle = 90))  
           g <- g+ xlab("IMDB Ratings") + ylab("Count of Movies")
           g
    #we can remove the aspect ration now
  df <- df[,!(colnames(df) %in% c("aspect_ratio"))]
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #taking top expensive movies out and compairng against screen ratio
  expensive <- budget[head(order(budget,decreasing=TRUE),400)]
  barplot(expensive,xlim = c(1,80)) ##I see only a few movies are too expensive
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
  
  gg <- ggplot(df,aes(x=country,fill=factor(imdb_score_cat))) 
  gg <- gg + geom_histogram(stat = "count") 
  gg <- gg +theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  gg <- gg + scale_fill_discrete(name = "Aspect Ratio")
  gg <- gg + ggtitle("Movies by country")
  gg
  ##after carefully analysing the data, we realised that india & japan does not have bdget values
  #in dollars, so we are going to remove those rows since they can potentially be all wrong info
  #as far as other small contries are conceerned, they are not too many and does not affect the data that much.
  sum(df$country== "India" | df$country== "Japan" ) # removing 55 more rows
  df <- df[-which(df$country=="India"|df$country=="Japan"),]
  
  
   ## no country bias 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
  
  
  #year vs movie facebook likes
  #year release of movie having 0 facebook likes
  dd <- df %>% select(title_year)  %>% filter(df$movie_facebook_likes==0)
  dd<- as.data.frame(dd)
  hist(dd$title_year,breaks=200,main="Movies with Zero Facebook Likes",xlab="Year Released")
  ##i see that even older movies does not have zero likes
  ##movies with zero likes are from 1980 to till date
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## lets check older movies and their facebook likes
  ttyy <- df %>% select(title_year,movie_facebook_likes)
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
  g <- g+ facet_wrap(~df$imdb_score_cat) + stat_smooth(method="lm")
  g <- g+ xlab("Count of movie") + ylab("Gross Dollars")
  g
  ## line keeps become steeper from 5-to-9
  ## which means as the rating would the average number of movies in that rating is also increases
  
  
  
  #adding prfit from gross and budget
  df["profit"] <- df[,colnames(df) %in% c("gross")] - df[,colnames(df) %in% c("budget")]
  
  
  # remove director facebook likes and movie facebook likes becuase of uncertainity
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
 par(mfrow=c(1,2))
   plot(df$director_facebook_likes,ylab="Director Facebook Likes",xlab="Count") 
  abline(h=10000,col="red")
  plot(log(df$movie_facebook_likes),ylab="Movie Facebook Likes",xlab="Movie Count") 
  abline(h=8.9,col="red")
  abline(h=7.12,col="red")
  dev.off()
  del <- c("director_facebook_likes","movie_facebook_likes")  
  df <- df[,!(colnames(df) %in% del)]
  ## see a distinction between director likes above and below 10000 
  ## let' see what makes them differ
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
  #text analysis
  # install.packages("tm")
  
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
  
     #text analysis ends
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
   # randomforest starts
  
  df1<- df
  #Create the test and training data set with 75:25 ratio
  set.seed(12345)
  row.number <- sample(x = 1:nrow(df1), size = 0.75 * nrow(df1))
  train <- df1[row.number, ]
  test <- df1[-row.number, ]
  
  
  #Linear model
  
  fit <- lm(imdb_score ~ num_critic_for_reviews+duration+gross+num_voted_users
            +cast_total_facebook_likes+num_user_for_reviews,
            data = train)
  
  summary(fit)    #R-sq: 0.27
  
  # num <- cbind(dta1$imdb_score, dta1$num_critic_for_reviews, dta1$duration, dta1$gross
  #              , dta1$num_voted_users, dta1$cast_total_facebook_likes
  #              , dta1$num_user_for_reviews, dta1$budget, dta1$movie_facebook_likes)
  # corMatrix <- cor(num)
  # corrplot(corMatrix)
  
  #Random Forest:
  
  df1$binned_score <- cut(df1$imdb_score, breaks = c(0,4,6,8,10))
  train$binned_score <- cut(train$imdb_score, breaks = c(0,4,8,10))
  test$binned_score <- cut(test$imdb_score, breaks = c(0,4,8,10))
  
  #random forest model with training data
  model_train <- randomForest(binned_score ~ num_critic_for_reviews+duration+gross+num_voted_users
                              +cast_total_facebook_likes+num_user_for_reviews+budget
                              , data = train, ntree = 500, mtry = 5 
                              , importance = TRUE, na.action = na.roughfix,
                              proximity = TRUE)
  
  
  model_train      #error rate: 6.33%
  
  # Variable importance plot
  varImpPlot(model_train, main = 'Variable Importance Plot For Random Forest')
  
  
  #Normal Plot
  # plot(model_train)
  # legend('topright', colnames(model_train$err.rate), col = 1:5, fill = 1:5)
  
  
  # Get OOB data from plot and coerce to data.table
  oobData = as.data.table(plot(model_train))
  oobData
  
  # Define trees as 1:ntree
  oobData[, trees := .I]
  
  # Cast to long format
  oobData2 = melt(oobData, id.vars = "trees")
  setnames(oobData2, "value" ,"error")
  
  # Plot using ggplot
  ggplot(data = oobData2, aes(x = trees, y = error, color = variable)) + geom_line( size = 1.5)+
    ggtitle("Error Rates of Random Forest Model By Binned IMDB Score")+
    xlab("Number of Trees")+
    ylab("Error Rate of Each IMDB Score Bin")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # Variables that span between single digits and millions:  make them (log() + .1)---  log (0) = NAN
 par(mfrow=c(2,3))
   df1$logGross<-log(df1$gross + .1)
  hist(df1$logGross,main="Log Gross",xlab="Gross")
  df1$logBudget<-log(df1$budget + .1)
  hist(df1$logBudget,main="Log Budget",xlab="Budget")
  df1$num_user_for_reviews   <- log(df1$num_user_for_reviews + .1)
  hist(df1$num_user_for_reviews)
  df1$num_voted_users <- log(df1$num_voted_users + .1)
  hist(df1$num_voted_users,main="Number of Voted users",xlab="Number" ) 
  df1$cast_total_facebook_likes <- log(df1$cast_total_facebook_likes + .1)
  hist(df1$cast_total_facebook_likes,main="Full cast FB likes",xlab="count")
  log(.1)     # = -2.3
  
  
  
  
  # FIRST, Create numdf1 which has only the numerical variables of df1
  
  
  numdf1 <- dplyr::select_if(df1, is.numeric)
  # numdf1 <- as.matrix(numdf1)
  
  colnames(numdf1)
  
  
  
  # Now try Random Forest for Log(Gross).   These values have been binned before the Train/Test split
  # Now try Random Forest for Log(Gross).   These values have been binned before the Train/Test split
  
  
  # Create Bins of movies by LogGross
  numdf1$binned_LogGross <- cut(numdf1$logGross, breaks = c(0,10,12,14,16,18,20))
  summary(numdf1$binned_LogGross)
  
  
  
  ##   Double check if i want to do this.
  colSums(is.na(numdf1))   # Yes- only lose 5 rows
  
  numdf1 <- na.omit(numdf1)
  dim(numdf1)     #  Lost 5 rows of NAs in binned_LogGross 3773 to 3768
  
  
  
  # Take out Gross because it dominates the binned LogGross, and the Log Gross Random Forest Models
  # Take out the Log or the actual number to avoid duplication of variables
  # Create new df called numdf3 that will contain the most relevant variables to put into the Random Forest Model
  # to predict binned IMDB Scores
  numdf4<- numdf1
  numdf4 <- subset(numdf4, select = -c(facenumber_in_poster))
  numdf4 <- subset(numdf4, select = -c(num_voted_users))
  numdf4 <- subset(numdf4, select = -c(num_user_for_reviews))
  numdf4 <- subset(numdf4, select = -c(gross))
  numdf4 <- subset(numdf4, select = -c(budget))
  numdf4 <- subset(numdf4, select = -c(logGross))
  numdf4 <- subset(numdf4, select = -c(duration))
  numdf4 <- subset(numdf4, select = -c(actor_1_facebook_likes))
  numdf4 <- subset(numdf4, select = -c(cast_total_facebook_likes))
  
  
  
  
  
  # Check the columns that are left and going into the Random Forest Model for Log Gross Sales
  colnames(numdf4)
  
  
  #  Create Train and Test Datasets
  library(caTools)
  set.seed(200)
  
  split = sample.split(numdf4$binned_LogGross, SplitRatio = 0.75)
  train1= subset(numdf4, split == TRUE)
  test1 = subset(numdf4, split==FALSE)
  
  head(train1)
  head(test1)
  
  str(train1)
  summary(train1)
  dim(train1)
  str(test1)
  dim(test1)
  
  
  # Double check the columns left and that there are 0 NAs in each column
  colnames(numdf4)
  #  complete.cases(numdf4) 
  colSums(is.na(numdf4))
  
  library(ggplot2) # visualization
  library("gridExtra")
  library(randomForest)
  library(data.table)
  
  
  #mtry is number of Variables randomly chosen at each split, importance is so we can graph how important each variable is
  mtry <- c(1:5)
  rfGross=randomForest(binned_LogGross ~ ., data = numdf4, mtry=mtry,ntree=400, importance = TRUE) 
  rfGross
  
  
  predGross<-predict(rfGross,test1) #Predictions on Test Set for each Tree
  t2<- table(predGross, test1$binned_LogGross)
  grid.table(t2)    
  #  Accuracy = 98%
  
  
  # Plot importance of variables 
  varImpPlot(rfGross)
  imp = as.data.frame(importance(rfGross))
  imp
  
  
  
  
  # Show model error
  plot(rfGross)
  
  
  # Get OOB data from plot and coerce to data.table
  oobData = as.data.table(plot(rfGross))
  oobData
  
  # Define trees as 1:ntree
  oobData[, ntree := .I]
  
  
  
  
  
  # Get OOB data from plot and coerce to data.table
  oobData = as.data.table(plot(rfGross))
  oobData
  
  # Define trees as 1:ntree
  oobData[, ntree := .I]
  
  # Cast to long format
  oobData2 = melt(oobData, id.vars = "ntree")
  setnames(oobData2, "value", "error")
  
  # Plot using ggplot
  ggplot(data = oobData2, aes(x = ntree, y = error, color = variable)) + geom_line( size = 1.5)+
    ggtitle("Error Rates of Random Forest Model By Binned Log(Gross Sales)")+
    xlab("Number of Trees")+
    ylab("Error Rate of Each Log(Gross Sales) Bin")
  
  #_____________________________________________________________________________________________________________________
  #   WHOLE CODE TO CREATE CORRELATION MATRICESS              Lines 183 to 275
  #          1.  CORRELATION MATRIX OF ORIGINAL DATA
  #          2.  CORRELATION MATRIX INCLUDING LOG DATA
  
  #  Extract only fields with numeric items
  numdf11 <- dplyr::select_if(df1, is.numeric)
  # numdf1 <- as.matrix(numdf1)
  
  # To simplify correlation matrix, take out fields that are not very correlated---for presentation exhibit
  
  
  numdf11 <- subset(numdf11, select = -c(actor_2_facebook_likes))
  numdf11 <- subset(numdf11, select = -c(actor_3_facebook_likes))
  numdf11 <- subset(numdf11, select = -c(facenumber_in_poster))
  colnames(numdf11)
  
  # Now create simplified correlation matrix
  cor_mat <- cor(numdf11)
  cor_mat
  dim(cor_mat)
  corrplot(cor_mat)
  
  library(tibble)
  library(dplyr)
  library(tidyr)
  
  
  numdf21 <- numdf1[,1:17] %>% 
    as.matrix %>%
    cor %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1)
  
  colnames(numdf11)
  colnames(numdf21)
  
  
  # find the  0.5 <values< 1.000
  filter(numdf21, value > .5 & value<1)
  filter(numdf21, value > -1 & value< -.5)    #  No large negative correlations
  filter(numdf21, value > .3 & value<1)
  filter(numdf21, value > -1 & value< -.3)    #  No weak negative correlations (other than x)
  
  
  
  #  Here's the code for the full correlation matrix of the original variables
  #  Extract only fields with numeric items
  numdf1 <- dplyr::select_if(df1, is.numeric)
  # numdf1 <- as.matrix(numdf1)
  
  colnames(numdf1)
  
  
  

  
  numdf2 <- numdf1 %>% 
    as.matrix %>%
    cor %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1)
  
  colnames(numdf1)
  colnames(numdf2)
  
  
  # find the  0.5 <values< 1.000
  filter(numdf2, value > .5 & value<1)
  filter(numdf2, value > -1 & value< -.5)    #  No large negative correlations
  filter(numdf2, value > .3 & value<1)
  filter(numdf2, value > -1 & value< -.3)    #  No weak negative correlations (other than x)
  
  
  
  #  Correlation Findings:  These variables are correlated to each other.   They have correlations > 0.5:
  # num_voted_users 
  # num_user_for_reviews
  # num_critic_for_reviews
  # movie_facebook_likes
  # cast_total_facebook_likes
  # gross 
  #
  #  Findings:  These variables are correlated to each other.   For correlations > 0.3 add to the list above:
  # duration
  # imdb_score
  
  
  #   ______________________________________________________________________________________________________________
  #   DATA EXPLORATION HISTOGRAMS AND PLOTS                     Lines 280 to 321
  #  Findings:  These variables are correlated to each other.   For correlations > 0.3 add to the list above:
  # duration
  # imdb_score
  
  
  hist(df1$num_critic_for_reviews)
  hist(df1$num_user_for_reviews)
  hist(df1$num_voted_users)
  hist(df1$duration)
  hist(df1$gross)
  hist(df1$facenumber_in_poster)
  hist(df1$budget)
  max(df1$budget)
  
  hist(df1$title_year, xlim = c(1980,2016), breaks = 21)
  
  hist(df1$imdb_score)
  #  df1$imdb_score_cat <- as.numeric(df1$imdb_score_cat)
  #  hist(df1$imdb_score_cat)      Don't know why these two don't work
  
  # Variables that span between single digits and millions:  make them (log() + .1)---  log (0) = NAN
  df1$logGross<-log(df1$gross + .1)
  hist(df1$logGross)
  
  df1$logBudget<-log(df1$budget + .1)
  hist(df1$logBudget)
  
  df1$logNum_user_for_reviews   <- log(df1$num_user_for_reviews + .1)
  hist(df1$logNum_user_for_reviews)
  
  df1$logNum_voted_users <- log(df1$num_voted_users + .1)
  hist(df1$logNum_voted_users ) 
  
  df1$LogCast_total_facebook_likes <- log(df1$cast_total_facebook_likes + .1)
  hist(df1$LogCast_total_facebook_likes)
  
  log(.1)     # = -2.3
  
  # *****  REPEAT CORRELATION MATRIX WHICH NOW INCLUDES LOG VALUES ****************
  #  Extract only fields with numeric items
  numdf1 <- dplyr::select_if(df1, is.numeric)
  
  
  
  plot (df1$num_critic_for_reviews , df1$num_user_for_reviews  )
  plot (df1$num_critic_for_reviews , df1$num_voted_users  )
  plot (df1$num_critic_for_reviews , df1$movie_facebook_likes  )
  plot (df1$num_critic_for_reviews , df1$imdb_score )
  plot (df1$num_critic_for_reviews , df1$duration)
  
  
  plot (df1$logBudget , df1$logGross  )
  plot (df1$logNum_user_for_reviews , df1$logNum_voted_users  )
  plot (df1$num_critic_for_reviews, df1$logNum_user_for_reviews )
  plot (df1$logGross , df1$logNum_user_for_reviews  )
  plot (df1$title_year , df1$logGross  )
  
  #_________________________________________________________________________________________________________________
  # Experiment with different linear regression models for the df data frame    Lines 347-470
  
  
  # Really intersting!!!  Predicting Gross makes a significantly better model than logGross.
  #  Best linear model R2 is 0.61 to predict Gross Sales
  # Strongest predictors are
  #     num_voted_users
  #     num_user_for_reviews  
  #     actor_1_facebook_likes 
  #     actor_2_facebook_likes
  #     actor_1_facebook_likes 
  #     num_critic_for_reviews 
  
  # numdf1 <- subset(numdf1, select = -c(logGross))
  model1 <-  lm(gross ~ ., data = numdf1)
  summary(model1)            # R2  = 0.61
  
  # Strongest predictors of Gross Sales are
  #     num_voted_users
  #     cast_total_facebook_likes
  #     actor_1_facebook_likes
  #     actor_2_facebook_likes 
  #     num_user_for_reviews 
  #     num_critic_for_reviews 
  #     actor_3_facebook_likes 
  #     duration
  
  #    
  #     
  
  model3 <-  lm(imdb_score ~ ., data = numdf1)
  summary(model3)            
  #  R2 = .40  without Log..... BUT R2 = .44 with Log values    BEST SO FAR for IMDB score
  # This is based on all numeric columns.   
  
  # Strongest predictors of IMDB score are
  #     num_voted_users
  #     num_user_for_reviews 
  #     duration
  #     num_critic_for_reviews 
  #     actor_1_facebook_likes 
  
  
  
  model1 <-  lm(logGross ~ ., data = numdf1)
  summary(model1)            # R2  = 0.50
  
  model1 <-  lm(gross ~ imdb_score, data = numdf1)
  summary(model1)            # R2  = 0.04
  
  
  model1 <-  lm(gross ~ num_critic_for_reviews, data = numdf1)
  summary(model1)            #  R2 = .23
  
  model1 <-  lm(logGross ~ num_critic_for_reviews, data = numdf1)
  summary(model1)            #  R2 = .11
  
  #  Now we will continue looking at Gross Sales
  
  
  model1 <-  lm(gross ~ num_critic_for_reviews + num_user_for_reviews, data = numdf1)
  summary(model1)            #  R2 = .35  Better
  
  model1 <-  lm(gross ~ num_critic_for_reviews + budget , data = numdf1)
  summary(model1)            #  R2 = .23  Budget adds nothing to R2
  
  
  # Linear model for IMDB score
  model3 <-  lm(imdb_score ~ num_critic_for_reviews, data = numdf1)
  summary(model3)            #  R2 = .11
  
  model3 <-  lm(imdb_score ~ numdf1$num_user_for_reviews, data = numdf1)
  summary(model3)            #  R2 = .09
  
  model3 <-  lm(imdb_score ~ logNum_user_for_reviews , data = numdf1)
  summary(model3)            #  R2 = .11  *****  Log is a little better than straight, but not much
  
  model3 <-  lm(imdb_score ~ num_user_for_reviews + gross, data = numdf1)
  summary(model3)            #  R2 = .09   Gross adds nothing to model
  
  model3 <-  lm(imdb_score ~ logNum_user_for_reviews + logGross, data = numdf1)
  summary(model3)            #  R2 = .11  *****  Log is a little better than straight, but not much
  
  model3 <-  lm(imdb_score ~ num_voted_users, data = numdf1)
  summary(model3)            #  R2 = .185
  
  model3 <-  lm(imdb_score ~ logNum_voted_users + logGross+ num_critic_for_reviews+ duration, data = numdf1)
  summary(model3)            #  R2 = .22   Adding a few more variables doesn't add much to R2
  
  model3 <-  lm(imdb_score ~ num_critic_for_reviews, data = numdf1)
  summary(model3)            #  R2 = .11
  
  model3 <-  lm(imdb_score ~ num_critic_for_reviews, data = numdf1)
  summary(model3)            #  R2 = .11
  
  
  
  model3 = lm(imdb_score ~ duration + logNum_voted_users + logBudget + title_year + num_critic_for_reviews + LogCast_total_facebook_likes, data = numdf1)
  summary(model3)            # R2 = .31
  
  model2 <- lm(logGross ~ logNum_user_for_reviews, data = numdf1)
  summary(model2)            #  R2 = .17
  
  model2 <- lm(logGross ~ logNum_user_for_reviews + num_critic_for_reviews, data = numdf1)
  summary(model2)            #  R2 = .18    #  Critic Reviews Don't Matter
  
  model2 <- lm(logGross ~ logNum_user_for_reviews + logBudget, data = numdf1)
  summary(model2)            #  R2 = .23
  
  model2 <- lm(logGross ~ logNum_user_for_reviews + logBudget + logNum_voted_users, data = numdf1)
  summary(model2)            #  R2 = .24
  
  model2 <- lm(logGross ~ num_critic_for_reviews, data = numdf1)
  summary(model2)            #  R2 = .11  Not much
  
  model2 <- lm(logGross ~ budget, data = numdf1)
  summary(model2)            #  R2 = .003   Have to plot logGross vs logBudget
  
  
  

 