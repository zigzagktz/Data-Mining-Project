getwd()
setwd("~/Data Mining Appns ALY 6040")
getwd()


# Load packages
library(ggplot2) # visualization
library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(VIM)
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(car)
library("Hmisc")

imdb <- read.csv("movie_metadata.csv")
str(imdb)



# Check for duplicates in cleaned data
# df <- read.csv("data frame.csv")
  df <- read.csv("movie1.csv")
sum(duplicated(df))

str(df)
summary(df)

# How many columns have no NAs
dfna <-df[ , colSums(is.na(df)) == 0]
dim(df)

# check for columns with NAs
#  gross has 779, budget has 392 NAs  ,  also 13 in facenumber_in_poster
colSums(is.na(df))

# Keep only rows with no NAs = 3810---so Remove rows with any NAs 5000 down to 3810 rows
df <- na.omit(df)
dim(df)


#  Extract only fields with numeric items
numdf <- dplyr::select_if(df, is.numeric)
# numdf <- as.matrix(numdf)


cor_mat <- cor(numdf)
cor_mat
dim(cor_mat)
corrplot(cor_mat)

library(tibble)
library(dplyr)
library(tidyr)


numdf2 <- numdf %>% 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)

colnames(numdf)
colnames(numdf2)


# find the  0.5 <values< 1.000
filter(numdf2, value > .5 & value<1)
filter(numdf2, value > -1 & value< -.5)    #  No large negative correlations
filter(numdf2, value > .3 & value<1)
filter(numdf2, value > -1 & value< -.3)    #  No weak negative correlations (other than x)









#  Findings:  These variables are correlated to each other.   For correlations > 0.5:
# num_voted_users 
# num_critic_for_reviews
# movie_facebook_likes
# imdb_score 
#
#  Findings:  These variables are correlated to each other.   For correlations > 0.3 add to the list above:
# duration
#


hist(df$num_critic_for_reviews)
hist(df$num_user_for_reviews)
hist(df$num_voted_users)
hist(df$duration)
hist(df$gross)
hist(df$director_facebook_likes)
hist(df$facenumber_in_poster)
hist(df$budget)

hist(df$title_year, xlim = c(1980,2016), breaks = 21)

hist(df$imdb_score)
hist(df$imdb_score_cat)

# Variables that span between single digits and millions:  make them (log() + .1)---  log (0) = NAN
df$logGross<-log(df$gross + .1)
hist(df$logGross)
df$logBudget<-log(df$budget + .1)
hist(df$logBudget)
df$logNum_user_for_reviews   <- log(df$num_user_for_reviews + .1)
hist(df$logNum_user_for_reviews)
df$logNum_voted_users <- log(df$num_voted_users + .1)
hist(df$logNum_voted_users ) 
df$logMovie_facebook_likes <-log(df$movie_facebook_likes + .1)
hist(df$logMovie_facebook_likes)    # there are about 1700 with 0 movie FB likes
df$logNum_critic_for_reviews <-(df$num_critic_for_reviews + .1)
hist(df$logNum_critic_for_reviews)
df$LogCast_total_facebook_likes <- log(df$cast_total_facebook_likes + .1)
hist(df$LogCast_total_facebook_likes)
log(.1)     # = -2.3

# *****  REPEAT CORRELATION MATRIX WHICH NOW INCLUDES LOG VALUES ****************
#  Extract only fields with numeric items
numdf <- dplyr::select_if(df, is.numeric)
# numdf <- as.matrix(numdf)  if needed


plot (df$num_critic_for_reviews , df$num_user_for_reviews  )
plot (df$num_critic_for_reviews , df$num_voted_users  )
plot (df$num_critic_for_reviews , df$movie_facebook_likes  )
plot (df$num_critic_for_reviews , df$imdb_score )
plot (df$num_critic_for_reviews , df$duration)


plot (df$logBudget , df$logGross  )
plot (df$logMovie_facebook_likes , df$logNum_user_for_reviews  )
plot (df$logMovie_facebook_likes, df$logNum_voted_users)
plot (df$logNum_user_for_reviews , df$logNum_voted_users  )
plot (df$num_critic_for_reviews, df$logNum_user_for_reviews )
plot (df$logGross , df$logNum_user_for_reviews  )
plot (df$title_year , df$logGross  )


# Experiment with different linear regression models for the df data frame

model1 <-  lm(gross ~ ., data = numdf)
summary(model1)            # R2  = 0.61

model1 <-  lm(logGross ~ ., data = numdf)
summary(model1)            # R2  = 0.63

model1 <-  lm(gross ~ imdb_score, data = numdf)
summary(model1)            # R2  = 0.04

model1 <-  lm(logGross ~ logMovie_facebook_likes, data = numdf)
summary(model1)            # R2  = 0.13

model1 <-  lm(gross ~ num_critic_for_reviews, data = df)
summary(model1)            #  R2 = .23

model1 <-  lm(gross ~ num_critic_for_reviews + movie_facebook_likes, data = df)
summary(model1)            #  R2 = .234    No difference from modeld above- due to collinarity?

model1 <-  lm(gross ~ num_critic_for_reviews + num_user_for_reviews, data = df)
summary(model1)            #  R2 = .35  Better

model1 <-  lm(gross ~ num_critic_for_reviews + num_user_for_reviews +, data = df)
summary(model1)            #  R2 = .23


# Linear model for IMDB score
model2 <-  lm(df$imdb_score ~ num_critic_for_reviews, data = df)
summary(model2)            #  R2 = .11

model3 <-  lm(df$imdb_score ~ num_critic_for_reviews, data = df)
summary(model3)            #  R2 = .11

model3 <-  lm(imdb_score ~ logNum_user_for_reviews + logGross, data = df)
summary(model3)            #  R2 = .13  *****

model3 <-  lm(imdb_score ~ logNum_voted_users + logGross+ num_critic_for_reviews+ duration, data = df)
summary(model3)            #  R2 = .31    # Probably the best one...but it's pretty poor

model3 <-  lm(df$imdb_score ~ num_critic_for_reviews, data = df)
summary(model3)            #  R2 = .11

model3 <-  lm(df$imdb_score ~ num_critic_for_reviews, data = df)
summary(model3)            #  R2 = .11

model3 <-  lm(imdb_score ~ ., data = numdf)
summary(model3)            #  R2 = .35  without Log..... BUT R2 = .45 with Log values    BEST SO FAR
# This is based on all numeric columns.  Try removing director FB, movieFB, aspect and budget  

model3 = lm(numdf$imdb_score ~ duration + logNum_voted_users + logBudget + title_year + num_critic_for_reviews + df$LogCast_total_facebook_likes, data = numdf)
summary(model3)            # R2 = .36

model2 <- lm(df$logGross ~ df$logNum_user_for_reviews, data = df)
summary(model2)            #  R2 = .34

model2 <- lm(df$logGross ~ df$logNum_user_for_reviews + df$num_critic_for_reviews, data = df)
summary(model2)            #  R2 = .34    #  Critic Reviews Don't Matter

model2 <- lm(df$logGross ~ df$logNum_user_for_reviews + df$logBudget, data = df)
summary(model2)            #  R2 = .48

model2 <- lm(df$logGross ~ df$logNum_user_for_reviews + df$logBudget + df$logNum_voted_users, data = df)
summary(model2)            #  R2 = .51

model2 <- lm(df$logGross ~ df$logMovie_facebook_likes, data = df)
summary(model2)            #  R2 = .0019   Nope

model2 <- lm(df$logGross ~ df$num_critic_for_reviews, data = df)
summary(model2)            #  R2 = .15  Not much

model2 <- lm(df$logGross ~ df$budget, data = df)
summary(model2)            #  R2 = .002   Have to plot logGross vs logBudget

# __________________________________________________________________________________
# Text mine the plot keywords

library("tm")  
library("dplyr")
library("class") 
libs <- c("tm", "dplyr", "class")
lapply(libs, require, character.only = TRUE)  

# Force characters to be categorized as characters
options(stringsAsFactors = FALSE)

#  need to continue this example- it's a youtube video on text mining matrix of obama and romney speeches


# _________________________________________________________________________________
# Now Try random forest, knn and decision tree

library(randomForest)

# Create Bins of movies Poor, Fair, Good, Exellent
numdf$binned_score <- cut(numdf$imdb_score, breaks = c(0,4,6,8,10))

# Create Bins of movies by LogGross
numdf$binned_LogGross <- cut(numdf$logGross, breaks = c(0,10,14,15,16,17,18,19,20))
hist(numdf$logGross)
summary(numdf$binned_LogGross)
numdf <- na.omit(numdf)
dim(numdf)     #  Lost 5 rows of NAs in binned_LogGross 3773 to 3768

# Take out IMDB SCore because it dominates the binned score and the Log Gross Random Forest Models
# Take out the Log or the actual number to avoid duplication of variables
numdf <- subset(numdf, select = -c(imdb_score))
numdf <- subset(numdf, select = -c(num_user_for_reviews))
numdf <- subset(numdf, select = -c(num_voted_users))
numdf <- subset(numdf, select = -c(gross))
numdf <- subset(numdf, select = -c(budget))
numdf <- subset(numdf, select = -c(movie_facebook_likes))
numdf <- subset(numdf, select = -c(logNum_critic_for_reviews))
numdf <- subset(numdf, select = -c(aspect_ratio))
numdf <- subset(numdf, select = -c(cast_total_facebook_likes))
numdf <- subset(numdf, select = -c(facenumber_in_poster))




#  Create Train and Test Datasets
library(caTools)
set.seed(200)

split = sample.split(numdf$binned_score, SplitRatio = 0.75)
train1= subset(numdf, split == TRUE)
test1 = subset(numdf, split==FALSE)

head(train1)
head(test1)

str(train1)
summary(train1)
dim(train1)
str(test1)
dim(test1)

colnames(numdf)
complete.cases(numdf) 
colSums(is.na(numdf))

# Remove the binned_score if needed
# numdf <- subset(numdf, select = -c(binned_score))


#  Now run Random Forest on the Binned IMDB scores
#mtry is number of Variables randomly chosen at each split
mtry <- c(1:13)
  rf=randomForest(binned_score ~ ., data = train1, mtry=mtry,ntree=400, importance = TRUE) 
  rf

  pred<-predict(rf,test1) #Predictions on Test Set for each Tree
  
  confusionMatrix(pred, test1$binned_score)    #  Accuracy if over 77%  
  
  # Get importance and plot it
  varImpPlot(rf)
  imp = as.data.frame(importance(rf))
  #  This plot shows that log(the big numbers) is very close to the actual number- no need to have both- pick one
  
 # Create Out of Bag Errors and Test Errors
   oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  test.err[mtry]= with(test1, mean( (binned_score- pred)^2)) #Mean Squared Test Error
  
#  Look at test.err  and Out of Bag Error Estimation
  test.err
  oob.err
  
# Show model error
plot(rf)


# _______________________________________________________________________________________________________________
# Now try Random Forest for Log(Gross).   These values have been binned before the Train/Test split


#mtry is number of Variables randomly chosen at each split
mtry <- c(1:13)
rfGross=randomForest(binned_LogGross ~ ., data = train1, mtry=mtry,ntree=400, importance = TRUE) 
rfGross

predGross<-predict(rfGross,test1) #Predictions on Test Set for each Tree

confusionMatrix(predGross, test1$binned_LogGross)    
#  Accuracy = 83%  !!!

# Get importance and plot it
varImpPlot(rfGross)
imp = as.data.frame(importance(rf))
#  This plot shows that log(the big numbers) is very close to the actual number- no need to take logs

# Create Out of Bag Errors and Test Errors
oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
test.err[mtry]= with(test1, mean( (binned_score- pred)^2)) #Mean Squared Test Error

#  Look at test.err  and Out of Bag Error Estimation
test.err
oob.err

# Show model error
plot(rf)



# Compare Out of Bag Sample Erros and Error on Test Set
oob.err=double(22)
test.err=double(22)


rf=randomForest(imdb_score ~ ., data = train1,ntree=400, importance = TRUE) 
importance <- importance(rf)
oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
set.seed(633)
# apply model on test set
rf.pred.test <- predict(rf, test)
# generate confusion matrix for test data
confusionMatrix(rf.pred.test, test1$imdb_score)

# Get importance
varImpPlot(rf)
imp = as.data.frame(importance(rf))

#  Next part doesn't work
 imp <- imp[, ncol(imp) - 1]
rf.imdbscore <- names(imp)[order(imp, decreasing = T)[1:30]]
rf.imdbscore

# ________________________________________________________________________
# Try Classification Trees modeling

library("caTools")
set.seed(200)
# Web3$Transactions<- as.numeric(Web3$Transactions)
split = sample.split(numdf$imdb_score, SplitRatio = 0.75)
train1= subset(numdf, split == TRUE)
test1 = subset(numdf, split==FALSE)

head(train1)
dim(train1)
head(test1)
dim(test1)

str(train1)
str(test1)
class.tree <- rpart(imdb_score ~ . , data = train1, method = "anova")

## plot tree using different formats
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0) 
prp(class.tree)
rpart.plot(class.tree, type = 3, fallen.leaves = TRUE)
rpart.plot(class.tree, type = 4, extra = 101)

# prune by lowest cp
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

# apply model on training set
tree.pred.train <- predict(pruned.ct, train1, type = "anova")
# generate confusion matrix for training data
confusionMatrix(tree.pred.train, train$binned_score)



# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
set.seed(51)
cv.ct <- rpart(binned_score ~ . -imdb_score, data = train, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct)

# prune by lowest cp
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

# apply model on training set
tree.pred.train <- predict(pruned.ct, train1, type = "anova")
# generate confusion matrix for training data
confusionMatrix(tree.pred.train, train1$imdb_score)



# _________________________________________________________________________________
# Experiment with Correlations

#  Correlation of Gross Sales/1M to other numeric variables
cor.test(df$gross, df$num_critic_for_reviews)                 # .48
cor.test(df$gross, df$budget)                                 # .10
cor.test(df$gross, df$duration)                               # .254
cor.test(df$gross, df$director_facebook_likes)                # .145         
cor.test(df$gross, df$gross)                                  # 1.0
cor.test(df$gross, df$num_voted_users)                        # .
cor.test(df$gross, df$cast_total_facebook_likes)              # .247
cor.test(df$gross, df$facenumber_in_poster)                   # -.03
cor.test(df$gross, df$num_user_for_reviews)                   # .56   *
cor.test(df$gross, df$title_year)                             # .03
cor.test(df$gross, df$imdb_score)                             # .20
cor.test(df$gross, df$movie_facebook_likes)                   # .374  *

# Correlation of some variables to each other, looking for collinearity
cor.test(df$num_user_for_reviews, df$num_critic_for_reviews)                 # .606  *
cor.test(df$num_user_for_reviews, df$num_voted_users)                        # .798  *
cor.test(df$num_user_for_reviews, df$movie_facebook_likes)                   # .40
cor.test(df$num_user_for_reviews, df$cast_total_facebook_likes)              # .623  *
cor.test(df$num_critic_for_reviews, df$num_voted_users)                      # .623  *
cor.test(df$num_critic_for_reviews, df$cast_total_facebook_likes)            # .258  
cor.test(df$num_critic_for_reviews, df$movie_facebook_likes)                 # .688  *



cor.test(df$num_voted_users, df$cast_total_facebook_likes)                   # .26
cor.test(df$director_facebook_likes, df$cast_total_facebook_likes)           # .11
cor.test(df$num_user_for_reviews, df$cast_total_facebook_likes)              # .20





cor.test(df$imdb_score, df$num_critic_for_reviews)                 # .33
cor.test(df$imdb_score, df$budget)                                 # .03
cor.test(df$imdb_score, df$duration)                               # .34
cor.test(df$imdb_score, df$director_facebook_likes)                # .17         
cor.test(df$imdb_score, df$gross)                                  # .20
cor.test(df$imdb_score, df$num_voted_users)                        # .427
cor.test(df$imdb_score, df$cast_total_facebook_likes)              # .095
cor.test(df$imdb_score, df$facenumber_in_poster)                   # -.07
cor.test(df$imdb_score, df$num_user_for_reviews)                   # .31   *
cor.test(df$imdb_score, df$logNum_user_for_reviews)                # .34
cor.test(df$imdb_score, df$title_year)                             # .21
cor.test(df$imdb_score, df$imdb_score)                             # 1.00
cor.test(df$imdb_score, df$movie_facebook_likes)                   # .253  * 







#____________________________________________________________________________________________________________________

# Code I researched from Kaggle Competition


imdb <- imdb[!duplicated(imdb), ]

library(stringr)
imdb$movie_title <- gsub("Â", "", as.character(factor(imdb$movie_title)))
str_trim(imdb$movie_title, side = "right")
dim(imdb)

# create a new data frame
genres.df <- as.data.frame(imdb[,c("genres", "imdb_score")])
# separate different genres into new columns
genres.df$Action <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Action") 1 else 0)
genres.df$Adventure <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Adventure") 1 else 0)
genres.df$Animation <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Animation") 1 else 0)
genres.df$Biography <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Biography") 1 else 0)
genres.df$Comedy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Comedy") 1 else 0)
genres.df$Crime <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Crime") 1 else 0)
genres.df$Documentary <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Documentary") 1 else 0)
genres.df$Drama <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Drama") 1 else 0)
genres.df$Family <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Family") 1 else 0)
genres.df$Fantasy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Fantasy") 1 else 0)
genres.df$`Film-Noir` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Film-Noir") 1 else 0)
genres.df$History <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "History") 1 else 0)
genres.df$Horror <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Horror") 1 else 0)
genres.df$Musical <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Musical") 1 else 0)
genres.df$Mystery <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Mystery") 1 else 0)
genres.df$News <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "News") 1 else 0)
genres.df$Romance <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Romance") 1 else 0)
genres.df$`Sci-Fi` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sci-Fi") 1 else 0)
genres.df$Short <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Short") 1 else 0)
genres.df$Sport <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sport") 1 else 0)
genres.df$Thriller <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Thriller") 1 else 0)
genres.df$War <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "War") 1 else 0)
genres.df$Western <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Western") 1 else 0)
# get the mean of imdb score for different genres
means <- rep(0,23)
for (i in 1:23) {means[i] <- mean(genres.df$imdb_score[genres.df[i+2]==1])}
# plot the means
barplot(means, main = "Average imdb scores for different genres")

colSums(sapply(IMDB, is.na))
missing.values <- aggr(IMDB2, sortVars = T, prop = T, sortCombs = T, cex.lab = 1.5, cex.axis = .6, cex.numbers = 5, combined = F, gap = -.2)





#_____________________________________________________________________________________________________________

# Experiments


imdb$logGross <- log(imdb$gross + .1)
hist(imdb$logGross)

imdb$logBudget <- log(imdb$budget + .1)
hist(imdb$logBudget)

plot(imdb$title_year, imdb$gross, main="Gross Sales vs Title Year", 
     xlab="Title Year $ ", ylab="Gross Sales", pch=19)

plot(imdb$title_year, imdb$logGross, main="Gross Sales vs Title Year", 
     xlab="Title Year $ ", ylab="Gross Sales", pch=19)


plot(imdb$num_user_for_reviews, imdb$num_critic_for_reviews, main="User Reviews For vs Critics' Reviews For", 
     xlab="Movie User Reviews For ", ylab="Critics' Reviews For", pch=19)

ggplot(imdb,aes(logGross))+geom_histogram(fill='springgreen4',color='black')+
  ggtitle("Log of Gross Sales - Frequency Distribution")+
  xlab("Log(Gross Sales)")+
  ylab("Count")

plot(c$PREF_CLASS_YEAR, c$logLife, main="Log Lifetime Giving vs. Grad Year", 
     xlab="Year of Graduation ", ylab="Log of Lifetime Giving", pch=19)

ggplot(c,aes(logLife))+geom_histogram(fill='springgreen4',color='black')+
  ggtitle("Number of Alumni by Log of Lifetime Contribution")+
  xlab("Log(Amount of Lifetime Contribution)")+
  ylab("Number of Alumni")





model1 <-  lm(imdb_score ~ ., data = df)
summary(model1)

model2 <- lm(imdb_score~ num_voted_users + num_critic_for_reviews, data = imdb)
summary(model2)

confint(model3C)

cor(IMDB2$title_year, IMDB2$budget)
