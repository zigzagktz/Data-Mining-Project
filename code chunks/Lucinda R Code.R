setwd("~/Data Mining Appns ALY 6040")
getwd()

# WHOLE CODE TO GET TO RANDOM FOREST ON BINNED lOG(GROSS REVENUE)    Lines 8 to 178
# WHOLE CODE TO CREATE CORRELATION MATRICESS                         Lines 183 to 275
# DATA EXPLORATION HISTOGRAMS AND PLOTS                              Lines 280 to 321
# Experiment with different linear regression models for the df data frame    Lines 347-470


# Load packages
library(ggplot2) # visualization
library(dplyr) # data manipulation
library(plotly)
library(corrplot)
library(caret)
library(tibble)
library(tidyr)
library("gridExtra")
library(randomForest)
library(data.table)

# Variables that span between single digits and millions:  make them (log() + .1)---  log (0) = NAN
df1$logGross<-log(df1$gross + .1)
hist(df1$logGross)

df1$logBudget<-log(df1$budget + .1)
hist(df1$logBudget)

df1$logNum_user_for_reviews   <- log(df1$num_user_for_reviews + .1)
hist(df1$logNum_user_for_reviews)

df1$logNum_voted_users <- log(df1$num_voted_users + .1)
hist(df1$logNum_voted_users ) 

df1$logMovie_facebook_likes <-log(df1$movie_facebook_likes + .1)
hist(df1$logMovie_facebook_likes)    # there are about 1700 with 0 movie FB likes

df1$LogCast_total_facebook_likes <- log(df1$cast_total_facebook_likes + .1)
hist(df1$LogCast_total_facebook_likes)

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
numdf4 <- subset(numdf4, select = -c(director_facebook_likes))
numdf4 <- subset(numdf4, select = -c(logMovie_facebook_likes))
numdf4 <- subset(numdf4, select = -c(facenumber_in_poster))
numdf4 <- subset(numdf4, select = -c(num_voted_users))
numdf4 <- subset(numdf4, select = -c(num_user_for_reviews))
numdf4 <- subset(numdf4, select = -c(gross))
numdf4 <- subset(numdf4, select = -c(budget))
numdf4 <- subset(numdf4, select = -c(logGross))
numdf4 <- subset(numdf4, select = -c(duration))
numdf4 <- subset(numdf4, select = -c(actor_1_facebook_likes))
numdf4 <- subset(numdf4, select = -c(movie_facebook_likes))
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


numdf11 <- subset(numdf11, select = -c(aspect_ratio))
numdf11 <- subset(numdf11, select = -c(movie_facebook_likes))
numdf11 <- subset(numdf11, select = -c(director_facebook_likes))
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


numdf21 <- numdf1 %>% 
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

# Now create simplified correlation matrix
cor_mat <- cor(numdf1)
cor_mat
dim(cor_mat)
corrplot(cor_mat)

library(tibble)
library(dplyr)
library(tidyr)


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
hist(df1$director_facebook_likes)
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

df1$logMovie_facebook_likes <-log(df1$movie_facebook_likes + .1)
hist(df1$logMovie_facebook_likes)    # there are about 1700 with 0 movie FB likes

df1$LogCast_total_facebook_likes <- log(df1$cast_total_facebook_likes + .1)
hist(df1$LogCast_total_facebook_likes)

log(.1)     # = -2.3

# *****  REPEAT CORRELATION MATRIX WHICH NOW INCLUDES LOG VALUES ****************
#  Extract only fields with numeric items
numdf1 <- dplyr::select_if(df1, is.numeric)
# And remove aspect ratio
numdf1 <- subset(numdf1, select = -c(aspect_ratio))
# numdf1 <- as.matrix(numdf1)  if needed


plot (df1$num_critic_for_reviews , df1$num_user_for_reviews  )
plot (df1$num_critic_for_reviews , df1$num_voted_users  )
plot (df1$num_critic_for_reviews , df1$movie_facebook_likes  )
plot (df1$num_critic_for_reviews , df1$imdb_score )
plot (df1$num_critic_for_reviews , df1$duration)


plot (df1$logBudget , df1$logGross  )
plot (df1$logMovie_facebook_likes , df1$logNum_user_for_reviews  )
plot (df1$logMovie_facebook_likes, df1$logNum_voted_users)
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

model1 <-  lm(logGross ~ logMovie_facebook_likes, data = numdf1)
summary(model1)            # R2  = 0.0013

model1 <-  lm(gross ~ num_critic_for_reviews, data = numdf1)
summary(model1)            #  R2 = .23

model1 <-  lm(logGross ~ num_critic_for_reviews, data = numdf1)
summary(model1)            #  R2 = .11

#  Now we will continue looking at Gross Sales

model1 <-  lm(gross ~ num_critic_for_reviews + movie_facebook_likes, data = numdf1)
summary(model1)            #  R2 = .236    No difference from modeld above- due to collinarity?

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

model2 <- lm(logGross ~ logMovie_facebook_likes, data = numdf1)
summary(model2)            #  R2 = .00129   Nope

model2 <- lm(logGross ~ num_critic_for_reviews, data = numdf1)
summary(model2)            #  R2 = .11  Not much

model2 <- lm(logGross ~ budget, data = numdf1)
summary(model2)            #  R2 = .003   Have to plot logGross vs logBudget


