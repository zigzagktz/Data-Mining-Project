#IMDB Useful code
library(MASS)
library(corrplot)
library(randomForest)
library(randomForestSRC)
library(data.table)

df1 <- read.csv("D:/study/Data Mining Applications/Final Proj/main_dataset.csv")[ ,-1]

df1 <- df
#Create the test and training data set with 75:25 ratio
set.seed(12345)
row.number <- sample(x = 1:nrow(df1), size = 0.75 * nrow(df1))
train <- df1[row.number, ]
test <- df1[-row.number, ]


#Linear model

fit <- lm(imdb_score ~ num_critic_for_reviews+duration+gross+num_voted_users
          +num_user_for_reviews,
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

