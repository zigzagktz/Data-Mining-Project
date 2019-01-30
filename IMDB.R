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

likes <- cbind(b,c,a)
colnames(likes) <- c("fb_likes_actor_1","fb_likes_actor_2","fb_likes_actor_3")

head(likes)

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