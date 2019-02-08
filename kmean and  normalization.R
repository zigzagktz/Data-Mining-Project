cl <- cbind(likes,numb,df1$duration,df1$gross,df1$budget,df1$imdb_score_cat )

sub <- cl[1:200,]
sub <- as.data.frame(sub[,-c(7,11)])

normalize <- function(x){
  
  (x-min(x))/(max(x)-min(x))
}

sub <- as.data.frame(apply(sub, 2, normalize))

distance <- dist(sub)
hc <- hclust(distance)

k <- kmeans(sub,8)
