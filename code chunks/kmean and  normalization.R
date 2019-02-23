cl <- cbind(df1$imdb_score_cat,df1$imdb_score_cat )
cl<-as.data.frame(cl)
cl$V2 <- as.factor(cl$V2)

normalize <- function(x){
  
  (x-min(x))/(max(x)-min(x))
}

sub <- as.data.frame(apply(cl, 2, normalize))


k <- kmeans(sub,8)

t <- table(k$cluster,cl$V2)
sum(diag(t))/sum(t)
