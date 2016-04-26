movies <- read.csv("/home/awaz/Documents/UISCourses/DataScience/MovieReviews.csv",stringsAsFactors = FALSE)
str(movies)
matx <- cor(movies[,-1],use = "pairwise.complete.obs")
get_movies <- function(x, y){
  movie.corr <- rep(NA,ncol(y)-1)
  movie.name <- rep(NA,ncol(y)-1)
  k <- 1
  i <- which(row.names(y)==x)
  if(!length(i)){
    stop("Invalid movie name")
  }
  for(j in 1:ncol(y)){
    if(colnames(y)[j]==x){
      next
    }
    else {
      movie.corr[k] <- y[i,j]
      movie.name[k] <- colnames(y)[j]
      k <- k+1
    }
      
  }
  movies.df <- data.frame(movie.name,movie.corr)
  movies.df <- movies.df[order(-movie.corr),]
  return(movies.df)
}
fault.of.star <- get_movies("The.Fault.in.Our.Stars",matx)
str(fault.of.star)
View(fault.of.star)
#Add my ratings to the dataset
myrating <- data.frame(Name="Nayan",
                       American.Sniper=NA,
                       The.Hunger.Games..Mockingjay...Part.1=4,
                       Guardians.of.the.Galaxy=5,
                       The.Lego.Movie=NA,
                       The.Hobbit=4,
                       Transformers=2,
                       Malificent=3,
                       Big.Hero.6=5,
                       Godzilla=2,
                       Interstellar=4,
                       How.to.Train.your.Dragon.2=4,
                       Gone.Girl=NA,
                       Divergent=3,
                       The.Fault.in.Our.Stars=NA,
                       Unbroken=NA,
                       X300..Rise.of.an.Empire=2)
myrating$Name = as.character(myrating$Name)
movies2 <- rbind(movies,myrating)
str(movies2)
matx2 <- cor(movies[,-1],use = "pairwise.complete.obs")
#I have already rated Guardians of Galaxy and Big Hero 6 the highest
GoG <- get_movies("Guardians.of.the.Galaxy",matx2)
View(GoG)
#Predict my rating
rate_movie <- function(x,y,z){
  if(nrow(x)==0 | nrow(y)==0){
    stop("Empty data frame")
  }
  temp_corr <- -1
  for (i in 1:nrow(x)) {
    m <- which(row.names(z)==x[i,1])
    for (j in 1:nrow(y)) {
      n <- which(colnames(z)==y[j,1])
      if(z[m,n] > temp_corr){
        temp_corr <- z[m,n]
        stor_n <- j
      }
    }
    x[i,2] <- y[stor_n,2]
  }
  return(x)
}
#Create a df of movies I have seen
ind <- grep("Nayan",movies2$Name)
val <- sum(!is.na(movies2[ind,]))
seen <- matrix(ncol = 2, nrow = val-1)
i <- 1
for (j in 2:ncol(movies2)) {
  if(!is.na(movies2[ind,j])) {
    seen[i,1] <- colnames(movies2)[j]
    seen[i,2] <- movies2[ind,j]
    i <- i+1
  }
}
seen <- data.frame(seen,stringsAsFactors = FALSE)
seen$X2 <- as.numeric(seen$X2)
#Create a df of movies I have not seen
val2 <- sum(is.na(movies2[ind,]))
unseen <- matrix(ncol = 2, nrow = val2)
i <- 1
for (j in 2:ncol(movies2)) {
  if(is.na(movies2[ind,j])) {
    unseen[i,1] <- colnames(movies2)[j]
    unseen[i,2] <- movies2[ind,j]
    i <- i+1
  }
}
unseen <- data.frame(unseen,stringsAsFactors = FALSE)
unseen$X2 <- as.numeric(unseen$X2)
unseen.ratings <- rate_movie(x=unseen,y=seen,z=matx2)
