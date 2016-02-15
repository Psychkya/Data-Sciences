#clear previous workspace
rm(list = ls());
#install packages to work with 20 newsgroup data
install.packages('tm');
install.packages('SnowballC');
#load the libraries
require('tm');
require('SnowballC');
#Prior to processing, download the file 20news-bydate.tar.gz from http://qwone.com/~jason/20Newsgroups/
#Extract the folder rec.sport.baseball into a folder using suitable app (7-zip for windows)
#Assign folder location to a variable
dirpath = "D:/UIS courses/Data Sciences/20news-bydate-train/rec.sport.baseball";
#Load the corpus
newsposts <- Corpus(DirSource(dirpath,encoding = "UTF-8"),readerControl = list(reader=readPlain,language="en"));
#Convert all text to lower case. First want to make sure what version of 'tm' we got loaded
packageVersion('tm');
#The version info is needed so that the right command to convert to lowercase is used
#Actually, before we preprocess, lets inspect our corpus
newsposts;
inspect(newsposts);
#display the first doc
newsposts[[1]]$content;
#now lets change to lower case
newsposts.lower <- tm_map(newsposts,content_transformer(tolower));
#removing punctuations
newsposts.lower <- tm_map(newsposts.lower, removePunctuation);
#removing numbers
newsposts.lower <- tm_map(newsposts.lower,removeNumbers);
#remove stopwords
newsposts.lower <- tm_map(newsposts.lower, removeWords, stopwords("english"));
#steming document
newsposts.lower <- tm_map(newsposts.lower,stemDocument);
#removing whitespaces
newsposts.lower <- tm_map(newsposts.lower, stripWhitespace);
#View first doc after transformation
newsposts.lower[[1]]$content;
newsposts[[1]]$content;
#Create term matrix
docmatrix <- DocumentTermMatrix(newsposts.lower);
docmatrix;
dim(docmatrix);
#docmatrix <- DocumentTermMatrix(newsposts.lower,control = list(weighting=weightTfIdf));
#Reduce dimension by removing words occuring less
docmatrix.tfid = removeSparseTerms(docmatrix,0.90);
dim(docmatrix.tfid);
#Use tfidf
docmatrix.tfid <- weightTfIdf(docmatrix.tfid);
#truncated svd
svd.matrix <- svd(docmatrix.tfid, nu = 50, nv = 100);
dim(svd.matrix$v);
View(svd.matrix$v);
#create diagonal matrix
dia.matrix <- diag(svd.matrix$d, 136, 100);
#create concepts matrix
concepts <- dia.matrix %*% t(svd.matrix$v);
dim(concepts);
View(concepts);
#displaying first 10 concepts
for (i in 1:10)
{
  cat("concept ", i, ": \n")
  for (j in 1:nrow(concepts))
  {
    if (abs(concepts[i,j]) > .1) {print(colnames(docmatrix.tfid[,j]))}
  }
}