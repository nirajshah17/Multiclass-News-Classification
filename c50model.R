setwd("E:/data")
options(scipen = 999)

file.info('news.csv')$size / 2^30
readLines('news.csv',n=28)

newsfile <- 'news.csv'
index <- 0
chunksize <- 1000000
con <- file(description = newsfile , open = "r")
datachunk <- read.table(con, nrows = chunksize, header =  T , fill = T , sep = ",",quote = '"')

counter <- 0
actualcolnames <- names(datachunk)

repeat {
  index <- index + 1
  print(paste('Processing rows:', index * chunksize))
  
  #create split files
  outfile <- paste(index,".csv",sep="")
  write.csv(file=outfile, datachunk, row.names = F)
  
  
  if (nrow(datachunk) != chunksize){
    print('Processed all files!')
    break}
  
  datachunk <- read.table(con, nrows=chunksize, skip=0, header=FALSE, fill = TRUE, sep=",", quote = '"', col.names=actualcolnames)
  
  if (index > 10) break
  
}
close(con)


d1 <- read.csv("1.csv", stringsAsFactors = F)

#replace blanks with NA
d1[d1==""]<-NA

#check for nulls
sapply(d1,function(x) sum(is.na(x)))

#remove irrelevant columns
d1$source <- NULL
d1$summary<- NULL
d1$scraped_at <- NULL
d1$inserted_at <- NULL
d1$updated_at <- NULL
d1$tags <- NULL
d1$url <- NULL
d1$id <- NULL
d1$title <- NULL


#remove rows where type and content are missing
d1 <- d1[!(is.na(d1$type)),]
d1 <- d1[!(is.na(d1$content)),]
#check for nulls
sapply(d1,function(x) sum(is.na(x)))

write.csv(d1, file = "1cleaned.csv",row.names=FALSE)

d2 <- read.csv("1cleaned.csv", stringsAsFactors = F)


#convert type to factors
d2$type <- factor(d2$type,levels=c("fake", "satire", "bias", "conspiracy", "junksci","hate", "clickbait", "unreliable", "political", "reliable","rumor"))

#check structure
str(d2)

#check for nulls
sapply(d2,function(x) sum(is.na(x)))
d2 <- d2[!(is.na(d2$type)),]


count_type <- table(d2$type)
prop.table(count_type)
barplot(count_type)

#remove special characters
d2$content <- gsub("[[:punct:]]", "", d2$content)
#d2$content <- gsub("�???T", "'", d2$content)


# load text mining package
require(tm)  


#vectorsource considers each element in the vector as a document
vs <- VectorSource(d2$content)

# build corpus
corpus <- Corpus(vs)  

# remove numbers
corpus <- tm_map(corpus, removeNumbers)  

# remove puntucations
corpus <- tm_map(corpus, removePunctuation)

# remove  white spaces
corpus <- tm_map(corpus, stripWhitespace)

#remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords('english'))

# build document term matrix
tdm <- DocumentTermMatrix(corpus)

# remove sparse terms
tdm <- removeSparseTerms(tdm, 0.90)

# count matrix
tdm_df <- as.data.frame(as.matrix(tdm))

# binary instance matrix
#tdm_df <- as.matrix((tdm_df > 0) + 0)

#tdm_df <- as.data.frame(tdm_df)

# append type class from original dataset
tdm_df <- cbind(tdm_df, d2$type)

set.seed(18139108)
#create training and testing datasets
index <- sample(1:nrow(tdm_df), nrow(tdm_df) * .80, replace=FALSE)
training <- tdm_df[index, ]
testing <- tdm_df[-index, ]

#create training and validation datasets from training
index1 <- sample(1:nrow(training), nrow(training) * .80, replace=FALSE)
training_t <- tdm_df[index1, ]
valid_t <- tdm_df[-index1, ]

# class instances in training data
table(training_t$`d2$type`)

# class instances in testing data
table(valid_t$`d2$type`)

library(C50)
#build model
c50model <- C5.0(training_t$`d2$type` ~., data=training_t, trials=10)
summary(c50model)
cFiftyPrediction <- predict(c50model, newdata = valid_t[, -236]) #remove type column while prediction

#accuracy
(cFiftyAccuracy <- 1- mean(cFiftyPrediction != valid_t$`d2$type`)) #0.7545036

#confusion matrix
library(caret)
cMat <- confusionMatrix(cFiftyPrediction, valid_t$`d2$type`)
cMat