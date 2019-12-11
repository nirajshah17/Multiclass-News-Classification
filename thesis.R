
setwd("E:/data")
options(scipen = 999)

#file size and overview
file.info('news.csv')$size / 2^30
readLines('news.csv',n=28)

#Attribution - adapted and modified from http://amunategui.github.io/dealing-with-large-files/

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
  
  datachunk <- read.table(con, nrows=chunksize, skip=0, header=FALSE, fill = TRUE, 
                          sep=",", quote = '"', col.names=actualcolnames)
  
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

d2 <- d2[1:100000,]
#convert type to factors
d2$type <- factor(d2$type,levels=c("fake", "satire", "bias", "conspiracy", "junksci","hate", "clickbait", "unreliable", "political", "reliable","rumor"))

#check structure
str(d2)

#check for nulls
sapply(d2,function(x) sum(is.na(x)))
#remove nulls
d2 <- d2[!(is.na(d2$type)),]


count_type <- table(d2$type)
count_type
prop.table(count_type)
barplot(count_type)

#remove special characters
d2$content <- gsub("[[:punct:]]", "", d2$content)
#sd2$content <- gsub("â€™", "'", d2$content)


# load text mining package
require(tm)  


#vectorsource considers each element in the vector as a document
vs <- VectorSource(d2$content) 

# build corpus
corpus <- Corpus(vs)  

#Adapted and modified from - https://www.youtube.com/watch?v=lRTerj8fdY0 - cleaning of text
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

#dimension of data
dim(tdm_df)
# 97811    274

write.csv(tdm_df, file = "tdmdf100k.csv",row.names=FALSE)

set.seed(18139108)
#create training and testing datasets
index <- sample(1:nrow(tdm_df), nrow(tdm_df) * .80, replace=FALSE)
training <- tdm_df[index, ] 
testing <- tdm_df[-index, ]

#create training and validation datasets from training 
#index1 <- sample(1:nrow(training), nrow(training) * .80, replace=FALSE)
#training_t <- tdm_df[index1, ] 
#valid_t <- tdm_df[-index1, ]

# class instances in training data
table(training$`d2$type`)

# class instances in testing data
table(testing$`d2$type`)

#Attribution- adapted and modified from Advanced Data Mining notes provided on Moodle by Mr.Noel Cosgrave - C5.0 model
library(C50) 
#build model
system.time(c5model <- C5.0(training$`d2$type` ~., data=training, trials=10))
summary(c50model)
cFiftyPrediction <- predict(c5model, newdata = testing[, -274]) #remove type column while prediction

#accuracy
(cFiftyAccuracy <- 1- mean(cFiftyPrediction != testing$`d2$type`)) #0.8345

#confusion matrix
library(caret)
cMat <- confusionMatrix(cFiftyPrediction, testing$`d2$type`) 
cMat

library(e1071)
system.time(svm_model <- svm(training$`d2$type` ~., data=training))
svm_predict <- predict(svm_model, newdata = testing[, -274]) #remove type column while prediction


#accuracy
(svmAccuracy <- 1- mean(svm_predict != testing$`d2$type`)) #0.7998

#confusion matrix
library(caret)
cMat1<- confusionMatrix(svm_predict, testing$`d2$type`) 
cMat1

tdm_df <- tdm_df[,-274]
tdm_df <- ifelse(tdm_df[] > 0, "Yes", "No")

tdm_df <- as.data.frame(tdm_df)

str(tdm_df)
# append type class from original dataset
tdm_df <- cbind(tdm_df, d2$type)

set.seed(18139108)

#create training and testing datasets
index <- sample(1:nrow(tdm_df), nrow(tdm_df) * .80, replace=FALSE)
training <- tdm_df[index, ] 
testing <- tdm_df[-index, ]


# Train the classifier
system.time( classifier <- naiveBayes(training, training$`d2$type`, laplace = 1) )
pred <- predict(classifier, newdata = testing[, -274]) 
(nbAccuracy <- 1- mean(pred != testing$`d2$type`))  #0.5552
cMat2 <- confusionMatrix(pred, testing$`d2$type`) 
cMat2

