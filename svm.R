setwd("E:/data")
options(scipen = 999)

d2 <- read.csv("1cleaned.csv", stringsAsFactors = F)

str(d2)

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

d2$textlength <- nchar(d2$content)
#library(ggplot2)
#ggplot(d2,aes(textlength, fill=type)) +  geom_histogram(binwidth = 15000)
d3 <- d2[1:10000,]

library(lattice)
histogram( ~textlength | type , data = d2)
#remove special characters
d3$content <- gsub("[[:punct:]]", "", d3$content)
d3$content <- gsub("â€™", "'", d3$content)

dt1 <- d3 

# load text mining package
require(tm)  

#vectorsource considers each element in the vector as a document
vs <- VectorSource(dt1$content) 

# build corpus
corpus <- Corpus(vs)  

#remove numbers
corpus <- tm_map(corpus, removeNumbers)  

inspect(corpus[1:3])

# remove puntucations
corpus <- tm_map(corpus, removePunctuation) 

# remove unnecessary white spaces
corpus <- tm_map(corpus, stripWhitespace)

#remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords('english'))

#covert to lower
corpus <- tm_map(corpus, tolower)

# build document term matrix
tdm <- DocumentTermMatrix(corpus) 

# remove sparse terms
tdm_sparse <- removeSparseTerms(tdm, 0.90) 

# count matrix
tdm_dm <- as.data.frame(as.matrix(tdm_sparse)) 

# binary instance matrix
#tdm_df <- as.matrix((tdm_dm > 0) + 0) 

#tdm_df <- as.data.frame(tdm_dm)

# append type class from original dataset
tdm_df <- cbind(tdm_dm, dt1$type) 

#####################################################################
#create training and testing datasets
index <- sample(1:nrow(tdm_df), nrow(tdm_df) * .80, replace=FALSE)
training <- tdm_df[index, ] 
testing <- tdm_df[-index, ]

#create training and validation datasets from training 
index1 <- sample(1:nrow(training), nrow(training) * .80, replace=FALSE)
training_t <- tdm_df[index1, ] 
valid_t <- tdm_df[-index1, ]

# class instances in training data
table(training_t$`dt1$type`)

# class instances in testing data
table(valid_t$`dt1$type`)


library(e1071)
svm_model <- svm(training_t$`dt1$type` ~., data=training_t)
svm_predict <- predict(svm_model, newdata = valid_t[, -392]) #remove type column while prediction

#confusion matrix
library(caret)
cMat <- confusionMatrix(svm_predict, valid_t$`dt1$type`) 
cMat

