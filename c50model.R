
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
d2$content <- gsub("â€™", "'", d2$content)


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
tdm_df <- as.matrix((tdm_df > 0) + 0) 

tdm_df <- as.data.frame(tdm_df)

# append type class from original dataset
tdm_df <- cbind(tdm_df, d2$type) 

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
(cFiftyAccuracy <- 1- mean(cFiftyPrediction != valid_t$`d2$type`)) #0.7326111

#confusion matrix
library(caret)
cMat <- confusionMatrix(cFiftyPrediction, valid_t$`d2$type`) 
cMat
