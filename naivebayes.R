library(tm)
library(e1071)
library(dplyr)
library(caret)

setwd("E:/data")
options(scipen = 999)
d1 <- read.csv("1cleaned.csv", stringsAsFactors = F)
d2 <- d1

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

d3 <- d2
d2 <- d2[,-5]

#remove special characters
d3$content <- gsub("[[:punct:]]", "", d3$content)
d3$content <- gsub("â€™", "'", d3$content)

dt1 <- d3[1:50000,]


#vectorsource considers each element in the vector as a document
vs <- VectorSource(dt1$content) 

# build corpus
corpus <- Corpus(vs)  
print(corpus)
# remove numbers
#<<SimpleCorpus>>
#Metadata:  corpus specific: 1, document level (indexed): 0
#Content:  documents: 100000
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
tdm_df <- as.matrix((tdm_dm > 0) + 0) 

tdm_df <- ifelse(tdm_df[] > 0, "Yes", "No")
#tdm_df[1:50000,] <- as.factor(tdm_df[1:50000,])

tdm_df <- as.data.frame(tdm_df)
 
str(tdm_df)
# append type class from original dataset
tdm_df <- cbind(tdm_df, dt1$type)

set.seed(1703)

#create training and testing datasets
index <- sample(1:nrow(tdm_df), nrow(tdm_df) * .80, replace=FALSE)
training <- tdm_df[index, ] 
testing <- tdm_df[-index, ]

#create training and validation datasets from training 
index1 <- sample(1:nrow(training), nrow(training) * .80, replace=FALSE)
training_t <- tdm_df[index1, ] 
valid_t <- tdm_df[-index1, ]

# Train the classifier
system.time( classifier <- naiveBayes(training_t, training_t$`dt1$type`, laplace = 1) )
system.time( pred <- predict(classifier, newdata = valid_t[, -302]) )
(nbAccuracy <- 1- mean(pred != valid_t$`dt1$type`))
cMat <- confusionMatrix(pred, valid_t$`dt1$type`) 
cMat
#Accuracy : 0.5435               
#95% CI : (0.5362, 0.5508)     
#No Information Rate : 0.4661               
#P-Value [Acc > NIR] : < 0.00000000000000022