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

d2$textlength <- nchar(d2$content)
#library(ggplot2)
#ggplot(d2,aes(textlength, fill=type)) +  geom_histogram(binwidth = 15000)
d3 <- d2

library(lattice)
histogram( ~textlength | type , data = d2)
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
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

# build document term matrix
tdm <- DocumentTermMatrix(corpus.clean)

# remove sparse terms
tdm_sparse <- removeSparseTerms(tdm, 0.90) 

# count matrix
tdm_dm <- as.data.frame(as.matrix(tdm_sparse))

# append type class from original dataset
tdm_df <- cbind(tdm_dm, dt1$type) 

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
system.time( pred <- predict(classifier, newdata = valid_t[, -280]) )
