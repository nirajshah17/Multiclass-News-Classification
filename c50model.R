d2 <- read.csv("1cleaned.csv", stringsAsFactors = F)


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
# fake     satire       bias     conspiracy   junksci      hate    clickbait  unreliable  political   reliable   rumor 

#123876      14252     135196     109597      17402       3619      21614     133653     287481       6599       42603 


prop.table(count_type)
barplot(count_type)

#remove special characters
d2$content <- gsub("[[:punct:]]", "", d2$content)

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

#covert to lower
corpus <- tm_map(corpus, tolower)

# build document term matrix
tdm <- DocumentTermMatrix(corpus)
tdm
#<<DocumentTermMatrix (documents: 895892, terms: 2027163)>>

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
# 895892    236

write.csv(tdm_df, file = "tdmdf.csv",row.names=FALSE)

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

library(C50) 
#build model

#Attribution- adapted and modified from Advanced Data Mining notes provided on Moodle by Mr.Noel Cosgrave C5-model
c5model <- C5.0(training$`d2$type` ~., data=training, trials=10)
summary(c50model)
cFiftyPrediction <- predict(c5model, newdata = testing[, -236]) #remove type column while prediction

#accuracy
(cFiftyAccuracy <- 1- mean(cFiftyPrediction != testing$`d2$type`)) #0.7660831 

#confusion matrix
library(caret)
cMat <- confusionMatrix(cFiftyPrediction, testing$`d2$type`) 
cMat


#Attribution- adapted and modified from https://stackoverflow.com/questions/32185176/how-to-save-machine-learning-models-in-r
#pickled model- save the model
saveRDS(c5model, file = "c5modelfinal.rds")
loadModel <- readRDS("c5modelfinal.rds")

################################   Convert to 3 classes  ###################################################################

#compress to three factors
d2$type1 <- factor(d2$type,levels=c("fake", "satire", "bias", "conspiracy", "junksci","hate", "clickbait", "unreliable", "political", "reliable","rumor"),
                   labels=c("unreliable","unreliable","maybe_true","unreliable","unreliable","maybe_true","maybe_true","unreliable","maybe_true","reliable","maybe_true"))

dt1<-d2

#read document term matrix
tdm_df <- read.csv("tdmdf.csv", stringsAsFactors = F)


tdm_df <- cbind(tdm_df, dt1$type1) 

#####################################################################
#create training and testing datasets
set.seed(18139108)
index <- sample(1:nrow(tdm_df), nrow(tdm_df) * .80, replace=FALSE)
training <- tdm_df[index, ] 
testing <- tdm_df[-index, ]

#create training and validation datasets from training 
#index1 <- sample(1:nrow(training), nrow(training) * .80, replace=FALSE)
#training_t <- tdm_df[index1, ] 
#valid_t <- tdm_df[-index1, ]

# class instances in training data
table(training$`dt1$type1`)
#unreliable maybe_true   reliable 
#318785     392685       5243 

# class instances in testing data
table(testing$`dt1$type1`)
#unreliable maybe_true   reliable 
#79995      97828       1356 

library(C50) 
#build model
c50_3model <- C5.0(training$`dt1$type1` ~., data=training, trials=10)
summary(c50model)
cFiftyPrediction_3 <- predict(c50_3model, newdata = testing[, -236]) #remove type column while prediction
(cFiftyAccuracy_3 <- 1- mean(cFiftyPrediction_3 != testing$`dt1$type1`))
#accuracy

#confusion matrix
library(caret)
cMat_3 <- confusionMatrix(cFiftyPrediction_3, testing$`dt1$type1`) 
cMat_3


