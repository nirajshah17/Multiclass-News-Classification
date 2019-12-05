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

str(d2)


#convert type to factors
d2$type <- factor(d2$type,levels=c("fake", "satire", "bias", "conspiracy", "junksci","hate", "clickbait", "unreliable", "political", "reliable","rumor"))

#compress to three factors
d2$type1 <- factor(d2$type,levels=c("fake", "satire", "bias", "conspiracy", "junksci","hate", "clickbait", "unreliable", "political", "reliable","rumor"),
                   labels=c("unreliable","unreliable","maybe_true","unreliable","unreliable","maybe_true","maybe_true","unreliable","maybe_true","reliable","maybe_true"))
#check structure
str(d2)

#check for nulls
sapply(d2,function(x) sum(is.na(x)))
d2 <- d2[!(is.na(d2$type)),]


count_type <- table(d2$type1)
count_type
#unreliable maybe_true   reliable 
#398780     490513       6599 
prop.table(count_type)
#unreliable  maybe_true    reliable 
#0.445120617 0.547513540 0.007365843 
barplot(count_type)

#library(lattice)
#histogram( ~textlength | type , data = d2)
#remove special characters
d2$content <- gsub("[[:punct:]]", "", d2$content)

dt1 <- d2#[1:100000,]

# load text mining package
require(tm)  

#vectorsource considers each element in the vector as a document
vs <- VectorSource(dt1$content) 

# build corpus
corpus <- Corpus(vs)  
print(corpus)
# remove numbers
#<<SimpleCorpus>>
#Metadata:  corpus specific: 1, document level (indexed): 0
#Content:  documents: 895892

#remove numbers
corpus <- tm_map(corpus, removeNumbers)  

inspect(corpus[1:3])

# remove puntucations
corpus <- tm_map(corpus, removePunctuation) 

# remove unnecessary white spaces
corpus <- tm_map(corpus, stripWhitespace)

#remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords('english'))

#convert to lower
corpus <- tm_map(corpus, tolower)

inspect(corpus[1:3])

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
tdm_df <- cbind(tdm_dm, dt1$type1) 

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
c50_3model <- C5.0(training$`dt1$type1` ~., data=training, trials=10)a
summary(c50model)
cFiftyPrediction_3 <- predict(c50_3model, newdata = testing[, -236]) #remove type column while prediction
(cFiftyAccuracy_3 <- 1- mean(cFiftyPrediction_3 != testing$`dt1$type1`))
#accuracy
#0.7326111 #0.7431389 - without binary instance matrix

#confusion matrix
library(caret)
cMat_3 <- confusionMatrix(cFiftyPrediction_3, testing$`dt1$type1`) 
cMat_3 #accuracy 82.19

#pickled model- save the model
saveRDS(c50model, file = "c50model.rds")
loadModel <- readRDS("c50model.rds")

#load kaggle data
kaggle <- read.csv("train.csv", stringsAsFactors = F)

kaggle$title <- NULL
kaggle$author <- NULL
kaggle$id <- NULL
kaggle$type1 <- factor(kaggle$label,levels=c(0,1),
                       labels=c("fake","true"))

#check structure
str(kaggle)

#check for nulls
sapply(kaggle,function(x) sum(is.na(x)))

count_type <- table(kaggle$type1)
prop.table(count_type)
barplot(count_type)

k1 <- kaggle

#k1$type1 <- gsub("[[:punct:]]", "", k1$text)

#vectorsource considers each element in the vector as a document
vs1 <- VectorSource(k1$text) 

# build corpus
corpus <- Corpus(vs1)  
print(corpus)

#remove numbers
corpus <- tm_map(corpus, removeNumbers)  

# remove puntucations
corpus <- tm_map(corpus, removePunctuation) 

# remove unnecessary white spaces
corpus <- tm_map(corpus, stripWhitespace)

#remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords('english'))

#convert to lower
corpus <- tm_map(corpus, tolower)

# build document term matrix
tdm1 <- DocumentTermMatrix(corpus) 

# remove sparse terms
tdm_sparse1 <- removeSparseTerms(tdm1, 0.90) 

# count matrix
tdm_dm1 <- as.data.frame(as.matrix(tdm_sparse1)) 

#tdm_df <- as.data.frame(tdm_dm)

# append type class from original dataset
tdm_df1 <- cbind(tdm_dm1, k1$type1) 

kagglePrediction <- predict(c50model, newdata = tdm_dm1) #remove type column while prediction

#confusion matrix
library(caret)
kag_cmat <- confusionMatrix(kagglePrediction, tdm_df1$`dt1$type1`) 
kag_cMat #accuracy 82.19







