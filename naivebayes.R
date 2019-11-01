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