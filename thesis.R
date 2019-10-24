
setwd("E:/data")
options(scipen = 999)

file.info('news.csv')$size / 2^30
readLines('news.csv',n=28)

newsfile <- 'news.csv'
index <- 0
chunksize <- 1000000
con <- file(description = newsfile , open = "r")
datachunk <- read.table(con, nrows = chunksize, header =  T , fill = T , sep = ",",quote = '"')


#chunkSize <- 100000
#con <- file(description = newsfile , open = "r")
#data <- read.table(con, nrows = chunksize, header =  T , fill = T , sep = ",",quote = '"')
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

#replace blanks with NA
d1 <- read.csv("1.csv", stringsAsFactors = F)
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

d1 <- read.csv("1cleaned.csv", stringsAsFactors = F)

str(d1)

d2 <- d1

#convert type to factors
d2$type <- factor(d2$type,levels=c("fake", "satire", "bias", "conspiracy", "junksci","hate", "clickbait", "unreliable", "political", "reliable","rumor"))

#check structure
str(d2)

#check for nulls
sapply(d2,function(x) sum(is.na(x)))
d2 <- d2[!(is.na(d2$type)),]

table(d2$type)
plot(table(d2$type))


d3 <- d2

#remove special characters
d3$content <- gsub("[[:punct:]]", "", d3$content)
d3$content <- gsub("â???T", "'", d3$content)

# load text mining package
require(tm)  

vs <- VectorSource(d3$content) 
corpus <- Corpus(vs)  # build corpus

