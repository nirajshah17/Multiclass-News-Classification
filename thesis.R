
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
d1$meta_description <- NULL
d1$keywords <- NULL
d1$meta_keywords <- NULL
d1$authors <- NULL 
