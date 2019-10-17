
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
data[data==""]<-NA

sapply(data,function(x) sum(is.na(x)))

