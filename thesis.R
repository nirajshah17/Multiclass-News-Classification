setwd("C:\\Users\\niraj\\OneDrive\\Desktop\\THESIS\\DataFinal")
library(openxlsx)

file.info('news.csv')$size / 2^30
readLines('news.csv',n=28)

newsfile <- 'news.csv'
chunksize <- 10000
con <- file(description = newsfile , open = "r")
data <- read.table(con, nrows = chunksize, header =  T , fill = T , sep = ",",quote = '"')
close(con)
head(data)

#replace blanks with NA
data[data==""]<-NA

sapply(data,function(x) sum(is.na(x)))
