setwd("C:\\Users\\niraj\\OneDrive\\Desktop\\THESIS")
library(openxlsx)

#import files

fake_data <- read.csv("Fake.csv", stringsAsFactors = F)
true_data <- read.csv("True.csv", stringsAsFactors = F)

fake_data$class <- "fake"
true_data$class <- "true"

data <- rbind(fake_data, true_data)

#check for null values
length(which(!complete.cases(data)))

#replace blanks with NA
data[data==""]<-NA

#structure of data
str(data) 

#convert class label to a factor
data$class <- as.factor(data$class)

#randomize
data2 <- data[sample(nrow(data)),]

prop.table(table(data2$class))
