install.packages("tm")
library("tm")

setwd("/Users/davidstonehouse/Dropbox/Backup/UMUC/DATA650/Assignment1/part1ds")
dir(".")

docs <- Corpus(DirSource("."))
summary(docs)
data.frame(text=unlist(sapply(docs, '[', "content")), stringsAsFactors=F)

docs <- tm_map(docs, removePunctuation)
data.frame(text=unlist(sapply(docs, '[', "content")), stringsAsFactors = F)

for(j in seq(docs))
{
  docs[[j]] <-gsub("/", " ", docs[[j]])
  docs[[j]] <-gsub("@", " ", docs[[j]])
  docs[[j]] <-gsub("\\|", " ", docs[[j]])
}
