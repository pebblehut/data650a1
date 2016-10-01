# install.packages("tm")
library("tm")
setwd("/Users/davidstonehouse/Dropbox/Backup/UMUC/DATA650/Assignment1/RaleighCityCouncil")

sf<-system.file("texts", "txt", package = "tm")
ds <-DirSource(sf)
your_corpus <-Corpus(ds)

docs <- Corpus(DirSource("worktxt"))
summary(docs)
dd <- data.frame(id=1:9,text=unlist(sapply(docs, '[', "content")), stringsAsFactors=F)
#Now, in order to read special attributes from a data.frame, we will use the readTabular function to make our own custom data.frame reader. This is all we need to do
myReader <- readTabular(mapping=list(content="text", id="id"))
#We just specify the column to use for the contents and the id in the data.frame. Now we read it in with DataframeSource but use our custom reader.
docs2 <- VCorpus(DataframeSource(dd), readerControl=list(reader=myReader))

docs <- tm_map(docs, removePunctuation)

for(j in seq(docs))
{
  docs[[j]] <-gsub("/", " ", docs[[j]])
  docs[[j]] <-gsub("@", " ", docs[[j]])
  docs[[j]] <-gsub("\\|", " ", docs[[j]])
}
#inspect(docs)

# Remove numeric characters -----------------------------------------------
docs<-tm_map(docs, removeNumbers)
#inspect(docs)


# Remove Stopwords --------------------------------------------------------
docs<-tm_map(docs, removeWords, stopwords("english"))
#inspect(docs)


# Stemming ----------------------------------------------------------------
# install.packages("SnowballC")
library(SnowballC)
docs <- tm_map(docs, stemDocument)
# Apply all these - now we have a corpus of plain docs:
docs <- tm_map(docs, PlainTextDocument)


# Word Frequencies --------------------------------------------------------
dtm <- DocumentTermMatrix(docs)
dtm
freq <- colSums(as.matrix(dtm))
freq
length(freq)
ord <- order(freq)
ord
m <-as.matrix(dtm)
dim(m)
m[, 1:2 ]

findAssocs(dtm, c("data", "analysis", "methods", "mining"), corlimit = 0.75)


findFreqTerms(dtm, lowfreq = 5)
# Remove words that appear in less than 70% of documents
dtms <- removeSparseTerms(dtm, 0.7)
dtms


docs <- Corpus(DirSource("worktxt"))

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords('english'))

docsCopy <- docs

docs <- tm_map(docs, stemDocument)
inspect(docs[1:5])

for (i in 1:2) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(docs[[i]], width=73))
}

dtm <- DocumentTermMatrix(docs)
dtm
freq <- colSums(as.matrix(dtm))
freq
length(freq)

findFreqTerms(dtm, lowfreq = 55)
dtms <- removeSparseTerms(dtm, 0.01)
dtms

terms <-DocumentTermMatrix(docs,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
terms

tdm <- TermDocumentMatrix(docs)

library(cluster)
#or compute cosine distance among documents
dissimilarity(tdm, method = "cosine")
daisy(as.matrix(dtm), metric = c("euclidean"))
