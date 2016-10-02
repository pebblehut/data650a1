# install.packages("tm")
library("tm")

setwd("~/GitHub/DATA650Assign1")
dir("RaleighCityCouncil/worktxt")

docs <- Corpus(DirSource("RaleighCityCouncil/worktxt"))
meta(docs[[1]], "id")

summary(docs)
# data.frame(text=unlist(sapply(docs, '[', "content")), stringsAsFactors=F)

# Remove Punctuation and Special Characters -------------------------------
docs <- tm_map(docs, removePunctuation)
# data.frame(text=unlist(sapply(docs, '[', "content")), stringsAsFactors = F)
docs <- tm_map(docs, content_transformer(tolower))

for(j in seq(docs))
{
  docs[[j]] <-gsub("/", " ", docs[[j]])
  docs[[j]] <-gsub("@", " ", docs[[j]])
  docs[[j]] <-gsub("\\|", " ", docs[[j]])
}
# This reveals that we removed our document names (IDs) in the process
meta(docs[[1]], "id")
# From Yelena: clean up the metadata
docs <- tm_map(docs, PlainTextDocument)
fnames <- dir("RaleighCityCouncil/worktxt")
meta(docs, type="local", tag="id") <- fnames
meta(docs[[1]], "id")

# Remove numeric characters -----------------------------------------------
docs<-tm_map(docs, removeNumbers)
#inspect(docs)

# Remove Stopwords --------------------------------------------------------
docs<-tm_map(docs, removeWords, stopwords("english"))
inspect(docs)

# Strip WhiteSpace --------------------------------------------------------
docs<-tm_map(docs, stripWhitespace)
inspect(docs)

# Stemming ----------------------------------------------------------------
#install.packages("SnowballC")
library(SnowballC)
docs <- tm_map(docs, stemDocument)



# Create DTMs with bounds on frequency ------------------------------------------------------
dtm <- DocumentTermMatrix(docs)
dtm

# Create a DTM that excludes words common to all documents
ndocs <- length(dtm)
# ignore terms appearing in all of the documents 
# This is a "brute force" way of removing the general stop words in this 
# corpus. However, this could also remove a hot topic common to all documents!
maxDocFreq <- ndocs * 0.99
dtm99<- DocumentTermMatrix(docs, control = list(bounds = list(global = c(1, maxDocFreq))))
dtm99                         

# Find Frequent Terms --------------------------------------------------------
findFreqTerms(dtm, lowfreq = 45)
findFreqTerms(dtm99, lowfreq = 15)

# Word Cloud --------------------------------------------------------------
#install.packages("wordcloud")
library(wordcloud)

MyWordCloud <- function(dtm) {
  freq <- colSums(as.matrix(dtm)) # word frequencies
  # dark2 <- brewer.pal(8, "Dark2")
  # wordcloud(names(freq), freq, max.words=30, rot.per=0.2, colors = dark2)
  # wordcloud(names(freq), freq, scale=c(8,.2),min.freq=10,
  #           max.words=Inf, random.order=FALSE, rot.per=.15, colors=dark2)
  pal <- brewer.pal(8, "Dark2")
  # pal <- brewer.pal(9, "BuGn")
  pal <- pal[-(1:2)]
  wordcloud(names(freq), freq, scale=c(8,.3),min.freq=2,max.words=50, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
}

MyWordCloud(dtm)
MyWordCloud(dtm99)

# Word Frequencies --------------------------------------------------------
MyTopWords <- function(dtm, num.words=10) {
  freq <- colSums(as.matrix(dtm))
  ord <- order(freq, decreasing = TRUE)
  freq[head(ord,num.words)]
}
MyTopWords(dtm)
MyTopWords(dtm99)
MyTopWords(dtm99, num.words=20)

# Word Associations -------------------------------------------------------
# Want correlations >=0.9
findAssocs(dtm99, c("leesvill", "trailwood", "stormwat", "birch"), corlimit = 0.9)


# Create IDF --------------------------------------------------------------
m <- as.matrix(dtm99)
#m[ ,1:10]

ncorpus <- nrow(m)

MyIDF <- function(column, ndocs.total) {
  idf <- 0
  ndocs.found <- 0
  for (i in 1:length(column)) {
    if (column[i] > 0) 
      ndocs.found <- ndocs.found + 1
  }
  if (ndocs.found > 0)
    idf = log (1 + ndocs.total / ndocs.found)
  idf 
}
m.idf <- apply(m, 2, function(r) MyIDF(r,ncorpus))

m.tf.idf <- m
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m.tf.idf[i,j] <- m.tf.idf[i,j] * m.idf[j]
  }
}
m[ ,1:10]
m.tf.idf[ ,1:10]
m.idf[1:10]
m.freq <- colSums(m)
m.freq[1:10]
m.tf.idf.total <- colSums(m.tf.idf)
m.tf.idf.total[1:10]
m[ ,"leesvill"]
m.tf.idf[ , "leesvill"]

# Word Frequency Plots ----------------------------------------------------
# install.packages("ggplot2")
library(ggplot2)

MyFreqPlot <- function(m) {
  frequency <- colSums(m)
  frequency <- sort(frequency, decreasing=TRUE)
  frequency <- frequency[1:15]
  words <- names(frequency)
  wf <- data.frame(word=words, freq=frequency)
  p <- ggplot(wf, aes(word, freq))
  p <- p + geom_bar(stat="identity") + coord_flip()
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
  p <- p + theme(text=element_text(face="bold",family="serif",size=20))
  p
}
MyFreqPlot(as.matrix(dtm))
MyFreqPlot(as.matrix(dtm99))
MyFreqPlot(m.tf.idf)

# subset just the terms with the highest tf-idf value ---------------------

# First, add the tf-idf totals per term as an additional row
m.tf.idf.tmp <- m.tf.idf
m.tf.idf.tmp <- rbind(m.tf.idf.tmp, m.tf.idf.total)
m.tf.idf.tmp[ ,1:10]

# Now prepare for subset command - transpose terms to be rows
m.tf.idf.transpose <- t(m.tf.idf.tmp)
m.tf.idf.transpose[1:9,]
dim(m.tf.idf.transpose)

# subset on the tf-idf-total - experiment on the threshold  
# but drop tf-idf-total by only getting columns 1:ncorpus
m.tf.idf.transpose2 <- subset(m.tf.idf.transpose, m.tf.idf.total > 30, c(1:ncorpus))
dim(m.tf.idf.transpose2)
#m.tf.idf.transpose2

# Cluster Dendogram -------------------------------------------------------

#install.packages("fpc")
#install.packages("cluster")
library("cluster")
library ("fpc")

PlotDendrogram <- function(tdm, num.clusters) {
  #d <- dist(m.tf.idf.transpose2, method="euclidian")
  
  d <- dist(tdm, method="euclidian")
  #d
  fit <- hclust(d=d, method = "ward.D2")
  plot(fit, hang=-1)
  groups <- cutree(fit, k=num.clusters)
  rect.hclust(fit, k=num.clusters, border="red")
  fit
}

#dtms <- removeSparseTerms(dtm, 0.8)
dtms <- removeSparseTerms(dtm99, 0.75)
dtms
inspect(dtms)
fit <- PlotDendrogram(t(dtms), 4)

# This is too many terms - look for clusters using the cut function
hcd <- as.dendrogram(fit)
plot(cut(hcd, h=30)$upper, 
     main="Upper tree of cut at h=30")
plot(cut(hcd, h=30)$lower[[11]], 
     main="Second branch # 11 of lower tree with cut at h=30")

# Now do a plot of the tf-idf tdm
fit <- PlotDendrogram(m.tf.idf.transpose2, 8)

# K-means clustering ------------------------------------------------------
DoKMeans <- function(tdm, num.clusters) {
  d <- dist(tdm, method="euclidean")
  kfit <- kmeans(d,num.clusters)
  plotcluster(d, kfit$cluster)
  clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
  kfit
}

# First do K-Means using DTM without sparse terms
dtms <- removeSparseTerms(dtm99, 0.75)
kfit <- DoKMeans(t(dtms), 4)

# Now try with tf-idf
kfit <- DoKMeans(m.tf.idf.transpose2,8)

# Bigger Stop List --------------------------------------------------------

# Create a bigger stop words list with words that are in every document:
dtm01 <- removeSparseTerms(dtm, 0.01)
dtm01
# Dumping the words here - represent formalities of meeting minutes, titles
colnames(dtm01)
commonWords <- colnames(dtm01)
myStopWords <- c(stopwords("english"),commonWords)
myStopWords
docsNoCommon <-tm_map(docs, removeWords, myStopWords)
inspect(docsNoCommon)
dtmNoCommon <- DocumentTermMatrix(docsNoCommon)
dtmNoCommon01 <- removeSparseTerms(dtmNoCommon, 0.01)
dtmNoCommon01
colnames(dtmNoCommon01)

