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
meta(docs[[1]], "id")
# From Yelena: clean up the metadata
docs <- tm_map(docs, PlainTextDocument)
fnames <- dir("RaleighCityCouncil/worktxt")
meta(docs, type="local", tag="id") <- fnames
meta(docs[[1]], "id")
# rownames(m) <- fnames
# m

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
# Apply all these - now we have a corpus of plain docs:


dtm <- DocumentTermMatrix(docs)
dtm


# Create DTMs with bounds on frequency ------------------------------------------------------

# Creating a seriew of DTMs, each going further to exclude overly common words.

ndocs <- length(dtm)
# For all of these, ignore overly sparse terms (appearing in more than one document)
minDocFreq <- ndocs * 0.2
# ignore terms appearing in all of the documents
maxDocFreq <- ndocs * 0.99
dtm99<- DocumentTermMatrix(docs, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
dtm99                         
# Go further and remove words that show up in almost every document
maxDocFreq <- ndocs * 0.8
dtm8<- DocumentTermMatrix(docs, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
dtm8                         

# Find Frequent Terms --------------------------------------------------------
findFreqTerms(dtm, lowfreq = 45)
findFreqTerms(dtm99, lowfreq = 15)
findFreqTerms(dtm8, lowfreq = 10)

# Word Cloud --------------------------------------------------------------
#install.packages("wordcloud")
library(wordcloud)

MyWordCloud <- function(dtm) {
  freq <- colSums(as.matrix(dtm)) # word frequencies
  dark2 <- brewer.pal(8, "Dark2")
  # wordcloud(names(freq), freq, max.words=30, rot.per=0.2, colors = dark2)
  # wordcloud(names(freq), freq, scale=c(8,.2),min.freq=10,
  #           max.words=Inf, random.order=FALSE, rot.per=.15, colors=dark2)
  pal <- brewer.pal(9, "BuGn")
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
# Want correlations >=0.75
findAssocs(dtm99, c("leesvill", "trailwood", "stormwat", "birch"), corlimit = 0.75)

# Word Frequency Plots ----------------------------------------------------
# install.packages("ggplot2")
library(ggplot2)

MyFreqPlot <- function(dtm) {
  freq <- colSums(as.matrix(dtm))
  ord <- order(freq, decreasing = TRUE)
  graphset <- freq[head(ord, 15)]  
  graphset
  wf <- data.frame(word=names(graphset), freq=graphset)
  # wf
  p <- ggplot(wf, aes(word, freq))
  p <- p + geom_bar(stat="identity")
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
  p
}
MyFreqPlot(dtm)
MyFreqPlot(dtm99)

# Cluster Dendogram -------------------------------------------------------

#install.packages("fpc")
#install.packages("cluster")
library("cluster")
library ("fpc")

#dtms <- removeSparseTerms(dtm, 0.8)
dtms <- removeSparseTerms(dtm99, 0.75)
dtms
inspect(dtms)

d <- dist(t(dtms), method="euclidian")
d
fit <- hclust(d=d, method = "ward.D2")
plot(fit, hang=-1)
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=4)
rect.hclust(fit, k=4, border="red")

hcd <- as.dendrogram(fit)
plot(cut(hcd, h=15)$upper, 
     main="Upper tree of cut at h=75")
plot(cut(hcd, h=15)$lower[[8]], 
     main="Second branch # 8 of lower tree with cut at h=15")

# K-means clustering ------------------------------------------------------

dtms <- removeSparseTerms(dtm99, 0.75)
d <- dist(t(dtms), method="euclidean")
kfit <- kmeans(d,4)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
plotcluster(d, kfit$cluster)

           

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

