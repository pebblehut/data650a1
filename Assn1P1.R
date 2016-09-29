install.packages("tm")
library("tm")

setwd("~/GitHub/DATA650Assign1")
dir("part1ds")

docs <- Corpus(DirSource("part1ds"))
meta(docs[[1]], "id")

summary(docs)
data.frame(text=unlist(sapply(docs, '[', "content")), stringsAsFactors=F)

# Remove Punctuation and Special Characters -------------------------------
docs <- tm_map(docs, removePunctuation)
data.frame(text=unlist(sapply(docs, '[', "content")), stringsAsFactors = F)

for(j in seq(docs))
{
  docs[[j]] <-gsub("/", " ", docs[[j]])
  docs[[j]] <-gsub("@", " ", docs[[j]])
  docs[[j]] <-gsub("\\|", " ", docs[[j]])
}
inspect(docs)

# Remove numeric characters -----------------------------------------------
docs<-tm_map(docs, removeNumbers)
inspect(docs)

# Remove Stopwords --------------------------------------------------------

docs<-tm_map(docs, removeWords, stopwords("english"))
inspect(docs)

# Stemming ----------------------------------------------------------------
install.packages("SnowballC")
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
m

findFreqTerms(dtm, lowfreq = 5)
# Remove words that appear in less than 70% of documents
dtms <- removeSparseTerms(dtm, 0.7)
dtms

# Word Associations -------------------------------------------------------
# Want correlations >=0.75
findAssocs(dtm, c("data", "analysis", "methods", "mining"), corlimit = 0.75)


# Word Frequency Plots ----------------------------------------------------
library(ggplot2)
wf <- data.frame(word=names(freq), freq=freq)
wf
p <- ggplot(subset(wf, freq>5), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

# Word Cloud --------------------------------------------------------------
library(wordcloud)
dtms<-removeSparseTerms(dtm, 0.7)
freq <- colSums(as.matrix(dtm)) # word frequencies
dark2 <- brewer.pal(12, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors = dark2)

# Cluster Dendogram -------------------------------------------------------

install.packages("")
library("cluster")
library("fpc")

dtms <- removeSparseTerms(dtm, 0.8)
d <- dist(t(dtms), method="euclidian")
d
fit <- hclust(d=d, method = "ward.D2")
plot(fit, hang=-1)
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=4)
rect.hclust(fit, k=4, border="red")

# K-means clustering ------------------------------------------------------

dtms <- removeSparseTerms(dtm, 0.75)
d <- dist(t(dtms), method="euclidean")
kfit <- kmeans(d,4)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
plotcluster(d, kfit$cluster)
