dtms <- removeSparseTerms(dtm, 0.8)
dtms
dtms <- removeSparseTerms(dtm99, 0.5)
dtms


ndocs <- length(dtm)
# For all of these, ignore overly sparse terms (appearing in more than one document)
minDocFreq <- ndocs * 0.01
# ignore terms appearing in all of the documents
# ignore overly common terms (appearing in all of the documents)
maxDocFreq <- ndocs * 0.99
dtm99_0<- DocumentTermMatrix(docs, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
dtm99_0                       

frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
words <- names(frequency)


# Plotting Correlations ---------------------------------------------------


http://handsondatascience.com/TextMiningO.pdf
install.packages("Rgraphviz")
# Not available for R 3.3.1
library(Rgraphviz)
plot(dtm,
     terms=findFreqTerms(dtm, lowfreq=100)[1:50],
     corThreshold=0.5)