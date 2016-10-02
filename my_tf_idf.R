# Trying to figure out tf-idf

m <- as.matrix(dtm)
#m

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
m.idf[1:10]
m.backup <- m
ncol(m)
#m <-  m.backup
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m[i,j] <- m[i,j] * m.idf[j]
  }
}
m[ ,1:10]

m.backup[ ,1:10]
m[ ,1:10]
m.idf[1:10]
m.freq <- colSums(m)
m.freq[1:10]
m.backup.freq <- colSums(m.backup)
m.backup.freq[1:10]
