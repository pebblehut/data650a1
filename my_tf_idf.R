# Trying to figure out tf-idf

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


# subset just the terms with the highest tf-idf value ---------------------

# First, add the tf-idf totals as an additional row
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
m.tf.idf.transpose2
# m.tf.idfe <- t(m.tf.idf)
