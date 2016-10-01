
# install.packages("ggplot2")
library(ggplot2)

df <- data.frame(x = c(1:20,1:20),
  y = c(1/1:20,log(20/1:20)/log(20)),
  idf = c(rep("1/n",20),rep("log",20)))

  
  ggplot(data = df, aes(x,y)) + 
  geom_line(aes(color = idf)) + 
  geom_point(aes(color = idf)) + 
  scale_x_discrete() + 
  labs(title = "comparison of relative impact of idf-formula (scaled to 1) if term occurs in more or less documents") + 
  xlab("number of documents a term is contained in") + 
  ylab("") + 
  theme(axis.title.x = element_text(color="grey20"))
  
  
  # calculates tf-idf for different parameters and using
  # different tf-idf-versions
  tab_tfidf <- function(ncorpus=20) {
    # I assume a maximum word frequency of 4000
    max_ft <- 4000
    
    # tf-idf without log
    tfidf0 <- function(ft,max_ft,ndocs,ncorpus) (ft/max_ft) * (ncorpus/ndocs)
    
    # traditional tf-idf
    tfidf1 <- function(ft,max_ft,ndocs,ncorpus) (ft/max_ft) * log(ncorpus/ndocs)
    
    # tf-idf with added idf/N
    tfidf2 <- function(ft,max_ft,ndocs,ncorpus) (1/ncorpus + ft/max_ft) * log(ncorpus/ndocs)
    
    # ft = frequency of term / ndocs = how often it showed up in other documents
    df <- expand.grid(ft=c(5,10,20,30),ndocs=c(1,2,3,5,10))
    
    res0 <- apply(df,1,function(r) tfidf0(r["ft"],max_ft,r["ndocs"],ncorpus))
    ranks0 <- order(order(-res0))
    
    res1 <- apply(df,1,function(r) tfidf1(r["ft"],max_ft,r["ndocs"],ncorpus))
    ranks1 <- order(order(-res1))
    
    res2 <- apply(df,1,function(r) tfidf2(r["ft"],max_ft,r["ndocs"],ncorpus))
    ranks2 <- order(order(-res2))
    
    result <- cbind(df,res0,res1,res2,ranks0,ranks1,ranks2)
    result <- result[order(result$ft),]
    
    return(list("ncorpus" = ncorpus, "max_ft" = max_ft, result))
  }