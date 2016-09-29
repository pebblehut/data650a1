# Example test case for corruption of IDs in corpuses
library("tm")


myTxt <- Corpus(DirSource("part1ds"))
# This was from the tm package documentation:
meta(myTxt[[1]], "id")

# All of these steps work fine
str(myTxt)
dtm <- DocumentTermMatrix(myTxt)
str(dtm)


# Now use gsub to do a find and replace:

for(j in seq(myTxt))
{
  myTxt[[j]] <-gsub("/", " ", myTxt[[j]])
}

# For this command I get an error
meta(myTxt[[1]], "id")
str(myTxt)
dtm <- DocumentTermMatrix(myTxt)
str(dtm)

myTxt <- tm_map(myTxt, PlainTextDocument)
meta(myTxt[[1]], "id")

