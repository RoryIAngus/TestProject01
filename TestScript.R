getwd()

library(tm)

#Create Corpus
docs <- Corpus(DirSource('~/TestProject01/Data/'))

docs

#inspect a particular document
writeLines(as.character(docs[[30]]))

#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")

#Remove punctuation – replace punctuation marks with " “
docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, " -")

#Transform to lower case (need to wrap in content_transformer)
docs <- tm_map(docs,content_transformer(tolower))

#Strip digits (std transformation, so no need for content_transformer)
docs <- tm_map(docs, removeNumbers)

#remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))

#Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)
               
writeLines(as.character(docs[[30]]))

#load library
library(SnowballC)
#Stem document
docs <- tm_map(docs,stemDocument)
writeLines(as.character(docs[[30]]))

docs <- tm_map(docs, content_transformer(gsub), pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")

dtm <- DocumentTermMatrix(docs)

dtm

inspect(dtm[1:2,1000:1005])

freq <- colSums(as.matrix(dtm))

#length should be total number of terms
length(freq)


#create sort order (descending)
ord <- order(freq,decreasing=TRUE)


#inspect most frequently occurring terms
freq[head(ord)]

#inspect least frequently occurring terms
freq[tail(ord)]


dtmr <-DocumentTermMatrix(docs, 
                          control=list(wordLengths=c(4, 20),
                                       bounds = list(global = c(3,27))))

dtmr


freqr <- colSums(as.matrix(dtmr))
#length should be total number of terms
length(freqr)

#create sort order (asc)
ordr <- order(freqr,decreasing=TRUE)
#inspect most frequently occurring terms
freqr[head(ordr)]

#inspect least frequently occurring terms
freqr[tail(ordr)]

findFreqTerms(dtmr,lowfreq=80)

findAssocs(dtmr,"project",0.6)


findAssocs(dtmr,"enterpris",0.6)


findAssocs(dtmr,"system",0.6)


wf=data.frame(term=names(freqr),occurrences=freqr)
library(ggplot2)
p <- ggplot(subset(wf, freqr>100), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqr),freqr, min.freq=70)


#…add color
wordcloud(names(freqr),freqr,min.freq=70,colors=brewer.pal(6,"Dark2"))








