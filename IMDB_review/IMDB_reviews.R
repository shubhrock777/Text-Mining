library(rvest)
library(XML)
library(magrittr)

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# IMDBReviews #############################
aurl <- "https://www.imdb.com/title/tt0468569/reviews?ref_=tt_urv"
IMDB_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)



write.table(IMDB_reviews,"the_dark_night.txt")


##################################
#### Sentiment Analysis ####
txt <- IMDB_reviews

str(txt)
length(txt)
View(txt)


library(tm)

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x[1])
inspect(x[2])


x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))


# Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

inspect(x1[5])

x1 <- tm_map(x1, removeNumbers)
inspect(x1[1])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm) # transpose
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 


tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20, 1:20]

inspect(x[1])

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 100)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term phone repeats maximum number of times
x1 <- tm_map(x1, removeWords, c('its','two','some','would','there','wait','then','such'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

# Bar plot after removal of the term 'phone'
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 100)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

##### Word cloud #####

library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# better visualization
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors= rainbow(30),scale=c(3,0.5),rot.per=0.3)


windowsFonts(JP1 = windowsFont("MS Gothic"))
par(family = "JP1")
wordcloud(x1, scale= c(2,0.5))


############# Wordcloud2 ###############


library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.3, shape='circle')


wordcloud2(w1, size=0.3, shape = 'triangle')
wordcloud2(w1, size=0.3, shape = 'star')


#####################################
# lOADING Positive and Negative words  
pos.words <- readLines(file.choose())	# read-in positive-words.txt
neg.words <- readLines(file.choose()) 	# read-in negative-words.txt

stopwdrds <-  readLines(file.choose())

### Positive word cloud ###
pos.matches <- match(names(w_sub1), pos.words)
pos.matches <- !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
names <- names(freq_pos)
windows()
wordcloud(names, freq_pos, scale=c(4,1), colors = brewer.pal(8,"Dark2"))


### Matching Negative words ###
neg.matches <- match(names(w_sub1), neg.words)
neg.matches <- !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
names <- names(freq_neg)
windows()
wordcloud(names, freq_neg, scale=c(4,.5), colors = brewer.pal(8, "Dark2"))


