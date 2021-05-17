
######### Amazon iphone 11 URL ###########
aurl <- "https://www.amazon.in/New-Apple-iPhone-11-64GB/product-reviews/B08L8DV7BX/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1"

amazon_reviews <- NULL


library(rvest)
library(XML)
library(magrittr)



for (i in 1:50){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".review-text") %>% html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}


write.table(amazon_reviews,"iphnoe_11.txt")
getwd()

##################################
#### Sentiment Analysis ####
txt <- amazon_reviews

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
x1 <- tm_map(x1, removeWords, c('iphone','amazon','please','also','apple','phone','can`t','told'))
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


#### Bigram ####
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

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


