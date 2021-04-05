#Loading and installing Required Packages
setwd("Path/Twitter")

############################################################################
# PACKAGES

#install.packages("corrplot")
#install.packages("plotly")
#install.packages("C50")
#install.packages("rtweet")
#install.packages("qdapRegex")
#install.packages("twitteR")
#install.packages('Rtools')
library(tidyverse)
library(rtweet)
library(qdapRegex)
library(plyr)
library(ggplot2)
library(SnowballC)
library(tm)
library(twitteR)
library(wordcloud)
library(syuzhet)
library(stringr)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(cluster)
library(wordcloud2)
library(corrplot)
library(rpart)
library(C50)
library(RWeka)
library(randomForest)


###########################################################################
# ANDROID

android_data<-read.csv(file.choose())
View(android_data)

#text cleaning
android_data$text=gsub("&amp", "", android_data$text)
android_data$text = gsub("&amp", "", android_data$text)
android_data$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", android_data$text)
android_data$text = gsub("@\\w+", "", android_data$text)
android_data$text = gsub("[[:punct:]]", "", android_data$text)
android_data$text = gsub("[[:digit:]]", "", android_data$text)
android_data$text = gsub("http\\w+", "", android_data$text)
android_data$text = gsub("[ \t]{2,}", "", android_data$text)
android_data$text = gsub("^\\s+|\\s+$", "", android_data$text)
android_data$text <- iconv(android_data$text, "UTF-8", "ASCII", sub="")

write.csv(android_data,'android_clean.csv')
getwd()

class(android_data)
View(android_data)
head(android_data)

unique<-length(unique(android_data$text))
unique

uniqueusers<-length(unique(android_data$from_user))
uniqueusers


summary(android_data$text)
glimpse(android_data)

#Sentiment Analysis
emotions <- get_nrc_sentiment(android_data$text)
head(emotions)

k<-get_sentiment(android_data$text)

most.positive <- android_data$text[k == max(k)]
most.positive

most.negative<-android_data$text[k==min(k)]
most.negative

emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #Android")
p

#create corpus
docs <- Corpus(VectorSource(android_data$text))

#text cleaning
docs = tm_map(docs, tolower)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english")))
docs = tm_map(docs, removeWords, c("ios"))
#docs = tm_map(docs, removeWords, c("apple"))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
#docs = tm_map(docs, stemDocument)

# create document term matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 10)

#
write.csv(d,'android_cleaner.csv')

#word frequencies
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#wordcloud
set.seed(9)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=200, random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(8, "Dark2"))
#d
wordcloud2(d, size = 0.7, shape = 'pentagon')
#wordcloud2(d, figPath = "Android.jpg", size = 1.5,color = "skyblue")
#letterCloud(d, word="A", size = 2)

################################################################################
# APPLE

apple_data<-read.csv(file.choose())
View(apple_data)

#text cleaning
apple_data$text=gsub("&amp", "", apple_data$text)
apple_data$text = gsub("&amp", "", apple_data$text)
apple_data$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", apple_data$text)
apple_data$text = gsub("@\\w+", "", apple_data$text)
apple_data$text = gsub("[[:punct:]]", "", apple_data$text)
apple_data$text = gsub("[[:digit:]]", "", apple_data$text)
apple_data$text = gsub("http\\w+", "", apple_data$text)
apple_data$text = gsub("[ \t]{2,}", "", apple_data$text)
apple_data$text = gsub("^\\s+|\\s+$", "", apple_data$text)
apple_data$text <- iconv(apple_data$text, "UTF-8", "ASCII", sub="")

write.csv(apple_data,'iPhone_clean.csv')
getwd()

class(apple_data)
View(apple_data)
head(apple_data)

apple_unique<-length(unique(apple_data$text))
apple_unique

apple_uniqueusers<-length(unique(apple_data$from_user))
apple_uniqueusers


summary(apple_data$text)

#Sentiment Analysis
emotions1 <- get_nrc_sentiment(apple_data$text)
head(emotions1)

k1<-get_sentiment(apple_data$text)

most.positive1 <- apple_data$text[k1 == max(k1)]
most.positive1

most.negative1<-apple_data$text[k1==min(k1)]
most.negative1

emo_bar1 = colSums(emotions1)
emo_sum1 = data.frame(count=emo_bar1, emotion=names(emo_bar1))
emo_sum1$emotion = factor(emo_sum1$emotion, levels=emo_sum1$emotion[order(emo_sum1$count, decreasing = TRUE)])

p1 <- plot_ly(emo_sum1, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #iPhone")

p1
emo_sum1$count
emo_sum1$emotion





#create corpus
docs1 <- Corpus(VectorSource(apple_data$text))

#text cleaning
docs1 = tm_map(docs1, tolower)
docs1 = tm_map(docs1, removePunctuation)
docs1 = tm_map(docs1, removeWords, c(stopwords("english")))
#docs1 = tm_map(docs1, removeWords, c("android","ios"))
docs1 = tm_map(docs1, removeWords, c("iphone"))
docs1 = tm_map(docs1, removeNumbers)
docs1 = tm_map(docs1, stripWhitespace)
#docs1 = tm_map(docs1, stemDocument)

# create document term matrix
dtm1 = TermDocumentMatrix(docs1)
m1 = as.matrix(dtm1)
v1 = sort(rowSums(m1),decreasing=TRUE)
d1 = data.frame(word = names(v1),freq=v1)
head(d1, 10)

#
write.csv(d1,'apple_cleaner.csv')


#word frequencies
barplot(d1[1:10,]$freq, las = 2, names.arg = d1[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#wordcloud
set.seed(9)
wordcloud(words = d1$word, freq = d1$freq, min.freq = 3,
          max.words=200, random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(8, "Dark2"))

wordcloud2(d1, size = 0.7, shape = 'circle')


##################################################################################

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")




##################################################################################
and = paste(d$word, collapse=" ")
iph = paste(d1$word, collapse=" ")

# put everything in a single vector
all = c(and, iph)

all = removeWords(all,c(stopwords("english"), "android", "iphone"))
# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

# add column names
colnames(tdm) = c("Android", "iPhone")
# comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("#00B2FF", "red"),
                 title.size=1.5, max.words=500)

#################################################################################

Animals <- read.table(
  header=TRUE, text='Device        Sentiments Appearances
1  Android    Positive      2730
2  iPhone     Positive      3295
3  iPhone     Trust      2336
4  Android    Trust      1518
5  Android    Anticipation       1849
6  iPhone     Anticipation       2008
7  Android    Surprise      796
8  iPhone     Surprise     1706
9  Android    Negative      1318
10  iPhone     Negative      2135
11  iPhone     Sadness      1567
12  Android    Sadness      625

15  Android    Fear      603
16  iPhone     Fear     754  ')

ggplot(Animals, aes(factor(Sentiments), Appearances, fill = Device)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")
