########## Packages #################
#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("httr")
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("RCurl")
#install.packages("syuzhet")
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(tm)
library(RCurl)
library(syuzhet)

oauth_endpoint(authorize="https://api.twitter.com/oauth",
               access="https://api.twitter.com/oauth/access_token")
#connect to API
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'
##### Authentication parameters##############
consumer_key <- "consumer_key"
consumer_secret <- "consumer_secret"
access_token <- "access_token"
access_secret <- "access_secret"
##########App Authentication ################
setup_twitter_oauth(consumer_key=consumer_key, consumer_secret=consumer_secret, access_token =access_token, access_secret = access_secret )
####### Find trending topics###########
trend_locations=availableTrendLocations()
trend_country=subset(trend_locations,country=="India")
View(trend_locations)
head(trend_country)
#woeid is 32 bit unique identifier assigned to each city across the world
# Get City's woeid

city_woeid = subset(trend_country, name == "Mumbai")$woeid
country_woeid=trend_country$woeid
# Get trending topics in city

trends = getTrends(woeid=city_woeid)
trends_country = getTrends(woeid=country_woeid)

head(trends)
View(trends)
View(trends_country)
##### Tweet Extraction#########
some_tweets = searchTwitter("mumbai", n=1000, lang= "en")
# Explore Tweets

length.some_tweets <- length(some_tweets)
length.some_tweets
some_tweets.df <- ldply(some_tweets, function(t) t$toDataFrame())
write.csv(some_tweets.df, "tweets.csv")
some_txt = sapply(some_tweets, function(x) x$getText())
#Cleaning 1 :removing ppl name,RT text
# Cleaning 1-  remove people name, RT text etc. 

some_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",some_txt)
some_txt1


# Cleaning 2- remove html links
some_txt2 = gsub("http[^[:blank:]]+", "", some_txt1)

# Cleaning 3- remove people names

some_txt3 = gsub("@\\w+", "", some_txt2)

# Cleaning 4- remove Punctuations 

some_txt4 = gsub("[[:punct:]]", " ", some_txt3)

# Cleaning 5- remove alphanumeric words

some_txt5 = gsub("[^[:alnum:]]", " ", some_txt4)

# Exporting to Excel

write.csv(some_txt5, "tweets1.csv")

# Creating wordcorpus and cleaning

some_txt6 <- Corpus(VectorSource(some_txt5))

some_txt6 <- tm_map(some_txt6, content_transformer(tolower))

some_txt6 <- tm_map(some_txt6, removeWords, stopwords("english"))

# Building wordcloud

pal <- brewer.pal(8,"Dark2")

wordcloud(some_txt6, min.freq = 5,  max.words = Inf, width=1000, height =1000,  random.order = FALSE, color=pal)



sample_sentiment = get_nrc_sentiment("Yesterday it was raining heavily so I could not go for football")
get_nrc_sentiment("I bought an iPhone a few days ago. It is such a nice phone, although a little large. The touch screen is cool.The voice quality is clear too. I simply love it!")

bing_lexicon = get_sentiments("bing")
View(bing_lexicon)


mysentiment <- get_nrc_sentiment(some_txt5)
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")


install.packages("coreNLP")
devtools::install_github("statsmaths/coreNLP")
coreNLP::downloadCoreNLP()

# Load the Library
library(coreNLP)

# Initialize CoreNLP
initCoreNLP()


sentence = "the sun did not shine, it was too wet to play. So we sat in the house all that cold, cold, wet day"
output = annotateString(sentence)
sentiment = getSentiment(output)

View(sentiment)
