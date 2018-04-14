library(twitteR)
library(ROAuth)
library(RCurl)
library(bit)
library(tm)
library(wordcloud)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

consumerKey <- "0041slvrblDuTIv9PVjX6SdFu"
consumerSecret <- "NFjc3WR68zAAU6oo1EN8k3SFCMVV8bdzyASaofaUqOj6dPOKe4"

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL = reqURL, accessURL = accessURL,
                             authURL = authURL)

download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "cacert.pem")

twitCred$handshake(cainfo = "cacert.pem")
1510561
3028718

accessToken <- "66104619-lHYvX2U1XHzwYXpDzy31UUEveyX18RVmFFM2ppmRg"
accessTokenSecret <-"Lz4GZOuNYPWlDTWX14mFlBEoe3jV7RfySeTT0CKF5UViE"

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#rdmTweets<-userTimeline("imvkohli",n=200)
rdmTweets<-searchTwitter("#trump",n=500)
rdmTweets

class(rdmTweets)

rdmTweets[[100]]$text

dfTweets<-twListToDF(rdmTweets)

getwd()

write.csv(dfTweets,"vkohli_tweets.csv")

#create a corpus from the tweets
myCorpus <- Corpus(VectorSource(dfTweets$text))
class(myCorpus)

myCorpus[[1]]$content


removeSpecial<-function(x) gsub("[^[:alnum:]]"," ",x)
myCorpus<-tm_map(myCorpus,removeSpecial)
myCorpus[[168]]$content


myCorpus<-tm_map(myCorpus,tolower) #lower case

myCorpus<-tm_map(myCorpus,removePunctuation) #remove punctuation

myCorpus<-tm_map(myCorpus,removeNumbers) #remove Numbers

removeURL <- function(x) gsub("http[[:alnum:]]*"," ",x)
myCorpus <- tm_map(myCorpus, removeURL)

#stopwords - in, on, the, before, after, to, at

myStopWords <- c(stopwords('english'),"paaji","anushka")
#myStopWords<-setdiff(myStopWords,c("r"))

myCorpus<-tm_map(myCorpus,removeWords,myStopWords)

#myCorpus<-tm_map(myCorpus,PlainTextDocument)

myTDM<-TermDocumentMatrix(myCorpus,control=list(wordlengths=c(1,Inf)))

class(myTDM)

checkMatrix<-as.data.frame(as.matrix(myTDM))

write.csv(checkMatrix,"checkMatrix.csv")


findFreqTerms(myTDM,lowfreq = 5)

termFrequency<-rowSums(as.matrix(myTDM))
termFrequency<-subset(termFrequency,termFrequency >= 5)

barplot(termFrequency)

findAssocs(myTDM, "india", 0.25 )

#wordcloud
m<-as.matrix(myTDM)
wordFreq<-sort(rowSums(m),decreasing = TRUE)
set.seed(375)
graylevels<-gray((wordFreq+10)/(max(wordFreq)+10))
wordcloud(words=names(wordFreq),freq = wordFreq, min.freq = 2,
          random.order = FALSE, colors = rainbow(50))

#Sentiment Analysis
getwd()
setwd("C:/R")
list.files()

pos<-readLines("positive_words.txt")
neg<-readLines("negative_words.txt")

names(wordFreq)

pos.matches<-match(names(wordFreq),pos)
neg.matches<-match(names(wordFreq),neg)

pos.matches
neg.matches

pos.matches<-!is.na(pos.matches)
neg.matches<-!is.na(neg.matches)

#final score

final_score<-sum(pos.matches) - sum(neg.matches)
final_score
