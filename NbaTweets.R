library(tm)
library(wordcloud)
library(lattice)
library(car)

## PART 1 - COLLECTION
##

## EnsurePackage tests if the package is installed using require() function
## if the package is not installed (require() returns FALSE)
## it installs the package
EnsurePackage = function(x) {
  x=as.character(x)
  if (!require(x, character.only=TRUE)) {
    install.packages(pkgs=x, repos="http://cran.r-project.org")
    require(x, character.only=TRUE)
  }
}
##

## PrepareTwitter makes sure that the packages
## are loaded into R
PrepareTwitter=function() {
  EnsurePackage("bitops")
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  EnsurePackage("twitteR")
  EnsurePackage("ROAuth")
}

PrepareTwitter()

## this is my work directory
## you need to change it to Desktop or whatever folder you are using
## on your own computer or on drive P:
setwd("Desktop/QAC211/final")

## the consumer key and consumer secret are saved in the file twt.txt
## first line is consumer key
## second line is consumer secret
a = readLines("tw.txt")

## this function will download the newest copy of the
## security certificates file
download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="cacert.pem")

## credential is an object that stores information
## about authorization
credential = OAuthFactory$new(
  consumerKey=a[1] ,
  consumerSecret=a[2],
  accessURL="https://api.twitter.com/oauth/access_token",
  authURL="https://api.twitter.com/oauth/authorize",
  requestURL="https://api.twitter.com/oauth/request_token")


## handshake() is a method defined for the object OAuthFactory
## this is where you will need to copy a URL, open it in a browser
## authorize the user and obtain the PIN code
credential$handshake(cainfo="cacert.pem")

## this makes sure that our Twitter requests
## use the authorization object that we received
registerTwitterOAuth(credential)

## the tweet search function is userTimeline()
##
## THIS IS WHERE YOU WILL ENTER YOUR SEARCH STRING!

#pull in new data

for (i in 1:10) {
  print(i)
  if (nrow(d2) == 0) {
    tweetList = userTimeline("NBA", n=500, cainfo="cacert.pem")
    d2 = do.call("rbind", lapply(tweetList, as.data.frame))
  } else {
    
    ## m_id is the smallest (i.e. earliest) status ID number retrieved minus 1
    m_id = as.numeric(d2$id[ nrow(d2) ]) - 1
    tweetList = userTimeline("NBA", n=500, maxID=m_id,
                             cainfo="cacert.pem")
    d3 = do.call("rbind", lapply(tweetList, as.data.frame))
    d2 = rbind(d2,d3)
  }
}

## append tweets 

col_types = c("character",  ## text
              "logical",    ## favorited
              "numeric",    ## favoriteCount
              "character",  ## replyToSN
              "character",  ## created
              "logical",    ## truncated
              "character",  ## replyTOSID
              "character",  ## id
              "character",  ## replyToUID
              "character",  ## statusSource
              "character",  ## screenName
              "numeric",    ## retweetCount
              "logical",    ## isRetweet
              "logical",    ## retweeted
              "character",  ## longitude
              "character")  ## latitude


f_name = "nba_tweets.csv"

if (file.exists(f_name)) {
  
  d2$created = as.character(d2$created)
  
  d1 = read.csv(file=f_name, header=TRUE,
                stringsAsFactors=FALSE, colClasses = col_types)
  
  d1 = rbind(d1,d2)
  
  write.csv(x=d1, file=f_name, row.names=FALSE)
  
} else {
  print("file does not exist yet")
  d2$created = as.character(d2$created)
  write.csv(x=d2, file=f_name, row.names=FALSE)
}

## PART 2 - Word cloud for tweets AS IS 

f = read.csv(file="nba_tweets.csv",header=TRUE,stringsAsFactors=FALSE)
#cols = text, datetime,ID, # of retweets
cols = c(1,5,8,12)
g = f[,cols]
tweets = f$text

#Example of top 20 tweets 
top20 = read.csv("nba_tweets copy.csv", stringsAsFactors=FALSE, header = TRUE)
top20tweets = top20$text
#combine tweets for initial wordcloud
## tweets = paste(g$text, collapse="") 

## remove URLs 
tweets = gsub("http://t.co/[a-z,A-Z,0-9]{8,9}", "", tweets)
tweets = gsub("https://t.co/[a-z,A-Z,0-9]{8,9}", "", tweets)
tweets = gsub("amp","",tweets)

## create a corpus out of a vector of tweets 
tweetCorpus = Corpus(VectorSource(tweets))

## remove punctuation and convert to lower case 
tweetCorpus = tm_map(tweetCorpus, removePunctuation)
tweetCorpus = tm_map(tweetCorpus, content_transformer(tolower))

## remove stop words
tweetCorpus = tm_map(tweetCorpus, function (x) removeWords(x, stopwords()))

##word cloud 
wordcloud(tweetCorpus, max.words=200, scale=c(4,1))

# PART 3 - get the time 
a = strptime(f$created, "%Y-%m-%d %H:%M:%S")
b = as.numeric(format(a, "%H"))
t_seg = ifelse((b>=13 & b<17), 1, ifelse((b>=17 & b<21), 2,
               ifelse((b>=21 | b<1), 3, ifelse((b>=1 & b<5), 4, 
               ifelse((b>=5 & b<9), 5, 6)))))
x = factor(t_seg)
plot(x,xlab = "Time of Day", ylab = "# of retweets",main="Distribution of tweets in day", 
     col = rgb(255,102,102, maxColorValue=255))

#plot time vs retweets
plot(f$retweetCount~x)
mod = lm(f$retweetCount ~ x)
summary(mod)

#PART 4 - the keywords that we think have predictive power are in the keywords.csv file
##
kw = read.csv("keywords.csv", stringsAsFactors=FALSE, header=TRUE)

## TermDocumentMatrix counts the incidence of words from our dictionary
## the dictionary is made of the keywords from the file
t_matrix1 = DocumentTermMatrix(tweetCorpus, list(dictionary=kw$Keyword[kw$type == "person"]))
t_matrix1 = as.matrix(t_matrix1)
players = rowSums(t_matrix1, na.rm=TRUE)
hist(players, 
     col=rgb(79, 121, 66, 180, maxColorValue=255))
histogram(players, 
          col=rgb(0.3, 0.5, 0.4, 0.5),
          main = "Histogram with relative frequency")

t_matrix2 = DocumentTermMatrix(tweetCorpus, list(dictionary=kw$Keyword[kw$type == "team"]))
t_matrix2 = as.matrix(t_matrix2)
team = rowSums(t_matrix2, na.rm=TRUE)
hist(team,
     col=rgb(79, 121, 66, 180, maxColorValue=255))
histogram(team,
           col=rgb(0.3, 0.5, 0.4, 0.5),
          main = "Histogram with relative frequency")

m = lm(f$retweetCount ~ players + x)
summary(m)
plot(m, col=rgb(79, 121, 66, 180, maxColorValue=255) )

boxplot(x)