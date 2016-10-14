library("twitteR")
library("httr")
library("tm")
library("wordcloud")
library("RColorBrewer")
library("RCurl")
library("ROAuth")

#Connection between RStudio and twitter

key <- "4aN0Mvc78BJvHG0EC7BM80su5"
secret <- "rHcFMOmCx6vh8jP7ZydovrtXjrehlQtd50FC0QR96va4ESoNxM"
secrettk <- "DzdYlNviNPg7VhZczj7l34luWAvkosZEzAgn1gWDI5ISC"
mytoken <- "3146945200-fD12d1UEfBAK5wx0IjRPIiJjPQ5RZ6nj9FxpBei"

setup_twitter_oauth(key,secret,mytoken,secrettk)



#scraping data from twitter.
burritotweets = searchTwitter("burrito",n = 1000, lang = "en",geocode='40.712940,-73.987920,5mi')


#using function getText to extract text part of tweets
burritolist <- sapply(burritotweets,function(x) x$getText())


# converting all the latin1 values to ASCII. These are basically the emoticons
burritolist <- sapply(burritolist,function(row) iconv(row, "latin1", "ASCII", sub = ""))





#Converting to corpus which is a text body consisting of all text along with the meta data.
burritocorpus <- Corpus(VectorSource(burritolist))




#Preprosessing the text data

burritocorpus <- tm_map(burritocorpus, removeNumbers)  #to remove numbers
burritocorpus <- tm_map(burritocorpus, removePunctuation) # to remove punctuations
burritocorpus <- tm_map(burritocorpus, tolower)   #converting everything to lowercase  
burritocorpus <- tm_map(burritocorpus,function(x)removeWords(x,c("burrito",stopwords()))) 
#removing meaningless words like I, you ,just etc.
#removing the word burrito to eliminate it from wordcloud

#converting to plain text document which is the input for making word cloud
burritocorpus <- tm_map(burritocorpus,PlainTextDocument)


#Making the wordcloud from the above plain text document.
col <- brewer.pal(5,"Dark2") 

wordcloud(burritocorpus, min.freq = 5,rot.per = 0.3, scale = c(5,1), random.color = T,max.word = 45, random.order = F, colors=col)



#Making a TermDocument Matrix
burritotdm <- TermDocumentMatrix(burritocorpus)

#to Find top 10 frequent terms used in the tweets
findFreqTerms(burritotdm, lowfreq = 10)


#to find terms which have correlation with the term chicken having correlation score more than 0.3
findAssocs(burritotdm,'chicken', 0.3)




#To create a dendogram for visualization.

burritotdm2 <- removeSparseTerms(burritotdm, sparse = 0.98)
#remove sparse (infrequentyl used) terms from the term Document matrix




burritotdm2scale <- scale(burritotdm2)  #to scale the data

burritodist <- dist(burritotdm2scale, method = 'euclidean') # to create a distance matrix

burritofit <- hclust(burritodist) #Hierarchical clustering

plot(burritofit) # Visualization the results

cutree(burritofit, k = 7) # to calculate certain number of groups

rect.hclust(burritofit, k=7, border = "red") #Colour the groups with border for better visialization.







#-----------Targeting Influencers-------------------------------------------------------------


#using function to get usernames of of owner of the tweets
burritolistnames <- sapply(burritotweets,function(x) x$getScreenName())


#Scraping user details
users <- lookupUsers(burritolistnames)



#converting this list to dataframe
userprofiledf <- twListToDF(users)



#userprofiledf
#write.csv(userprofiledf,file = "C:/Users/Tanmay/Desktop/userprofiledf1.csv",row.names = F)


#sorting data on the variable 'followersCount'
sortedusers <- arrange(userprofiledf, desc(followersCount))


#getting top 10 influencers
x <- sortedusers[1:10,]
top10influencers <- x$screenName
top10influencers



#getting top 50 influencers
y <- sortedusers[1:50,]
top50influencers <- y$screenName
top50influencers





#-------------------Sentimental Analysis----------------------------------------------------





#importing positive and negative lexicon
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")


library("stringr")
library("plyr")
library("lattice")



#Function definition
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  #Parameters
  #Sentences : Vector of text to score
  #pos.words: Vector of words of positive sentiments
  #neg.words: vector of words of negative statement
  #.progress:control of progress bar
  
  
  scores = laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    # remove punctuation - using global substitute
                    sentence = gsub("[[:punct:]]", "", sentence)
                    # remove control characters
                    sentence = gsub("[[:cntrl:]]", "", sentence)
                    # remove digits
                    sentence = gsub('\\d+', '', sentence)
                    # define error handling function when trying tolower
                    tryTolower = function(x)
                    {
                      # create missing value
                      y = NA
                      # tryCatch error
                      try_error = tryCatch(tolower(x), error=function(e) e)
                      # if not an error
                      if (!inherits(try_error, "error"))
                        y = tolower(x)
                      # result
                      return(y)
                    }
                    # use tryTolower with sapply
                    sentence = sapply(sentence, tryTolower)
                    # split sentence into words with str_split (stringr package)
                    word.list = str_split(sentence, "\\s+")
                    words = unlist(word.list)
                    # compare words to the dictionaries of positive & negative terms
                    pos.matches = match(words, pos.words)
                    neg.matches = match(words, neg.words)
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    pos.matches = !is.na(pos.matches)
                    neg.matches = !is.na(neg.matches)
                    # final score
                    score = sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}





#getting tweets for three big burrito restaurants
qdobatweets = searchTwitter("qdoba",n = 1000, lang = "en")
bolocotweets = searchTwitter("boloco",n = 1000, lang = "en")
chipotletweets = searchTwitter("chipotle",n = 1000, lang = "en")



#Extracting text out of tweets by using getText function
qdoba_txt = sapply(qdobatweets, function(x) x$getText())
boloco_txt = sapply(bolocotweets, function(x) x$getText())
chipotle_txt = sapply(chipotletweets, function(x) x$getText())


#converting latin1 characters to ASCII.
qdoba_txt <- sapply(qdoba_txt,function(row) iconv(row, "latin1", "ASCII", sub = ""))
boloco_txt <- sapply(boloco_txt,function(row) iconv(row, "latin1", "ASCII", sub = ""))
chipotle_txt <- sapply(chipotle_txt,function(row) iconv(row, "latin1", "ASCII", sub = ""))


#Getting number of tweets and putting into a integer matrix
nooftweets = c(length(qdoba_txt), length(boloco_txt), length(chipotle_txt))



#join texts
burritocompany = c(qdoba_txt, boloco_txt, chipotle_txt)



#applying function score.sentiment
scores = score.sentiment(burritocompany, pos, neg, .progress='text')



#add variable burritocompany ti the data frame
scores$burritocompany = factor(rep(c("qdoba", "boloco", "chipotle"), nooftweets))





head(scores)
par(bty="l")
boxplot(score~burritocompany, data=scores) #making a boxplot of sentiments


#creating a histogram of sentiments. We need lattice library for this
histogram(data=scores, ~score|burritocompany, main="Sentiment Analysis of 3 Mexican Companies",col = col, sub="Sentiment Score")



#--------Individual Sentimental Analysis-----------------------------------



#scraping data from twitter.
burritotweets = searchTwitter("burrito",n = 1000, lang = "en",geocode='40.712940,-73.987920,5mi')


#using function getText to extract text part of tweets
burritolist <- sapply(burritotweets,function(x) x$getText())


# converting all the latin1 values to ASCII. These are basically the emoticons
burritolist <- sapply(burritolist,function(row) iconv(row, "latin1", "ASCII", sub = ""))

#applying function score.sentiment
scores = score.sentiment(burritolist, pos, neg, .progress='text')

#to calculate global sentiment score
scores$very.pos = as.numeric(scores$score >= 2) #adding varablevery.pos
scores$very.neg = as.numeric(scores$score <= -2)# adding variable very.neg

#to calculate numberof very positives and number of very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)


#to calculate global score
global_score = round( 100 * numpos / (numpos + numneg) )


#---------------------Sentiment analysis for predicitng stock--------------------------------

#getting tweets for three big phone restaurants
appletweets = searchTwitter("apple",n = 1000, lang = "en")
blackberrytweets = searchTwitter("blackberry",n = 1000, lang = "en")
htctweets = searchTwitter("htc",n = 1000, lang = "en")
googletweets = searchTwitter("google",n = 1000, lang = "en")



#Extracting text out of tweets by using getText function
apple_txt = sapply(appletweets, function(x) x$getText())
blackberry_txt = sapply(blackberrytweets, function(x) x$getText())
htc_txt = sapply(htctweets, function(x) x$getText())
google_txt = sapply(googletweets, function(x) x$getText())


#converting latin1 characters to ASCII.
apple_txt <- sapply(apple_txt,function(row) iconv(row, "latin1", "ASCII", sub = ""))
blackberry_txt <- sapply(blackberry_txt,function(row) iconv(row, "latin1", "ASCII", sub = ""))
htc_txt <- sapply(htc_txt,function(row) iconv(row, "latin1", "ASCII", sub = ""))
google_txt <- sapply(google_txt,function(row) iconv(row, "latin1", "ASCII", sub = ""))


#Getting number of tweets and putting into a integer matrix
nooftweets = c(length(apple_txt), length(blackberry_txt), length(htc_txt),length(google_txt))



#join texts
phonecompany = c(apple_txt, blackberry_txt, htc_txt,google_txt)



#applying function score.sentiment
scores = score.sentiment(phonecompany, pos, neg, .progress='text')



#add variable phonecompany ti the data frame
scores$phonecompany = factor(rep(c("apple", "blackberry", "htc","google"), nooftweets))




#creating a histogram of sentiments. We need lattice library for this
histogram(data=scores, ~score|phonecompany, main="Sentiment Analysis of 4 phone Companies",col = "red", sub="Sentiment Score")


