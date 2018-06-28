options(stringsAsFactors = FALSE)
install.packages("tm")
install.packages("httr")
install.packages("devtools")
install.packages("twitteR")
install.packages("base64enc")
install.packages("party")
install.packages("topicmodels")
install.packages("wordcloud")

library(plyr)

library(NLP)
library(tm)
library(httr)
library(devtools)
library(twitteR)
library(base64enc)
library(sandwich)
library(party)
library(topicmodels)
library(RColorBrewer)
library(wordcloud)

#================authentication======#
api_key<-"HfCNuZezF8aYK6wOT3YPto9j4"
api_secret<-"ASQKUyjN1HQOvxbXFsEVbeEG6KjQlfkJW4M7rcF541Bly9d7aH"
access_token<-"777562847098314752-jgfcz5rL2hVHpjs2gJTTw4AIkQlOXN2"
access_token_secret<-"Wja8YdF3fC33ZferwRfQENFv4bTmKeqN28g9FhzLQsMzV"
setup_twitter_oauth(consumer_key = api_key,
                    consumer_secret = api_secret,
                    access_token = access_token,
                    access_secret = access_token_secret)
#============downloading the text=========#
NBA=searchTwitter("@NBA",n=100,lang="en")#n is for o. tweets
#=================================#
head(NBA)#
tail(NBA)
length(NBA)
NBA_text=sapply(NBA,function(x) x$getText())

tweet.df<-twListToDF(NBA)#list to data frame
Created<-tweet.df$created
Created
#=======================preprocessing text==========#
NBA_text=gsub("RT","",NBA_text)#first para. pattern replace by second para. of nba_text 
NBA_text=gsub("@\\w+","",NBA_text)
NBA_text=gsub("[[:punct:]]","",NBA_text)
NBA_text=gsub("http\\w+","",NBA_text)
NBA_text=gsub("[|\t]{2,}","",NBA_text)#replace 2 or more than two tab
NBA_text=gsub("^ ","",NBA_text)
NBA_text=gsub("$","",NBA_text)

#============================================#
#============wordcloud=======================#
NBAcorpus=Corpus(VectorSource(NBA_text))
NBAcorpus=tm_map(NBAcorpus,function(x)removeWords(x,stopwords()))#converting into text to apply further transformation
#==========================================#
#create term document matrix form corpus

tdm=TermDocumentMatrix(NBAcorpus,control = list(removePunctuation=TRUE,
                                                stopwords=c("new","year",stopwords("english")),
                                                removeNumbers=TRUE,
                                                tolower=TRUE))#tdm gives terms in row and document in column
#convert as matrix
m=as.matrix(tdm)
#get word count in decreasing order
word_freqs=sort(rowSums(m),decreasing = TRUE)
#create dataframe with word and frequencies
dm=data.frame(word=names(word_freqs),freq=word_freqs)
#plot wordcloud
wordcloud(dm$word,dm$freq,random.order=FALSE ,
          colors = brewer.pal(8,"Dark2"))

#==============hierarchical clustering==========#
mystopwords<-c(stopwords('english'),"http","and","yes","no")
NBAcorpus=tm_map(NBAcorpus,removeWords,mystopwords)
NBAdtm=TermDocumentMatrix(NBAcorpus)
NBAterms=findFreqTerms(NBAdtm,lowfreq = 10)
NBAdtm1=removeSparseTerms(NBAdtm,sparse = 0.95)
NBAfit=hclust(dist(scale(NBAdtm1)),method="ward.D")
plot(NBAfit)

#=======================================#
#===========topic modeling=============#
NBAdtm2=scale(NBAdtm1)
#nbadtm2
NBAdtm2=DocumentTermMatrix(NBAcorpus)

