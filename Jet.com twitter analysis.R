library("SnowballC")
library("twitteR")
library("syuzhet")
library("ROAuth")
library("base64enc")
library("openssl")
library("httpuv")
library("dplyr")
library("ggplot2")
library("wordcloud")
library("dplyr")
library("tidytext")
library("tidyr")
library("stringr")


#Invoke Twitter API
consumer_key <- 'm2etGFP8mDt46zrxdd5YcsUp3'
consumer_secret <- 'DcyxuGx24GlruzPQNh0gu6OtmOTyeCYR4ye593ZaiZRPLw6TGd'
access_token <- '2817226470-LvFh8gVbttM4qf5ee0YcU6pbrpneCjswaW54hpM'
access_secret <- 'pdwKXpWeXwHcbWuMQmI3JRRBbnqV10vX2foeBDXGgrehV'

twitteR:::setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#Pulling latest 2000 tweets from Jet's 'customer service' twitter handle - @JetHeads 
tweets <- userTimeline("JetHeads", n=2000)
n.tweet <- length(tweets) 


#Summarizing information about tweets in a data frame
tweets.df <- twListToDF(tweets) 

head(tweets.df)
head(tweets.df$text)


#Cleaning the tweets for further analysis
tweets.df2 = gsub("&amp", "", tweets.df$text)
tweets.df2 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df2)
tweets.df2 = gsub("@\\w+", "", tweets.df2)
tweets.df2 = gsub("[[:punct:]]", "", tweets.df2)
tweets.df2 = gsub("[[:digit:]]", "", tweets.df2)
tweets.df2 = gsub("http\\w+", "", tweets.df2)
tweets.df2 = gsub("[ \t]{2,}", "", tweets.df2)
tweets.df2 = gsub("^\\s+|\\s+$", "", tweets.df2) 
tweets.df2 = gsub("[^0-9A-Za-z///' ]","", tweets.df2)


head(tweets.df2) #Cleaned list of tweets contaning only english alphabets


#Converting datatype to vectors
word.df <- as.vector(tweets.df2)


#Using 'Syuzhet' to break the senitimets into 10 different emotions
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets.df2, emotion.df) 

#Creating the binary output to a data frame 
head(emotion.df2)
a <- sum(emotion.df2$anger)
b <- sum(emotion.df2$anticipation)
c <- sum(emotion.df2$disgust)
d <- sum(emotion.df2$fear)
e <- sum(emotion.df2$joy)
f <- sum(emotion.df2$sadness)
g <- sum(emotion.df2$surprise)
h <- sum(emotion.df2$trust)
i <- sum(emotion.df2$positive)
j <- sum(emotion.df2$negative)

emotion.df3 <- data.frame(a,b,c,d,e,f,g,h,i,j)

colnames(emotion.df3) <- c("anger", "aniticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust",
                           "positive", "negative")
emotion.df3


#extracting sentiment score for each tweet
sent.value <- get_sentiment(word.df)
sent.value

#Pulling out the most positive tweet
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

#Pulling out the most negative tweet
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative

#Sentiment score plot
plot(sent.value, type = "l")
abline(h=0, col="red")


#Segregating positive, negative and neutral tweets
positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)


#Reformatting all tweets in 3 categories
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)
table(category_senti)



#Creating Positive and Negative tweets dataframe with customized stop words 
data <- data_frame(text = positive.tweets)
data2 <- data_frame(text = negative.tweets)
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = c("hey", "jet", "dm", "yo", "tyler", "nicole", "send",
                                                   "youve", "youre", "dont", "hear"),lexicon = "nrp"))



data("stop_words") # Remove stop_words

#tokenized each word in each line for Positive tweets
tidydata <- data %>% unnest_tokens(word, text) %>% 
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% custom_stop_words$word,
         str_detect(word, "[a-z]"))


#visualizing word ranking of Positive tweets
tidydata %>% top_n(25) %>% 
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() 

#Wordcloud Visualization of Positive tweets
tidydata %>% with(wordcloud(word, n, max.words = 30, random.order=FALSE, 
                            rot.per=0.30, 
                            use.r.layout=FALSE, 
                            colors=brewer.pal(8, "Dark2")))





#tokenized each word in each line for Negative tweets
tidydata <- data2 %>% unnest_tokens(word, text) %>% 
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% custom_stop_words$word,
         str_detect(word, "[a-z]"))


#visualizing word ranking of Negative tweets
tidydata %>% top_n(13) %>% 
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() 

#Wordcloud Visualization of Negative tweets
tidydata %>% with(wordcloud(word, n, max.words = 50, random.order=FALSE, 
                            rot.per=0.30, 
                            use.r.layout=FALSE, 
                            colors=brewer.pal(8, "Dark2")))



#Exploring high frequency words in sentences

#For negative tweets - 'terribly' is mostly used in a sentence with 'Sorry'
Negative_word <-data2 %>% 
  filter(str_detect(text, "terribly"))
Negative_word






