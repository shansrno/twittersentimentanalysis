#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("tidytext")
install.packages("textdata")
#install.packages("devtools")
#install.packages("widyr")
#install.packages("tidyverse")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(textdata)
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")
library(widyr)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
--------------------------------------------------------------------------------------------------------------------------
#read tweets.csv data set 
rawtweets <- read.csv("/Users/shannenserrano/Google Drive/tweets.csv", header = T, sep = ",")

#review data set
head(rawtweets)
str(rawtweets)
summary(rawtweets)

#change text data type from character to string


--------------------------------------------------------------------------------------------------------------------------
#EXPLORATORY ANALYSIS w/ text | favoriteCount | retweetCount
--------------------------------------------------------------------------------------------------------------------------
#Tweets & FavoriteCount
  
tweetsfav <- rawtweets[c("text", "favoriteCount")]  

favorite <- tweetsfav %>%
  top_n(10) %>%
  arrange(desc(favoriteCount)) %>%
  select(text, favoriteCount)

#Top 10 Favourited Tweets
favorite_bp <- ggplot(data=favorite, aes(x=reorder(text, -favoriteCount), y=favoriteCount))+
  geom_bar(stat="identity", fill="darkred") + 
  labs(title= "Top 10 Favourited Tweets") +
  scale_x_discrete(label=abbreviate) + 
  xlab("Tweets") 
  theme(text = element_text(angle=90, vjust =0.5, hjust=1)) +
  theme(plot.title = element_text(angle=0)) +
  theme(axis.title.x = element_text(angle=0, vjust=0.5, hjust=1))

favorite_bp

--------------------------------------------------------------------------------------------------------------------------
#Tweets & RetweetCount  

#for retweetCount, I realized that the dataset contained multiple duplicate tweets as they were 
#retweeted. Moreover, all these retweet shared the same amount of retweetCount so I decided to delete
#all the duplicate tweets.
#read tweet dataset with no duplicates  
  
tweetsrt_raw <- read.csv("/Users/shannenserrano/Google Drive/tweets_noduplicates.csv", header = T, sep = ",")
tweetsrt <-tweetsrt_raw[c("text", "retweetCount")]

#Top 10 Retweeted Tweets
retweet <- tweetsrt %>%
  arrange(desc(retweetCount)) %>%
  top_n(10) %>%
  select(text, retweetCount)

retweet_bp <- ggplot(data=retweet, aes(x=reorder(text, -retweetCount), y=retweetCount)) +
  geom_bar(stat="identity", fill="darkgreen") + 
  labs(title= "Top 10 Retweeted Tweets") +
  scale_x_discrete(label=abbreviate) + 
  xlab("Tweets") +
  theme(text = element_text(angle=90, vjust =0.5, hjust=1)) +
  theme(plot.title = element_text(angle=0))+
  theme(axis.title.x = element_text(angle=0, vjust=0.5, hjust=1))

retweet_bp
--------------------------------------------------------------------------------------------------------------------------
#PREPROCESSING w/ text
--------------------------------------------------------------------------------------------------------------------------
  
#tweetsdf <- data.frame(rawtweets$text)
#names(tweetsdf)[names(tweetsdf) == "rawtweets.text"] <- "tweets"

tweetsdf <- read.csv("/Users/shannenserrano/Google Drive/tweetstext.csv", header = T, sep = ",")
names(tweetsdf)[names(tweetsdf) == "text"] <- "tweets"

#remove URLs
tweetsdf$stripped_text <- gsub("http.*","", tweetsdf$tweets)
tweetsdf$stripped_text <- gsub("https.*","", tweetsdf$stripped_text)

#remove punctuation, convert to lowercase, add id for each word 
clntweets0 <- tweetsdf %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

#count words
clntweets0 %>% count(word, sort = T) %>% head #top words: RT, the, u, a, to...
nrow(clntweets0) #212636 #39964

#graph clntweets0
clntweets0 %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  labs(x = "Unique Words",
       y = "Count",
       title = "Count of Unique Words Found in #AvengersEndgame - Top 25")

#remove stop words: the, y, a, to...
data("stop_words") #load list of stop words
clntweets1 <- clntweets0 %>% anti_join(stop_words) #apply stop words to tweets
nrow(clntweets1) #124287 #22929

#count words
clntweets1 %>% count(word, sort = T) %>% head

--------------------------------------------------------------------------------------------------------------------------
#UNIVARIATE ANALYSIS
--------------------------------------------------------------------------------------------------------------------------
#Unique word count - Unigram 
clntweets1 %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = "#99CCFF")+
  xlab(NULL)+
  coord_flip()+
  labs(x = "Unique Words",
       y = "Count",
       title = "Count of Unique Words Found in #AvengersEndgame - Top 25")

clntweets1 %>%
  count(word) %>%
  wordcloud(word, n, mac.words=100)

--------------------------------------------------------------------------------------------------------------------------
#Unique word count - Bigram 
clntweets2 <- clntweets1 %>%
  dplyr::select(word) %>%
  unnest_tokens(word_pair, word, token = "ngrams", n=2)

clntweets2 %>%
  count(word_pair, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word_pair, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = "#66B2FF")+
  xlab(NULL)+
  coord_flip()+
  labs(x = "Unique Word-Pairs",
       y = "Count",
       title = "Count of Unique Word-Pairs Found in #AvengersEndgame - Top 25")

--------------------------------------------------------------------------------------------------------------------------
#Unique word count - Trigram 
clntweets3 <- clntweets1 %>%
  dplyr::select(word) %>%
  unnest_tokens(word_trio, word, token = "ngrams", n=3)

clntweets3 %>%
  count(word_trio, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word_trio, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = "#0080FF")+
  xlab(NULL)+
  coord_flip()+
  labs(x = "Unique Word-Trios",
       y = "Count",
       title = "Count of Unique Word-Trios Found in #AvengersEndgame - Top 25")

--------------------------------------------------------------------------------------------------------------------------
#SENTIMENT LEXICON ANALYSIS - labelling the tokens with sentiment
--------------------------------------------------------------------------------------------------------------------------

#BING AND NRC SENTIMENT LEXICON

get_sentiments("bing") %>%
  count(sentiment)

get_sentiments("nrc") %>%
  filter(sentiment %in% c(
    "positive",
    "negative"
  )) %>%
  count(sentiment) 

#BING          | #NRC
#negative 4781 | #negative 3324
#positive 2005 | #positive 2312

#both sentiment lexicons have more negative sentiment than positive.
--------------------------------------------------------------------------------------------------------------------------
#BING
#positive
bing_positive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

clntweets1 %>%
  inner_join(bing_positive) %>%
  count(word, sort = TRUE)

#negative
bing_negative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

clntweets1 %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

#full list of positive and negative words w/ sentiment
bing <- rbind(bing_positive, bing_negative)

#bing consolidated
bing_poscnt <- count(bing_positive)
bing_negcnt <- count(bing_negative)

bing_pos_neg <- c("positive", "negative")
bing_pos_neg_cnt <- rbind(bing_poscnt, bing_negcnt)

bing_pos_neg_count <- data.frame(sentiment=bing_pos_neg,
                                 count=bing_pos_neg_cnt)

bing_plot <- ggplot(data=bing_pos_neg_count, aes(x=sentiment, y=n)) +
  geom_bar(stat="identity", fill="grey") +
  labs(title="Bing: Positive vs Negative Sentiment Count") +
  xlab("Sentiment") +
  ylab("Count") 

bing_plot

--------------------------------------------------------------------------------------------------------------------------
#NRC
#positive
nrc_positive <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

clntweets1 %>%
  inner_join(nrc_positive) %>%
  count(word, sort = TRUE)

#negative
nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")

clntweets1 %>%
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)

#full list of positive and negative words w/ sentiment
nrc <- rbind(nrc_positive, nrc_negative)

#nrc consolidated
nrc_poscnt <- count(nrc_positive)
nrc_negcnt <- count(nrc_negative)

nrc_pos_neg <- c("positive", "negative")
nrc_pos_neg_cnt <- rbind(nrc_poscnt, nrc_negcnt)

nrc_pos_neg_count <- data.frame(sentiment=nrc_pos_neg,
                            count=nrc_pos_neg_cnt)

pos_neg_plot <- ggplot(data=nrc_pos_neg_count, aes(x=sentiment, y=n)) +
  geom_bar(stat="identity", fill="black") +
  labs(title="NRC: Positive vs Negative Sentiment Count") +
  xlab("Sentiment") +
  ylab("Count") 

pos_neg_plot

--------------------------------------------------------------------------------------------------------------------------
#WORDCLOUD: BING VS NRC
--------------------------------------------------------------------------------------------------------------------------

#most common positive and negative words
  
library(reshape2)

bing_wordcloud <- clntweets1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("red", "blue"),
    max.words = 100
  )
bing_wordcloud


nrc_wordcloud <- clntweets1 %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("red", "blue"),
    max.words = 100
  )
nrc_wordcloud

#FLAWS
#there are a couple of flaws in the sentiment lexicon labels
# 1. the main word in dataset is "Marvel" which shouldn't have a negative or positive sentiment because it is the title
# of the film and should have remained a neutral word in the analysis
# 2. cry/crying could be both negative and positive as people say ...
# 3. The sentiment lexicon misreads slang: "hype" is a positive term for excitement
# Through this analysis, I have learned that unigram sentiment analysis is not as accurate as it does not analyze
# the context of the words and the sentence structure for example wait is seen as a negative word but if in the context
# of "I can't wait..." then its sentiment is excitement/positive

--------------------------------------------------------------------------------------------------------------------------
# MACHINE LEARNING ALGORITHM
--------------------------------------------------------------------------------------------------------------------------

#ref above
#nrc <- rbind(nrc_positive, nrc_negative)

#SPLIT DATA INTO TRAINING AND TEST DATASETS
#train_index <- sample(1:nrow(yelp_data), 0.7*nrow(yelp_data))
train_index <- sample(1:nrow(nrc), 0.7*nrow(nrc))  

train_set <-
test_set <-

# NAIVE BAYES









