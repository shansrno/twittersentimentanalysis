#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidytext")
#install.packages("textdata")
#install.packages("devtools")
#install.packages("widyr")
#install.packages("tidyverse")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
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
library(reshape2)
--------------------------------------------------------------------------------------------------------------------------
#read tweets.csv data set 
rawtweets <- read.csv("/Users/shannenserrano/Google Drive/tweets.csv", header = T, sep = ",")

#review data set
head(rawtweets)
str(rawtweets)
summary(rawtweets)

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
  xlab("Tweets") +
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

# FINAL - remove neutral words - Marvel Avengers specific terms that should be neutral
clntweets_final <- clntweets1 %>%
  filter(str_detect(word, 'marvel', negate = TRUE)) %>%
  filter(str_detect(word, 'critics', negate = TRUE)) %>%
  filter(str_detect(word, 'stark', negate = TRUE)) %>%
  filter(str_detect(word, 'captain', negate = TRUE)) %>%
  filter(str_detect(word, 'infinity', negate = TRUE)) %>%
  filter(str_detect(word, 'intervention', negate = TRUE)) %>%
  filter(str_detect(word, 'stone', negate = TRUE)) %>%
  filter(str_detect(word, 'war', negate = TRUE)) %>%
  filter(str_detect(word, 'avenger', negate = TRUE)) %>%
  filter(str_detect(word, 'iron', negate = TRUE)) %>%
  filter(str_detect(word, 'black', negate = TRUE)) %>%
  filter(str_detect(word, 'enforcement', negate = TRUE))

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
# REVISE BING AND NRC LABELS AS PER INITIAL ANALYSIS CONCLUSIONS
--------------------------------------------------------------------------------------------------------------------------

# make a dataframe for BING lexicon so we can revise the lexicon
bing <- get_sentiments("bing")

# edit incorrectly labelled words
bing$sentiment <- as.character(bing$sentiment)
bing$sentiment[bing$word == 'hype'] <- 'positive'

bing$sentiment <- as.character(bing$sentiment)
bing$sentiment[bing$word == 'crazy'] <- 'positive'

bing$sentiment <- as.character(bing$sentiment)
bing$sentiment[bing$word == 'cry'] <- 'positive'

bing$sentiment <- as.character(bing$sentiment)
bing$sentiment[bing$word == 'lost'] <- 'positive'

bing$sentiment <- as.character(bing$sentiment)
bing$sentiment[bing$word == 'fucking'] <- 'positive'

--------------------------------------------------------------------------------------------------------------------------

# make a dataframe for NRC lexicon so we can revise the lexicon
nrc <- get_sentiments("nrc") %>%
  filter(sentiment %in% c(
    "positive",
    "negative"))

# edit incorrectly labelled words
nrc$sentiment <- as.character(nrc$sentiment)
nrc$sentiment[nrc$word == 'crying'] <- 'positive'

nrc$sentiment <- as.character(nrc$sentiment)
nrc$sentiment[nrc$word == 'cry'] <- 'positive'

nrc$sentiment <- as.character(nrc$sentiment)
nrc$sentiment[nrc$word == 'hype'] <- 'positive'

nrc$sentiment <- as.character(nrc$sentiment)
nrc$sentiment[nrc$word == 'lost'] <- 'positive'

nrc$sentiment <- as.character(nrc$sentiment)
nrc$sentiment[nrc$word == 'wait'] <- 'positive'

--------------------------------------------------------------------------------------------------------------------------
# BING ANALYSIS
--------------------------------------------------------------------------------------------------------------------------
#positive
bing_positive <- bing %>% 
  filter(sentiment == "positive")

tweet_bing_positive <- clntweets_final %>%
  inner_join(bing_positive) %>%
  count(word, sort = TRUE)

#positive - distribution of words
tweet_bing_positive_graph <- tweet_bing_positive %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = "Navy") +
  labs(x = "Words",
       y = "Count",
       title = "Bing: Top 10 Positive Words")

tweet_bing_positive_graph

--------------------------------------------------------------------------------------------------------------------------
#negative
bing_negative <- bing %>% 
  filter(sentiment == "negative")

tweet_bing_negative <- clntweets_final %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

#negative - distribution of words
tweet_bing_negative_graph <- tweet_bing_negative %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = "Dark Red")+
  labs(x = "Words",
       y = "Count",
       title = "Bing: Top 10 Negative Words")

tweet_bing_negative_graph

--------------------------------------------------------------------------------------------------------------------------
# BING positive and negative count
  
#full list of positive and negative words w/ sentiment
bing_full <- rbind(tweet_bing_positive, tweet_bing_negative)

#bing consolidated
bing_poscnt <- sum(tweet_bing_positive$n)
bing_negcnt <- sum(tweet_bing_negative$n)

bing_pos_neg <- c("positive", "negative")
bing_pos_neg_cnt <- c(bing_poscnt, bing_negcnt)

bing_pos_neg_count <- data.frame(sentiment=bing_pos_neg,
                                 count=bing_pos_neg_cnt)

#bing plot
bing_plot <- ggplot(data=bing_pos_neg_count, aes(x=sentiment, y=count)) +
  geom_bar(stat="identity", fill=c("navy", "dark red")) +
  labs(title="Bing: Positive vs Negative Sentiment Count") +
  xlab("Sentiment") +
  ylab("Count") 

bing_plot

--------------------------------------------------------------------------------------------------------------------------
# NRC ANALYSIS
--------------------------------------------------------------------------------------------------------------------------
#positive
nrc_positive <- nrc %>% 
  filter(sentiment == "positive")

tweet_nrc_positive <- clntweets_final %>%
  inner_join(nrc_positive) %>%
  count(word, sort = TRUE)

#positive - distribution of words
tweet_nrc_positive_graph <- tweet_nrc_positive %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = "Navy")+
  labs(x = "Words",
       y = "Count",
       title = "NRC: Top 10 Positive Words")

tweet_nrc_positive_graph

--------------------------------------------------------------------------------------------------------------------------
#negative
nrc_negative <- nrc %>% 
  filter(sentiment == "negative")

tweet_nrc_negative <- clntweets_final %>%
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)

#negative - distribution of words
tweet_nrc_negative_graph <- tweet_nrc_negative %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col(fill = "Dark Red")+
  labs(x = "Words",
       y = "Count",
       title = "NRC: Top 10 Negative Words")

tweet_nrc_negative_graph

--------------------------------------------------------------------------------------------------------------------------
# NRC positive and negative count
  
#full list of positive and negative words w/ sentiment
nrc <- rbind(tweet_nrc_positive, tweet_nrc_negative)
  
#nrc consolidated
nrc_poscnt <- sum(tweet_nrc_positive$n)
nrc_negcnt <- sum(tweet_nrc_negative$n)

nrc_pos_neg <- c("positive", "negative")
nrc_pos_neg_cnt <- c(nrc_poscnt, nrc_negcnt)

nrc_pos_neg_count <- data.frame(sentiment=nrc_pos_neg,
                                 count=nrc_pos_neg_cnt)

#nrc plot
nrc_plot <- ggplot(data=nrc_pos_neg_count, aes(x=sentiment, y=count)) +
  geom_bar(stat="identity", fill=c("navy", "dark red")) +
  labs(title="NRC: Positive vs Negative Sentiment Count") +
  xlab("Sentiment") +
  ylab("Count") 

nrc_plot

--------------------------------------------------------------------------------------------------------------------------
  #WORDCLOUD: BING VS NRC
--------------------------------------------------------------------------------------------------------------------------
  
#Top 100 most common positive and negative words in Tweets
  
bing_wordcloud <- clntweets_final %>%
 inner_join(bing) %>%
 count(word, sentiment, sort = TRUE) %>%
 acast(word ~ sentiment, value.var = "n", fill = 0) %>%
 comparison.cloud(
    colors = c("red", "blue"),
    max.words = 100
  )
bing_wordcloud

nrc_wordcloud <- clntweets_final %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("red", "blue"),
    max.words = 100
  )
nrc_wordcloud

--------------------------------------------------------------------------------------------------------------------------

#NRC emotions

anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger") 

ang_cnt <- clntweets_final %>%
  inner_join(anger) %>%
  count(word, sort = TRUE)


anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticpation")

ant_cnt <- clntweets_final %>%
  inner_join(anticipation) %>%
  count(word, sort = TRUE)


disgust <- get_sentiments("nrc") %>% 
  filter(sentiment == "disgust")

dis_cnt <- clntweets_final %>%
  inner_join(disgust) %>%
  count(word, sort = TRUE)


fear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")
fea_cnt <- clntweets_final %>%
  inner_join(fear) %>%
  count(word, sort = TRUE)


joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

joy_cnt <- clntweets_final %>%
  inner_join(joy) %>%
  count(word, sort = TRUE)


sadness <- get_sentiments("nrc") %>% 
  filter(sentiment == "sadness")

sad_cnt <- clntweets_final %>%
  inner_join(sadness) %>%
  count(word, sort = TRUE)


surprise <- get_sentiments("nrc") %>% 
  filter(sentiment == "surprise")

sur_cnt <- clntweets_final %>%
  inner_join(surprise) %>%
  count(word, sort = TRUE)


trust <- get_sentiments("nrc") %>% 
  filter(sentiment == "trust")

tru_cnt <- clntweets_final %>%
  inner_join(trust) %>%
  count(word, sort = TRUE)

nrc <- rbind(tweet_nrc_positive, tweet_nrc_negative)

#emo consolidated
ang_sum <- sum(ang_cnt$n)
ant_sum <- sum(ant_cnt$n)
dis_sum <- sum(dis_cnt$n)
fea_sum <- sum(fea_cnt$n)
joy_sum <- sum(joy_cnt$n)
sad_sum <- sum(sad_cnt$n)
sur_sum <- sum(sur_cnt$n)
tru_sum <- sum(tru_cnt$n)

emotionlabels <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
emotionsum <- c(ang_sum, ant_sum, dis_sum, fea_sum, joy_sum, sad_sum, sur_sum, tru_sum)

emotion_df <- data.frame(sentiment=emotionlabels,
                                count=emotionsum)

#emotions plot
emo_plot <- ggplot(data=emotion_df, aes(x=sentiment, y=count)) +
  geom_bar(stat="identity") +
  labs(title="NRC: Emotion Sentiment Count") +
  xlab("Sentiment") +
  ylab("Count") 

emo_plot
  