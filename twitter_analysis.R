# Twitter Data Analysis
# This R script allows users to perform retweet network analysis, structural topic modelling (STM), hashtag analysis, and text analysis on a Twitter dataset.

# Clean the environment
rm(list = ls())

# Set working directory
# Update this path to your working directory
# setwd("/path/to/your/directory")

# Load required libraries
# --- For Data Handling and Wrangling ---
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# --- Text Processing and Analysis ---
library(tidytext)
library(quanteda)
library(tm)
library(tokenizers)
library(stopwords)
library(SentimentAnalysis)
library(DT)

# --- Statistical Modelling and Topic Modelling ---
library(lavaan)
library(stm)
library(topicmodels)

# --- Social Media APIs and Network Analysis ---
library(rtweet)
library(twitteR)
library(ROAuth)
library(graphTweets)
library(igraph)
library(tidygraph)
library(visNetwork)
library(quanteda.textplots)
library(vosonSML)
library(textnets)
library(gephi)

# --- Data Visualisation ---
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(gganimate)
library(plotly)
library(cowplot)
library(forcats)
library(graphlayouts)
library(ggraph)
library(formattable)
library(wordcloud)
library(networkD3)

# --- Miscellaneous Utilities ---
library(rvest)
library(syuzhet)
library(magrittr)
library(extrafont)
library(summarytools)
library(devtools)
library(stringi)
library(scales)

# --- How to login (if not using Twarc) ---

#Put your keys within the quote marks, copy them from the developer portal of Twitter https://developer.twitter.com/en/portal/dashboard
#Set up connection with Twitter developer app
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "" 
consumerSecret <- "" #Same as above
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,accessURL=accessURL,
                             authURL=authURL)
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
twitCred$handshake(cainfo="cacert.pem")

#Set up authorisation
options(httr_oauth_cache=T)
api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth (api_key,api_secret,access_token,access_token_secret)

# --- Set query from R (if not using third party collection interface)
q <- "#MeToo"
streamtime <- 60 * 60 * 24
filename <- "tweets.json"
Mar31 <- stream_tweets(q = q, timeout = streamtime, file_name = filename,parse = FALSE)

# --- Load data ---
# Replace these paths with your own datasets
tweets_data <- read.csv("your_dataset.csv")

# --- Cleaning data ---

# Change name of certain columns
colnames(tweets_data)[colnames(tweets_data) == "referenced_tweets.replied_to.id"] ="referenced_id"

# Subset the data to only include tweets containing a certain language eg. German ("de")
german_tweets <- tweets_data %>%
  filter(lang == "de")

# Filter location manually
tweets_data <- tweets_data %>%
  filter(author.location != "Here and there")

# Remove tweets containing certain authors or words
tweets_data <- tweets_data %>% 
  filter(!grepl('bitcoin', author.username)) #Replace column name to 'text' to search and remove tweets containing certain terms

# Change names of locations
tweets_data$author.location[tweets_data$author.location=="Italia"]<-"Italy"

# --- Descriptive analysis ---

#Total tweets
total_tweet_number <- tweets_data %>% 
  filter(!is_retweet) %>% 
  pull(status_id) %>% 
  unique() %>% 
  length()

# Total retweets
total_retweet_number <- tweets_data %>% 
  filter(is_retweet) %>% 
  pull(status_id) %>% 
  unique() %>% 
  length()

#Most retweeted tweets
tweets_data %>% 
  arrange(-retweet_count) %>%
  slice(10) %>% #Adjust value
  select(created_at, screen_name, text, retweet_count)

# Top author locations
tweets_data %>%
  count(author.location, sort = TRUE) %>%
  filter(author.location != " ") %>%
  mutate(author_location = reorder(author.location, n)) %>%
  na.omit() %>%
  top_n(10) #Adjust value

# Unique Twitter accounts in the sample
length(unique(tweets_data$screen_name))

#top tweeters
tweets_data %>% 
  count(author.username, sort = TRUE) %>%
  top_n(10) %>% #Adjust value
  mutate(author.username = paste0("@", author.username))

# How many tweets by language - create pie chart with slice labels
plotdata <- tweets_data %>%
  filter(lang=="de" | lang== "pl" | lang=="it") %>%
  count(lang) %>%
  arrange(desc(lang)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

plotdata$lang <- as.character(plotdata$lang)
plotdata$lang[plotdata$lang == "pl"] <- "Polish"
plotdata$lang[plotdata$lang == "it"] <- "Italian"
plotdata$lang[plotdata$lang == "de"] <- "German"

plotdata$label <- paste0(plotdata$lang, "\n", plotdata$n, "\n",
                         round(plotdata$prop), "%")
png("LanguageProportion.png", width=11, height=6, units="in", res=720)
ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = lang)) +
  geom_bar(width = 1, 
           stat = "identity", color = NA) +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "white", family="Arial Black") +
  coord_polar("y", 
              start = 0) +
  scale_fill_manual(values=c("#5886a5", "#004c6d", "#9dc6e0"), labels = c("Germany", "Italy", "Poland"))+
  theme_tufte() +
  theme(legend.position = "FALSE", text = element_text(family="mono", face="bold", size=12)) +
  labs(title = "Total tweets: 53,365")
dev.off()

# --- Retweet/Quote/Reply Networks ---

# Add original tweets author's name based on user ID
users_info <- lookup_users(as_userid(tweets_data$retweeted_user_id)) # Replace with quoted_user_is for quote networks and in_reply_to_user_id for reply networks
users_info$user_id <- as.double(users_info$user_id) #Convert variable class in case of errors
tweets_data <- tweets_data %>%
  left_join(
    select(users_info, retweeted_user_id = user_id, screen_name),
    by = "retweeted_user_id"
  )
# Create retweet network graph
retweet_graph <- tweets_data %>%
  filter(public_metrics.retweet_count > 0) %>%
  select(author.username, screen_name) %>%
  unnest(screen_name) %>%
  filter(!is.na(screen_name)) %>%
  graph_from_data_frame()

# Summarise the retweet graph
summary(retweet_graph)

# Export the graph for Gephi
write_graph(simplify(retweet_graph),  "retweet_network.gml", format = "gml")

# --- Friends networks of top users --- Mutual followers among the 100 most retweeted users

#Create subset of 100 most tweeted users
topusers <- tweets_data %>% 
  arrange(-retweet_count) %>%
  distinct(author.username, .keep_all = TRUE) %>%
  slice(1:100)
topusers %>% select(author.username) %>% unique() %>%  arrange(author.username)  %>% DT::datatable()
users_info <- lookup_users(topusers$author.username)
users_info %>% arrange(-followers_count) %>% head()
users_info %>% select(screen_name) %>% unique() %>%  arrange(screen_name)  %>% DT::datatable()

# Calculate percentile-based scores for various metrics (followers, friends, etc.)
users_ranking <- users_info %>%
  filter(protected == FALSE) %>%
  select(screen_name, followers_count, friends_count, listed_count, favourites_count, statuses_count) %>%
  mutate(
    followers_percentile = ecdf(followers_count)(followers_count),
    friends_percentile = ecdf(friends_count)(friends_count),
    listed_percentile = ecdf(listed_count)(listed_count),
    favourites_percentile = ecdf(favourites_count)(favourites_count),
    statuses_percentile = ecdf(statuses_count)(statuses_count)
  ) %>%
  group_by(screen_name) %>%
  summarise(top_score = sum(followers_percentile, friends_percentile, listed_percentile, favourites_percentile, statuses_percentile)) %>%
  ungroup() %>%
  mutate(ranking = rank(-top_score))

# Select the top 30 users based on their calculated scores
top_30 <- users_ranking %>% arrange(desc(top_score)) %>% head(30)
top_30_users_ranking <- top_30 %>% select(screen_name) %>% DT::datatable()

# Save the ranking of top 30 users to an HTML and PNG file
saveWidget(top_30_users_ranking, "users_ranking.html")
webshot("users_ranking.html", "top_30_users_ranking.png")

# Get friends list for each top user in groups to avoid hitting rate limits
usernames <- users_info$screen_name

# Extract friends in batches of 15 users at a time to avoid rate limit
extract_friends_batch <- function(start_index, end_index) {
  Sys.sleep(15 * 60)  # Wait for 15 minutes to avoid rate limit
  friends_batch <- purrr::map(usernames[start_index:end_index], get_friends)
  names(friends_batch) <- usernames[start_index:end_index]
  return(friends_batch)
}

# Extract friends for all 100 users in batches
friends_top100 <- c(
  extract_friends_batch(1, 15),
  extract_friends_batch(16, 30),
  extract_friends_batch(31, 45),
  extract_friends_batch(46, 60),
  extract_friends_batch(61, 75),
  extract_friends_batch(76, 90),
  extract_friends_batch(91, 100)
)

# Save the friends list to a CSV file
write.list(z = friends_top100, file = "friends_top100.csv", t.name = NULL, row.names = FALSE)

# Merge the friends list with the top 100 user data
friends_top100_df <- map2_df(friends_top100, names(friends_top100), ~ mutate(.x, twitter_top_user = .y)) %>%
  rename(friend_id = user_id) %>%
  filter(friend_id %in% users_info$user_id) %>%
  mutate(friend_name = users_info$screen_name[match(friend_id, users_info$user_id)])

# Convert to an igraph object
network <- graph_from_data_frame(friends_top100_df, directed = TRUE)

# Add "Popularity" (degree centrality) as a node attribute
V(network)$Popularity <- degree(network, mode = 'in')

# Create a tidygraph object for easier plotting
tg <- as_tbl_graph(network) %>%
  activate(nodes) %>%
  mutate(label = name)

# Calculate Eigenvector Centrality to determine influence
eigenCent <- evcent(network)$vector
colorVals <- rev(heat.colors(30))[cut(eigenCent, 30, labels = FALSE, include.lowest = TRUE)]

# Plot the network
png("Top100_MutualFriendsNetwork.png", width = 18, height = 12, units = "in", res = 720)
tg %>%
  ggraph(layout = "stress") +
  geom_edge_diagonal(alpha = .2, color = 'white') +
  geom_node_point(size = log(V(network)$Popularity) * 2, color = colorVals) +
  geom_node_text(aes(label = label, size = log(V(network)$Popularity)), repel = TRUE) +
  labs(
    title = "Influential Mutual Friends: Do the Most Retweeted Users Follow Each Other?",
    subtitle = "Popularity (node size) and influence (node color) among top 100 most retweeted users in the dataset."
  ) +
  theme_graph() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
dev.off()

# --- User-hashtag networks ---

# Function to create user-hashtag network: users connected by shared hashtag usage
create_user_hashtag_network <- function(data, network_name, min_hashtag_freq = 5, min_user_hashtags = 3, min_edge_weight = 2) {
  # Extract hashtags for each user
  user_hashtags <- data %>%
    mutate(hashtags = str_extract_all(text, "#\\w+")) %>%
    unnest(hashtags) %>%
    select(User.Name, hashtags) %>%
    distinct()  # Remove duplicates if a user uses the same hashtag multiple times
  
  # Filter hashtags by frequency
  frequent_hashtags <- user_hashtags %>%
    count(hashtags) %>%
    filter(n >= min_hashtag_freq) %>%
    pull(hashtags)
  
  # Filter user_hashtags to only include frequent hashtags
  user_hashtags <- user_hashtags %>%
    filter(hashtags %in% frequent_hashtags)
  
  # Filter users by the number of unique hashtags they have used
  active_users <- user_hashtags %>%
    group_by(User.Name) %>%
    summarise(hashtag_count = n_distinct(hashtags), .groups = "drop") %>%
    filter(hashtag_count >= min_user_hashtags) %>%
    pull(User.Name)
  
  # Filter user_hashtags to only include active users
  user_hashtags <- user_hashtags %>%
    filter(User.Name %in% active_users)
  
  # Create edges between users who share hashtags
  edges <- user_hashtags %>%
    inner_join(user_hashtags, by = "hashtags") %>%
    filter(User.Name.x != User.Name.y) %>%  # Avoid self-loops
    group_by(User.Name.x, User.Name.y) %>%
    summarise(weight = n(), .groups = "drop") %>%  # Count shared hashtags
    filter(weight >= min_edge_weight) %>%  # Only keep strong edges
    distinct()  # Remove duplicate edges
  
  # Ensure each pair is unique and undirected
  edges <- edges %>%
    mutate(User1 = pmin(User.Name.x, User.Name.y),
           User2 = pmax(User.Name.x, User.Name.y)) %>%
    select(User1, User2, weight) %>%
    distinct()
  
  # Create graph from the edges
  graph <- graph_from_data_frame(edges, directed = FALSE)
  
  # Add node attribute for total hashtags used by each user
  nodes <- user_hashtags %>%
    group_by(User.Name) %>%
    summarise(hashtag_count = n(), .groups = "drop") %>%
    rename(Id = User.Name)
  
  # Export edges and nodes to CSV for Gephi
  write_csv(edges, paste0(network_name, "_edges.csv"))
  write_csv(nodes, paste0(network_name, "_nodes.csv"))
  
  return(graph)
}

# Example usage for Spain and Italy networks
user_hashtag_network <- create_user_hashtag_network(
  tweets_data, 
  "user_hashtag_network", 
  min_hashtag_freq = 5, 
  min_user_hashtags = 3, 
  min_edge_weight = 3
)

# --- User co-occurrence network

#Extract most frequently mentioned usernames
user_dfm <- dfm_select(tweet_dfm, pattern = "@*")
user_top <- names(topfeatures(user_dfm, 100))
head(user_top)

#Construct feature-occurrence matrix of usernames
user_top_fcm <- fcm(user_top)
head(user_top_fcm)

#plot
user_top_fcm <- fcm_select(user_top_fcm, pattern = user_top)
jpeg("Top100UserCoccurence.jpeg", width=18, height=12, units="in", res=720)
textplot_network(user_top_fcm, min_freq = 0.4, edge_color = "mediumpurple3", edge_alpha = 0.8, edge_size = 3)
dev.off()

# --- Two-mode networks ---

# Prepare tweet corpus by each author to prepare discourse network. Nodes will be authors, with edges drawn according to the overlap of words used in their tweets
twomode_data <- tweets_data %>% select(text, author.username) %>% group_by(author.username) 
twomode_data <- textnets::PrepText(twomode_data, groupvar = "author.username", textvar = "text", node_type = "groups", tokenizer = "tweets", strip_url = TRUE, pos = "nouns", remove_stop_words = TRUE, compound_nouns = FALSE)
twomode_network <- CreateTextnet(twomode_data)
graph_centrality <- TextCentrality(twomode_network) #Analyse centrality
graph_communities <- TextCommunities(twomode_network) #Louvain community detection algorithm
VisTextNet(twomode_network, label_degree_cut = 0) #Visualise network
gephi_write_edges(twomode_network, "edges.csv") #Or export for Gephi

# --- Structural Topic Modelling (STM) ---
# Convert the tweet texts into a corpus and perform topic modelling
tweet_corpus <- corpus(tweets_data$text)
tweet_dfm <- tokens(tweet_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE)
tweet_dfm <- tokens_remove(tweet_dfm, stopwords("english")) #Replace with language of tweet corpus
tweet_dfm <- dfm(tweet_dfm)
tweet_dfm <- dfm_trim(tweet_dfm, max_docfreq = 0.50, min_docfreq = 0.01, docfreq_type = 'prop') #Adjust frequencies

# Convert to STM-compatible format
stm_data <- quanteda::convert(tweet_dfm, to = "stm", docvars=tweets_data)

# Run STM model with 20 topics
stm_model <- stm(
  documents = stm_data$documents,
  vocab = stm_data$vocab,
  data = stm_data$meta,
  init.type = 'Spectral',
  prevalence = ~author.public_metrics.followers_count,
  K = 20,
  verbose = FALSE
)

# Save the STM topic model plot
png("Top_Topic_Model.png", width=16, height=10, units="in", res=300)
plot(stm_model, n = 10)
dev.off()

# Correlation between STM topics
cor <- topicCorr(stm_model, verbose = TRUE)
png("CorrelationBetweenTopics.png", width=12, height=10, units="in", res=300)
plot(cor)
dev.off()

# FindThoughts - find tweets carrying these topics
thoughts <- findThoughts(stm_model, texts = newgercorpus, n = 5, topics = 10) #Adjust values of n for number of tweets and topics for number of topics from which to pull these tweets
png("ExampleTweets.png", width=10, height=12, units="in", res=300)
plot(thoughts)
dev.off()

# --- Sentiment analysis ---

# Calculate sentiment score
score <- get_sentiment(tweet_corpus, method="syuzhet", language = "german") #Change according to your language
sentiments_df <- data.frame(score)
text <- VectorSource(tweet_corpus$text)
timestamp <- as.POSIXct(sapply(text, function(x)x$getCreated()), origin="1970-01-01", tz="GMT")
plot_ly(sentiments_df, x=~timestamp, y=~score, type="scatter", mode="jitter", name="syuzhet") %>%
  add_trace(y=~bing, mode="lines", name="bing") %>%
  add_trace(y=~afinn, mode="lines", name="afinn") %>%
  add_trace(y=~nrc, mode="lines", name="nrc") %>%
  layout(title="Sentiment scores",
         yaxis=list(title="score"), xaxis=list(title="date"))

# Distribution of emotions in tweets
emotions <- get_nrc_sentiment(tweet_corpus, language = "german") #Change according to your language
emo_bar <-  colSums(emotions)
emo_sum <-  data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion <-  factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
colnames(emo_sum) <- c("WordCount","Emotions")

png("Emotions.png", width=25, height=13, units="in", res=720) #Visualise
emo_sum %>%
  ggplot(aes(x = reorder(Emotions, WordCount), y = WordCount, fill=Emotions)) +
  geom_col() +
  scale_fill_manual(values = harrypotter(n=10,  alpha = .5, option = "Gryffindor", direction=-1), name = "")+
  labs(x="Emotions", y="Word Count",
       caption = "Data collected from Twitter API for Academic Research",
       title = "Distribution of emotions in tweets")+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0))+
  theme(plot.title = element_text(family = "Gill Sans", size = 28, face = "bold", hjust = 0.1, vjust = 1.2),
        axis.text = element_text(family = "Arial", size = 18),
        axis.title.x =  element_text(family = "Helvetica", size = 18, face = "bold", vjust = -1),
        axis.title.y =  element_text(family = "Helvetica", size = 18, face = "bold"),
        plot.caption = element_text(size = 12, face = "italic", hjust = 1.1),
        legend.title = element_text(family = "Helvetica", size = 18, face = "bold"),
        legend.text = element_text(family = "Arial", size = 18),
        panel.background = element_blank())
dev.off()

# --- Textual Analysis ---

# Most Retweeted Tweets
most_retweeted <- tweets_data %>% 
  arrange(-public_metrics.retweet_count) %>% #Can do the same for likes count, quotes count
  slice(1:20) %>% 
  select(text, public_metrics.retweet_count, date, author.username, author.description)
write.csv(most_retweeted, "most_retweeted.csv")

# Hashtag Analysis
extract.hashes = function(vec) {
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  hash.matches = gregexpr(pattern = hash.pattern, text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("hashtag", "freq")
  df = df[order(df$freq, decreasing = TRUE),]
  return(df)
}

# Top 50 hashtags from the dataset
hashtags <- head(extract.hashes(tweets_data$text), 50) #Adjust number to select desired number of top hashtags
write.csv(hashtags, "top_hashtags.csv")

# Alternative method to count top tweets
tweets_data %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#")) %>%
  count(hashtag, sort = TRUE)%>%
  top_n(50) #Adjust value

# Function to check if a tweet starts with @username  to capture replies and add a column containing the username if that is the case
starts_with_mention <- function(text) {
  # Allow for any whitespace or punctuation before @username
  str_detect(text, "^[\\s.,;:()-]*@[A-Za-z0-9_]+")
}

tweets_data <- tweets_data %>%
  mutate(
    starts_with_at = starts_with_mention(text),
    first_word = word(str_trim(text), 1)
  ) 

tweets_data <- tweets_data %>%
  mutate(
    starts_with_at = starts_with_mention(text),
    first_word = word(str_trim(text), 1)
  ) 

# N-grams Analysis
tweet_bigrams_data <- data.frame("texts" = as.character(tweets_data$text), stringsAsFactors = FALSE)
tweet_bigrams_data <- as.data.frame(removeWords(tweet_bigrams_data$texts, stopwords("english")))

# Unigram analysis
unigrams <- tweet_bigrams_data %>%
  unnest_tokens(output = unigram, input = texts, token = "ngrams", n = 1)

unigram_count <- unigrams %>%
  group_by(unigram) %>%
  summarise(n = n()) %>%
  mutate(freq = formattable::percent(n / sum(n))) %>%
  top_n(100) %>%
  arrange(desc(n))

write.csv(unigram_count, "unigrams.csv")

# Bigram and Trigram analysis can be performed similarly.

# --- Time Series Analysis ---
tweets_data$date <- as.POSIXct(tweets_data$created_at, tz = "GMT", format = "%Y-%m-%d")
png("timeseries.png", width=7, height=8, units="in", res=720)
tweets_data %>%
  mutate(Date = as_date(date)) %>%
  group_by(Date) %>%
  summarise(count = n_distinct(text)) %>%
  ggplot(aes(x = Date, y = count)) +
  geom_line() +
  labs(title = "Number of Tweets Over Time", x = "Date", y = "Number of Tweets") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'dashed', colour = "grey"),
        plot.caption = element_text(size = 7.5, face = "italic", color = "dark grey"))
dev.off()




# --- End of Script ---
# This script can be used to replicate the analysis on various datasets by replacing the data source.
# Ensure that file paths, data formats, and variables are adjusted according to the specific dataset being used.
