# Exploring Mitski's Discography Through Text Mining with R"

## Introduction

Mitski is a Japanese-American singer-songwriter based in the United States. Described by [*NPR*](https://www.npr.org/2018/09/06/642252679/mitski-is-the-21st-centurys-poet-laureate-of-young-adulthood) as "the 21st Century's Poet Laureate Of Young Adulthood," Mitski's lyrics skew towards feelings of lonesomeness and forlorning. A profile in [*The New Yorker*](https://www.newyorker.com/magazine/2019/07/08/on-the-road-with-mitski) describes the musician as one who "writes achingly intense songs about private yearnings."

Drawing upon Mitski's three studio release albums, I engage with the following questions: 

 - How do Mitski's three studio albums compare to one another?
 - What are the sentiments shared in Mitski's discography? 
 - How do the lyrics vary among Mitski's three studio albums? 
 
I seek to gain a  data-informed perspective on Mitski's lastest records, drawing upon both summary statistics and text analysis. In so being, I first retrieve and prepare song lyrics via the Genius package. Next, I analyze basic basic statistics about Mitski's three studio releases. I then conduct a sentiment analysis to analyze the lyrical content of Mitski's songs. Lastly, I create word clouds to visualize the common diction employed in Mitski's tracks.

## Retrieving the Data 

Genius is a media company that allows users and artists to annotate lyrics. I used [Josiah Parry's Github documentation](https://github.com/josiahparry/genius) to learn how to access Mitski's discography as text data via the Genius package.

I downloaded three separate data.frames for Mitski's three studio albums: "Be The Cowboy," "Puberty 2," and "Bury Me At Makeout Creek." 

```{r setup}
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)

# Clear memory
rm(list=ls())

# Load package
library(genius)

# Get songlyrics using Genius lyrics package
be_the_cowboy <- genius_album(artist = "Mitski", album = "Be the Cowboy")
puberty <- genius_album(artist = "Mitski", album = "Puberty 2")
bury_me <- genius_album(artist = "Mitski", album = "Bury Me At Makeout Creek")
```

## Creating a new column in df 

The Genius package includes four columns in their data.frames: `track_title`, `track_n`, `line`, and `lyric`. In the code below, I add a new `album` column in each data.frame.

```{r new_column, echo=TRUE, message=FALSE, warning=FALSE}
# Create new column named "album" filled with "NA"
be_the_cowboy["album"] <- NA 

# Replace NAs in "album" column with "Be The Cowboy"
be_the_cowboy$album <- "Be The Cowboy (2018)"  

# Repeat for other two dfs 
puberty["album"] <- NA  
puberty$album <- "Puberty 2 (2016)"

bury_me["album"] <- NA 
bury_me$album <- "Bury Me At Makeout Creek (2014)"
```

## Combining three dfs into one df 

I combined the three separate data.frames of lyrics and tracks to one aggregate data.frame: `mitski_albums`. 

```{r combine}
mitski_albums <- rbind(be_the_cowboy, puberty, bury_me)
```

## Basic statistics

Using the [dplyr package](https://www.rdocumentation.org/packages/dplyr/versions/0.7.8), I cleaned up and analyzed the `mitski_albums` data.frame. I specifically looked for data to assess the:

 - Number of tracks per album
 - Number of song lyric lines per album
 - Number of individual words per each of her 35 songs. 
 
 I created bar graphs using ggplot2 to visualize this information.

```{r summary}
library(dplyr)
library(ggplot2)
library(tidytext)

# Create a new df to summarize data 
tracks_per_alb <- mitski_albums %>%
  group_by(album) %>%
  summarize(n_distinct(track_title)) 

# Rename the second column in the new data.frame
colnames(tracks_per_alb)[colnames(tracks_per_alb) == "n_distinct(track_title)"] <- "track_count"

g_tracks_per_alb <- ggplot(tracks_per_alb, aes(album, track_count)) +
  geom_bar(stat = "identity") + ggtitle("Track Count Per Album")

g_tracks_per_alb # try to reorder x in graph #

# Create a new df to summarize data
lines_per_alb <- mitski_albums %>%
  group_by(album) %>%
  summarize(length(track_title)) 

# Rename the second column in the new data.frame
colnames(lines_per_alb)[colnames(lines_per_alb) == "length(track_title)"] <- "line_count"

# ggplot of lines per album
g_lines_per_alb <- ggplot(lines_per_alb, aes(album, line_count)) +
  geom_bar(stat = "identity") + ggtitle("Lyric Line Count Per Album")

g_lines_per_alb # try to reorder x in graph #

# Create a new data.frame separating rows by individual words and removing stop words
lyric_words <- mitski_albums %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) 

# data.frame that shows number of words per track in a table 
words_per_track <- lyric_words %>%
  group_by(track_title) %>%
  summarize(length(word)) 

# Rename the column "length(word)" to word_count
colnames(words_per_track)[colnames(words_per_track) == "length(word)"] <- "word_count"

# ggplot of words per track
g_words_per_track <- ggplot(words_per_track, aes(track_title, word_count)) + 
    geom_text(aes(label = word_count), vjust = -1, size=2.5) + geom_bar(stat = 
    "identity") + ggtitle("Word Count Per Track") + coord_flip() 

g_words_per_track

```

## Sentiment Analysis I

With the help of the [tidytext](https://cran.r-project.org/web/packages/tidytext/index.html) and [topic models](https://cran.r-project.org/web/packages/topicmodels/index.html) packages, Sentiment Analysis allows us to gain an understanding of the emotions evoked from a particular text. For my research, I used the ggplot2 package to visualize the top ten negative and top ten positive sentiments expressed from Mitski's three studio albums. 
In order to visualize the negative and positive sentiments, I prepared the data.frame by using piping to separate each individual word in the line into its own row in the data.frame, removing stop words using the tidytext package, and joining in the "bing" lexicon, which classifies individual words (or unigrams) as either "positive" or "negative". 

According to my visualization, one of the more positive sentiments expressed in Mitski's music is "love." However, any Mitski listener would know that Mitski's sentiment of "love" is often negative. Mitski's songs generally connote love in relation to lonesomeness. While sentiment analysis on the unigram level can help us gain a cursory understanding to the scope of emotions expressed in textual data, this sentiment analysis fails to contextualize the true meaning of Mitski's lyrics.  

```{r sentiment}
library(topicmodels)

# Tidy data for Sentiment Analysis ggplot
tidy_mitski <- mitski_albums %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()  

tidy_mitski
```

## Sentiment Analysis II

As described in the previous section, I was unsatisfied with the text analysis using the "bing" lexicon. The "bing" lexicon sees sentiment as a binary: positive or negative. Given Mitski's poetic complexity, I explored another lexicon to conduct my sentiment analysis. 

The [textdata package](https://cran.r-project.org/web/packages/textdata/index.html) provides a framework to download, parse, and store text datasets on the disk and load them when needed. Moreover, it gave me access to the "nrc" lexicon. According to ["Text Mining with R"](https://www.tidytextmining.com/sentiment.html) documentation, the nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise." 

I am more satisfied with the results of this sentiment analysis as it provides a more thematic sentiment analysis. For example, this sentiment analysis explores how different individual unigrams contribute to more thematic sentiments of anger, anticipation, disgust, fear, and more. 

```{r sentiment_2}
library(textdata)

get_sentiments("nrc")

# Tidy data for Sentiment Analysis ggplot
tidy_mitski_2 <- mitski_albums %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()  

tidy_mitski_2
```

## Tidying the Data with tm package

As described in [RDocumentation](https://www.rdocumentation.org/packages/tidytext/versions/0.2.0/topics/tidy.Corpus), a "tidied corpus" returns a data.frame with one-row-per-document. This format contains the document's text and one column for each local (per-document) metadata tag. While I previously used the `unnest_token` function to break up words into individual rows, I use the [tm library](https://www.google.com/search?q=tm+library&oq=tm+library&aqs=chrome..69i57j69i60l3j69i65j69i60.987j0j7&sourceid=chrome&ie=UTF-8) here as it allows me to use other Natural Language Processing tools, such as Latent Dirichlet Allocation (LDA).

Drawing upon the [Text Mining with R](https://www.tidytextmining.com/tfidf.html#the-bind_tf_idf-function), I specifically use the `bind_tf_idf` function to find important words for the content of each document. This function decreases the weight for commonly used words and increases the weight for words that are not used very much in a collection or corpus of documents. 

```{r corpus, echo=TRUE, message=FALSE, warning=FALSE}
library(tm)

# Create raw corpus from genius lyrics 
corpus_raw <- Corpus(VectorSource(mitski_albums$lyric))

# Transform everything to lowercase
corpus <- tm_map(corpus_raw,content_transformer(tolower))

# Strip whitespace
corpus <- tm_map(corpus, stripWhitespace)

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation) 

# Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Stem the document
corpus <- tm_map(corpus, stemDocument)

# Create document term matrix
dtm <- DocumentTermMatrix(corpus)

# Tidy dtm 
corpus_tidy <- tidy(dtm)
  corpus_tidy %>% 
  bind_tf_idf(term, document, count) %>% 
  arrange(desc(tf_idf))
```

## Latent Dirichlet Allocation (LDA) 

The tidying done in the section above sets the stage for my LDA below. As described in [this Medium article](https://medium.com/@lettier/how-does-lda-work-ill-explain-using-emoji-108abf40fa7d), LDA is a form of natural language processing and topic modeling that creates a “generative probabilistic model” of a collection of composites made up of parts. In other words, LDA facilitates exploration of themes in a collection of documents.

In reference to the "Topic Modelling "chapter from [Tidy Text Mining](https://www.tidytextmining.com/topicmodeling.html), I use a `LDA_VEM` topic model with 3 topics and use the `tidy()` method to extract "beta" per-topic-per-word probabilities. For each topic-per-word, the "beta" is the probability of that term being generated from that topic.

```{r LDA}
# Deletes rows with zero entry because each row needs to contain at least one non-zero entry
raw.sum <- apply(dtm, 1, FUN=sum)
dtm <- dtm[raw.sum!=0,]

# LDA 
output <- LDA(dtm, k = 3, control = list(seed = 1234))
beta <- tidy(output, matrix = "beta")
filter(beta, topic==1)%>% arrange(desc(beta))
filter(beta, topic==2)%>% arrange(desc(beta))
filter(beta, topic==3)%>% arrange(desc(beta))
round(head(posterior(output, dtm)$topics), digits = 3)

# Use dplyr’s top_n() to find the 10 terms that are most common within each of the 3 topics
mitski_top_terms <- beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Create ggplot
g_mitski_top_terms <- mitski_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

g_mitski_top_terms
```

## In Defense of Wordclouds

While the use of wordclouds are often criticized by data scientists, wordclouds provide an easily digestable visual in service of lyrical content analysis. Analyzing a wordcloud is simple: the bigger and bolder a text is, the more frequently it appears in the data.frame. Below is a wordcloud for all three of Mitski's studio albums.

```{r wordcloud}
library(wordcloud)

# Wordcloud for three studio albums
cloud <- wordcloud(corpus, max.words = 70, random.order = FALSE, ordered.clouds = TRUE)
```

Using the [ggwordcloud](https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html) package, I also created three separate wordclouds for each of Mitski's three records in ascending order of release.

```{r gg_wordcloud}
library(ggwordcloud)

# Separate words and join stop_words table to each individual record data.frame
bury_me_2 <- bury_me %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) 

# Create ggplot wordcloud
set.seed(42)
bury_me_2_wc <- ggplot(bury_me_2, aes(label = word, size = word)) +
  geom_text_wordcloud() +
  theme_minimal()

# Separate words and join stop_words table to each individual record data.frame
puberty_2 <- puberty %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) 

# Create ggplot wordcloud
set.seed(42)
puberty_2_wc <- ggplot(puberty_2, aes(label = word, size = word)) +
  geom_text_wordcloud() +
  theme_minimal()

# Separate words and join stop_words table to each individual record data.frame
be_the_cowboy_2 <- be_the_cowboy %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) 

# Create ggplot wordcloud
set.seed(42)
be_the_cowboy_2_wc <- ggplot(be_the_cowboy_2, aes(label = word, size = word)) +
  geom_text_wordcloud() +
  theme_minimal()

bury_me_2_wc
puberty_2_wc
be_the_cowboy_2_wc
```

# Conclusion

Mitski is famed for her poetic prose about heartache and loneliness, strong vocal abilities, and esoteric performance style. In an effort to better understand Mitski's songs, this project was able to offer a more data-driven analysis of the themes in Mitski's discography. 

The ggplots displayed under the "Descriptive Statistics" section offered quantitative data points to my analysis. The core text analysis methods used-- Sentiment Analysis and LDA-- allowed me to hone into the words and feelings evoked by Mitski's lyrical diction. The wordclouds served as a fun visual to gain a cursory understanding of how the lyrical content differed among Mitski's three studio albums between 2014 and 2018.

Some drawbacks to my analysis included an ambiguity to the role of the unnest function and stop words. In other words, to fully get to know Mitski's lyrical themes, it is important to analyze her lyrics in the context of complete phrases and stop words. Lastly, I posit that if the research goal is to analyze Mitski's discography, this lyrical text analysis would need to be complemented with an analysis of Mitski's music-- how do instruments, beats, and voice intonation manifest in Mitski's music? 
