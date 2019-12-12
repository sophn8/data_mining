---
title: "Exploring Mitski's Discography Through Text Mining With R"
author: "Sophia Ng"
date: "11/24/2019"
output: github_document
---

# Introduction

Mitski is a Japanese-American singer-songwriter based in the United States. Described by [*NPR*](https://www.npr.org/2018/09/06/642252679/mitski-is-the-21st-centurys-poet-laureate-of-young-adulthood) as "the 21st Century's Poet Laureate Of Young Adulthood," Mitski's lyrics skew towards feelings of lonesomeness and forlorning. A profile in [*The New Yorker*](https://www.newyorker.com/magazine/2019/07/08/on-the-road-with-mitski) describes the musician as one who "writes achingly intense songs about private yearnings."

Drawing upon Mitski's three studio release albums, I engage with the following questions: 

 - How do Mitski's three studio albums compare to one another?
 - What are the sentiments shared in Mitski's discography? 
 - How do the lyrics vary among Mitski's three studio albums? 
 
I seek to gain a  data-informed perspective on Mitski's lastest records, drawing upon both summary statistics and text analysis. In so being, I first retrieve and prepare song lyrics via the Genius package. Next, I analyze basic basic statistics about Mitski's three studio releases. I then conduct a sentiment analysis to analyze the lyrical content of Mitski's songs. Lastly, I create word clouds to visualize the common diction employed in Mitski's tracks.


# Retrieving the Data 

Genius is a media company that allows users and artists to annotate lyrics. I used [Josiah Parry's Github documentation](https://github.com/josiahparry/genius) to learn how to access Mitski's discography as text data via the Genius package.

I downloaded three separate data.frames for Mitski's three studio albums: "Be The Cowboy," "Puberty 2," and "Bury Me At Makeout Creek." 

```{r setup}
library(genius)

# Get songlyrics using Genius lyrics package
be_the_cowboy <- genius_album(artist = "Mitski", album = "Be the Cowboy")
puberty <- genius_album(artist = "Mitski", album = "Puberty 2")
bury_me <- genius_album(artist = "Mitski", album = "Bury Me At Makeout Creek")
```


# Creating a new column in df 

The Genius package includes four columns in their data.frames: `track_title`, `track_n`, `line`, and `lyric`. In the code below, I add a new `album` column in each data.frame.

```{r new_column, echo=TRUE, message=FALSE, warning=FALSE}

# Create "album" column and fill with album name, "Be The Cowboy"
be_the_cowboy["album"] <- "Be The Cowboy (2018)"  

# Repeat for other two dfs 
puberty["album"] <- "Puberty 2 (2016)"  

bury_me["album"] <- "Bury Me At Makeout Creek (2014)" 
```


# Combining three dfs into one df 

I combined the three separate data.frames of lyrics and tracks to one aggregate data.frame: `mitski_albums`. 

```{r combine}
mitski_albums <- rbind(be_the_cowboy, puberty, bury_me)
```


# Summary statistics

Using the [dplyr package](https://www.rdocumentation.org/packages/dplyr/versions/0.7.8), I cleaned up and analyzed the `mitski_albums` data.frame. I specifically looked for data to assess the:

 - Number of tracks per album
 - Number of song lyric lines per album
 - Number of individual words per each of her 35 songs. 
 
 I created bar graphs using ggplot2 to visualize this information.

```{r summary}
library(dplyr)
library(ggplot2)
library(tidytext)

# creating a new df to summarize data 
tracks_per_alb <- mitski_albums %>%
  group_by(album) %>%
  summarize(n_distinct(track_title)) 

# renamed the second column in the new data.frame
colnames(tracks_per_alb)[colnames(tracks_per_alb) == "n_distinct(track_title)"] <- "track_count"

g_tracks_per_alb <- ggplot(tracks_per_alb, 
                           aes(x = reorder(album, -track_count), 
                               y = track_count)) + geom_bar(stat = "identity") + 
                           ggtitle("Track Count Per Album")

g_tracks_per_alb # try to reorder x in graph #

# creating a new df to summarize data
lines_per_alb <- mitski_albums %>%
  group_by(album) %>%
  summarize(length(track_title)) 

# renamed the second column in the new data.frame
colnames(lines_per_alb)[colnames(lines_per_alb) == "length(track_title)"] <- "line_count"

# ggplot of lines per album
g_lines_per_alb <- ggplot(lines_per_alb, 
                          aes(x = reorder(album, -line_count), 
                              y = line_count)) + 
                   geom_bar(stat = "identity") + 
                   ggtitle("Lyric Line Count Per Album")

g_lines_per_alb # try to reorder x in graph #

# create a new data.frame separating rows by individual words and removing stop words
lyric_words <- mitski_albums %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) 

# dataframe that shows number of words per track in a table 
words_per_track <- lyric_words %>%
  group_by(track_title) %>%
  summarize(length(word)) 

# renamed the column "length(word)" to word_count
colnames(words_per_track)[colnames(words_per_track) == "length(word)"] <- "word_count"

# ggplot of words per track
g_words_per_track <- ggplot(words_per_track, aes(x = reorder(track_title, word_count), 
                                                 y = word_count)) +
                     geom_text(aes(label = word_count), vjust = 2, size=2.5) + 
                     geom_bar(stat = "identity") + 
                     ggtitle("Word Count Per Track") + coord_flip() 
g_words_per_track
```
