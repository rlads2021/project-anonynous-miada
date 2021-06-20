#visualization

## load packages
library(readxl)
library(tidytext)
library(textdata)
library(dplyr)
library(SnowballC)
library(readr)

#plot
library(RColorBrewer)
library(ggplot2)
library(wordcloud)

# importing files
zhou <- read_excel("zhou.xlsx")
hua_wordcount <- read.csv("hua_wordcount.csv")

# some other files
path <- "C:/Users/ekao5/OneDrive/桌面/R/term project"
files <- list.files(path)
files_csv <- files %>%
    str_subset(pattern = ".csv$")
files_csv <- files_csv[-c(5, 10, 13)]

aus_wordcount <- read.csv("australia.csv")
us_state_wordcount <- read.csv("us_state.csv")
eng_wordcount <- read.csv("eng.csv")
eng_rabb_wordcount <- read.csv("eng_rabb.csv")
indian_wordcount <- read.csv("indian.csv")
russia_wordcount <- read.csv("russia.csv")
russia_UN_wordcount <- read.csv("russia_UN.csv")
singapore_wordcount <- read.csv("singapore.csv")
us_secretary_wordcount <- read.csv("us_secretary.csv")


# Preparation
## date
as.character(zhou$date) %>%
    str_sub(
        pattern = "201005\\d\\d",
        replacement = paste("2010-05-", zhou$date))
        
# tokenize
tidy.zhou <- zhou %>%
    select("content") %>%
    unnest_tokens(
        output = "word",
        input = content,
        token = "words"
        ) %>%
    anti_join(get_stopwords())

# stopwords
custom_stop_words <- tribble(
    ~ word,
    ~ lexicon,
    "amp",
    "CUSTOM",
    "s",
    "CUSTOM",
    "2",
    "CUSTOM",
    "m",
    "CUSTOM",
    "1",
    "CUSTOM",
    "t",
    "CUSTOM",
    "3",
    "CUSTOM",
    "u",
    "CUSTOM"
)
        
# Bind the custom stop words to stop_words
stop_words2 <- get_stopwords() %>%
    bind_rows(custom_stop_words)

# for given word count file: n=15|20
word.freq.plot <- function(wordcount) {
    wordcount %>%
    anti_join(stop_words2) %>%
    mutate(word = reorder(word, n)) %>%
    slice_head(n = 15) %>%
    ggplot(aes(x = n, y = word)) +
    geom_col(fill = "firebrick1") +
        labs(title = paste("Tweet word count, 2021-05~2021-06",
                           x = "word count"))
        }

word.freq.plot(aus_wordcount) +
    labs(title = "Tweet word count of Australian Minister of Foreign Affairs,\n Marise Payne, 2021-05~2021-06") +
    theme(title = element_text(size = 6))

word.freq.plot(eng_wordcount) +
    labs(title = "Tweet word count of UK Minister of Foreign Affairs, \n 2021-05~2021-06") +
    theme(title = element_text(size = 6))
    
word.freq.plot(eng_rabb_wordcount) +
    labs(title = "Tweet word count of UK Diplomat, Rabb \n 2021-05~2021-06") +
    theme(title = element_text(size = 6))
        
word.freq.plot(indian_wordcount) +
    labs(title = "Tweet word count of Indian Minister of Foreign Affairs \n 2021-05~2021-06") +
    theme(title = element_text(size = 6))

word.freq.plot(russia_wordcount) +
    labs(title = "Tweet word count of MFA Russia \n 2021-05~2021-06") +
    theme(title = element_text(size = 6))
        
word.freq.plot(russia_UN_wordcount) +
    labs(title = "Tweet word count of Russia UN representative, Dmitry Polyanskiy\n 2021-05~2021-06") +
    theme(title = element_text(size = 6))

word.freq.plot(singapore_wordcount) +
    labs(title = "Tweet word count of Singapore Minister of Foreign Affairs\n 2021-05~2021-06") +
    theme(title = element_text(size = 6))
        
word.freq.plot(us_secretary_wordcount) +
    labs(title = "Tweet word count of US secretary of State, \n 2021-05~2021-06") +
    theme(title = element_text(size = 10))

word.freq.plot(us_state_wordcount) +
    labs(title = "Tweet word count of US Department of State, \n 2021-05~2021-06") +
    theme(title = element_text(size = 10))

# word frequency
tidy.zhou %>%
    count(word, sort = TRUE) %>%
    slice_max(n, n = 20) %>%
    mutate(word = reorder(word, n))
plot <- ggplot(word_counts_20, aes(x = n, y = word)) +
    geom_col(fill = "firebrick1") +
        labs(title = "Tweet word count of zhou, 2021-05~2021-06",
             x = "word count")
 
# sentiment anaylsis
## bing
tidy.zhou.sentiment.bing <- tidy.zhou %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = T) %>%
    group_by(sentiment) %>%
    summarize(n = n())
    ggplot(tidy.zhou.sentiment.bing, aes(x = n, y = sentiment)) +
            geom_col()
        
        
## afinn(scored)
tidy.zhou %>%
    inner_join(get_sentiments("afinn")) %>%
    count(word, value, sort = T) %>%
    summarize()
    ggplot(aes(x = n, y = word)) +
    geom_col() +
    facet_wrap( ~ value)
        

# word frequency timeline
        
        
# prepare
us_state_cloud <- us_state_wordcount %>%
    anti_join(stop_words2) %>%
    mutate(word = reorder(word, n))

us_secretary_cloud <-
    us_secretary_wordcount %>%
    anti_join(stop_words2) %>%
    mutate(word = reorder(word, n))

hua_cloud <- hua_wordcount %>%
    anti_join(stop_words2) %>%
    mutate(word = reorder(word, n))

# have a look
us_secretary_cloud %>%
    slice_head(n = 20) %>%
    ggplot(aes(x = n, y = word)) +
        geom_col()
        
#workcloud
wordcloud(
    words = hua_cloud$word,
    freq = hua_cloud$n,
    max.words = 33,
    random.order = F,
    colors = "darkblue"
    )
