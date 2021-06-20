#Word Count
library(readr)
library(purrr)
library(dplyr)
library(twitteR)
consumer_key <-  "zMcOBSlqDnqhDkbtc7zf4eyLR"
consumer_secret <- "RsSpbOmQ7OML5jxjAd5fLh3mOjmulfh72mUkHq2JiVrrOYGW38"
access_token <- "769911070823583744-QTSaO3fhnZ1NqTQOGkV5gKqg0ZJYm4D"
access_secret <- "vb3q9lR1odfuAHq2RG1x8aW5EB5NkLhEfKwcbqyvkagga"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweetz <- read.csv('/Users/stevetai/Downloads/zhou.xlsx - 工作表1.csv')
tweetsaa <- userTimeline('MEAIndia', n=2400)
tweets_df <- twListToDF(tweetsaa)
content <- tweetz$conteent
View(content)
content <- content
result <- sapply(content, function(x) {
    return(sub('https://.*','',x))
},USE.NAMES = FALSE)
View(result)


seg <- worker(user = "NRC-Emotion-Lexicon-Wordlevel-v0.92.txt")
docs_segged <- segment(result, seg)

docs_df <- tibble::tibble(
    doc_id = seq_along(docs_segged),
    content = docs_segged
)

library(tidytext)

zlj_tidy <- docs_df %>%
    unnest_tokens(output = "word", input = "content",
                  token = "regex", pattern = "\u3000")

T <- zlj_tidy %>%
    group_by(word) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

View(zlj_tidy)
View(T)
write.csv(zlj_tidy, "/Users/stevetai/Desktop/tidytext/趙立堅.csv")
write.csv(T, "/Users/stevetai/Desktop/tidytext/趙立堅統計.csv")


#convert all text to lower case
content<- tolower(content)

# Replace blank space (“rt”)
content <- gsub("rt", "", content)

# Replace @UserName
content <- gsub("@\\w+", "", content)


# Remove punctuation
content <- gsub("[[:punct:]]", "", content)


# Remove links
content <- gsub("http\\w+", "", content)

# Remove tabs
content <- gsub("[ |\t]{2,}", "", content)


# Remove blank spaces at the beginning
content <- gsub("^ ", "", content)


# Remove blank spaces at the end
content <- gsub(" $", "", content)

sentiment_CHNSPK<-get_nrc_sentiment((content))

#calculationg total score for each sentiment
Sentimentscores_CHNSPK<-data.frame(colSums(sentiment_CHNSPK[,]))

names(Sentimentscores_CHNSPK)<-"Score"
Sentimentscores_CHNSPK<-cbind("sentiment"=rownames(Sentimentscores_CHNSPK),Sentimentscores_CHNSPK)
rownames(Sentimentscores_CHNSPK)<-NULL
Sentimentscores_CHNSPK
