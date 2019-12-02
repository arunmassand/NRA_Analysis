library(tidyverse)
library(modelr)
library(tidytext)
library(stringr)
library(lubridate)

load("ira.RData")

ira1 = ira %>%
    select(userid, user_display_name, follower_count, following_count, account_creation_date,
           is_retweet, retweet_userid, like_count, retweet_count, poll_choices) %>%
    group_by(userid) %>%
    mutate(ff_ratio = ifelse(following_count != 0, follower_count / following_count,
                             follower_count / (following_count + 1))) %>%
    select(userid, ff_ratio) %>%

quantile(new_ira$ff_ratio, 0.90, na.rm = T)
quantile(new_ira$ff_ratio, 0.10, na.rm = T)

ira2 = ira1 %>%
    mutate(type = ifelse(ff_ratio > 6.37904761904762, "A",
                        ifelse(ff_ratio < 0.531961792799412, "B", "neither"))) %>%
    filter(type != "neither") %>%
    print


ira2 = ira %>%
    filter(is_retweet == FALSE &
          year(tweet_time) == 2016 &
          month(tweet_time) >= 5 &
          month(tweet_time) < 12)

og_tweets = unnest_tokens(ira2, input = tweet_text, output = word, token = "words", to_lower = TRUE) %>%
    select(word) %>% print
    
og_words = og_tweets %>%
    group_by(word) %>%
    filter(str_length(word) > 4) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    print
    
ira3 = ira %>%
    filter(is_retweet == TRUE &
          year(tweet_time) == 2016 &
          month(tweet_time) >= 5 &
          month(tweet_time) < 12)
retweets = unnest_tokens(ira3, input = tweet_text, output = word, token = "words", to_lower = TRUE) %>%
    select(word)

rt_words = retweets %>%
    group_by(word) %>%
    filter(str_length(word) > 4) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    print
    
table = inner_join(og_words, rt_words, by = "word") %>%
    filter(n.x > 800) %>%
    mutate(ratio = n.y / n.x) %>%
    arrange(desc(ratio)) %>%
    top_n(20)
names(table) = c("word", "orginal_count", "retweet_count", "ratio")
print(table)
