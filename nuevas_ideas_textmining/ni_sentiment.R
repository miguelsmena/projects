setwd('C:/Users/migue/rfiles/nuevas_ideas_textmining')
#twitter exploration
library(rtweet)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
#twitter keys
appname <- 'mikesanchezapp'
key <- "CdqGMRtGg0PBlXsPtyflnEAu1"
secret_key <- "xfzrbFeWIgpJGwkheOyJhSqOGT8hrxVLlKe0wT0GxmflKBWm4V"
#twitter tokens
access_token <- "788076739-cmreHoPN9QfPcWC9DqGvTj9TkX8n00XVt8yXKnlX"
access_secret <- "dLBzATb27T6fsG3lzaFhEzXFQ8ckHAR2xLCbx64aLpy2w"
#create eviroment
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret_key,
  access_token = access_token,
  access_secret = access_secret)
twitter_token
#read twitter data
ppto_2020 <- search_tweets( q = '#PRESUPUESTO2020YA',
                            n = 500,
                            include_rts = FALSE)
#tidy data to just text
ppto_2020_tidy <- ppto_2020 %>%
  select(text)
write.csv(ppto_2020_tidy, "ppto_2020_tidy_flat.csv")

#csv collected 12/12/19 with 6-9 days of tweets 
ppto_2020_flat <- read_csv("ppto_2020_tidy_flat.csv") %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  filter(word != "PRESUPUESTO2020YA")

#ncr
ncr_select_string <- c('Spanish (es)', 'Positive', 'Negative', 'Anger',	'Anticipation',	'Disgust', 'Fear', 'Joy', 'Sadness', 'Surprise', 'Trust')
ncr_spanish <- readxl:: read_xlsx('NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.xlsx') %>%
  select(ncr_select_string) %>%
  rename(word = `Spanish (es)`) %>%
  gather(-word, key = 'sentiment', value = 'value') %>%
  filter(value == 1, word != 'NO TRANSLATION') %>%
  select(-value)
head(ncr_spanish)
#inner join
ppto_join <- ppto_2020_flat %>%
  inner_join(ncr_spanish, by='word') %>%
  count(word)
head(ppto_join)
#------------------------------------------------------------
set.seed(1234)
ppto_2020_cloud <- wordcloud(words = ppto_join$word,
                               freq = ppto_join$n,
                               min.freq = 1,
                               max.words= 500,
                               random.order = FALSE,
                               random.color = FALSE,
                               colors=brewer.pal(8, "Dark2"))


