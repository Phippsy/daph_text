library(rvest)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(ggthemes)
library(tm)
library(tidyr)
library(wordcloud)
library(tm)
library(knitr)
library(tidyverse)
library(forcats)
library(lubridate)
library(plotly)

# Get the highest page count from the site
max_pages <- read_html("http://www.analyticshour.io/all-podcast-episodes/") %>% 
html_nodes("a.page-numbers") %>% 
html_text() %>% 
as.numeric() %>% 
max(na.rm = TRUE)

page_nums <- as.list(1:max_pages)

# Function to get a list of all archive urls along with the episode number and year
get_url_list <- function(page_num) {
x <- read_html(paste0("http://www.analyticshour.io/all-podcast-episodes/page/", page_num))

# Make a DF containing the url, year and episode number
urls <- data.frame( urls = x %>% 
html_nodes(paste0("body > div.super-container.light-icons > div.main-content.page.archive-page > div > div > div > div  div > footer > ul > li.title.not-truncate ")) %>%
html_children %>% 
as.character() %>% 
str_extract('\\\".*?\\\"') %>% 
str_replace_all(c('\"' = ''))) %>% 
separate(urls, 
into = c("bin", "bin2", "bin3", "year", "bin4", "bin5", "episode"),
sep = "\\/",
remove = FALSE) %>% 
mutate(urls = as.character(urls), episode = str_replace_all(episode, "\\-.*", "")) %>% 
select(urls, year, episode)

urls
}

# Pull the urls list from the site. Ignore the "Too many values" error, this comes from the call to separate()
full_urls <- map_df(page_nums, get_url_list) 

# Split the url DF into a list so it can be called by map_df below
full_urls_split <- full_urls %>% 
split(.$episode)

# Function to scrape the web content per url, append the episode #, year and speaker name (where known) then tokenize into words
get_post_content <- function(ep_df) {
# Quick line for debugging purposes
# print(paste("Running for :", ep_df$url))
url <- ep_df$url
episode <- ep_df$episode
year <- ep_df$year

post_html <- read_html(url) 

post_ps <- post_html %>% 
html_nodes(".post p") %>% 
html_text() %>% 
as.data.frame()

post_title <- post_html %>% 
html_nodes(".entry-title") %>% 
html_text() %>% 
as.character()

names(post_ps) <- "text"

post_ps <- post_ps %>% 
dplyr::filter(str_detect(text, "(\\d{2}:)")) 

if (length(post_ps$text != 0)) {
post_ps <- post_ps %>% 
mutate(
text = as.character(text),
speaker = case_when(
str_detect(text, "(MH:)|(Michael Helbling:)|(Michael:)") ~ "Drunk Helbs",
str_detect(text, "(MK:)|(Moe Kiss:)|(Moe:)") ~ "Moe from down under",
str_detect(text, "(TW:)|(Tim Wilson:)|(Tim:)") ~ "Grumpy Cat",
TRUE ~ "Unknown"
),
url = url,
episode = as.numeric(episode),
year = year,
title = post_title,
time_start = as.numeric(ms(str_extract(text, "\\d{2}:\\d{2}"))),
time_end = lead(time_start, 1),
duration = time_end - time_start
)
post_ps
} else {
NULL
}
}

daph_post_text <- map_df(full_urls_split, get_post_content)      
saveRDS(daph_post_text, "datafiles/daph_post_text.rds")

# Longest monologue

monologue <- daph_post_text %>% 
arrange(desc(duration)) %>% 
top_n(10, duration) %>% 
select(speaker, text, title, duration)


monologue$text[1]


# Tokenize our words



data("stop_words")
daph_post_words <- daph_post_text %>% 
unnest_tokens(output = word, input = text, format = "text") %>% 
anti_join(stop_words) %>% 
filter(!str_detect(word, "\\d{2}"))
saveRDS(daph_post_words, "datafiles/daph_post_words.rds")



## Profanity by presenter

swears <- daph_post_words %>% 
filter(str_detect(word, "(shit)|(fuck)|(crap)"))

swears_by_speaker <- swears %>% 
group_by(episode, speaker) %>% 
filter(speaker!= "Unknown") %>% 
summarise(ep_count = n()) %>% 
ungroup() %>% 
group_by(speaker) %>% 
summarise(avg_swears = mean(ep_count, na.rm = TRUE)) %>% 
mutate(speaker = fct_reorder(speaker, avg_swears))

swears_by_speaker %>% 
ggplot(aes(x = speaker, y = avg_swears)) +
geom_col(fill = "#3FA0D9") + 
theme_hc() + 
  labs(
    x = "Speaker",
    y = "Avg. swear jar deposits per episode",
    title = "Avg. swear count by speaker",
    subtitle = "Was there ever any doubt?")



# Most talkative presenter


# word count per episode by presenter


words_by_speaker <- daph_post_words %>% 
  group_by(episode, speaker) %>% 
  filter(speaker!= "Unknown") %>% 
  summarise(ep_count = n()) %>% 
  ungroup() 

wbs_overall <- words_by_speaker %>% 
  group_by(speaker) %>% 
  summarise(avg_words = mean(ep_count, na.rm = TRUE)) %>% 
  mutate(speaker = fct_reorder(speaker, avg_words))

wbs_overall %>% 
  ggplot(aes(x = speaker, y = avg_words)) +
  geom_col(fill = "#3FA0D9") + 
  theme_hc() + 
  xlab('Speaker') + 
  ylab('Avg words spoken per episode') + 
  ggtitle("Chatty Catty")

wbs_be <- words_by_speaker %>% 
  filter(ep_count!=0) %>% 
  ggplot(aes(as.numeric(episode), ep_count, colour = speaker)) +
  geom_line(na.rm = TRUE) +
  geom_point() +
  labs(
    title = "Words spoken by episode, by speaker",
    y = "Words spoken",
    x = "Episode number",
    colour = "Speaker Name"
  )
ggplotly(wbs_be)


# Episode sentiments


daph_sentiment <- daph_post_words %>% 
  inner_join(get_sentiments("bing")) 

sentiment_by_episode <- daph_sentiment %>% 
  count(episode, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

senti_plot <- sentiment_by_episode %>% 
  filter(episode > 50) %>% 
  ggplot(aes(episode, sentiment)) +
  geom_col()

ggplotly(senti_plot)


daph_sentiment %>% 
filter(episode == 69) %>% 
group_by(word, sentiment) %>% 
count() %>% 
arrange(sentiment, desc(n))




daph_sentiment %>% 
filter(episode == 69, !str_detect(word, "bias")) %>% 
group_by(sentiment) %>% 
count()




# Happiest episode

daph_sentiment %>% 
  filter(episode == 57) %>% 
  group_by(word, sentiment) %>% 
  count() %>% 
  arrange(desc(sentiment), desc(n))


daph_post_words %>% filter(word == 'laughter') %>% group_by(episode) %>% summarise(n = n()) %>% ggplot(aes(x = episode, y = n)) + geom_col(fill = "#EF4A62") + theme_hc() + xlab('Episode') + ylab('Number of laughs') + ggtitle("The funniest episode?")

