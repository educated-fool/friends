# Scraping the Data ####

## Part I: parse_and_scrape function ####

# Load necessary libraries
library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# Base URL for the main Friends transcript page
base_url <- "https://fangj.github.io/friends/"

# Function to parse episode details and extract dialogues
parse_and_scrape <- function(html_line) {
  # Extract the href attribute and text
  link <- html_attr(html_line, "href")
  text <- html_text(html_line)
  
  # Construct full link
  full_link <- paste0(base_url, link)
  
  # Adjusted to handle season 10 and special cases like 212-213 for episode numbers
  # Now also handles ranges like 1017-1018
  if(str_detect(link, "-")) {
    match_data <- str_match(link, "season/(\\d{2})(\\d{2})-(\\d{2})\\.html")
    season <- as.integer(match_data[, 2])
    episode <- as.integer(match_data[, 3]) # Only the first episode in the range
    episode_number <- sprintf("S%02dE%02d", season, episode)
  } else {
    match_data <- str_match(link, "season/(\\d{2})(\\d{2})\\.html")
    season <- as.integer(match_data[, 2])
    episode <- as.integer(match_data[, 3])
    episode_number <- sprintf("S%02dE%02d", season, episode)
  }
  
  title <- str_extract(text, "(?<=\\d\\s|-\\d\\s).*$") # Updated regex to handle ranges
  
  # Scrape dialogues from the episode's page
  page <- read_html(full_link)
  dialogues <- page %>%
    html_nodes("p") %>%
    html_text() %>%
    .[str_detect(., regex("^(Monica|Joey|Chandler|Phoebe|Ross|Rachel):", ignore_case = TRUE))]
  
  if (length(dialogues) == 0) {
    return(tibble())
  }
  
  authors <- str_extract(dialogues, "^[A-Za-z]+")
  quotes <- str_replace_all(dialogues, "^[A-Za-z]+:", "") %>%
    str_trim()
  quote_order <- seq_along(quotes)
  
  data_frame <- tibble(
    season = season,
    episode = episode,
    episode_number = episode_number,
    title = title,
    author = authors,
    quote = quotes,
    quote_order = quote_order
  )
  
  return(data_frame)
}

# Read the main page HTML and extract episode links
main_page <- read_html(base_url)
episode_links <- html_nodes(main_page, "ul li a")

# Parse episode details and scrape dialogues for each episode link
dialogues_data <- map_df(episode_links, parse_and_scrape)

## Part II: parse_single_episode function ####

#Due to missing lines in the HTML source of episodes 3-24 of season 2, caused by <br> tags not 
#being captured during the initial scrape, these episodes were excluded from the original dataset. 
#A new function has been utilized to accurately scrape and incorporate the data from these pages.

dialogues_data_wo_s2 <- dialogues_data %>%
  filter(!(!is.na(season) & season == 2 & episode >= 3 & episode <= 24))

# Function to parse a single episode's page
parse_single_episode <- function(episode_url) {
  # Read the HTML content of the page
  page_content <- read_html(episode_url)
  
  # Extract the title of the episode
  title <- page_content %>%
    html_nodes("title") %>%
    html_text() %>%
    str_trim()
  
  # Extract all text from the page
  text_all <- page_content %>%
    html_nodes("body") %>%
    html_text()
  
  # Use regular expression to find dialogues and split text into lines
  dialogues_lines <- unlist(str_split(text_all, "\r\n|\n|\r"))
  
  # Filter lines that represent dialogues
  dialogue_lines <- dialogues_lines[str_detect(dialogues_lines, "^(JOEY|CHANDLER|MONICA|PHOEBE|ROSS|RACHEL):")]
  
  # Extract author and quote
  authors <- str_extract(dialogue_lines, "^[A-Z]+") %>% 
    tolower() %>% 
    str_to_title()
  
  quotes <- str_replace(dialogue_lines, "^[A-Z]+:", "")
  
  # Generate quote order
  quote_order <- seq_along(quotes)
  
  # Handle special case for "0212-0213"
  if (grepl("0212-0213.html", episode_url)) {
    season <- NA
    episode <- NA
    episode_number <- NA
  } else {
    # Extract season and episode from URL
    url_parts <- str_extract(episode_url, "(\\d{2})(\\d{2})\\.html$")
    season <- as.integer(substr(url_parts, 1, 2))
    episode <- as.integer(substr(url_parts, 3, 4))
    episode_number <- sprintf("S%02dE%02d", season, episode)
  }
  
  # Create a dataframe
  data_frame <- tibble(
    season = rep(season, length(quote_order)),
    episode = rep(episode, length(quote_order)),
    episode_number = rep(episode_number, length(quote_order)),
    title = rep(title, length(quote_order)),
    author = authors,
    quote = quotes,
    quote_order = quote_order
  )
  
  return(data_frame)
}

# List of episode URLs
episode_urls <- c(
  "https://fangj.github.io/friends/season/0203.html",
  "https://fangj.github.io/friends/season/0204.html",
  "https://fangj.github.io/friends/season/0205.html",
  "https://fangj.github.io/friends/season/0206.html",
  "https://fangj.github.io/friends/season/0207.html",
  "https://fangj.github.io/friends/season/0208.html",
  "https://fangj.github.io/friends/season/0209.html",
  "https://fangj.github.io/friends/season/0210.html",
  "https://fangj.github.io/friends/season/0211.html",
  "https://fangj.github.io/friends/season/0212-0213.html",
  "https://fangj.github.io/friends/season/0214.html",
  "https://fangj.github.io/friends/season/0215.html",
  "https://fangj.github.io/friends/season/0216.html",
  "https://fangj.github.io/friends/season/0217.html",
  "https://fangj.github.io/friends/season/0218.html",
  "https://fangj.github.io/friends/season/0219.html",
  "https://fangj.github.io/friends/season/0220.html",
  "https://fangj.github.io/friends/season/0221.html",
  "https://fangj.github.io/friends/season/0222.html",
  "https://fangj.github.io/friends/season/0223.html",
  "https://fangj.github.io/friends/season/0224.html"
)

# Process each episode and combine data
dialogues_data_s2 <- map_df(episode_urls, parse_single_episode)

## Part III: Merge and Clean ####

# Merge dialogues_data_wo_s2 and dialogues_data_s2
df <- bind_rows(dialogues_data_wo_s2, dialogues_data_s2)

# Convert author names to Title Case
df <- df %>%
  mutate(author = str_to_title(tolower(author)))

# Convert special cases
df <- df %>%
  mutate(
    season = case_when(
      title == "In Barbados" ~ 9,
      title == "That Could Have Been, Part I & II" ~ 6,
      title == "The Last One, Part I & II" ~ 10,
      title == "outtakesFriends Special: The Stuff You've Never Seen" ~ 7,
      title == "The One After the Superbowl" ~ 2,
      TRUE ~ season
    ),
    episode = case_when(
      title == "In Barbados" ~ 23,
      title == "That Could Have Been, Part I & II" ~ 15,
      title == "The Last One, Part I & II" ~ 17,
      title == "outtakesFriends Special: The Stuff You've Never Seen" ~ 24,
      title == "The One After the Superbowl" ~ 12,
      TRUE ~ episode
    ),
    episode_number = case_when(
      title == "In Barbados" ~ "S09E23",
      title == "That Could Have Been, Part I & II" ~ "S06E15",
      title == "The Last One, Part I & II" ~ "S10E17",
      title == "outtakesFriends Special: The Stuff You've Never Seen" ~ "S07E24",
      title == "The One After the Superbowl" ~ "S02E12",
      TRUE ~ episode_number
    )
  )

# Calculate the number of quotes per episode
quotes_count_per_episode <- df %>%
  group_by(season, episode, episode_number, title) %>%
  summarise(quotes_count = n())

# Display the result
print(quotes_count_per_episode)

# Display the unique authors
print(unique(df$author))

## Part IV: CSV and ZIP Files ####

# Write the dataframe to a CSV file
write.csv(df, "friends_quotes.csv", row.names = FALSE)

# Compress the CSV file into a ZIP file
zip(zipfile = "friends_quotes.zip", files = "friends_quotes.csv")

# Text Mining & Data Visualization ####

## Part I: Initial Setup and Data Preparation ####

# Load necessary libraries
library(tidyverse)
library(tidytext)
library(topicmodels)
library(DT)
library(png)
library(grid)

# Convert the dataframe 'df' to a tibble for easier manipulation and viewing
df %>% as_tibble()

## Part II: Sentiment Lexicons Loading ####

# Load various sentiment lexicons
afinn <- get_sentiments('afinn')
nrc <- get_sentiments('nrc')
bing <- get_sentiments('bing')
loughran <- get_sentiments('loughran')

## Part III: Text Cleaning and Preprocessing ####

# Clean and preprocess text data
tidy_text <- df %>%
  unnest_tokens(word, quote) %>%  # Tokenize the quotes into words
  anti_join(stop_words) %>%  # Remove stop words
  filter(!word %in% tolower(author)) %>%  # Remove character names
  # Additional custom cleaning steps
  filter(!word %in% c("uhm", "it’s", "ll", "im", "don’t", "i’m", "that’s", "ve", "that’s", "you’re",
                      "woah", "didn", "what're", "alright", "she’s", "we’re", "dont", "c'mere", "wouldn",
                      "isn","pbs", "can’t", "je", "youre", "doesn", "007", "haven", "whoah", "whaddya", 
                      "somethin", "yah", "uch", "i’ll", "there’s", "won’t", "didn’t", "you’ll", "allright",
                      "yeah", "hey", "uh", "gonna", "umm", "um", "y'know", "ah", "ohh", "wanna", "ya", "huh", "wow",
                      "whoa", "ooh", "don")) %>%
  mutate(word = str_remove_all(word, "'s")) 

tidy_text %>% as_tibble()  # Display the cleaned text

## Part IV: Sentiment Analysis with Various Lexicons ####

# Sentiment analysis with Bing lexicon
tidy_bing <- tidy_text %>% inner_join(bing)

# Sentiment analysis with NRC lexicon
tidy_nrc <- tidy_text %>% inner_join(nrc)

# Sentiment analysis with AFINN lexicon
tidy_afinn <- tidy_text %>% inner_join(afinn)

## Part V: Visualizing Sentiments ####

tidy_nrc %>% 
  filter(author %in% c("Ross", "Monica", "Rachel", "Joey", "Chandler", "Phoebe")) %>% 
  ggplot(aes(sentiment, fill = author)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~author) +
  theme_dark() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  ) +
  labs(fill = NULL, x = NULL, y = "Sentiment Frequency", title = "Sentiments of Each Character Using NRC Lexicon") +
  scale_fill_manual(values = c("#EA181E", "#00B4E8", "#FABE0F", "#EA181E", "#00B4E8", "#FABE0F"))

tidy_bing %>% 
  filter(author %in% c("Ross", "Monica", "Rachel", "Joey", "Chandler", "Phoebe")) %>% 
  group_by(season, author) %>% 
  count(sentiment) %>%
  ungroup() %>%
  ggplot(aes(season, n, fill = sentiment)) +
  geom_col(position = "fill") +
  geom_text(aes(label = n), position = position_fill(0.5), color = "white") +
  coord_flip() +
  facet_wrap(~author) +
  theme_dark() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  ) +
  scale_fill_manual(values = c("#EA181E", "#00B4E8")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(y = NULL, x = "Season", fill = NULL, title = "Negative-Positive Ratio in All Seasons Using Bing Lexicon")

df %>% 
  group_by(season) %>% 
  mutate(seq = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word, quote) %>% 
  anti_join(stop_words) %>% 
  filter(!word %in% tolower(author)) %>% 
  inner_join(bing) %>% 
  count(season, index = seq %/% 50, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(index, sentiment, fill = factor(season))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(paste0("Season ",season)~., ncol = 2, scales = "free_x")+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+
  labs(x = "Index", y = "Sentiment", title = "Negative-Positive Distribution in all seasons by using afinn lexicon")

all <- tidy_afinn %>% 
  mutate(Episode = factor(paste0("S",season,"-","E",episode_number))) %>% 
  group_by(Episode) %>% 
  summarise(total = sum(value), .groups = 'drop') %>% 
  ungroup %>% 
  mutate(Neg = if_else(total < 0, TRUE, FALSE))

ggplot()+
  geom_path(all, mapping = aes(Episode, total, group = 1), color = "#BA0E00")+
  geom_hline(mapping = aes(yintercept = 0), color = "#024D38")+
  theme_classic()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#EA181E", size = 20, face = "bold"))+
  geom_text((all %>% filter(Neg == TRUE)), mapping = aes(Episode, total-15, label = Episode), angle = 90, size = 3)+
  labs(title = "Total Sentiment Score each Episode with Afinn Lexicon", 
       y = "Total Sentiment Score")

tidy_afinn %>% 
  filter(author %in% c("Ross", "Monica", "Rachel", "Joey", "Chandler", "Phoebe")) %>% 
  group_by(season, author) %>% 
  summarise(total = sum(value), .groups = 'drop') %>% 
  ungroup() %>% 
  mutate(Neg = if_else(total < 0, TRUE, FALSE)) %>% 
  ggplot()+
  geom_path(aes(season, total, color = author), linewidth = 1.2)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_color_manual(values = c("#EA181E", "#00B4E8", "#FABE0F", "seagreen2", "orchid", "royalblue"))+
  labs(x = "Season", color = NULL, y = "Total Sentiment Score")

## Part VI: Analyzing LDA Model Results ####

# Prepare a Document-Term Matrix (DTM) for LDA topic modeling
dtm <- tidy_text %>% 
  select(season, word) %>% 
  group_by(season, word) %>% 
  count() %>% 
  cast_dtm(season, word, n)

# Perform LDA topic modeling with a fixed seed for reproducibility
lda <- LDA(dtm, k = 10, control = list(seed = 1234))

### Word-Topic Probabilities ####
# Extract and visualize the word-topic probabilities from the LDA model
topics <- tidy(lda, matrix = "beta")

# Identify and arrange the top terms for each topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualization of top terms in each topic
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Word-Topic Probabilities") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

### Document-Topic Probabilities ####
# Extract and visualize the document-topic probabilities from the LDA model
documents <- tidy(lda, matrix = "gamma")

# Visualization of document-topic distribution for each season
documents %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(position = "fill") +
  labs(x = "Season", fill = "Topic", y = "Gamma", title = "Document-Topic Probabilities") +
  scale_fill_ordinal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  )