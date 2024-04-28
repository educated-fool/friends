## 1. Data Collection and Preprocessing 

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

## 2. Exploratory Data Analysis
## Part I: Initial Setup and Data Preparation ####

# Load necessary libraries
library(tidyverse)
library(tidytext)
library(topicmodels)
library(DT)
library(png)
library(grid)
library(wordcloud)
library(circlize)
library(RColorBrewer)
library(ggraph)
library(igraph)
library(reshape2)
library(ggimage)
library(plotly)


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

## Part IV: Sentiment Analysis Preparation ####

# Sentiment analysis with Bing lexicon
tidy_bing <- tidy_text %>% inner_join(bing)

# Sentiment analysis with NRC lexicon
tidy_nrc <- tidy_text %>% inner_join(nrc)

# Sentiment analysis with AFINN lexicon
tidy_afinn <- tidy_text %>% inner_join(afinn)

## Voices of Influence: Highlighting the Dominant Characters in Each Episode of 'Friends' ####
# Summarize the count of quotes by season, episode, and author
quote_counts <- df %>%
  group_by(season, episode, author) %>%
  summarise(quote_count = n(), .groups = "drop")

# Select the character with the most quotes in each episode
top_authors <- quote_counts %>%
  arrange(desc(quote_count)) %>%
  group_by(season, episode) %>%
  slice(1) %>%
  ungroup()

# Create labels and parent nodes for the treemap
labels <- c("Friends", paste("Season", unique(top_authors$season)), paste("Season", top_authors$season, "Episode", top_authors$episode, sep = " "))
parents <- c("", rep("Friends", length(unique(top_authors$season))), rep(paste("Season", top_authors$season, sep = " "), each = 1))

# Generate hover text showing only season and episode
hover_text <- paste(labels, "<br>Most Vocal Character: ", top_authors$author)

# Generate the treemap
fig <- plot_ly(
  type = "treemap",
  labels = labels,
  parents = parents,
  text = hover_text,
  hoverinfo = "text",
  marker = list(colorscale = "Reds")
)

# Display the treemap
fig

## Dialogue Dynamics: Words and Lines Spoken by Friends Characters ####
character_summary <- df %>%
  group_by(author) %>%
  summarise(
    line_count = n(),
    word_count = sum(str_count(quote, "\\S+"))
  ) %>%
  ungroup()
# Set the image path for each character
character_summary$image_path <- c(
  "Chandler" = "pics/Chandler.png",
  "Joey" = "pics/Joey.png",
  "Monica" = "pics/Monica.png", 
  "Phoebe" = "pics/Phoebe.png",
  "Rachel" = "pics/Rachel.png", 
  "Ross" = "pics/Ross.png"
)

# Create the plot using ggplot2
ggplot(character_summary, aes(x = word_count, y = line_count, size = line_count)) +
  geom_image(aes(image = image_path), size = 0.05) +
  scale_size_continuous(range = c(3, 10)) +
  theme_minimal() +
  labs(title = "Dialogue Dynamics: Words and Lines Spoken by Friends' Characters",
       subtitle = "Analyzing character engagement throughout the series",
       x = "Count of Words",
       y = "Number of lines") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16)) 

## Seasonal Dialogue Distribution Among Friends Characters ####
speaking_count <- df %>%
  group_by(season, author) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup() %>%
  mutate(max_count = max(count)) %>%
  mutate(norm_count = count / max_count) %>%
  select(season, author, norm_count)

speaking_count_long <- speaking_count %>%
  pivot_longer(cols = norm_count, names_to = "variable", values_to = "value")

ggplot(speaking_count_long, aes(x = author, y = season, fill = value)) +
  geom_tile() + 
  geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
  scale_fill_gradientn(colors = brewer.pal(9, "Blues")) +
  labs(title = "Seasonal Dialogue Distribution Among Friends' Characters",
       subtitle = "Comparative analysis of speaking volumes by season",
       x = "",
       y = "Season") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14))

## Character Interaction Dynamics in Narrative Analysis ####
# Calculate dialogue counts
dialogue_counts <- df %>%
  mutate(next_author = lead(author)) %>%
  filter(author != next_author) %>%
  group_by(author, next_author) %>%
  summarise(count = n(), .groups = 'drop') %>%  # Use .groups='drop' to ungroup after summarising
  filter(!is.na(next_author)) %>%
  rename(From = author, To = next_author, Value = count)

# Plotting a chord diagram to visualize interactions
chordDiagram(as.data.frame(dialogue_counts))

# Calculate interaction pairs
interaction_pairs <- df %>%
  mutate(next_author = lead(author)) %>%
  filter(!is.na(next_author) & author != next_author) %>%
  group_by(author, next_author) %>%
  summarise(interactions = n(), .groups = 'drop')  # Again, dropping groups after summarising

# Create an undirected graph from the interaction pairs
graph <- graph_from_data_frame(interaction_pairs, directed = FALSE)

# Calculate the correlation (or some measure of strength of relationship) between characters
# Here, we just use the number of interactions as a proxy for correlation
correlation_matrix <- as_adjacency_matrix(graph, attr = "interactions", sparse = FALSE)
colnames(correlation_matrix) <- V(graph)$name
rownames(correlation_matrix) <- V(graph)$name

## Mapping Character Interactions: Network Visualization of Dialogue Dynamics ####
# Use a circle layout to evenly distribute nodes
# Adjust edge width and transparency to be more pronounced for higher interactions
ggraph(graph, layout = 'circle') +
  geom_edge_link(aes(edge_width = sqrt(interactions), edge_alpha = sqrt(interactions)), edge_colour = "gold") +
  geom_node_point(color = "darkred", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3.5) +
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) 

## Word Cloud: Positive vs. Negative Words in Friends ####
Friends <- read.csv("data/friends_quotes.csv", stringsAsFactors = FALSE)
Friends <- Friends %>%
  mutate(text = as.character(quote))
friends_tokens <- Friends %>%
  unnest_tokens(word, text)
wordcloud_pos_neg <- friends_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#E91E22", "#F1BF45"), max.words = 200)

generate_wordcloud_for_author <- function(data, author_name, min_freq = 1, max_words = 200, colors = brewer.pal(8, "Dark2")) {
  # Subset the data for the specified author
  author_data <- subset(data, author == author_name)
  
  # Unnest words from the quotes
  author_words <- author_data %>%
    unnest_tokens(word, quote)
  
  # Remove common stop words
  data("stop_words")
  author_words_cleaned <- author_words %>%
    anti_join(stop_words, by = "word")
  
  # Create a frequency table of words
  word_freq <- author_words_cleaned %>%
    count(word, sort = TRUE)
  
  # Set seed for reproducibility of the word cloud
  set.seed(1234)
  
  # Generate the word cloud
  wordcloud(words = word_freq$word,
            freq = word_freq$n,
            min.freq = min_freq,  # minimum frequency for words to be plotted
            max.words = max_words,  # maximum number of words to be plotted
            random.order = FALSE,  # plot words according to their frequency
            rot.per = 0.35,  # proportion of words displayed vertically
            scale = c(3, 0.5),  # scale between the largest and smallest words
            colors = colors)  # colors for the word cloud
  main = paste("Word Cloud for", author_name)
}

## Word Clouds for Each Friends Character ####
# Use the function to generate word clouds for a list of six authors
authors <- c("Ross", "Rachel", "Joey", "Monica", "Chandler", "Phoebe")
for (author in authors) {
  generate_wordcloud_for_author(df, author)  # Generate the word cloud for each author
}

create_word_proportion_plot <- function(character1, character2) {
  plot_data <- tidy_text %>%
    filter(author %in% c(character1, character2)) %>% 
    count(author, word) %>%
    group_by(author) %>% 
    mutate(proportion = round(n / sum(n), 3)) %>%
    select(-n) %>% 
    pivot_wider(names_from = author, values_from = proportion, values_fill = list(proportion = 0)) %>%
    ungroup() %>%
    mutate(
      !!character1 := ifelse(.data[[character1]] == 0, 0.0001, .data[[character1]]),
      !!character2 := ifelse(.data[[character2]] == 0, 0.0001, .data[[character2]])
    )
  
  log_format <- function(base = 10) {
    function(x) {
      paste0(base, "^", round(log(x, base), 1))
    }
  }
  
  ggplot(plot_data, aes(x = .data[[character1]], y = .data[[character2]], color = abs(.data[[character1]] - .data[[character2]]))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.05, size = 1, width = 0.1, height = 0.1) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 0.5, size = 3.5) +
    scale_x_log10(labels = log_format()) +
    scale_y_log10(labels = log_format()) +
    scale_color_gradient(limits = c(0, 0.01), low = "darkslategray4", high = "gray75") +
    theme_minimal() +
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
    labs(title = paste("Word Proportion Comparison:", character1, "vs", character2),
         x = paste("Proportion of Words by", character1),
         y = paste("Proportion of Words by", character2))
}

## Word Proportion Plot: Comparing Word Usage Between Two Characters ####
create_word_proportion_plot("Ross", "Rachel")
create_word_proportion_plot("Chandler", "Monica")
create_word_proportion_plot("Joey", "Phoebe")

## Negative-Positive Distribution in all seasons by using afinn lexicon ####
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
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14))+
  labs(x = "Index", y = "Sentiment", title = "Negative-Positive Distribution in all seasons by using afinn lexicon", subtitle = "Emotional Arcs Across Friends Seasons: A Sentiment Analysis Breakdown")

## Total Sentiment Score by Episode with Afinn Lexicon ####
all <- tidy_afinn %>%
  group_by(episode_number) %>%
  summarise(total = sum(value), .groups = 'drop') %>%
  ungroup() %>%
  mutate(Neg = if_else(total < 0, TRUE, FALSE))

# Plotting
ggplot() +
  geom_path(data = all, mapping = aes(episode_number, total, group = 1), color = "#BA0E00") +
  geom_hline(mapping = aes(yintercept = 0), color = "#024D38") +
  theme_classic() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#EA181E", size = 20, face = "bold")) +
  geom_text(data = all %>% filter(Neg == TRUE), mapping = aes(episode_number, total - 15, label = episode_number), angle = 90, size = 2) +
  labs(title = "Total Sentiment Score by Episode with Afinn Lexicon", 
       y = "Total Sentiment Score")

## Research Question 1

## Expressions of Affection: Analyzing 'Love' in Character Dialogues ####
data_tokens <- df %>%
  mutate(line = as.character(quote)) %>%
  unnest_tokens(word, quote)

# Count the occurrences of the word "love" for each author
love_counts <- data_tokens %>%
  filter(word == "love") %>%
  count(author, sort = TRUE)


ggplot(love_counts, aes(x = reorder(author, -n), y = n)) +
  geom_point(size = 25, color = "lightpink2") +  
  geom_segment(aes(x = author, y = 0, xend = author, yend = n), color = "lightpink2") +  # Add lines connecting circles to the x-axis
  geom_text(aes(label = n), hjust = 0.5, vjust = 0.5,size=4) +  
  labs(x = NULL, y = "Number of times 'love' is used", title = "Expressions of Affection: Analyzing 'Love' in Character Dialogues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5 , size=10,face = "bold"),plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))  

## Tracing Material Desires: Analyzing Materialism in Character Dialogues
data_tokens <- data_tokens %>%
  mutate(word = tolower(word))

materialism_topics <- c("cars","jewelry","contracts","gucci","prada","chanel","louis vuitton", "estate", "fashion","money", "career", "wealth", "riches","rich","shopping", "possessions", "luxury", "affluence", "consumerism", "greed", "ambition", "success", "prosperity", "fortune", "money-minded", "capitalism","fortune","successful", "acquisition","bloomingdale","boots","prestige","design","designer","brand","glamour","prestige","affluence","fame","greed","prosperties","trendy","couture","fashionable","luxury","extravagance","status")


# Count the occurrences of the relevant topics for each author
materialism_counts <- data_tokens %>%
  filter(word %in% materialism_topics) %>%
  count(author, sort = TRUE)

# Plot the graph
ggplot(materialism_counts, aes(x = reorder(author, -n), y = n)) +
  geom_point(size = 25, color = "royalblue") +  
  geom_segment(aes(x = author, y = 0, xend = author, yend = n), color = "royalblue") +  
  geom_text(aes(label = n), hjust = 0.5, vjust = 0.5, size=5, color="white") + 
  labs(x = NULL, y = "Number of times materialistic topics are mentioned", title = "Tracing Material Desires: Analyzing Materialism in Character Dialogues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5 , size=10, face = "bold"),plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

## Tracking the Pulse of Relationships: Seasonal Trends in Love, Break-ups, and Marriage ####
# Create a new variable indicating the topic of each quote
data_topic <- df %>%
  mutate(topic = case_when(
    grepl("\\b(break[- ]?up|split|separate|divorce|part ways|broken[- ]?heart|end[- ]?relationship)\\b", quote, ignore.case = TRUE) ~ "break-up",
    grepl("\\b(love|adore|affection|romance|passion|devotion|amour)\\b", quote, ignore.case = TRUE) ~ "Love",
    grepl("\\b(marriage|wedding|matrimony|union|nuptials|spouse|husband|wife)\\b", quote, ignore.case = TRUE) ~ "Marriage",
    TRUE ~ "Other"
  ))

# Aggregate the data by season and count the occurrences of each topic
topic_counts <- data_topic %>%
  group_by(season, topic) %>%
  summarise(count = n()) %>%
  filter(topic %in% c("break-up", "Love", "Marriage")) 

ggplot(topic_counts, aes(x = season, y = count, color = topic, group = topic)) +
  geom_line(size = 1) +
  labs(x = "Season", y = "Frequency", title = "Tracking the Pulse of Relationships: Seasonal Trends in Love, Break-ups, and Marriage") +
  scale_color_manual(values = c("break-up" = "black", "Love" = "plum2", "Marriage" = "indianred3")) + # Specify colors for each topic
  scale_x_continuous(breaks = 1:max(topic_counts$season)) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

## Research Question 2

## Sentiments of Each Character Using NRC Lexicon ####
# Create a specific order for the authors
tidy_nrc$author <- factor(tidy_nrc$author, levels = c("Chandler", "Joey", "Ross", "Monica", "Phoebe", "Rachel"))

# Generate the plot
ggplot(tidy_nrc %>% filter(author %in% c("Ross", "Monica", "Rachel", "Joey", "Chandler", "Phoebe")), 
       aes(sentiment, fill = author)) +
  geom_bar(stat = "count", show.legend = FALSE) +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, color = "white", size = 2.5) +
  facet_wrap(~ author, nrow = 2, ncol = 3) +  # Set the number of rows and columns
  theme_dark() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels to 45 degrees
  ) +
  labs(fill = NULL, x = NULL, y = "Sentiment Frequency", title = "Sentiments of Each Character Using NRC Lexicon") +
  scale_fill_manual(values = c("#EA181E", "#00B4E8", "#FABE0F", "#EA181E", "#00B4E8", "#FABE0F"))

## Negative-Positive Ratio in All Seasons Using Bing Lexicon ####
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

## Sentiment Trajectory Across Seasons for Friends Characters ####
tidy_afinn %>% 
  filter(author %in% c("Ross", "Monica", "Rachel", "Joey", "Chandler", "Phoebe")) %>% 
  group_by(season, author) %>% 
  summarise(total = sum(value), .groups = 'drop') %>% 
  ungroup() %>% 
  mutate(Neg = if_else(total < 0, TRUE, FALSE)) %>% 
  ggplot()+
  geom_path(aes(season, total, color = author), linewidth = 1.2)+
  geom_point(aes(season, total, color = author), size = 3)+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_color_manual(values = c("#EA181E", "#00B4E8", "#FABE0F", "seagreen2", "orchid", "royalblue"))+
  labs(x = "Season", color = NULL, y = "Total Sentiment Score", title = "Sentiment Trajectory Across Seasons for Friends Characters")

## Distinguishing Lexicons: Analyzing Character-Specific Keywords Across Narratives ####
# Convert the text data to a DTM
dtm <- tidy_text %>%
  count(author, word) %>%
  cast_dtm(document = author, term = word, value = n)

# Convert DTM to a tidy data frame and calculate TF-IDF
tidy_dtm <- dtm %>%
  tidy() %>%
  bind_tf_idf(term, document, count)

# Filter to get the top 10 terms for each author based on TF-IDF
top_terms_per_author <- tidy_dtm %>%
  group_by(document) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  arrange(document, -tf_idf)

# Define a specific order for the authors and their corresponding colors
author_order <- c("Chandler", "Joey", "Ross", "Monica", "Phoebe", "Rachel")
author_colors <- setNames(c("#EA181E", "#00B4E8", "#FABE0F", "#EA181E", "#00B4E8", "#FABE0F"), author_order)

# Ensure that the 'document' factor in the data frame is in the specified order
top_terms_per_author$document <- factor(top_terms_per_author$document, levels = author_order)

# Generate the plot with specified author colors and order
ggplot(top_terms_per_author, aes(x = reorder_within(term, tf_idf, document), y = tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, nrow = 2, scales = "free_y") +  # Organize facets in two rows
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = author_colors) +  # Apply the custom color scheme
  labs(title = "Distinguishing Lexicons: Analyzing Character-Specific Keywords Across Narratives",
       x = "Term Importance (TF-IDF)",
       y = "Terms") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)  # Improve x-axis label readability
  )