##### ##### ##### ##### ##### ##### 
##### Scraping the Data ##### ##### 
##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### 
#####       Part I      ##### ##### 
##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### 
# Part I: parse_and_scrape function
##### ##### ##### ##### ##### ##### 

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

##### ##### ##### ##### ##### ##### 
#####       Part II     ##### ##### 
##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### 
# Part II: parse_single_episode function
##### ##### ##### ##### ##### ##### 

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

##### ##### ##### ##### ##### ##### 
#####       Part III    ##### ##### 
##### ##### ##### ##### ##### ##### 


##### ##### ##### ##### ##### ##### 
# Part III: Merge and Clean
##### ##### ##### ##### ##### ##### 

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

##### ##### ##### ##### ##### ##### 
#####       Part IV     ##### ##### 
##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### 
# Part IV: CSV and ZIP Files
##### ##### ##### ##### ##### ##### 

# Write the dataframe to a CSV file
write.csv(df, "friends_quotes.csv", row.names = FALSE)

# Compress the CSV file into a ZIP file
zip(zipfile = "friends_quotes.zip", files = "friends_quotes.csv")