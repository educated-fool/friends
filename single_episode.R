library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# Function to parse a single episode's page
parse_single_episode <- function(page_content) {
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
  dialogues_lines <- str_split(text_all, "\n")[[1]]
  
  # Filter lines that represent dialogues
  dialogue_lines <- dialogues_lines[str_detect(dialogues_lines, "^(JOEY|CHANDLER|MONICA|PHOEBE|ROSS|RACHEL):")]
  
  # Extract author and quote
  authors <- str_extract(dialogue_lines, "^[A-Z]+")
  quotes <- str_replace(dialogue_lines, "^[A-Z]+:", "")
  
  # Generate quote order
  quote_order <- seq_along(quotes)
  
  # Assuming the season and episode are known or extracted from somewhere
  season <- 2 # Example value
  episode <- 14 # Example value
  episode_number <- sprintf("S%02dE%02d", season, episode)
  
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

# URL of the specific episode page
episode_url <- "https://fangj.github.io/friends/season/0214.html" # Example URL

# Read the HTML content of the page
page_content <- read_html(episode_url)

# Parse the episode page
episode_data <- parse_single_episode(page_content)

# Display the parsed data
print(episode_data)
