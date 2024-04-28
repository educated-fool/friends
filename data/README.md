# Friends Quotes Dataset

## Overview
This dataset contains transcripts from the TV show "Friends", capturing dialogues from various episodes across all seasons. Each row represents a single line spoken by a character.

## Dataset Structure
The `friends_quotes.csv` file is structured with the following columns:

- **season**: Numeric identifier of the season.
- **episode**: Numeric identifier of the episode within the season.
- **episode_number**: Unique code for the episode, usually combining season and episode number (e.g., S01E01 for Season 1, Episode 1).
- **title**: Title of the episode.
- **author**: The character who speaks the line.
- **quote**: The actual text of the quote spoken by the character.
- **quote_order**: A sequence number representing the order of the quote within the episode.

## Usage
This dataset can be used for various analytical and educational purposes, including but not limited to:
- Text analysis to study character speech patterns.
- Machine learning models for natural language processing, such as training dialogue generation models.
- Statistical analysis to understand the distribution of dialogue among characters.

## Sample Data
Here is a brief look at what the data entries look like in the `friends_quotes.csv`:

```csv
season,episode,episode_number,title,author,quote,quote_order
1,1,"S01E01","Monica Gets A Roommate","Monica","There's nothing to tell! He's just some guy I work with!",1
1,1,"S01E01","Monica Gets A Roommate","Joey","C'mon, you're going out with the guy! There's gotta be something wrong with him!",2
1,1,"S01E01","Monica Gets A Roommate","Chandler","All right Joey, be nice. So does he have a hump? A hump and a hairpiece?",3
1,1,"S01E01","Monica Gets A Roommate","Phoebe","Wait, does he eat chalk?",4
