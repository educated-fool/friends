library(data.table)
library(rvest) # read_html 的包

pages <- paste0("https://transcripts.foreverdreaming.org/viewforum.php?f=845&start=", seq(0, 225, 25))

process_one_link <- function(my_link){
   t <- read_html(my_link)
   episodes <- list()
   episodes[['name']] <- t %>% html_nodes(".topictitle") %>% html_text()
   episodes[['link']] <- t %>% html_nodes(".topictitle") %>% html_attr('href')
   return(episodes)
 }

episode_links <- data.table(rbindlist(lapply(pages, process_one_link)))
 
episode_links <- episode_links[name != "Updates: 5/1/22 Editors Needed :)",]

link <- episode_links$link[2]

get_transcript <- function(link) {
    print(link)
    t <- read_html(paste0("https://transcripts.foreverdreaming.org", str_sub(link, start = 2) ))
    transcript <- t %>% html_nodes("#pagecontent p") %>% html_text()
    tinfo <- t %>% html_nodes('h2') %>% html_text()
    transcript <- str_subset(transcript, "^(?!\\[)")
    transcript <- str_subset(transcript, "^(?!\\()")
    transcript <- str_subset(transcript, "^(?!Scene)")
    transcript<- transcript[grepl(':', transcript, fixed = T)]

    textdf <- 
     rbindlist(
        lapply(transcript, function(x){
          t_piaces <- strsplit(x, ':')[[1]]
          data.table('actor' = t_piaces[1], 'text' = trimws(paste(t_piaces[2:length(t_piaces)], collapse = " " )) )
        })
      )
    
    textdf$season <- substring(tinfo, 1, 2)
    textdf$episode <- substring(tinfo, 4, 5)
    textdf$title <- substring(tinfo, 9,nchar(tinfo))
    return(textdf)
}

# 先安装这两个
install.packages("pbapply")
library(pbapply)

install.packages("stringr")
library(stringr)


t_list <- pblapply(episode_links$link, get_transcript)
full_df <- rbindlist(t_list, fill = T)
# 这段运行需要等一会 大概10分钟？

saveRDS(full_df, "friends_data.rds")
 
write.csv(full_df, "friends_data.csv", row.names = F)
 

Data can be downloaded from here: https://github.com/maryamkhan1120/DS2/blob/main/Final-Project/friends_data.rds
Since the file was too big I was unable to call it directly from gtihub
data <- readRDS("friends_data.rds")