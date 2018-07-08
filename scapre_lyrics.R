library(rvest)
library(tidyverse)

# get all album names
url <- "https://www.azlyrics.com/q/queen.html"

get_album_name <- function(url) {
  url %>% 
    read_html() %>% 
    html_nodes("div#listAlbum") %>% 
    html_nodes("div.album") %>% 
    html_text()
}

get_album_links <- function(url) {
  url %>% 
    read_html() %>% 
    html_nodes("div#listAlbum") %>% 
    html_nodes("a") %>% 
    html_attr("href")  
}

url %>% 
  get_album_links() %>% 
  str_replace("..", "https://www.azlyrics.com") -> album_links

get_lyrics <- function(link) {
  song_album <- link %>% 
    read_html() %>% 
    html_nodes("b") %>% 
    html_text() %>% 
    str_split("\\\\") %>% 
    str_replace_all("\"", "")
  
  song_lyrics <- link %>% 
    read_html() %>% 
    html_node(xpath = "/html/body/div[3]/div/div[2]/div[5]") %>% 
    html_text() %>% 
    str_split("\\n") %>% 
    flatten_chr() %>% 
    discard(~ .x == "\r" | .x == "")
  
  tibble(
    song = song_album[[2]],
    album = song_album[[3]],
    lyrics = song_lyrics
  )  
}


get_lyrics_possibly <- possibly(get_lyrics, NULL)

pb <- progress_estimated(length(album_links[1:3]))
lyrics <- map_dfr(album_links[1:3], ~ {
  pb$tick()$print()
  # df <- .x
  df <- get_lyrics_possibly(.x)
  Sys.sleep(sample(3:10, 1))
  df
})


# get lyrics using songlyrics.com -----------------------------------------
url <- "http://www.songlyrics.com/queen-lyrics/"

url %>% 
  read_html() %>% 
  html_node("table.tracklist") %>% 
  html_table()

url %>% 
  read_html() %>% 
  html_nodes(".tracklist a") %>% 
  html_attr("href")

get_links <- function(url) {
  url %>% 
    read_html() %>% 
    html_nodes(".tracklist a") %>% 
    html_attr("href")
}

lyrics_links <- get_links(url)

get_lyrics <- function(link) {
  lyrics <- link %>% 
    read_html() %>% 
    html_node("#songLyricsDiv") %>% 
    html_text() %>% 
    str_split("\\n") %>% 
    flatten_chr() %>% 
    str_remove("\\r")
  
  artist <- lyrics[1]
  album <- lyrics[2]
  song <- lyrics[3]
  
  tibble(
    artist = artist,
    album  = album,
    song   = song,
    lyrics = lyrics[-c(1:3)]
  ) 
}

get_lyrics(lyrics_links[1])

get_lyrics_possibly <- possibly(get_lyrics, NULL)
pb <- progress_estimated(length(lyrics_links[1:5]))

lyrics <- map_dfr(lyrics_links[1:3], ~ {
  pb$tick()$print()
  # df <- .x
  df <- get_lyrics_possibly(.x)
  Sys.sleep(10)
  df
})
