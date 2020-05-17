library(tidyverse)
library(rvest)

# GBV Database website

gbv_disco_url <- "http://www.gbvdb.com/artist.asp?artistsort=Guided%20By%20Voices"

gbv_disco_read <- read_html(gbv_disco_url)

# scrape titles

disco_titles <- gbv_disco_read %>% 
  html_nodes('.HighlightOff') %>% 
  html_text() 

# remove unwanted characters - only keep studio albums - release year at end of each string
studio_albums_cleaned <- disco_titles %>% 
  str_remove_all("\r|\n|\t") %>% 
  .[1:31]

studio_albums_cleaned

# scrape the related urls
disco_urls <- gbv_disco_read %>% 
  html_nodes('.HighlightOff a') %>% 
  html_attr('href') 

# some urls duplicated - remove duplicates and take only studio albums
disco_urls_cleaned <- unique(disco_urls) %>% 
  .[1:31]

# create dataset of album name, url and release year
gbv_studio_albums <- tibble(album_name = studio_albums_cleaned,
                            url = disco_urls_cleaned) %>% 
  mutate(full_url = paste0("http://www.gbvdb.com/", url),
         year_release = str_sub(album_name, -4, -1),
         album_name = str_sub(album_name, 1, -5)) %>% 
  select(-url) %>% 
  # no track listing for new album yet
  filter(row_number() != 1)

# test 1 album url to scrape tracklist
# need to remove blank elements - will do that at end

test_tracklist <- read_html("http://www.gbvdb.com/album.asp?albumid=3405") %>% 
  html_nodes('.HighlightOff td') %>% 
  html_text() %>% 
  str_remove_all("\r|\n|\t") %>% 
  str_remove("^[0-9]{1,}\\.") %>% 
  str_trim()


