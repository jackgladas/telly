#load required packages ----
library(googlesheets4)
library(tidyverse)

#import raw data ----
import_films <- read_sheet("1LRJZX2ErRrWdxo4VAyyVbrUuEy4SZ1zUgr80aksLra0", sheet = "Films")
import_series <- read_sheet("1LRJZX2ErRrWdxo4VAyyVbrUuEy4SZ1zUgr80aksLra0", sheet = "Series")



#basic data cleaning ----
clean_films <- import_films %>%
  mutate(watched_film_id = row_number()) %>%
  rename(watched_date = Date,
         film_name = Name,
         rating = Rating,
         cinema = Cinema) %>%
  mutate(cinema_flag = as.factor(cinema)) %>%
  select(watched_film_id,
         watched_date,
         film_name,
         rating,
         cinema_flag)

## import IMDB data from: https://www.imdb.com/interfaces/
