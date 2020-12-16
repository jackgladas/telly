#load required packages ----
library(googlesheets4)
library(tidyverse)

#import raw personal data ----
import_personal_films <- read_sheet("1LRJZX2ErRrWdxo4VAyyVbrUuEy4SZ1zUgr80aksLra0", sheet = "Films")
import_personal_series <- read_sheet("1LRJZX2ErRrWdxo4VAyyVbrUuEy4SZ1zUgr80aksLra0", sheet = "Series")

#import raw IMDb data ----
download.file("https://datasets.imdbws.com/title.basics.tsv.gz",
              destfile = "title.basics")
import_imdb_titles <- read_delim("title.basics",
                                  delim = "\t",
                                  col_types = cols(
                                    tconst = "c",
                                    titleType = "c",
                                    primaryTitle = "c",
                                    originalTitle = "c",
                                    isAdult = "f",
                                    startYear = 'i',
                                    endYear = 'i',
                                    runtimeMinutes = 'i',
                                    genres = 'c'
                                  ),
                                  na = c("",
                                         "NA",
                                         "\\N",
                                         " "),
                                  quoted_na = FALSE,
                                  escape_double = FALSE)
download.file("https://datasets.imdbws.com/title.episode.tsv.gz",
              destfile = "title.episode")
import_imdb_episodes <-read_tsv("title.episode")

#basic data cleaning (no joins yet) ----
clean_films <- import_personal_films %>%
  mutate(watched_film_id = row_number()) %>%
  rename(watched_date = Date,
         film_name = Name,
         rating = Rating,
         cinema = Cinema,
         first_watch = 'First Watch',
         imdb_key = IMDb) %>%
  mutate(cinema_flag = as.factor(cinema),
         first_watch_flag = as.factor(first_watch)) %>%
  select(watched_film_id,
         watched_date,
         film_name,
         rating,
         cinema_flag,
         first_watch_flag,
         imdb_key)

x_checks <- clean_films %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst")) %>%
  select(film_name, primaryTitle)

films <- clean_films %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst"))
#remove raw imports
rm(import_personal_films)
