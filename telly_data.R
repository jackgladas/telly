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
import_imdb_episodes <- read_tsv("title.episode")
download.file("https://datasets.imdbws.com/title.ratings.tsv.gz",
             destfile = "title.ratings")
import_imdb_ratings <- read_tsv("title.ratings")

#basic data cleaning (no joins yet) ----
clean_films <- import_personal_films %>%
  mutate(cinema_flag = case_when(
          Cinema == 'Yes' ~ TRUE,
          is.na(Cinema) ~ FALSE
         ),
         first_watch_flag = case_when(
           `First Watch` == 'Yes' ~ TRUE,
           is.na(`First Watch`) ~ FALSE
         ),
         type = 'Film') %>%
  rename(watched_date = Date,
         name = Name,
         my_rating = Rating,
         cinema = Cinema,
         imdb_key = IMDb) %>%
  select(type,
         watched_date,
         name,
         my_rating,
         cinema_flag,
         first_watch_flag,
         imdb_key)

clean_series <- import_personal_series %>%
  mutate(watched_series_id = row_number(),
         type = 'Series',
         season = as.character(Season),
         watch_duration_days = difftime(`End Date`, `Start Date`)) %>%
  rename(name = Show,
         imdb_key = IMDb,
         start_date = 'Start Date',
         end_date = 'End Date') %>%
  select(type,
        watched_series_id,
        start_date,
        end_date,
        watch_duration_days,
        name,
        season,
        imdb_key)

#check that my manually entered film names match up with imdb's to ensure I haven't copied the wrong imdb IDs! ----
checks_films <- clean_films %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst")) %>%
  select(name, primaryTitle)
checks_series <- clean_series %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst")) %>%
  select(name, primaryTitle)

#join on imdb data and expand series data to episode-level ----
films <- clean_films %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst")) %>%
  left_join(import_imdb_ratings, c("imdb_key" = "tconst"))

series <- clean_series %>%
  #join episode data and expand to episode-level
  left_join(import_imdb_episodes, c("imdb_key" = "parentTconst",
                                    "season" = "seasonNumber")) %>%
  left_join(import_imdb_ratings, c("tconst" = "tconst")) %>%
  #join overall series data
  mutate(episode_number = as.double(episodeNumber)) %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst")) %>%
  group_by(watched_series_id, season) %>%
  mutate(season_length = max(episode_number)) %>%
  ungroup() %>%
  mutate(day_watched = round((episode_number-1)*(watch_duration_days)/(season_length-1)),
         watched_date = as.Date(start_date + day_watched)) %>%
  filter(episode_number == 1 | episode_number == season_length) %>%
  select(watched_series_id, start_date, end_date, episode_number, watched_date) %>%
  arrange(watched_series_id, episode_number)

#clean up raw imports ----
rm(import_personal_films)
rm(import_personal_series)
rm(clean_films)
rm(clean_series)
rm(import_imdb_episodes)
rm(import_imdb_titles)
rm(checks_films)
rm(checks_series)
