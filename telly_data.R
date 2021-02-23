#load required packages ----
library(googlesheets4)
library(tidyverse)
library(lubridate)

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
  select(type,
         watch_date = Date,
         name = Name,
         my_rating = Rating,
         cinema_flag,
         first_watch_flag,
         imdb_key = IMDb)

clean_series <- import_personal_series %>%
  mutate(watched_series_id = row_number(),
         type = 'Series',
         season = as.character(Season),
         watch_duration_days = difftime(coalesce(`End Date`,date(today())), `Start Date`)) %>%
  select(type,
         watched_series_id,
         start_date = `Start Date`,
         end_date = `End Date`,
         watch_duration_days,
         name = Show,
         season,
         latest_episode = `Latest Episode`,
         imdb_key = IMDb)

#calculate the length (in seasons) of each series, which is later used to estimate the release year of each season
mapping_series_lengths <- import_imdb_episodes %>%
  group_by(parentTconst) %>%
  summarise(series_length = max(as.numeric(seasonNumber)))

#check that my manually entered film names match up with imdb's to ensure I haven't copied the wrong imdb IDs! ----
checks_films <- clean_films %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst")) %>%
  select(name, primaryTitle)
checks_series <- clean_series %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst")) %>%
  select(name, primaryTitle)

#join on imdb data and expand series data to episode-level ----
final_films <- clean_films %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst")) %>%
  left_join(import_imdb_ratings, c("imdb_key" = "tconst")) %>%
  select(type,
         watch_date,
         name = primaryTitle,
         my_rating,
         cinema_flag,
         first_watch_flag,
         release_year = startYear,
         runtime = runtimeMinutes,
         genres,
         imdb_rating = averageRating)

final_series <- clean_series %>%
  #join episode data and expand to episode-level
  left_join(import_imdb_episodes, c("imdb_key" = "parentTconst",
                                    "season" = "seasonNumber")) %>%
  left_join(mapping_series_lengths, c("imdb_key" = "parentTconst")) %>%
  left_join(import_imdb_ratings, c("tconst" = "tconst")) %>%
  #join overall series data
  mutate(episode_number = as.double(episodeNumber)) %>%
  #for partially-watched seasons, only show episodes that I've seen
  filter(
    is.na(latest_episode) | episode_number <= latest_episode
  ) %>%
  left_join(import_imdb_titles, c("imdb_key" = "tconst")) %>%
  group_by(watched_series_id, season) %>%
  mutate(season_length = max(episode_number)) %>%
  ungroup() %>%
  mutate(
    watch_date = as.Date(start_date + (round((episode_number-1)*(watch_duration_days)/(season_length-1)))),
    release_year = case_when(
      #if series start year is the same as end year according to imdb, assume release year is start year
      startYear == endYear ~ startYear,
      #if (currently) series has only had 1 season, use start year as release year
      series_length == 1 ~ startYear,
      #otherwise interpolate to estimate release year based on current max season number for each show
      TRUE ~ as.integer(startYear+round((as.numeric(season)-1)*(coalesce(endYear,as.integer(year(today())))-startYear)/(series_length-1))))
         ) %>%
  select(
    type,
    watch_date,
    name = primaryTitle,
    release_year,
    runtime = runtimeMinutes,
    genres,
    imdb_rating = averageRating,
    season,
    episode_number
  )

##then union together ----
output <- final_films %>%
  union_all(final_series)
write_csv(output, file = "telly_data_cleaned.csv")
write_sheet(output, ss = "1LRJZX2ErRrWdxo4VAyyVbrUuEy4SZ1zUgr80aksLra0", sheet = "Combined")
