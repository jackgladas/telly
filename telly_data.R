#load required packages ----
library(googlesheets4)
library(tidyverse)
library(lubridate)

#import raw personal data ----
#sheet ID taken from sheets URL
gsheet_id <- ("1LRJZX2ErRrWdxo4VAyyVbrUuEy4SZ1zUgr80aksLra0")
import_personal_films <- read_sheet(gsheet_id, sheet = "Films")
import_personal_series <- read_sheet(gsheet_id, sheet = "Series")

#import raw IMDb data ----
#list of titles - 1 row per film/series
download.file("https://datasets.imdbws.com/title.basics.tsv.gz",
              destfile = "title.basics")
#need to use read_delim function as read_tsv throws lots of corrupt rows errors
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

#list of tv episodes - 1 row per episode
download.file("https://datasets.imdbws.com/title.episode.tsv.gz",
              destfile = "title.episode")
import_imdb_episodes <- read_tsv("title.episode")

#list of ratings - 1 row per film/episode
download.file("https://datasets.imdbws.com/title.ratings.tsv.gz",
             destfile = "title.ratings")
import_imdb_ratings <- read_tsv("title.ratings")

#basic data cleaning of my personal data (no joins to IMDB yet) ----
clean_films <- import_personal_films %>%
  mutate(
    #convert my cinema column to boolean
    cinema_flag = case_when(Cinema == 'Yes' ~ TRUE,
                            is.na(Cinema) ~ FALSE),
    #convert my first watch column to boolean
    first_watch_flag = case_when(`First Watch` == 'Yes' ~ TRUE,
                                 is.na(`First Watch`) ~ FALSE),
    #add type indicator for when films and series data are unioned later
    type = 'Film'
  ) %>%
  #only continue with columns of interest, and make naming convention consistent
  select(
    type,
    watch_date = Date,
    name = Name,
    my_rating = Rating,
    cinema_flag,
    first_watch_flag,
    imdb_title_key = IMDb
  )
#remove raw import
rm(import_personal_films)

clean_series <- import_personal_series %>%
  mutate(
    #add id for season, to be used later
    watched_season_id = row_number(),
    #add type indicator for when films and series data are unioned later
    type = 'Series',
    #how long I spent watching that season - or have done so far, if not finished yet
    watch_duration_days = difftime(coalesce(`End Date`, date(today(
      ))), `Start Date`),
    #converting to character to enable join to IMDB character field later 
    season = as.character(Season)
  ) %>%
  #only continue with columns of interest, and make naming convention consistent
  select(
    type,
    watched_season_id,
    start_date = `Start Date`,
    end_date = `End Date`,
    watch_duration_days,
    name = Show,
    season,
    latest_episode = `Latest Episode`,
    imdb_title_key = IMDb
  )
#remove raw import
rm(import_personal_series)

#calculate the length (in seasons) of each series, which is later used to estimate the release year of each season
mapping_series_lengths <- import_imdb_episodes %>%
  inner_join(clean_series, c("parentTconst" = "imdb_title_key")) %>%
  group_by(parentTconst) %>%
  summarise(series_length = max(as.numeric(seasonNumber)))

#check that my manually entered film names match up with IMDB's ----
#view each of these and skim through to ensure I haven't copied the wrong IMDB IDs! 
checks_films <- clean_films %>%
  left_join(import_imdb_titles, c("imdb_title_key" = "tconst")) %>%
  select(name, primaryTitle)
checks_series <- clean_series %>%
  left_join(import_imdb_titles, c("imdb_title_key" = "tconst")) %>%
  select(name, primaryTitle)
rm(checks_films, checks_series)

#join on imdb data and expand series data to episode-level ----
final_films <- clean_films %>%
  #get film names, runtimes, years and genres
  left_join(import_imdb_titles, c("imdb_title_key" = "tconst")) %>%
  #get film IMDB ratings
  left_join(import_imdb_ratings, c("imdb_title_key" = "tconst")) %>%
  select(type,
         watch_date,
         name = primaryTitle,
         my_rating,
         cinema_flag,
         first_watch_flag,
         release_year = startYear,
         runtime = runtimeMinutes,
         genres,
         imdb_rating = averageRating,
         imdb_title_key)
rm(clean_films)

final_series <- clean_series %>%
  #join to IMDB episode tables to expand to episode-level with episode_number column
  left_join(import_imdb_episodes, c("imdb_title_key" = "parentTconst",
                                    "season" = "seasonNumber")) %>%
  #get max season number for each series
  left_join(mapping_series_lengths, c("imdb_title_key" = "parentTconst")) %>%
  #get IMDB rating for each episode
  left_join(import_imdb_ratings, c("tconst" = "tconst")) %>%
  mutate(episode_number = as.double(episodeNumber)) %>%
  #for partially-watched seasons, only show episodes that I've seen
  filter(
    is.na(latest_episode) | episode_number <= latest_episode
  ) %>%
  #get series names, start/end years, genres, and typical episode runtimes
  left_join(import_imdb_titles, c("imdb_title_key" = "tconst")) %>%
  group_by(watched_season_id, season) %>%
  #calculate the total number of episodes (up to the latest that I've watched) in the given season
  mutate(season_length = max(episode_number)) %>%
  ungroup() %>%
  mutate(
    #estimate the date on which I watched each episode, based on the date on which I started/finished each season
    watch_date = as.Date(start_date + (round((episode_number-1)*(watch_duration_days)/(season_length-1)))),
    #estimate the release year of each episode/season, based on the start/end year of the series
    release_year = case_when(
      #if series start year is the same as end year according to imdb, can use the start year as the release year
      startYear == endYear ~ startYear,
      #if (currently) series has only had 1 season, can use start year as release year
      series_length == 1 ~ startYear,
      #otherwise interpolate to estimate release year based on current max season number for each series
      TRUE ~ as.integer(startYear+round((as.numeric(season)-1)*(coalesce(endYear,as.integer(year(today())))-startYear)/(series_length-1)))),
    #if series is marked as miniseries then IMDB provides total series runtime for some reason, so need to modify those (with one exception)
    runtime = case_when(
      imdb_title_key == 'tt0397150' ~ runtimeMinutes,
      titleType == 'tvMiniSeries' ~ as.integer(round(runtimeMinutes/season_length)),
      TRUE ~ runtimeMinutes)
         ) %>%
  select(
    type,
    watch_date,
    name = primaryTitle,
    release_year,
    runtime,
    genres,
    imdb_rating = averageRating,
    season,
    episode_number,
    imdb_title_key,
    titleType
  )
rm(clean_series)


##then union film and series outputs together to make combined output table ----
watchlist_output <- final_films %>%
  union_all(final_series) %>%
  mutate(imdb_title_url = paste("https://www.imdb.com/title/",imdb_title_key, sep = ""))
write_sheet(watchlist_output, gsheet_id, sheet = "Watchlist")
