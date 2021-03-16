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
clean_imdb_titles <- import_imdb_titles %>%
  select(imdb_title_id = tconst,
         imdb_name = primaryTitle,
         title_start_year = startYear,
         title_end_year = endYear,
         runtime = runtimeMinutes,
         genres,
         title_type = titleType)

#list of tv episodes - 1 row per episode
download.file("https://datasets.imdbws.com/title.episode.tsv.gz",
              destfile = "title.episode")
import_imdb_episodes <- read_tsv("title.episode")
clean_imdb_episodes <- import_imdb_episodes %>%
  mutate(imdb_season = as.integer(seasonNumber),
         imdb_episode_number = as.integer(episodeNumber)) %>%
  select(imdb_episode_id = tconst,
         imdb_title_id = parentTconst,
         imdb_season,
         imdb_episode_number)

#list of ratings - 1 row per film/episode
download.file("https://datasets.imdbws.com/title.ratings.tsv.gz",
             destfile = "title.ratings")
import_imdb_ratings <- read_tsv("title.ratings")
clean_imdb_ratings <- import_imdb_ratings %>%
  select(imdb_id = tconst,
         imdb_rating = averageRating)

#list of crew members
download.file("https://datasets.imdbws.com/title.crew.tsv.gz",
              destfile = "title.crew")
import_imdb_crew <- read_tsv("title.crew")
clean_imdb_crew <- import_imdb_crew %>%
  select(imdb_id = tconst,
         director_ids = directors,
         writer_ids = writers)

#list of principal cast members
download.file("https://datasets.imdbws.com/title.principals.tsv.gz",
              destfile = "title.principals")
import_imdb_principals <- read_tsv("title.principals")
clean_imdb_cast <- import_imdb_principals %>%
  filter(category %in% c("actor","actress")) %>%
  select(imdb_id = tconst,
         cast_id = nconst
  )

#list of crew/cast names
download.file("https://datasets.imdbws.com/name.basics.tsv.gz",
              destfile = "name.basics")
import_imdb_names <- read_tsv("name.basics")
clean_imdb_names <- import_imdb_names %>%
  select(crew_id = nconst,
         crew_name = primaryName)

#basic data cleaning of my personal data (no joins to IMDB yet) ----
clean_personal_films <- import_personal_films %>%
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
    imdb_title_id = IMDb
  )

clean_personal_series <- import_personal_series %>%
  mutate(
    #add id for season, to be used later
    watched_season_id = row_number(),
    #add type indicator for when films and series data are unioned later
    type = 'Series',
    #how long I spent watching that season - or have done so far, if not finished yet
    watch_duration_days = difftime(coalesce(`End Date`, date(today(
      ))), `Start Date`),
  ) %>%
  #only continue with columns of interest, and make naming convention consistent
  select(
    type,
    watched_season_id,
    start_date = `Start Date`,
    end_date = `End Date`,
    watch_duration_days,
    name = Show,
    season = Season,
    latest_episode = `Latest Episode`,
    imdb_title_id = IMDb
  )

#calculate the length (in seasons) of each series, which is later used to estimate the release year of each season
mapping_series_lengths <- clean_imdb_episodes %>%
  inner_join(clean_personal_series, c("imdb_title_id" = "imdb_title_id")) %>%
  group_by(imdb_title_id) %>%
  summarise(series_length = max(imdb_season))

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
final_films <- clean_personal_films %>%
  #get film names, runtimes, years and genres
  left_join(clean_imdb_titles, "imdb_title_id") %>%
  #get film IMDB ratings
  left_join(clean_imdb_ratings, c("imdb_title_id" = "imdb_id")) %>%
  select(type,
         watch_date,
         name = imdb_name,
         my_rating,
         cinema_flag,
         first_watch_flag,
         release_year = title_start_year,
         runtime,
         genres,
         imdb_rating,
         imdb_title_id)

final_series <- clean_personal_series %>%
  #join to IMDB episode tables to expand to episode-level with episode_number column
  left_join(clean_imdb_episodes, c("imdb_title_id" = "imdb_title_id",
                                    "season" = "imdb_season")) %>%
  #get max season number for each series
  left_join(mapping_series_lengths, "imdb_title_id") %>%
  #get IMDB rating for each episode
  left_join(clean_imdb_ratings, c("imdb_episode_id" = "imdb_id")) %>%
  #for partially-watched seasons, only show episodes that I've seen
  filter(
    is.na(latest_episode) | imdb_episode_number <= latest_episode
  ) %>%
  #get series names, start/end years, genres, and typical episode runtimes
  left_join(clean_imdb_titles, "imdb_title_id") %>%
  group_by(watched_season_id, season) %>%
  #calculate the total number of episodes (up to the latest that I've watched) in the given season
  mutate(season_length = max(imdb_episode_number)) %>%
  ungroup() %>%
  mutate(
    #estimate the date on which I watched each episode, based on the date on which I started/finished each season
    watch_date = as.Date(start_date + (round((imdb_episode_number-1)*(watch_duration_days)/(season_length-1)))),
    #estimate the release year of each episode/season, based on the start/end year of the series
    release_year = case_when(
      #if series start year is the same as end year according to imdb, can use the start year as the release year
      title_start_year == title_end_year ~ title_start_year,
      #if (currently) series has only had 1 season, can use start year as release year
      series_length == 1 ~ title_start_year,
      #otherwise interpolate to estimate release year based on current max season number for each series
      TRUE ~ as.integer(title_start_year+round((as.numeric(season)-1)*(coalesce(title_end_year,as.integer(year(today())))-title_start_year)/(series_length-1)))),
    #if series is marked as miniseries then IMDB provides total series runtime for some reason, so need to modify those (with one exception)
    runtime = case_when(
      imdb_title_id == 'tt0397150' ~ runtime,
      title_type == 'tvMiniSeries' ~ as.integer(round(runtime/season_length)),
      TRUE ~ runtime)
         ) %>%
  select(
    type,
    watch_date,
    name = imdb_name,
    release_year,
    runtime,
    genres,
    imdb_rating,
    season,
    episode_number = imdb_episode_number,
    imdb_title_id,
    imdb_episode_id
  )

#then union film and series outputs together to make combined output table ----
output_watchlist <- final_films %>%
  union_all(final_series) %>%
  mutate(imdb_title_url = paste("https://www.imdb.com/title/",imdb_title_id, sep = ""),
         imdb_id = coalesce(imdb_episode_id, imdb_title_id))

#create crew tables----
#create directors table
final_directors <- clean_imdb_crew %>%
  inner_join(output_watchlist, "imdb_id") %>%
  mutate(crew_id = strsplit(director_ids, ",")) %>%
  unnest(crew_id) %>%
  left_join(clean_imdb_names, "crew_id") %>%
  mutate(crew_type = "Director") %>%
  select(
    type, 
    crew_type,
    crew_name,
    crew_id,
    type,
    name,
    my_rating,
    cinema_flag,
    first_watch_flag,
    runtime,
    season,
    episode_number,
    imdb_title_id,
    imdb_episode_id,
    imdb_title_url
  )

#create writers table
final_writers <- clean_imdb_crew %>%
  inner_join(output_watchlist, "imdb_id") %>%
  mutate(crew_id = strsplit(writer_ids, ",")) %>%
  unnest(crew_id) %>%
  left_join(clean_imdb_names, "crew_id") %>%
  mutate(crew_type = "Writer") %>%
  select(
    type, 
    crew_type,
    crew_name,
    crew_id,
    type,
    name,
    my_rating,
    cinema_flag,
    first_watch_flag,
    runtime,
    season,
    episode_number,
    imdb_title_id,
    imdb_episode_id,
    imdb_title_url
  )

#create writers table
final_cast <- clean_imdb_cast %>%
  inner_join(output_watchlist, "imdb_id") %>%
  left_join(clean_imdb_names, c("cast_id" = "crew_id")) %>%
  mutate(crew_type = "Cast") %>%
  select(
    type, 
    crew_type,
    crew_name,
    crew_id = cast_id,
    type,
    name,
    my_rating,
    cinema_flag,
    first_watch_flag,
    runtime,
    season,
    episode_number,
    imdb_title_id,
    imdb_episode_id,
    imdb_title_url
  )

output_crewlist <- final_directors %>%
  union_all(final_writers) %>%
  union_all(final_cast)

#write data back to sheets----
write_sheet(output_watchlist, gsheet_id, sheet = "Watchlist")
write_sheet(output_crewlist, gsheet_id, sheet = "Crewlist")