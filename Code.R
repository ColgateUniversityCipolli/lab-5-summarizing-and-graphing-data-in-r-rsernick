library(tidyverse)

## Create tibbles for both csv's

all.dat = read_csv("data/essentia.data.csv") 
allentown.dat = read_csv("data/essentia.data.allentown.csv")

## Create tibble to compare overall loudness of different artists

loudness.dat = all.dat |>
  group_by(artist) |>
  summarize(
    IQR = (quantile(overall_loudness, .75)-quantile(overall_loudness, .25)),
    min = min(overall_loudness),
    LF = quantile(overall_loudness, .25) - 1.5*IQR,
    UF = quantile(overall_loudness, .75) + 1.5*IQR,
    max = max(overall_loudness)) |>
  mutate(out.of.range = case_when(
                        allentown.dat$overall_loudness < min ~ "TRUE",
                        allentown.dat$overall_loudness > max ~ "TRUE",
                        TRUE                                 ~ "FALSE")) |>
  mutate(unusual = case_when(
                   allentown.dat$overall_loudness < LF ~ "TRUE",
                   allentown.dat$overall_loudness > UF ~ "TRUE",
                   TRUE                                 ~ "FALSE")) |>
  mutate(description = case_when(
                       out.of.range == "TRUE"  ~ "Out of Range",
                       unusual == "TRUE"       ~ "Outlying",
                       TRUE                    ~ "Within Range"))

features = all.dat |>
  select(-c("artist", "album", "track", "chords_scale",
                                     "chords_key", "key", "mode")) |>
  colnames()


compare = function(feature){
  results = all.dat |>
  group_by(artist) |>
    summarize(
      IQR = (quantile(get(feature), .75)-quantile(get(feature), .25)),
      min = min(get(feature)),
      LF = quantile(get(feature), .25) - 1.5*IQR,
      UF = quantile(get(feature), .75) + 1.5*IQR,
      max = max(get(feature))) |>
    mutate(out.of.range = case_when(
      allentown.dat[[feature]] < min ~ "TRUE",
      allentown.dat[[feature]] > max ~ "TRUE",
      TRUE                                 ~ "FALSE")) |>
    mutate(unusual = case_when(
      allentown.dat[[feature]] < LF ~ "TRUE",
      allentown.dat[[feature]] > UF ~ "TRUE",
      TRUE                                 ~ "FALSE")) |>
    mutate(!!feature := case_when(
      out.of.range == "TRUE"  ~ "Out of Range",
      unusual == "TRUE"       ~ "Outlying",
      TRUE                    ~ "Within Range")) |>
    select(artist, !!sym(feature))
  return(results)
}

features.dat = compare("overall_loudness")

## Have to remove NA from tone 

all.dat = all.dat|>
  mutate(Tone = case_when(is.na(Tone) ~ 0,
                          TRUE        ~ Tone))

## Run the function for every feature

for(i in 2:length(features)){
  results = compare(features[i])
  features.dat = features.dat|>
  right_join(results, by = "artist", suffix = c("", ""))
}
  

