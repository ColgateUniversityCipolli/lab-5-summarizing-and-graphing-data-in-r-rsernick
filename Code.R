library(tidyverse)
library(xtable)

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
  select(-c("artist", "album", "track", "chords_scale", "chords_key", "key", 
            "mode")) |>
  colnames()


compare = function(feature){
  results = all.dat |>
  group_by(artist) |>
    summarize(
      IQR = (quantile(get(feature), .75, na.rm = T)-quantile(get(feature), .25, na.rm = T)),
      min = min(get(feature), na.rm = T),
      LF = quantile(get(feature), .25, na.rm = T) - 1.5*IQR,
      UF = quantile(get(feature), .75, na.rm = T) + 1.5*IQR,
      max = max(get(feature)), na.rm = T) |>
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


## Run the function for every feature

for(i in 2:length(features)){
  results = compare(features[i])
  features.dat = features.dat|>
  right_join(results, by = "artist", suffix = c("", ""))
}

features.dat = features.dat |>
  mutate(within.range = 0) |>
  mutate(outlying = 0) |>
  mutate(out.of.range = 0)

for(feature in features){
  features.dat = features.dat |>
    rowwise() |>
    mutate(within.range = case_when(get(feature) == "Within Range" ~ within.range+1,
                                    TRUE                           ~ within.range)) |>
    mutate(outlying = case_when(get(feature) == "Outlying" ~ outlying+1,
                                TRUE                       ~ outlying)) |>
    mutate(out.of.range = case_when(get(feature) == "Out of Range" ~ out.of.range+1,
                                TRUE                               ~ out.of.range)) 
}

################################################################################
## Repeat previous steps for musical features
################################################################################
musical.dat = features.dat[1:79]

musical.dat = musical.dat |>
  mutate(within.range = 0) |>
  mutate(outlying = 0) |>
  mutate(out.of.range = 0)

for(feature in features[1:78]){
  musical.dat = musical.dat |>
    rowwise() |>
    mutate(within.range = case_when(get(feature) == "Within Range" ~ within.range+1,
                                    TRUE                           ~ within.range)) |>
    mutate(outlying = case_when(get(feature) == "Outlying" ~ outlying+1,
                                TRUE                       ~ outlying)) |>
    mutate(out.of.range = case_when(get(feature) == "Out of Range" ~ out.of.range+1,
                                    TRUE                               ~ out.of.range)) 
}

musical.range = musical.dat |>
  select(artist, within.range, outlying, out.of.range)

################################################################################
## Repeat steps for lyrical features
################################################################################

lyrical.dat = features.dat[-(2:79)]|>
  select(-within.range, -out.of.range, -outlying)

lyrical.dat = lyrical.dat |>
  mutate(within.range = 0) |>
  mutate(outlying = 0) |>
  mutate(out.of.range = 0)

for(feature in features[79:length(features)]){
  lyrical.dat = lyrical.dat |>
    rowwise() |>
    mutate(within.range = case_when(get(feature) == "Within Range" ~ within.range+1,
                                    TRUE                           ~ within.range)) |>
    mutate(outlying = case_when(get(feature) == "Outlying" ~ outlying+1,
                                TRUE                       ~ outlying)) |>
    mutate(out.of.range = case_when(get(feature) == "Out of Range" ~ out.of.range+1,
                                    TRUE                               ~ out.of.range)) 
}

lyrical.range = lyrical.dat |>
  select(artist, within.range, outlying, out.of.range)

################################################################################
################################################################################
################################################################################

range.dat = features.dat |>
  select(artist, within.range, outlying, out.of.range)
################################################################################
## Save data to csv files to use in lab report
################################################################################

write_csv(range.dat, "range.csv")

write_csv(features.dat, "features.csv")

write_csv(musical.dat, "musical-dat.csv")

write_csv(lyrical.dat, "lyrical-dat.csv")


################################################################################
## Graph making
################################################################################


ggplot(data = features.dat)+
  geom_col(aes(x = artist, y =within.range))+
  ylab("Within Range")+
  xlab("Artist")

ggplot(data = features.dat)+
  geom_col(aes(x = artist, y = out.of.range+outlying))

ggplot(data = musical.dat)+
  geom_col(aes(x = artist, y = within.range))

ggplot(data = lyrical.dat)+
  geom_col(aes(x = artist, y = within.range))+
  ylab("Within Range")+
  xlab("Artist")

xtable(range.dat)

