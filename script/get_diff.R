library(tidyverse)

setwd('~/Github/Racz2025Bible/')

d = read_tsv('dat/gospels.gz')

d2 = d |> 
  filter(!translation %in% c('RUF','SzIT','KaldiNeo', 'KaroliRevid')) |> 
  select(work,year,type,book,verse,text) |> 
  summarise(
    verse_text = paste(text, collapse = ' '),
    .by = c(work,year,type,book,verse)
  ) 

d2a = d2 |> 
  filter(type == 'facsimile') |> 
  rename('facsimile' = verse_text) |> 
  select(-type)
d2b = d2 |> 
  filter(type == 'normalised') |> 
  rename('normalised' = verse_text) |> 
  select(-type)

d3 = inner_join(d2a,d2b)

d4 = d3 |> 
  mutate(
    verse_diff = stringdist::stringdist(facsimile,normalised, method = 'jaccard')
    )

write_tsv(d4, 'dat/parallel.gz')
