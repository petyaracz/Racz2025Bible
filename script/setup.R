# load gospel file (output of source.R) add information stats across chapters (not verses!), write to files

# -- head -- #

library(tidyverse)
library(tidytext)
library(glue)
library(entropy)
library(stringdist)

setwd('~/Github/Racz2025Bible/')

# -- fun -- #

# function to calculate Shannon entropy and perplexity
calculateEntropy = function(df) {
  df |>
    # calculate the frequency of each word
    count(word) |> 
    # convert to p 
    mutate(
      p = n / sum(n)
    ) |> 
    summarise(
      entropy = entropy(p, unit = "log2"),
      perplexity = 2^entropy
    )
}

# function to calculate Kolmogorov complexity
calculateComplexity = function(text){
  text_compressed = memCompress(text, type = 'gzip')
  length(text_compressed)
}

# -- read -- #

d = read_tsv('dat/gospels.gz')

# -- quick check -- #

# d |> distinct(book)
# d |> distinct(translation)
# range(d$chapter)
# range(d$verse)

# -- entropy and perplexity -- #

ed = d |> 
  unnest_tokens(word, text, drop = F) |> 
  nest(.by = c(file_name,work,translation,year,description,analysis_original,analysis_normalised,type,book,chapter)) |> 
  mutate(
    entropy_data = map(data, calculateEntropy)
  ) |> 
  unnest(entropy_data)

# -- complexity -- #

cd = d |> 
  summarise(
    chapter2 = paste(text, collapse = ' '),
      .by = c(file_name,work,translation,year,description,analysis_original,analysis_normalised,type,book,chapter)
              ) |> 
  rowwise() |> 
  mutate(
    complexity = calculateComplexity(chapter2)
  ) |> 
  ungroup() 
  
# -- descriptive stats -- #

# word count and avg word length
id = d |> 
  unnest_tokens(word, text, drop = F) |> 
  summarise(
    wc = n(),
    type_count = n_distinct(word),
    type_token_ratio = type_count / wc,
    .by = c(file_name,work,translation,year,description,analysis_original,analysis_normalised,type,book,chapter)
  )

# -- facsimile / normalised diff -- #

# jaccard distance between facsimile and normalised per chapter
diffs = d |> 
  filter(!translation %in% c('RUF','SzIT','KaldiNeo', 'KaroliRevid')) |> # these have only modern text
  select(work,year,type,book,chapter,text) |> 
  summarise(
    chapter_text = paste(text, collapse = ' '),
    .by = c(work,year,type,book,chapter)
  ) |> 
  pivot_wider(names_from = type, values_from = chapter_text) |> 
  mutate(
    chapter_diff = stringdist(facsimile,normalised, method = 'jaccard')
  ) |> 
  select(work,year,book,chapter,chapter_diff)

# -- combine -- #

combined = ed |> 
  left_join(id) |> 
  left_join(cd) |> 
  left_join(diffs) |> 
  select(-data,-chapter2) |> 
  mutate(
    period = ifelse(translation %in% c('RUF','SzIT','KaldiNeo', 'KaroliRevid'), 'modern','mediaeval / early modern')
  )
         
# -- comparisons -- #

c1 = combined |> 
  filter(type == 'facsimile', period != 'modern') |> 
  select(work,year,book,chapter,perplexity,complexity,wc,type_token_ratio) |> 
  rename_with(~ paste0(., "_orig"), -c(work,year,book,chapter))

c2 = combined |> 
  filter(type == 'normalised', period != 'modern') |> 
  select(work,year,book,chapter,perplexity,complexity,wc,type_token_ratio) |> 
  rename_with(~ paste0(., "_norm"), -c(work,year,book,chapter))

c3 = left_join(c1,c2)

# -- write -- #

write_tsv(combined, 'dat/gospel_entropy.tsv')
write_tsv(c3, 'dat/predictor_correlations.tsv')
