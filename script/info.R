# -- head -- #

library(tidyverse)
library(tidytext)
library(glue)

setwd('~/Github/Racz2025Bible/')

# -- fun -- #

# take df, calculate bigrams across normalised, return info
# colname is hard coded! I'm so sorry
calculateNgramInformativity = function(df, n) {
  stopifnot(n > 1)  # Ensure n is greater than 1
  
  # Create n-grams
  ngrams = df |>
    select(text) |> 
    unnest_tokens(ngram, text, token = "ngrams", n = n)
  
  # Separate the n-grams into individual words
  words = paste0("word", 1:n)
  ngrams_separated = ngrams |>
    separate(ngram, into = words, sep = " ")
  
  # Count the frequency of each n-gram
  ngram_counts = ngrams_separated |>
    count(across(all_of(words)), sort = TRUE)
  
  # Create a column for the previous words
  previous_words = paste0("word", 1:(n-1))
  
  # Count the frequency of each (n-1)-gram
  previous_words_counts = ngrams_separated |>
    count(across(all_of(previous_words)), sort = TRUE)
  
  # Calculate the probability of the nth word given the previous (n-1) words
  ngram_probabilities = ngram_counts |>
    left_join(previous_words_counts, by = previous_words, suffix = c("_ngram", "_prev")) |>
    mutate(
      probability = n_ngram / n_prev,
      information = -log2(probability)
    )
  
  return(ngram_probabilities)
}

calc2 = partial(calculateNgramInformativity, n = 2)
calc3 = partial(calculateNgramInformativity, n = 3)

# -- read -- #

d = read_tsv('dat/gospels.tsv')

# -- setup -- #

d = d |> 
  filter(target_text)

# -- info -- #

infos = d |> 
  select(translation,year,book,verse,line,text) |> 
  nest(.by = c(translation,year,book)) |> 
  mutate(
    info2 = map(data, calc2),
    info3 = map(data, calc3)
  )

infos2 = infos |> 
  select(translation,year,book,info2) |> 
  unnest(info2)

infos3 = infos |> 
  select(translation,year,book,info3) |> 
  unnest(info3)

# -- write -- #

write_tsv(infos2, 'dat/gospel_bigram_informativity.tsv')
write_tsv(infos3, 'dat/gospel_trigram_informativity.tsv')
