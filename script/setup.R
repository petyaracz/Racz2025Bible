# -- head -- #

library(tidyverse)
library(tidytext)
library(glue)
library(entropy)

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

# Function to calculate Shannon entropy
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

calc2 = partial(calculateNgramInformativity, n = 2)
calc3 = partial(calculateNgramInformativity, n = 3)

# -- read -- #

d = read_tsv('dat/gospels.tsv')

# -- setup -- #

d2 = d |> 
  filter(target_text)

d3 = d |> 
  filter(
    type == 'normalised' | (translation %in% c('Aranyos','RUF') & type == 'modern')
  )

# -- informativity and perplexity -- #

# bigram and trigram informativity

infos = d2 |> 
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

## normalised

# perplexity

ed = d2 |> 
  select(translation,year,book,verse,line,text) |> 
  unnest_tokens(word, text, drop = F) |> 
  nest(.by = c(translation,year,book,verse)) |> 
  mutate(
    entropy_data = map(data, calculateEntropy)
  ) |> 
  select(translation,year,book,verse,entropy_data) |> 
  unnest(entropy_data)

# word count and avg word length
id = d2 |> 
  select(translation,year,book,verse,line,text) |> 
  unnest_tokens(word, text, drop = F) |> 
  summarise(
    wc = n(),
    avg_word_length = mean(nchar(word)),
    type_count = n_distinct(word),
    type_token_ratio = type_count / wc,
    .by = c(translation,year,book,verse)
  )

## facsimile

# perplexity

ed2 = d3 |> 
  select(translation,year,book,verse,line,text) |> 
  unnest_tokens(word, text, drop = F) |> 
  nest(.by = c(translation,year,book,verse)) |> 
  mutate(
    entropy_data = map(data, calculateEntropy)
  ) |> 
  select(translation,year,book,verse,entropy_data) |> 
  unnest(entropy_data)

# word count and avg word length
id2 = d3 |> 
  select(translation,year,book,verse,line,text) |> 
  unnest_tokens(word, text, drop = F) |> 
  summarise(
    wc = n(),
    avg_word_length = mean(nchar(word)),
    type_count = n_distinct(word),
    type_token_ratio = type_count / wc,
    .by = c(translation,year,book,verse)
  )

# -- combine -- #

ed = ed |> 
  rename(
    'entropy_normalised' = entropy,
    'perplexity_normalised' = perplexity
  )

ed2 = ed2 |> 
  rename(
    'entropy_original' = entropy,
    'perplexity_original' = perplexity
  )

id = id |> 
  rename(
    'wc_normalised' = wc,
    'avg_word_length_normalised' = avg_word_length,
    'type_count_normalised' = type_count,
    'type_token_ratio_normalised' = type_token_ratio
  )

id2 = id2 |> 
  rename(
    'wc_original' = wc,
    'avg_word_length_original' = avg_word_length,
    'type_count_original' = type_count,
    'type_token_ratio_original' = type_token_ratio
  )

combined = d |> 
  distinct(translation,year,description) |> 
  left_join(ed) |> 
  left_join(ed2) |> 
  left_join(id) |> 
  left_join(id2)

# -- write -- #

write_tsv(infos2, 'dat/gospel_bigram_informativity.tsv')
write_tsv(infos3, 'dat/gospel_trigram_informativity.tsv')
write_tsv(combined, 'dat/gospel_entropy.tsv')
