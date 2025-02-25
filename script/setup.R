# load gospel file (output of source.R) create bigram and trigram informativity across books and create information stats across verses (not lines!), write to files

# -- head -- #

library(tidyverse)
library(tidytext)
library(glue)
library(entropy)

setwd('~/Github/Racz2025Bible/')

# -- fun -- #

# take df, calculate bigrams across normalised, return info
# colname is hard coded! I'm so sorry
# calculateNgramInformativity = function(df, n) {
#   stopifnot(n > 1)  # Ensure n is greater than 1
#   
#   # Create n-grams
#   ngrams = df |>
#     select(text) |> 
#     unnest_tokens(ngram, text, token = "ngrams", n = n)
#   
#   # Separate the n-grams into individual words
#   words = paste0("word", 1:n)
#   ngrams_separated = ngrams |>
#     separate(ngram, into = words, sep = " ")
#   
#   # Count the frequency of each n-gram
#   ngram_counts = ngrams_separated |>
#     count(across(all_of(words)), sort = TRUE)
#   
#   # Create a column for the previous words
#   previous_words = paste0("word", 1:(n-1))
#   
#   # Count the frequency of each (n-1)-gram
#   previous_words_counts = ngrams_separated |>
#     count(across(all_of(previous_words)), sort = TRUE)
#   
#   # Calculate the probability of the nth word given the previous (n-1) words
#   ngram_probabilities = ngram_counts |>
#     left_join(previous_words_counts, by = previous_words, suffix = c("_ngram", "_prev")) |>
#     mutate(
#       probability = n_ngram / n_prev,
#       information = -log2(probability)
#     )
#   
#   return(ngram_probabilities)
# }

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

# calc2 = partial(calculateNgramInformativity, n = 2)
# calc3 = partial(calculateNgramInformativity, n = 3)

# -- read -- #

d = read_tsv('dat/gospels.gz')

# -- quick check -- #

# d |> distinct(book)
# d |> distinct(translation)
# range(d$verse)
# range(d$line)

# -- informativity -- #

# bigram and trigram informativity

# infos = d |> 
#   nest(.by = c(translation,year,description,work,type,analysis_original, analysis_normalised,book)) |> 
#   mutate(
#     info2 = map(data, calc2),
#     info3 = map(data, calc3)
#   )
# 
# infos2 = infos |> 
#   unnest(info2)
# 
# infos3 = infos |> 
#   unnest(info3)

# -- entropy and perplexity -- #

ed = d |> 
  unnest_tokens(word, text, drop = F) |> 
  nest(.by = c(file_name,work,translation,year,description,analysis_original,analysis_normalised,type,book,verse)) |> 
  mutate(
    entropy_data = map(data, calculateEntropy)
  ) |> 
  unnest(entropy_data)

# -- complexity -- #

cd = d |> 
  summarise(
    verse2 = paste(text, collapse = ' '),
      .by = c(file_name,work,translation,year,description,analysis_original,analysis_normalised,type,book,verse)
              ) |> 
  rowwise() |> 
  mutate(
    complexity = calculateComplexity(verse2)
  ) |> 
  ungroup() 
  

# -- descriptive stats -- #

# word count and avg word length
id = d |> 
  unnest_tokens(word, text, drop = F) |> 
  summarise(
    wc = n(),
    avg_word_length = mean(nchar(word)),
    type_count = n_distinct(word),
    type_token_ratio = type_count / wc,
    .by = c(file_name,work,translation,year,description,analysis_original,analysis_normalised,type,book,verse)
  )

# -- combine -- #

combined = ed |> 
  left_join(id) |> 
  left_join(cd) |> 
  select(-data,-verse2)

# -- write -- #

# write_tsv(infos2, 'dat/gospel_bigram_informativity.gz')
# write_tsv(infos3, 'dat/gospel_trigram_informativity.gz')
write_tsv(combined, 'dat/gospel_entropy.tsv')
