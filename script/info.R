# -- head -- #

library(tidyverse)
library(tidytext)
library(glue)
library(ggrain)
library(ggthemes)
library(patchwork)

setwd('~/Github/Racz2025Bible/')

# -- fun -- #

# take df, calculate bigrams across normalised, return info
calculateNgramInformativity = function(df, n) {
  stopifnot(n > 1)  # Ensure n is greater than 1
  
  # Create n-grams
  ngrams = df |>
    select(normalised) |> 
    unnest_tokens(ngram, normalised, token = "ngrams", n = n)
  
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

d = read_tsv('dat/karoli_evangelium_tokenised.tsv')

# -- info -- #

infos = d |> 
  mutate(
    book_length = n(),
    .by = book
  ) |> 
  mutate(
    book2 = glue('{book} ({book_length})')
  ) |> 
  nest(.by = c(book,book2)) |> 
  mutate(
    info2 = map(data, calc2),
    info3 = map(data, calc3)
  )

infos |> 
  mutate(book2 = fct_relevel(book2, "Mk (12261)", "Jn (15720)", "Mt (19234)", "Lk (20968)")) |> 
  unnest(info2) |> 
  ggplot(aes(book2,information)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  theme_minimal() +
  ggtitle('bigram információ a Károli bibliában') +
  scale_x_discrete(labels = c('Márk (12e)', 'János (16e)', 'Máté (19e)', 'Lukács (21e)'), name = 'Evangélium (szavak száma)') + 
  ylab('információ')
  
  infos |> 
    mutate(book2 = fct_relevel(book2, "Mk (12261)", "Jn (15720)", "Mt (19234)", "Lk (20968)")) |> 
  unnest(info3) |> 
    ggplot(aes(book2,information)) +
    geom_violin() +
    geom_boxplot(width = .1) +
    theme_minimal() +
    ggtitle('trigram információ a Károli bibliában') +
    scale_x_discrete(labels = c('Márk (12e)', 'János (16e)', 'Máté (19e)', 'Lukács (21e)'), name = 'Evangélium (szavak száma)') + 
    ylab('információ') +
    coord_cartesian(ylim = c(0,2.5))
