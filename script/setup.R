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
calculateBigramEntropy = function(df){
    # unigram entropy
    unigram_entropy_tbl = df |> 
      unnest_tokens(word, text) |>              # one row per word
      count(word, name = "n") |>                # n_i
      mutate(p = n / sum(n))                     # P(w)
      
    unigram_entropy = unigram_entropy_tbl |> 
      summarise(H = -sum(p * log2(p))) |> 
      pull(H)
  
    unigram_perplexity = 2^unigram_entropy      # 2^H(W)
  
    # bigram entropy
    # joint counts
    bigram_tbl = df |> 
      unnest_tokens(bigram, text, token = "ngrams", n = 2) |> 
      separate(bigram, into = c("w1", "w2"), sep = " ") |> 
      count(w1, w2, name = "n")
  
    total_bigrams = sum(bigram_tbl$n)
  
    bigram_tbl = bigram_tbl |> 
      mutate(p_joint = n / total_bigrams)        # P(w1,w2)
  
    # marginal P(w1)
    marginal_tbl = bigram_tbl |> 
      group_by(w1) |> 
      summarise(n_w1 = sum(n), .groups = "drop") |> 
      mutate(p_w1 = n_w1 / total_bigrams)
  
    # H(W2|W1)  =  Σ P(w1,w2) · log 1/P(w2|w1)
    cond_entropy = bigram_tbl |> 
      left_join(marginal_tbl, by = "w1") |> 
      mutate(p_cond = n / n_w1,                       # P(w2|w1)
             contrib = p_joint * (-log2(p_cond))) |> # P(w1,w2)⋅log 1/P(w2|w1)
      summarise(H = sum(contrib)) |> 
      pull(H)
  
    bigram_perplexity = 2^cond_entropy            # 2^H(W2|W1)
  
    # out
    entropy_results = tibble(
      unigram_entropy       = unigram_entropy,
      unigram_perplexity    = unigram_perplexity,
      bigram_cond_entropy   = cond_entropy,
      bigram_perplexity     = bigram_perplexity
    )
  
    entropy_results
  }  

# function to calculate MDL / DL
calculateMDLoverDL = function(text){
  text_compressed = memCompress(text, type = 'gzip')
  length(text_compressed) / nchar(text)
}

# -- read -- #

d = read_tsv('dat/gospels.gz')

# -- quick check -- #

# d |> distinct(book)
# d |> distinct(translation)
# range(d$chapter)
# range(d$verse)

# -- clean up text -- #

# lowercase, no punctuation
d = d |> 
  mutate(
    text = text |> 
      str_to_lower() |> 
      str_remove_all("[[:punct:]]")
  )

# -- bigram entropy and mdl / dl -- #

ed = d |> 
  unnest_tokens(word, text, drop = F) |> 
  nest(.by = c(file_name,work,translation,year,description,analysis_original,analysis_normalised,type,book,chapter)) |> 
  mutate(
    entropy_data = map(data, calculateBigramEntropy)
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
    mdl_over_dl = calculateMDLoverDL(chapter2)
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
  select(work,year,book,chapter,unigram_perplexity,bigram_perplexity,mdl_over_dl,wc,type_token_ratio) |> 
  rename_with(~ paste0(., "_orig"), -c(work,year,book,chapter))

c2 = combined |> 
  filter(type == 'normalised', period != 'modern') |> 
  select(work,year,book,chapter,unigram_perplexity,bigram_perplexity,mdl_over_dl,wc,type_token_ratio) |> 
  rename_with(~ paste0(., "_norm"), -c(work,year,book,chapter))

c3 = left_join(c1,c2)

# -- write -- #

write_tsv(combined, 'dat/gospel_entropy.tsv')
write_tsv(c3, 'dat/predictor_correlations.tsv')
