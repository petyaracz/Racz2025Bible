# load files and matcher, keep gospel translations, combine into long format across facsimile, normalised, modern (b, n, m), write to file

# -- head -- #

library(tidyverse)
library(glue)
library(tidytext)
library(hunspell)

setwd('~/Github/Racz2025Bible/')

# -- fun -- #

# read, add names
readText = function(file_name,my_url){
  text_location = glue('{my_url}{file_name}')
  # read all as char, because some files have "-" in num cols
  my_text = read_tsv(text_location, col_names = F, col_types = cols(.default = col_character()))
  names(my_text) = c('book', 'verse', 'line', 'text')
  return(my_text)
}

readTextFilled = partial(readText, my_url = my_url)

# -- collect relevant -- #

# ids of Gospels
gospel_books = c('Mt','Mk','Lk','Jn')
# bible translation info from https://parallelbible.nytud.hu/info
matcher = read_tsv('dat/bible_matcher.tsv')
# cloned from github https://github.com/nytud/parallelbible
my_url = '~/Github/parallelbible/'
file_name = list.files(my_url)

# which translations have the Gospels
gospels = matcher |> 
  filter(file_name %in% c("MunchK", "Jordk", "Pesti", "Sylvester", "Heltai", "Karoli", "Kaldi", "RUF", "SzIT", "KaroliRevid", "KaldiNeo")) |>
  # add file name
  mutate(
    facsimile = glue('{file_name}_b.tsv'),
    normalised = glue('{file_name}_n.tsv'),
    modern = glue('{file_name}_m.tsv')
  ) |> 
  # make long
  rename('translation' = file_name) |> # this isn't very nice :E
  pivot_longer(-c(translation,year,description), names_to = 'type', values_to = 'file_name') |> 
  filter(file.exists(glue('{my_url}{file_name}')))

# safe_readTextFilled = quietly(readTextFilled)
# 
# gospels |>
#   # load file
#   mutate(
#     warning = map(file_name, ~ safe_readTextFilled(.)$warnings)
#   ) |> View()

gospel_texts = gospels |>
  # load file
  mutate(
    data = map(file_name, readTextFilled)
    )

gospel_texts_unnested = gospel_texts |> 
  unnest(data) |>
  filter(book %in% gospel_books) # this also drops misc stuff in the Gospels, like "tizenharmadik resz" in Sylvester. nice

# let's check this
# gospel_texts_unnested |>
# distinct(translation,type,book) |> View()
gospel_texts_unnested |>
  distinct(translation,type,book) |> 
  count(translation,type) |> 
  filter(n != 4)

# gospel_texts_unnested |>
#   distinct(book,verse) |> View()

# -- add info -- #

gospel_texts_unnested = gospel_texts_unnested |> 
  mutate(
    work = description |> 
      str_extract('^.*(?=, Forrás)') |> 
      str_replace('Keletkezési idő: ', ''),
    analysis_original = type == 'facsimile' | translation %in% c('KaldiNeo', 'KaroliRevid','SzIT','RUF'),
    analysis_normalised = type == 'normalised' | translation %in% c('KaldiNeo', 'KaroliRevid','SzIT','RUF')
  )
  
# -- write -- #

write_tsv(gospel_texts_unnested, 'dat/gospels.gz')
