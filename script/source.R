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

# which translations have the Gospels (and not revised translations of Karoli or Kaldi)
gospels = matcher |> 
  filter(file_name %in% c("MunchK", "Jordk", "Pesti", "Sylvester", "Heltai", "Karoli", "Kaldi", "RUF"))

# make long
gospels = gospels |> 
  # add file name
  mutate(
    facsimile = glue('{file_name}_b.tsv'),
    normalised = glue('{file_name}_n.tsv'),
    modern = glue('{file_name}_m.tsv')
  ) |> 
  # make long
  rename('translation' = file_name) |> # this isn't very nice :E
  pivot_longer(-c(translation,year,description), names_to = 'type', values_to = 'file_name')

# check if file exists
gospels = gospels |> 
  mutate(
    exists = file.exists(glue('{my_url}{file_name}')),
    target_text = case_when(
      translation %in% c('Aranyos', 'SzIT', 'RUF') & type == 'modern' ~ T,
      !translation %in% c('Aranyos', 'SzIT', 'RUF') & type == 'normalised' ~ T,
      T ~ F # yeehaa
    )
  ) |> 
  filter(exists)
# 
# safe_readTextFilled = safely(readTextFilled)
# 
# gospel_texts = gospels |> 
#   # load file
#   mutate(
#     data = map(file_name, ~ safe_readTextFilled(.)$result),
#     error = map(file_name, ~ safe_readTextFilled(.)$error)
#   )

gospel_texts = gospels |>
  # load file
  mutate(
    data = map(file_name, readTextFilled)
  ) |> 
  unnest(data) |> 
  filter(book %in% gospel_books)

# let's check this
# gospel_texts |>
  # distinct(translation,type,book) |> View()
# 
# gospel_texts |>
#   distinct(book,verse) |> View()

# there's a tab somewhere in there
# Function to check if a value contains a tab character
# contains_tab = function(x) {
#   grepl("\t", x)
# }
# 
# # Filter rows where any column contains a tab character
# filtered_df = gospel_texts %>%
  # filter(if_any(everything(), contains_tab))
# 
gospel_texts |>
  filter(str_detect(description, '\t'))
gospel_texts |>
  filter(str_detect(text, '\t'))# |> pull(text) |> 
  # str_extract('(?<=\n)..\t[0-9]+\t[0-9]+') |> 
  # unique()

# -- write -- #

write_tsv(gospel_texts, 'dat/gospels.gz')
