# -- head -- #

library(tidyverse)
library(glue)

setwd('~/Github/Racz2025Bible/')

preverbs = '^(meg|fel|le|be|szét|össze|ki|el|át|rá|vissza|ide|oda|hozzá|szembe|keresztül|végig|túl|alá|közé|félre).'

# -- read -- #

d = read_tsv('dat/karoli_evangelium_tokenised.tsv')

cselekszenek = read_tsv('~/Github/Racz2024/resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

# -- eda -- #

cselekszenek_regex = cselekszenek$base |> 
  unique() |> 
  str_replace('ik$', '') |> 
  str_replace("(.)$", ".\\1") |> 
  str_replace("s\\.z$", ".sz") |> 
  paste(collapse = '|^')
cselekszenek_regex = glue('({cselekszenek_regex})')

d |>
  mutate(
    hangkiveto = str_detect(word, cselekszenek_regex)
  ) |> 
  filter(hangkiveto) |> 
  pull(normalised)

# lol

