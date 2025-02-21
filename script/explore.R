# load Karoli versions, combine, tokenise normalised, keep Gospel (M,M,L,J) add hunspell analysis guesses, write to file

# -- head -- #

library(tidyverse)
library(glue)
library(tidytext)
library(hunspell)

setwd('~/Github/Racz2025Bible/')

# -- find -- #

my_url = '~/Github/parallelbible/'

file_name = list.files(my_url)

file_name[str_detect(file_name, 'Karoli')]
kb = read_tsv(glue('{my_url}/Karoli_b.tsv'), col_names = F)
kn = read_tsv(glue('{my_url}/Karoli_n.tsv'), col_names = F)
kr = read_tsv(glue('{my_url}/KaroliRevid_m.tsv'), col_names = F)
names(kb) = c('book','chapter','verse','facsimile')
names(kn) = c('book','chapter','verse','normalised')
names(kr) = c('book','chapter','verse','modern')

# -- build -- #

d = kb |> 
  left_join(kn) |> 
  left_join(kr)

rollBible = function(){
  prob = sample_n(d,1)
  l1 = pull(prob,facsimile)
  l2 = pull(prob,normalised)
  l3 = pull(prob,modern)
  list(l1,l2,l3)
  }

rollBible()

d2 = d |> 
  unnest_tokens(word, normalised, drop = F)
unique(d2$book)

evan = d2 |> 
  filter(book %in% c("Mt",     "Mk",     "Lk",     "Jn"))

evan2 = evan |> 
  mutate(
    akarmi = hunspell_analyze(word, dict = dictionary('hu_HU')),
    length_analysis = map_dbl(akarmi, length),
    hunspell_analysis = case_when(
      length_analysis > 0 ~ map_chr(akarmi, first),
      length_analysis == 0 ~ NA
    )
         ) |> 
  select(-length_analysis,-akarmi)

my_tags = unique(evan2$hunspell_analysis)
str_extract(my_tags, '[^ :]+:') |> unique()
"ip:PREF sp:meg  st:bocsát po:vrb ts:PRES_INDIC_INDEF_SG_3 al:bocsátván"
tibble(
  my_tags
) |> 
  mutate(my_lengths = nchar(my_tags)) |> 
  arrange(-my_lengths) |> 
  head(1) |> 
  pull(my_tags)

my_tags = evan2 |> 
  distinct(hunspell_analysis) |> 
  mutate(
    hunspell_stem = str_extract(hunspell_analysis, '(?<=st:)[^ ]+(?= )'),
    hunspell_prefix = str_extract(hunspell_analysis, '(?<=ip:)[^ ]+(?= )'),
    hunspell_stem_prefix = str_extract(hunspell_analysis, '(?<=sp:)[^ ]+(?= )'),
    hunspell_pos = str_extract(hunspell_analysis, '(?<=po:)[^ ]+(?= )'),
    hunspell_tense = str_extract(hunspell_analysis, '(?<=ts:)[^ ]+(?= )'),
    hunspell_auxiliary = str_extract(hunspell_analysis, '(?<=al:)[^ ]+(?= )'),
    hunspell_particle = str_extract(hunspell_analysis, '(?<=pa:)[^ ]+(?= )')
  )

evan3 = evan2 |> 
  left_join(my_tags)

# -- write -- #

evan3 |> 
  write_tsv('dat/karoli_evangelium_tokenised.tsv')
