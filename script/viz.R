# -- head -- #

library(tidyverse)
library(gghalves)
library(ggthemes)
library(patchwork)

setwd('~/Github/Racz2025Bible/')

# -- read -- #

info2 = read_tsv('dat/gospel_bigram_informativity.tsv')
info3 = read_tsv('dat/gospel_trigram_informativity.tsv')


# -- viz -- #

info2 |> 
  mutate(
    translation = fct_reorder(translation, -year),
    book = fct_relevel(book, 'Jn', 'Lk', 'Mk', 'Mt')
    ) |> 
  ggplot(aes(translation,information)) +
  geom_half_violin(side = 'r', position = position_nudge(x = 0.1)) +
  geom_tufteboxplot() +
  coord_flip() +
  facet_wrap( ~ book, ncol = 4) +
  theme_minimal() +
  xlab('Gospel') +
  ylab('Bigram information density')
ggsave('viz/gospel_bigram_info.pdf', width = 6, height = 6)

info3 |> 
  mutate(
    translation = fct_reorder(translation, -year),
    book = fct_relevel(book, 'Jn', 'Lk', 'Mk', 'Mt')
  ) |> 
  ggplot(aes(translation,information)) +
  geom_half_violin(side = 'r', position = position_nudge(x = 0.1)) +
  geom_tufteboxplot() +
  coord_flip() +
  facet_wrap( ~ book, ncol = 4) +
  theme_minimal() +
  xlab('Gospel') +
  ylab('Trigram information density')
ggsave('viz/gospel_trigram_info.pdf', width = 6, height = 6)
