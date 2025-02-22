# -- head -- #

library(tidyverse)
library(gghalves)
library(ggthemes)
library(patchwork)

setwd('~/Github/Racz2025Bible/')

# -- read -- #

info2 = read_tsv('dat/gospel_bigram_informativity.tsv')
info3 = read_tsv('dat/gospel_trigram_informativity.tsv')
d = read_tsv('dat/gospel_entropy.tsv')

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

d |> 
  mutate(fct_reorder(translation, year)) |> 
  ggplot(aes(entropy)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap( ~ translation)

d |> 
  mutate(fct_reorder(translation, year)) |> 
  ggplot(aes(perplexity)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap( ~ translation)

d |> 
  mutate(
    translation = fct_reorder(translation, -year),
    book = fct_relevel(book, 'Mt', 'Mk', 'Lk', 'Jn')
  ) |> 
  ggplot(aes(translation,perplexity)) +
  geom_half_violin(side = 'r', position = position_nudge(x = 0.1)) +
  geom_tufteboxplot() +
  coord_flip() +
  facet_wrap( ~ book, ncol = 4) +
  theme_minimal() +
  xlab('Gospel perplexity')
ggsave('viz/gospel_perplexity.pdf', width = 6, height = 6)

# https://media1.tenor.com/m/ijwOZvLtFvsAAAAC/kios-angry.gif

d |> 
  mutate(
    translation = fct_reorder(translation, -year),
    book = fct_relevel(book, 'Mt', 'Mk', 'Lk', 'Jn')
  ) |> 
  ggplot(aes(translation,perplexity)) +
  geom_half_violin(side = 'r', position = position_nudge(x = 0.1)) +
  geom_tufteboxplot() +
  coord_flip() +
  # facet_wrap( ~ book, ncol = 4) +
  theme_minimal() +
  xlab('Gospel perplexity')

d |> 
  mutate(
    translation = fct_reorder(translation, -year),
    book = fct_relevel(book, 'Mt', 'Mk', 'Lk', 'Jn')
  ) |> 
  ggplot(aes(translation,entropy)) +
  geom_half_violin(side = 'r', position = position_nudge(x = 0.1)) +
  geom_tufteboxplot() +
  coord_flip() +
  # facet_wrap( ~ book, ncol = 4) +
  theme_minimal() +
  xlab('Gospel entropy')
