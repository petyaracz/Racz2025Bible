# -- head -- #

library(tidyverse)
library(gghalves)
library(ggthemes)
library(patchwork)

setwd('~/Github/Racz2025Bible/')

# -- read -- #

info2 = read_tsv('dat/gospel_bigram_informativity.gz')
info3 = read_tsv('dat/gospel_trigram_informativity.gz')
d = read_tsv('dat/gospel_entropy.tsv')

# -- setup -- #

d = d |> 
  mutate(
    work = description |> 
      str_extract('^.*(?=, Forrás)') |> 
      str_replace('Keletkezési idő: ', '') |> 
      fct_reorder(-year)
  )

d1 = d |> 
  filter(
    type == 'facsimile' | translation == 'RUF'
  )

d2 = d |> 
  filter(
    type == 'normalised' | translation == 'RUF'
  )

d1b = d1 |> 
  filter(translation != 'RUF') |> 
  select(work,year,book,verse,perplexity,wc,avg_word_length,type_token_ratio) |> 
  rename_with(~ paste0(., "_orig"), -c(work,year,book,verse))

d2b = d2 |> 
  filter(translation != 'RUF') |> 
  select(work,year,book,verse,perplexity,wc,avg_word_length,type_token_ratio) |> 
  rename_with(~ paste0(., "_norm"), -c(work,year,book,verse))

d_cor = left_join(d1b,d2b) |> 
  mutate(work = fct_reorder(work, year)) # since I'm not flipping the cor plots

# -- viz -- #

## info

info2 |> 
  filter(type == 'facsimile') |> 
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
  ylab('Bigram information density (original)')
ggsave('viz/gospel_bigram_info.pdf', width = 6, height = 6)

info3 |> 
  filter(type == 'facsimile') |> 
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
  ylab('Trigram information density (original)')
ggsave('viz/gospel_trigram_info.pdf', width = 6, height = 6)

## other

names(d1)
buildPlots = function(d1){
p1 = d1 |> 
  ggplot(aes(work,perplexity)) +
  geom_half_violin(side = 'r', position = position_nudge(x = 0.1)) +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  theme_bw() +
  ylab('verse perplexity') +
  theme(
    axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank()
        ) +
  coord_flip()

p2 = d1 |> 
  ggplot(aes(work,wc)) +
  geom_half_violin(side = 'r', position = position_nudge(x = 0.1)) +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  theme_bw() +
  ylab('verse word count') +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ylab('verse word count') +
  coord_flip()

p3 = d1 |> 
  ggplot(aes(work,avg_word_length)) +
  geom_half_violin(side = 'r', position = position_nudge(x = 0.1)) +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  theme_bw() +
  ylab('verse average word length') +
  theme(
    axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank()
  ) +
  ylab('verse average word length') +
  coord_flip()

p4 = d1 |> 
  ggplot(aes(work,type_token_ratio)) +
  geom_half_violin(side = 'r', position = position_nudge(x = 0.1)) +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  theme_bw() +
  ylab('verse type token ratio') +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ylab('verse type token ratio') +
  coord_flip()

list(p1,p2,p3,p4)
}

plots1 = buildPlots(d1)
plots2 = buildPlots(d2)

## cors

drawCor = function(d_cor,col1,col2){
  d_cor |> 
    ggplot(aes({{col2}},{{col1}}, colour = work)) +
    geom_point(alpha = .5) +
    geom_smooth(alpha = .5) +
    scale_colour_viridis_d(option = 'D') +
    scale_fill_viridis_d(option = 'D') +
    theme_bw() +
    xlab('normalised') +
    ylab('original')
}

p5 = drawCor(d_cor,perplexity_orig,perplexity_norm)  +
  ggtitle('verse perplexity')
p6 = drawCor(d_cor,wc_orig,wc_norm) +
  ggtitle('verse word count')
p7 = drawCor(d_cor,avg_word_length_orig,avg_word_length_norm)  +
  ggtitle('verse average word length')
p8 = drawCor(d_cor,type_token_ratio_orig,type_token_ratio_norm)  +
  ggtitle('verse type token ratio')

# -- draw -- #

wrap_plots(plots1, ncol = 2) + plot_annotation(tag_levels = 'i', title = 'original text')
ggsave('viz/gospel_stats_original.png', dpi = 900, width = 8, height = 6)
wrap_plots(plots2, ncol = 2) + plot_annotation(tag_levels = 'i', title = 'normalised text')
ggsave('viz/gospel_stats_normalised.png', dpi = 900, width = 8, height = 6)

(p5 + p6) / (p7 + p8) + plot_layout(guides = "collect") & theme(legend.position = "left")
ggsave('viz/gospel_stats_correlations.png', dpi = 900, width = 8, height = 6)
