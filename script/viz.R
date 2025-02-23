# -- head -- #

library(tidyverse)
library(ggthemes)
library(ggridges)
library(nnet)
library(patchwork)
library(sjPlot)

setwd('~/Github/Racz2025Bible/')

# -- fun -- #

# take dat colnames return ridge plot w/ quantiles
ridgePlot = function(dat,col1,col2){
  dat |> 
    ggplot(aes({{col2}},{{col1}}, fill = factor(after_stat(quantile)))) +
    stat_density_ridges(
      geom = "density_ridges_gradient", 
      calc_ecdf = TRUE,
      quantiles = 4, 
      quantile_lines = TRUE,
      rel_min_height = 0.01,
      scale = 1.33
    ) +
    scale_fill_viridis_d(name = "Quartiles", option = 'C') +
    theme_few() +
    guides(fill = 'none') +
    theme(axis.title.y = element_blank()) +
    scale_y_discrete(expand = expansion(mult = c(.1, .25)))  
}

# draw cor bw col1 col2
drawCor = function(d_cor,col1,col2){
  d_cor |> 
    ggplot(aes({{col2}},{{col1}}, colour = work)) +
    geom_point(alpha = .5) +
    geom_smooth(alpha = .5) +
    scale_colour_viridis_d(option = 'H') +
    scale_fill_viridis_d(option = 'H') +
    theme_bw() +
    xlab('normalised') +
    ylab('original')
}

# take pred object and predictor name (see setup below) return plot
drawPred = function(pred,col1){
  pred |> 
    ggplot(aes({{col1}},value,colour = name)) +
    geom_point(alpha = .25) +
    geom_smooth() +
    scale_colour_viridis_d(option = 'H') +
    scale_fill_viridis_d(option = 'H') +
    theme_bw() +
    theme(axis.title.y = element_blank())
}

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

d |> 
  distinct(work)

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
  mutate(work = fct_rev(work)) # I need these in this order here

fit0 = multinom(work ~ perplexity + wc + type_token_ratio, data = d1)
fit4 = multinom(work ~ perplexity + wc + type_token_ratio, data = d2)

my_levels = as.character(unique(d$work))

pred1 = fit0 |> 
  predict(d1, type = 'probs') |> 
  as_tibble()

pred1 = d1 |> 
  select(work,perplexity,wc,type_token_ratio) |> 
  bind_cols(pred1) |> 
  pivot_longer(-c(work,perplexity,wc,type_token_ratio)) |> 
  mutate(name = fct_relevel(name, my_levels))

pred2 = fit4 |> 
  predict(d2, type = 'probs') |> 
  as_tibble()

pred2 = d2 |> 
  select(work,perplexity,wc,type_token_ratio) |> 
  bind_cols(pred2) |> 
  pivot_longer(-c(work,perplexity,wc,type_token_ratio)) |> 
  mutate(name = fct_relevel(name, my_levels))

# -- viz: info -- #

info2 |> 
  filter(type == 'facsimile') |> 
  mutate(
    translation = fct_reorder(translation, -year),
    book = fct_relevel(book, 'Jn', 'Lk', 'Mk', 'Mt')
    ) |> 
  ggplot(aes(information,translation)) +
  geom_density_ridges() +
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
  ggplot(aes(information,translation)) +
  geom_density_ridges() +
  facet_wrap( ~ book, ncol = 4) +
  theme_minimal() +
  xlab('Gospel') +
  ylab('Trigram information density (original)')
ggsave('viz/gospel_trigram_info.pdf', width = 6, height = 6)

# -- viz: stats -- #

range(d1$perplexity)
range(d2$perplexity)
range(d1$wc)
range(d2$wc)
range(d1$avg_word_length)
range(d2$avg_word_length)
range(d1$type_token_ratio)
range(d2$type_token_ratio)

p1 = ridgePlot(d1,work,perplexity) +
  ggtitle('original text') +
  xlim(90,410) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank()
  )

p2 = ridgePlot(d1,work,wc) +
  # ggtitle('original text') +
  xlab('verse word count') +
  xlim(250,1650) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

p3 = ridgePlot(d1,work,avg_word_length) +
  # ggtitle('original text') +
  xlab('verse avg. word length') +
  xlim(4,6.1) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

p4 = ridgePlot(d1,work,type_token_ratio) +
  # ggtitle('original text') +
  xlab('verse type/token') +
  xlim(.35,.9) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

p5 = ridgePlot(d2,work,perplexity) +
  ggtitle('normalised text') +
  xlab('verse perplexity') +
  xlim(90,410)

p6 = ridgePlot(d2,work,wc) +
  # ggtitle('normalised text') +
  xlim(250,1650) +
  xlab('verse word count') +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p7 = ridgePlot(d1,work,avg_word_length) +
  # ggtitle('original text') +
  xlab('verse avg. word length') +
  xlim(4,6.1) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p8 = ridgePlot(d2,work,type_token_ratio) +
  xlab('verse type/token') +
  xlim(.35,.9) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

info_plot = wrap_plots(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2)

# -- viz: corr -- #
## cors

p9 = drawCor(d_cor,perplexity_orig,perplexity_norm)  +
  ggtitle('verse perplexity')
p10 = drawCor(d_cor,wc_orig,wc_norm) +
  ggtitle('verse word count')
p11 = drawCor(d_cor,avg_word_length_orig,avg_word_length_norm)  +
  ggtitle('verse average word length')
p12 = drawCor(d_cor,type_token_ratio_orig,type_token_ratio_norm)  +
  ggtitle('verse type token ratio')

cor_plot = (p9 + p10) / (p11 + p12) + plot_layout(guides = "collect") & theme(legend.position = 'left')

# -- viz: preds -- #

p13 = drawPred(pred1, perplexity) +
  ggtitle('predicted work: original texts')

p14 = drawPred(pred2, perplexity) +
  ggtitle('predicted work: normalised texts')

p15 = drawPred(pred1, wc)

p16 = drawPred(pred2, wc)

p17 = drawPred(pred1, type_token_ratio)

p18 = drawPred(pred2, type_token_ratio)

pred_plot = wrap_plots(p13,p15,p17,p14,p16,p18, nrow = 2) + plot_layout(guides = 'collect') & theme(legend.position = 'left')

# -- draw -- #

info_plot
ggsave('viz/gospel_stats.png', dpi = 900, width = 9, height = 6)

cor_plot
ggsave('viz/gospel_stats_correlations.png', dpi = 900, width = 8, height = 5.28)

pred_plot
ggsave('viz/gospel_preds.png', dpi = 900, width = 9, height = 6)
