# -- head -- #

library(tidyverse)
library(glue)
library(ggthemes)
library(ggridges)
library(ggfortify)
library(lme4)
library(performance)
library(patchwork)
library(sjPlot)

setwd('~/Github/Racz2025Bible/')

# -- fun -- #

magyarni = function(dat){
  dat |> 
    mutate(
      típus = case_when(
        type == 'facsimile' ~ 'betűhű',
        type == 'normalised' ~ 'normalizált',
        type == 'modern' ~ 'modern'
      )
    ) |> 
    rename(
      'unigram entrópia' = unigram_perplexity,
      'bigram entrópia' = bigram_perplexity,
      'MDL/DL' = mdl_over_dl,
      'szószám' = wc,
      'típus/token' = type_token_ratio,
      'betűhű/normalizált' = chapter_diff
    )
}

paired_palette = RColorBrewer::brewer.pal(11, "Paired")
quantile_palette = c("#E6E6E6", "#AEAEAE", "#AEAEAE", "#E6E6E6")

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
    scale_fill_manual(name = "Quartiles", values = quantile_palette) +
    theme_minimal() +
    guides(fill = 'none') +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
      ) +
    scale_y_discrete(expand = expansion(mult = c(.1, .25)))  
}

# draw cor bw col1 col2
drawCor = function(d_cor,col1,col2){
  d_cor |> 
    ggplot(aes({{col2}},{{col1}}, colour = work)) +
    geom_point(alpha = .1) +
    geom_smooth() +
    scale_color_manual(values = paired_palette[1:7]) +
    scale_fill_manual(values = paired_palette[1:7]) +
    theme_bw() +
    xlab('normalised') +
    ylab('original')
}

# -- read -- #

d = read_tsv('dat/gospel_entropy.tsv')

lm1 = readRDS('models/lm1.rds')
lm2 = readRDS('models/lm2.rds')
lm3 = readRDS('models/lm3.rds')
lm4 = readRDS('models/lm4.rds')
lm5 = readRDS('models/lm5.rds')

# -- setup -- #

d = d |> 
  mutate(
    work2 = work |> 
      str_replace('(\\: Újtestamentum| fordítása)', '') |> 
      str_replace('-kódex', ' kódex') |> 
      str_replace('István Társulati', 'István\nTársulati') |> 
      str_replace('Bibliatársulat újfordítású', 'Bibliatársulat\nújfordítású') |> 
      str_replace('Gáspár Vizsolyi', 'Gáspár\nVizsolyi') |> 
      str_replace(', ', ',\n'),
    work2 = fct_reorder(work2, -year),
    work = fct_reorder(work, -year),
    work3 = ordered(work),
    chapter_sim = 1-chapter_diff
  )

d2 = filter(d, type != 'facsimile')

# -- viz -- #

## corrplot on variables

cors = d |> 
  magyarni() |>
  select(típus,szószám,`MDL/DL`,`típus/token`,`unigram entrópia`,`bigram entrópia`) |> 
  nest(.by = típus) |> 
  mutate(
    cor = map(
      data, 
      ~ cor(., method = 'spearman') |> 
        as.data.frame() |> 
        rownames_to_column(var = "Var1") %>%
        pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Korreláció")
       )
  ) |> 
  select(-data) |> 
  unnest(cor)

corplot = cors |> 
  mutate(
    típus = factor(típus, levels = c('betűhű', 'normalizált', 'modern')),
    Var1 = fct_inorder(Var1),
    Var2 = fct_inorder(Var2) |> fct_rev()
  ) |> 
  ggplot(aes(Var1,Var2,fill = Korreláció, label = round(Korreláció,2))) +
  geom_tile() +
  geom_text(colour = 'white') +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0),
    plot.margin = margin(5.5, 50, 5.5, 5.5)
  ) +
  guides(fill = 'none') +
  scale_x_discrete(position = 'top') +
  facet_wrap( ~ típus, strip.position='right', ncol = 1)

corplot
ggsave('viz/gospel_varcorr_corplot.png', dpi = 900, height = 6, width = 4.5)

## pca on variables

# drop modern texts, group by type, nest, build prcomp, build autoplot
prcomps = d |> 
  magyarni() |> 
  select(típus,`bigram entrópia`,`MDL/DL`,`típus/token`,`unigram entrópia`,szószám) |>
  nest(.by = típus) |> 
  mutate(
    prcomp = map(
      data, 
      ~ prcomp(., center = TRUE, scale. = TRUE)
      ),
    autoplot12 = pmap(
      list(prcomp, data, típus), 
      ~ autoplot(
          ..1, 
          data = ..2, 
          loadings = TRUE, 
          loadings.label = TRUE, 
          loadings.colour = 'lightgrey', 
          loadings.label.size = 3, 
          shape = FALSE, 
          label.size = 0
        ) + 
        ggtitle(..3) + 
        theme_few() #+
        #theme(
        #  axis.title.x = element_blank(),
        #  axis.ticks.x = element_blank(),
        #  axis.text.x = element_blank()
        #      )
      ),
    autoplot13 = pmap(
      list(prcomp, data, típus), 
      ~ autoplot(
        ..1, 
        data = ..2, 
        loadings = TRUE, 
        loadings.label = TRUE, 
        loadings.colour = 'lightgrey', 
        loadings.label.size = 3, 
        shape = FALSE, 
        label.size = 0,
        y = 3
      ) + 
        ggtitle(..3) + 
        theme_few() +
        theme(
          plot.title = element_blank()
        )
    )
  )

prcomps12 = pull(prcomps,autoplot12)
prcomps13 = pull(prcomps,autoplot13)

pc1 = wrap_plots(prcomps12, ncol = 1)
#pc2 = wrap_plots(prcomps13)
#pc1 / pc2
pc1
ggsave('viz/gospel_varcorr.png', dpi = 900, width = 4, height = 8)

## facsimile: stats

p11 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,bigram_perplexity) +
  xlab('bigram entrópia\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p12 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,mdl_over_dl) +
  xlab('MDL/DL\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p13 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,type_token_ratio) +
  xlab('típus/token\neloszlások')# +
  # theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p14 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,unigram_perplexity) +
  xlab('unigram entrópia\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p15 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,wc) +
  xlab('szószám\neloszlások') +
  ggtitle('betűhű szövegek') # +
  # theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
p16 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,chapter_sim) +
  xlab('betűhű-normalizált\nközelség') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

wrap_plots(p15,p16,p12,p13,p14,p11, nrow = 2) + plot_annotation(tag_levels = 'a')
ggsave('viz/gospel_stats_facsimile.png', dpi = 900, width = 9, height = 9)

## normalised

p21 = d |> 
  filter(type != 'facsimile') |> 
  ridgePlot(work2,bigram_perplexity) +
  xlab('bigram entrópia\neloszlások') +
  geom_hline(yintercept = 5, lty = 3) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p22 = d |> 
  filter(type != 'facsimile') |> 
  ridgePlot(work2,mdl_over_dl) +
  geom_hline(yintercept = 5, lty = 3) +
  xlab('MDL/DL\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p23 = d |> 
  filter(type != 'facsimile') |> 
  ridgePlot(work2,type_token_ratio) +
  geom_hline(yintercept = 5, lty = 3) +
  xlab('típus/token arány\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p24 = d |> 
  filter(type != 'facsimile') |> 
  ridgePlot(work2,unigram_perplexity) +
  xlab('unigram entrópia\neloszlások') +
  geom_hline(yintercept = 5, lty = 3)# +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p25 = d |> 
  filter(type != 'facsimile') |> 
  ridgePlot(work2,wc) +
  geom_hline(yintercept = 5, lty = 3) +
  xlab('szószám\neloszlások') +
  ggtitle('normalizált szövegek')# +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

#wrap_plots(p25,p22,p23,p24,p21, nrow = 2)
(p25 | p22 | p23 ) / (p24 | p21 | plot_spacer()) + plot_annotation(tag_levels = 'a')
ggsave('viz/gospel_stats_normalised.png', dpi = 900, width = 9, height = 12)

## pred

r21 = round(r2(lm1)[[2]],2)
r22 = round(r2(lm2)[[2]],2)
r23 = round(r2(lm3)[[2]],2)
r24 = round(r2(lm4)[[2]],2)
r25 = round(r2(lm5)[[2]],2)

p31 = plot_model(lm1, 'pred', terms = 'work3', ci.lvl = .99) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank(),   # Remove minor horizontal grid lines
    plot.title = element_blank()
  ) +
  ylab(glue('bigram entrópia (m. r2 = {r21})')) +
  # ggtitle('jósolt értékek (normalizált / modern szövegek)') +
  geom_vline(xintercept = 4.5, lty = 3)

p32 = plot_model(lm2, 'pred', terms = 'work3', ci.lvl = .99) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank(),   # Remove minor horizontal grid lines
    plot.title = element_blank()
  ) +
  ylab(glue('MDL/DL (m. r2 = {r22})')) +
  # ggtitle('jósolt értékek (normalizált / modern szövegek)') +
  geom_vline(xintercept = 4.5, lty = 3)

p33 = plot_model(lm3, 'pred', terms = 'work3', ci.lvl = .99) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank(),   # Remove minor horizontal grid lines
    plot.title = element_blank()
  ) +
  ylab(glue('típus/token arány (m. r2 = {r23})')) +
  # ggtitle('jósolt értékek (normalizált / modern szövegek)') +
  geom_vline(xintercept = 4.5, lty = 3)

p34 = plot_model(lm4, 'pred', terms = 'work3', ci.lvl = .99) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank(),   # Remove minor horizontal grid lines
    plot.title = element_blank()
  ) +
  ylab(glue('unigram entrópia (m. r2 = {r24})')) +
  # ggtitle('jósolt értékek (normalizált / modern szövegek)') +
  ggtitle('') +
  geom_vline(xintercept = 4.5, lty = 3)

p35 = plot_model(lm5, 'pred', terms = 'work3', ci.lvl = .99) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank()   # Remove minor horizontal grid lines
  ) +
  ylab(glue('szószám (m. r2 = {r25})')) +
  ggtitle('jósolt értékek (normalizált / modern szövegek)') +
  geom_vline(xintercept = 4.5, lty = 3)

(p35 | p32 ) / ( p33 | p34 ) / ( p31 | plot_spacer()) + plot_annotation(tag_levels = 'a')

ggsave('viz/gospel_preds.png', dpi = 900, width = 8, height = 8)

# -- example -- #

facet_labels = c(
  'facsimile' = 'betűhű',
  'normalised' = 'normalizált'
)

wcs = d |> 
  filter(translation == 'Karoli', type == 'facsimile') |> 
  distinct(book,chapter,wc) |> 
  rename(word_count = wc)

matthew_plots = d |> 
  filter(
    book == 'Mt', 
    chapter %in% 5:7,
    type != 'modern'
    ) |> 
  left_join(wcs) |> 
  mutate(
    work = fct_reorder(work, year),
    chapter2 = glue('Máté {chapter}\n({word_count} szó)')
         ) |> 
  rename(
    `bigram entrópia` = bigram_perplexity,
    `MDL/DL` = mdl_over_dl,
    `típus/token` = type_token_ratio,
    `unigram entrópia` = unigram_perplexity,
    szószám = wc
  ) |> 
  select(work,type,chapter2,`bigram entrópia`,`MDL/DL`,`típus/token`,`unigram entrópia`,szószám) |> 
  pivot_longer(-c(work,type,chapter2)) |> 
  nest(.by = c(name)) |> 
  mutate(
    plot = pmap(
      list(data, name),
      ~ ggplot(
          ..1,
          aes(work,value, group = chapter2, colour = chapter2, lty = chapter2)
        ) +
        scale_colour_grey() +
        geom_line() +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        ) +
        labs(
          y = ..2,
          colour = 'fejezet', 
          lty = 'fejezet'
        ) +
        facet_wrap( ~ type, ncol = 1, labeller = labeller(type = facet_labels))
    )
  ) |> 
  pull(plot)
  
wrap_plots(matthew_plots, ncol = 1) + plot_layout(guides = 'collect', axes = 'collect') & theme(legend.position = 'top')
ggsave('viz/gospel_matthew.png', dpi = 900, width = 3, height = 12)
