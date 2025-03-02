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
      'bizonytalanság' = perplexity,
      'összetettség' = complexity,
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
    work2 = fct_reorder(work2, -year)
  )

d2 = filter(d, type != 'facsimile')

lm1 = lmer(perplexity ~ work + (1| book/chapter), data = d2)
lm2 = lmer(complexity ~ work + (1| book/chapter), data = d2)


# -- viz -- #

## pca on variables

# drop modern texts, group by type, nest, build prcomp, build autoplot
prcomps = d |> 
  magyarni() |> 
  select(típus,bizonytalanság,összetettség,szószám,`típus/token`) |> 
  nest(.by = típus) |> 
  mutate(
    prcomp = map(
      data, 
      ~ prcomp(., center = TRUE, scale. = TRUE)
      ),
    autoplot = pmap(
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
        theme_few()
      )
  ) |> 
  pull(autoplot)

wrap_plots(prcomps)
ggsave('viz/gospel_varcorr.png', dpi = 900, width = 9.4, height = 3)

## facsimile: stats

p11 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,perplexity) +
  xlab('bizonytalansági\neloszlások') +
  ggtitle('betűhű szövegek')

p12 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,complexity) +
  xlab('összetettségi\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p13 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,wc) +
  xlab('szószám\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p14 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,type_token_ratio) +
  xlab('típus/token arány\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p15 = d |> 
  filter(type == 'facsimile') |> 
  ridgePlot(work2,chapter_diff) +
  xlab('betűhű-normalizált átfedési\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

wrap_plots(p11,p12,p13,p14,p15, nrow = 1)
ggsave('viz/gospel_stats_facsimile.png', dpi = 900, width = 10, height = 5)

## normalised

p21 = d |> 
  filter(type != 'facsimile') |> 
  ridgePlot(work2,perplexity) +
  geom_hline(yintercept = 5, lty = 3) +
  xlab('bizonytalansági\neloszlások') +
  ggtitle('normalizált szövegek')

p22 = d |> 
  filter(type != 'facsimile') |> 
  ridgePlot(work2,complexity) +
  geom_hline(yintercept = 5, lty = 3) +
  xlab('összetettségi\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p23 = d |> 
  filter(type != 'facsimile') |> 
  ridgePlot(work2,wc) +
  geom_hline(yintercept = 5, lty = 3) +
  xlab('szószám\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p24 = d |> 
  filter(type != 'facsimile') |> 
  ridgePlot(work2,type_token_ratio) +
  geom_hline(yintercept = 5, lty = 3) +
  xlab('típus/token arány\neloszlások') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

wrap_plots(p21,p22,p23,p24, nrow = 1)
ggsave('viz/gospel_stats_normalised.png', dpi = 900, width = 9, height = 6.5)

## pred

r21 = round(r2(lm1)[[2]],2)
r22 = round(r2(lm2)[[2]],2)

p31 = plot_model(lm1, 'pred', terms = 'work') +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank()   # Remove minor horizontal grid lines
  ) +
  ylab(glue('bizonytalanság (m. r2 = {r21})')) +
  ggtitle('jósolt értékek (normalizált / modern szövegek)') +
  geom_vline(xintercept = 4.5, lty = 3)

p32 = plot_model(lm2, 'pred', terms = 'work') +
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
  ylab(glue('összetettség (m. r2 = {r22})')) +
  geom_vline(xintercept = 4.5, lty = 3)

p31 + p32

ggsave('viz/gospel_preds.png', dpi = 900, width = 7, height = 4)

# -- brainrot -- #

facet_labels = c(
  'facsimile' = 'betűhű',
  'normalised' = 'normalizált'
)

matthew_plots = d |> 
  filter(
    book == 'Mt', 
    chapter %in% 5:7,
    type != 'modern'
    ) |> 
  mutate(
    work = fct_rev(work),
    chapter2 = glue('Máté {chapter}')
         ) |> 
  rename(
    bizonytalanság = perplexity,
    összetettség = complexity
  ) |> 
  select(work,type,chapter2,bizonytalanság,összetettség) |> 
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
          colour = 'vers', 
          lty = 'vers'
        ) +
        facet_wrap( ~ type, ncol = 1, labeller = labeller(type = facet_labels))
    )
  ) |> 
  pull(plot)
  
wrap_plots(matthew_plots) + plot_layout(guides = 'collect') & theme(legend.position = 'left')
ggsave('viz/gospel_matthew.png', dpi = 900, width = 7, height = 4)
