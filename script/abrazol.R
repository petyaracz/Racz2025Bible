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
library(scales)

setwd('~/Github/Racz2025Bible/')

# -- fun -- #

paired_palette = RColorBrewer::brewer.pal(11, "Paired")
# gray_palette = grey_pal(0.1,.9)(10)
quartile_palette = c("#E6E6E6", "#B3B3B3", "#B3B3B3", "#E6E6E6")

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
    #scale_fill_viridis_d(name = "Quartiles", option = 'C') +
    scale_fill_manual(name = "Quartiles", values = quartile_palette) +
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
    xlab('normalizált') +
    ylab('betűhű')
}

d = read_tsv('dat/gospel_entropy.tsv')

# -- setup -- #

d = d |> 
  mutate(
    work = fct_reorder(work, -year),
    fordítás = work
  ) |> 
  mutate(
    "bizonytalanság" = perplexity,
    "összetettség" = complexity,
    "szavak száma" = wc,
    "típus / token arány" = type_token_ratio
  ) 

d1 = d |> 
  filter(
    analysis_original
  )

d2 = d |> 
  filter(
    analysis_normalised
  )

d1b = d1 |> 
  filter(!translation %in% c('RUF','SzIT','KaldiNeo', 'KaroliRevid')) |> 
  select(work,year,book,verse,bizonytalanság,összetettség,`szavak száma`,`típus / token arány`) |> 
  rename_with(~ paste0(., ", betűhű"), -c(work,year,book,verse))

d2b = d2 |> 
  filter(!translation %in% c('RUF','SzIT','KaldiNeo', 'KaroliRevid')) |> 
  select(work,year,book,verse,bizonytalanság,összetettség,`szavak száma`,`típus / token arány`) |> 
  rename_with(~ paste0(., ", normalizált"), -c(work,year,book,verse))

d_cor = left_join(d1b,d2b) |> 
  mutate(work = fct_rev(work)) # I need these in this order here

illeszték11 = lmer(bizonytalanság ~ fordítás + (1 | book/verse), data = d1)
illeszték21 = lmer(bizonytalanság ~ fordítás + (1 | book/verse), data = d2)

# -- viz: var cor -- #

p01 = d1 |> 
  select(bizonytalanság,összetettség,`szavak száma`,`típus / token arány`) |> 
  prcomp(center = TRUE, scale. = TRUE) |> 
  autoplot(data = d1, loadings = TRUE, loadings.label = TRUE, loadings.colour = 'blue', loadings.label.size = 3, alpha = .01) +
  ggtitle("betűhű szövegek") +
  theme_few() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

p02 = d2 |> 
  select(bizonytalanság,összetettség,`szavak száma`,`típus / token arány`) |> 
  prcomp(center = TRUE, scale. = TRUE) |> 
  autoplot(data = d1, loadings = TRUE, loadings.label = TRUE, loadings.colour = 'blue', loadings.label.size = 3, alpha = .01) +
  ggtitle("normalizált szövegek") +
  theme_few() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

var_cors_plot_h = p01 + p02 + plot_annotation(title = 'főkomponens diagram')

# -- viz: stats -- #

range(d1$perplexity)
range(d2$perplexity)
range(d1$complexity)
range(d2$complexity)
range(d1$wc)
range(d2$wc)
range(d1$avg_word_length)
range(d2$avg_word_length)
range(d1$type_token_ratio)
range(d2$type_token_ratio)

p1 = ridgePlot(d1,work,bizonytalanság) +
  ggtitle('betűhű szöveg') +
  xlim(90,410) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank()
  )

p2 = ridgePlot(d1,work,összetettség) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

p3 = ridgePlot(d1,work,`szavak száma`) +
  xlim(250,1650) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

p5 = ridgePlot(d1,work,`típus / token arány`) +
  xlim(.35,.9) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

p6 = ridgePlot(d2,work,bizonytalanság) +
  ggtitle('normalizált szöveg') +
  xlim(90,410)

p7 = ridgePlot(d2,work,összetettség) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p8 = ridgePlot(d2,work,`szavak száma`) +
  xlim(250,1650) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p10 = ridgePlot(d2,work,`típus / token arány`) +
  xlim(.35,.9) +
  xlab("típus / token\narány") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

info_plot_h = wrap_plots(p1,p2,p3,p5,p6,p7,p8,p10, nrow = 2)

# -- viz: corr -- #
## cors

p11 = d_cor |> 
  drawCor(`bizonytalanság, betűhű`,`bizonytalanság, normalizált`)  +
  ggtitle('bizonytalanság')
p11b  = d_cor |> 
  drawCor(`összetettség, betűhű`,`összetettség, normalizált`)  +
  ggtitle('összetettség')
p12 = d_cor |> 
  drawCor(`szavak száma, betűhű`,`szavak száma, normalizált`) +
  ggtitle('szavak száma')
p14 = d_cor |> 
  drawCor(`típus / token arány, betűhű`,`típus / token arány, normalizált`)  +
  ggtitle('típus / token arány')

cor_plot_h = (p11 + p11b) / (p12 + p14) + plot_layout(guides = "collect") & theme(legend.position = 'left')

# -- viz: preds -- #

# 15,16
p15 = plot_model(illeszték11, 'pred', terms = 'fordítás') +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  ylim(90,410) +
  ggtitle('betűhű szövegek')

p16 = plot_model(illeszték21, 'pred', terms = 'fordítás') +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
        ) +
  ylim(90,410) +
  ggtitle('normalizált szövegek')

pred_plot_h = p15 + p16

# -- agyvihar -- #

p19 = d1 |> 
  filter(
    book == 'Mt',
    verse %in% 5:7
  ) |> 
  mutate(
    work = fct_rev(work),
    vers = glue('Máté {verse}')
  ) |> 
  ggplot(aes(work,perplexity, group = vers, colour = vers, lty = vers)) +
  scale_colour_grey() +
  geom_line() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    ) +
  ggtitle('Betűhű szöveg') +
  ylim(110,300)

p20 = d2 |> 
  filter(
    book == 'Mt',
    verse %in% 5:7
  ) |> 
  mutate(
    work = fct_rev(work),
    vers = glue('Máté {verse}')
  ) |> 
  ggplot(aes(work,bizonytalanság, group = vers, colour = vers, lty = vers)) +
  scale_colour_grey() +
  geom_line() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    ) +
  ggtitle('Normalizált szöveg') + 
  ylim(110,300)

lines_plot_h = p19 + p20 + plot_layout(guides = 'collect') & theme(legend.position = 'left')

# -- accuracy -- #

r2(fit11)
r2(fit21)

# -- draw -- #

var_cors_plot_h
ggsave('abra/gospel_varcor_h.png', dpi = 900, width = 6, height = 3)

info_plot_h
ggsave('abra/gospel_stats_h.png', dpi = 900, width = 9, height = 6.5)

cor_plot_h
ggsave('abra/gospel_stats_correlations_h.png', dpi = 900, width = 7.2, height = 4)

pred_plot_h
ggsave('abra/gospel_preds_h.png', dpi = 900, width = 8, height = 4)

lines_plot_h
ggsave('abra/gospel_lines_h.png', dpi = 900, width = 8, height = 6.22)
