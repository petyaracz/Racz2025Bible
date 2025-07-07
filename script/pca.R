# -- head -- #

library(tidyverse)
library(ggthemes)
library(lme4)
library(performance)
library(sjPlot)
library(broom.mixed)
library(patchwork)
library(mgcv)
library(glue)

setwd('~/Github/Racz2025Bible/')

# -- read -- #

d = read_tsv('dat/gospel_entropy.tsv')

# -- format -- #

d = d |>
  mutate(
    book_chapter = as.factor(glue('{book} {chapter}')),
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
    work_rank = 12-as.double(work2)
  ) |> 
  filter(type != 'facsimile')
  
pca1 = prcomp(select(d,unigram_perplexity,bigram_perplexity,wc,type_token_ratio,type_count,mdl_over_dl), center = TRUE, scale. = TRUE)

d = predict(pca1) |> 
  as.data.frame() |> 
  bind_cols(d)

# -- lm -- #

lm1 = lmer(PC1 ~ 1 + work3 + (1| book/chapter), data = d)
lm2 = lmer(PC2 ~ 1 + work3 + (1| book/chapter), data = d)
lm3 = lmer(PC3 ~ 1 + work3 + (1| book/chapter), data = d)

p1 = plot_model(lm1, 'pred', 'work3') +
  coord_flip() +
  theme_few()
p2 = plot_model(lm2, 'pred', 'work3') +
  coord_flip() +
  theme_few() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
  )
p3 = plot_model(lm3, 'pred', 'work3') +
  coord_flip() +
  theme_few() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
  )

p1 + p2 + p3

r2(lm1)
r2(lm2)
r2(lm3)

# -- gam -- #


d |> 
  distinct(work_rank,work)

ord1 = gam(work_rank ~ 
             s(PC1, k = 3) + 
             s(PC2, k = 3) + 
             s(PC3, k = 3) + 
             s(PC4, k = 3) + 
             s(PC5, k = 3) + 
             s(book_chapter, bs = 're'), 
           data = d,
           family = ocat(R=11),
           method = 'ML'
)

summary(ord1)
plot(ord1)

preds = ord1 |> 
  predict(type = 'response') |> 
  as.data.frame()

names(preds) = rev(levels(d$work2))

preds = preds |> 
  mutate(
    id = 1:n()
  ) |> 
  pivot_longer(-id)

preds |> 
  ggplot(aes(id,value,colour = name, group = name)) +
  geom_smooth() +
  scale_colour_viridis_d() +
  theme_few()
# need to bin by translation