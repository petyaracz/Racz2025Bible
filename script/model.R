# -- head -- #

library(tidyverse)
library(broom.mixed)
library(lme4)
library(performance)
library(sjPlot)
library(ggthemes)
library(patchwork)
library(stats)
library(ggfortify)

setwd('~/Github/Racz2025Bible/')

# -- read -- #

d = read_tsv('dat/gospel_entropy.tsv')

# -- setup -- #

d = d |> 
  mutate(
    work = work |> 
      fct_reorder(year),
    work_rank = as.double(work),
    book = as.factor(book)
  )

d1 = d |> 
  filter(
    analysis_original
  )

d2 = d |> 
  filter(
    analysis_normalised
  )

# -- check corr -- #

with(d1, cor(type_token_ratio,complexity))
with(d2, cor(type_token_ratio,complexity))
with(d1, cor(wc,complexity))
with(d2, cor(wc,complexity))

p1 = d1 |> 
  select(perplexity,complexity,wc,type_token_ratio) |> 
  prcomp(center = TRUE, scale. = TRUE) |> 
  autoplot(data = d1, loadings = TRUE, loadings.label = TRUE, loadings.colour = 'blue', loadings.label.size = 3, ) +
  ggtitle("PCA Biplot") +
  theme_minimal()

p2 = d2 |> 
  select(perplexity,complexity,wc,type_token_ratio) |> 
  prcomp(center = TRUE, scale. = TRUE) |> 
  autoplot(data = d1, loadings = TRUE, loadings.label = TRUE, loadings.colour = 'blue', loadings.label.size = 3, ) +
  ggtitle("PCA Biplot") +
  theme_minimal()

p1 + p2

# -- fit -- #

# watch out for collinearity, you can probs have one predictor per whatever

# original
lm11 = lmer(perplexity ~ work + (1 | book/verse), data = d1)
lm12 = lmer(complexity ~ work + (1 | book/verse), data = d1)
lm13 = lmer(wc ~ work + (1 | book/verse), data = d1)
lm14 = lmer(type_token_ratio ~ work + (1 | book/verse), data = d1)
lm14b = lmer(type_token_ratio ~ work + (1 | book), data = d1)

plot(compare_performance(lm11,lm12,lm13,lm14), metrics = 'common')
r2(lm11);r2(lm12);r2(lm13);r2(lm14)
check_model(lm14)
plot(compare_performance(lm14,lm14b), metrics = 'common')

# normalised
lm21 = lmer(perplexity ~ work + (1 | book/verse), data = d2)
lm22 = lmer(complexity ~ work + (1 | book/verse), data = d2)
lm23 = lmer(wc ~ work + (1 | book/verse), data = d2)
lm24 = lmer(type_token_ratio ~ work + (1 | book/verse), data = d2)

plot(compare_performance(lm21,lm22,lm23,lm24), metrics = 'common')
r2(lm21);r2(lm22);r2(lm23);r2(lm24)
check_model(lm14)
plot(compare_performance(lm14,lm14b), metrics = 'common')
