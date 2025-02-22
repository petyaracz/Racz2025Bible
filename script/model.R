# -- head -- #

library(tidyverse)
library(lme4)
library(performance)
library(sjPlot)
library(ggthemes)

setwd('~/Github/Racz2025Bible/')

# -- read -- #

info2 = read_tsv('dat/gospel_bigram_informativity.tsv')

# -- setup -- #

info2 = info2 |> 
  mutate(
    translation = translation |> 
      fct_reorder(year) |> 
      ordered()
  )

frequent_words = info2 |> 
  count(word2, sort = T) |> 
  slice(1:50)

info2s = info2 |> 
  filter(word2 %in% frequent_words$word2)

# -- fit -- #

# lm
fit1 = lm(information ~ translation, data = mark)
fit2 = lm(information ~ 1, data = mark)

plot(compare_performance(fit1,fit2, metrics = 'common'))
plot_model(fit1, 'pred', terms = 'translation') +
  theme_bw()
check_model(fit1)

# lm: translation, book
fit3 = lm(information ~ book, data = info2)
fit4 = lm(information ~ translation + book, data = info2)
fit5 = lm(information ~ translation * book, data = info2)
plot(compare_performance(fit3,fit4,fit5, metrics = 'common'))

plot_model(fit4, 'pred', terms = 'translation') +
  theme_bw()
plot_model(fit4, 'pred', terms = 'book') +
  theme_bw()

# extremely suspect:
# fit6 = lm(information ~ translation + book, data = info2s)
# fit7 = lmer(information ~ translation + book + (1|word2), data = info2s)
# fit8 = lmer(information ~ translation + book + (1 + translation|word2), data = info2s)
# 
# plot(compare_performance(fit6,fit7,fit8, metrics = 'common'))
