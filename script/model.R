# -- head -- #

library(tidyverse)
library(broom.mixed)
library(rstanarm)
library(performance)
library(sjPlot)
library(ggthemes)
library(patchwork)

setwd('~/Github/Racz2025Bible/')

# -- read -- #

d = read_tsv('dat/gospel_entropy.tsv')

# -- setup -- #

d = d |> 
  mutate(
    work = description |> 
      str_extract('^.*(?=, Forrás)') |> 
      str_replace('Keletkezési idő: ', '') |> 
      fct_reorder(year)
  )

d1 = d |> 
  filter(
    type == 'facsimile' | translation == 'RUF'
  )

d2 = d |> 
  filter(
    type == 'normalised' | translation == 'RUF'
  )

# -- fit -- #

# perplexity, original
fit1 = stan_glm(perplexity ~ wc, data = d1, cores = 4)
fit2 = stan_glm(perplexity ~ book + wc, data = d1, cores = 4)
fit3 = stan_glm(perplexity ~ work + wc, data = d1, cores = 4)
fit4 = stan_glm(perplexity ~ work + book + wc, data = d1, cores = 4)
fit5 = stan_glm(perplexity ~ work * book + wc, data = d1, cores = 4)
fit6 = stan_glm(perplexity ~ work * wc + book, data = d1, cores = 4) # you're winner
fit7 = stan_glm(perplexity ~ work * wc + book * wc, data = d1, cores = 4)
fit8 = stan_glm(perplexity ~ work * book * wc, data = d1, cores = 4)

loo1 = loo(fit1)
loo2 = loo(fit2)
loo3 = loo(fit3)
loo4 = loo(fit4)
loo5 = loo(fit5)
loo6 = loo(fit6)
loo7 = loo(fit7)
loo8 = loo(fit8, k_threshold = .7)

loo_compare(loo1,loo2,loo3,loo4,loo5,loo6,loo7,loo8) # 6

# perplexity, normalised
fit1b = stan_glm(perplexity ~ wc, data = d2, cores = 4)
fit2b = stan_glm(perplexity ~ book + wc, data = d2, cores = 4)
fit3b = stan_glm(perplexity ~ work + wc, data = d2, cores = 4)
fit4b = stan_glm(perplexity ~ work + book + wc, data = d2, cores = 4)
fit5b = stan_glm(perplexity ~ work * book + wc, data = d2, cores = 4)
fit6b = stan_glm(perplexity ~ work * wc + book, data = d2, cores = 4) # you're winner
fit7b = stan_glm(perplexity ~ work * wc + book * wc, data = d2, cores = 4)
fit8b = stan_glm(perplexity ~ work * book * wc, data = d2, cores = 4)

loo1b = loo(fit1b)
loo2b = loo(fit2b)
loo3b = loo(fit3b)
loo4b = loo(fit4b)
loo5b = loo(fit5b)
loo6b = loo(fit6b)
loo7b = loo(fit7b)
loo8b = loo(fit8b, k_threshold = .7)

loo_compare(loo1b,loo2b,loo3b,loo4b,loo5b,loo6b,loo7b,loo8b) # 6b

# -- write -- #

save(fit6, file = 'models/fit6.rda')
save(fit6b, file = 'models/fit6b.rda')
