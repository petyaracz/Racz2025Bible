# -- head -- #

library(tidyverse)
library(broom.mixed)
library(nnet)
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
      fct_reorder(year),
    work_rank = as.double(work),
    book = as.factor(book)
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
fit0 = multinom(work ~ perplexity + wc + type_token_ratio, data = d1)
fit1 = multinom(work ~ wc + type_token_ratio, data = d1)
fit2 = multinom(work ~ perplexity + type_token_ratio, data = d1)
fit3 = multinom(work ~ perplexity + wc, data = d1)

plot(compare_performance(fit0,fit1,fit2,fit3), metrics = 'common')

# perplexity, normalised
fit4 = multinom(work ~ perplexity + wc + type_token_ratio, data = d2)
fit5 = multinom(work ~ wc + type_token_ratio, data = d2)
fit6 = multinom(work ~ perplexity + type_token_ratio, data = d2)
fit7 = multinom(work ~ perplexity + wc, data = d2)

plot(compare_performance(fit4,fit5,fit6,fit7), metrics = 'common')
