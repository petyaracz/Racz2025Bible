# -- head -- #

library(tidyverse)
library(broom)
library(rstanarm)
library(performance)
library(sjPlot)
library(bayesplot)
library(tidybayes)
library(ggthemes)

setwd('~/Github/Racz2025Bible/')

# -- read -- #

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

# -- fit -- #

# perplexity, original
fit1 = stan_glm(perplexity ~ 1, data = d1, cores = 4)
fit2 = stan_glm(perplexity ~ book, data = d1, cores = 4)
fit3 = stan_glm(perplexity ~ work, data = d1, cores = 4)
fit4 = stan_glm(perplexity ~ work + book, data = d1, cores = 4)
fit5 = stan_glm(perplexity ~ work * book, data = d1, cores = 4)

loo1 = loo(fit1)
loo2 = loo(fit2)
loo3 = loo(fit3)
loo4 = loo(fit4)
loo5 = loo(fit5)

loo_compare(loo1,loo2,loo3,loo4,loo5) # 4

p1 = plot_model(fit4, 'pred', terms = 'work') +
  theme_bw() +
  xlab('') +
  ggtitle('predicted verse\nperplexity, original') +
  coord_flip()

# perplexity, normalised
fit6 = stan_glm(perplexity ~ 1, data = d2, cores = 4)
fit7 = stan_glm(perplexity ~ book, data = d2, cores = 4)
fit8 = stan_glm(perplexity ~ work, data = d2, cores = 4)
fit9 = stan_glm(perplexity ~ work + book, data = d2, cores = 4)
fit10 = stan_glm(perplexity ~ work * book, data = d2, cores = 4)

loo6 = loo(fit6)
loo7 = loo(fit7)
loo8 = loo(fit8)
loo9 = loo(fit9)
loo10 = loo(fit10)

loo_compare(loo6,loo7,loo8,loo9,loo10) # 9

p2 = plot_model(fit9, 'pred', terms = 'work') +
  theme_bw() +
  xlab('') +
  ggtitle('predicted verse\nperplexity, normalised') +
  coord_flip()

# word count, original

fit11 = stan_glm(wc ~ 1, data = d1, cores = 4, family = 'poisson')
fit12 = stan_glm(wc ~ book, data = d1, cores = 4, family = 'poisson')
fit13 = stan_glm(wc ~ work, data = d1, cores = 4, family = 'poisson')
fit14 = stan_glm(wc ~ work + book, data = d1, cores = 4, family = 'poisson')
fit15 = stan_glm(wc ~ work * book, data = d1, cores = 4, family = 'poisson')

loo11 = loo(fit11)
loo12 = loo(fit12)
loo13 = loo(fit13)
loo14 = loo(fit14)
loo15 = loo(fit15)

loo_compare(loo11,loo12,loo13,loo14,loo15) # 14 but pareto problems, should fit kfold

p3 = plot_model(fit14, 'pred', terms = 'work') +
  theme_bw() +
  xlab('') +
  ggtitle('predicted verse\n word count, original') +
  coord_flip()

# avg word length, original

fit16 = stan_glm(avg_word_length ~ 1, data = d1, cores = 4)
fit17 = stan_glm(avg_word_length ~ book, data = d1, cores = 4)
fit18 = stan_glm(avg_word_length ~ work, data = d1, cores = 4)
fit19 = stan_glm(avg_word_length ~ work + book, data = d1, cores = 4)
fit20 = stan_glm(avg_word_length ~ work * book, data = d1, cores = 4)

loo16 = loo(fit16)
loo17 = loo(fit17)
loo18 = loo(fit18)
loo19 = loo(fit19)
loo20 = loo(fit20)

loo_compare(loo16,loo17,loo18,loo19,loo20)

p4 = plot_model(fit20, 'pred', terms = c('work','book')) +
  theme_bw() +
  xlab('') +
  ggtitle('predicted verse\n average word length, original') +
  scale_fill_colorblind() +
  scale_colour_colorblind() +
  coord_flip()

# -- draw -- #

p1 + p2 + p3 + p4
ggsave('viz/predictions.png', dpi = 900, width = 10, height = 5)

# -------------- #
# plot function salad:
# Grab order
# works = d |> 
#   distinct(work,year) |> 
#   rename(variable = work)
# 
# # Figure out what the hell the names of the parameters are
# variable_names = colnames(as.matrix(fit4))
# 
# # Filter the variable names to get only those related to `work`
# work_variables = variable_names[str_detect(variable_names, "^work")]
# 
# # Extract the draws for the `work` variables using spread_draws
# draws_work = fit4 |> 
#   gather_draws(!!!rlang::syms(work_variables)) |> 
#   mutate(
#     value = .value,
#     variable = str_replace(.variable, 'work', '')
#   ) |> 
#   ungroup() |> # nem hiszem el tenyleg
#   left_join(works) |> 
#   select(variable,year,value) |> 
#   mutate(variable = fct_reorder(variable, -year))
# 
# # Plot the effect of `work`# Plot the effeyearct of `work`
# ggplot(draws_work, aes(x = value, y = variable)) +
#   stat_halfeye() +
#   theme_bw() +
#   geom_vline(xintercept = 0, lty = 3) +
#   labs(title = "Bibliai versek zavarodottságfüggvénye\na fordítások között,\n2014-hez képest (normalizált szövegek)",
#        y = "",
#        x = "Sűrűség")
# 
