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
      str_replace(' Keletkezési idő:', '') |> 
      fct_reorder(-year)
  )

# -- fit -- #

# lm
fit1 = stan_glm(perplexity_normalised ~ 1, data = d, cores = 4)
fit2 = stan_glm(perplexity_normalised ~ book, data = d, cores = 4)
fit3 = stan_glm(perplexity_normalised ~ work, data = d, cores = 4)
fit4 = stan_glm(perplexity_normalised ~ work + book, data = d, cores = 4)
fit5 = stan_glm(perplexity_normalised ~ work * book, data = d, cores = 4)

loo1 = loo(fit1)
loo2 = loo(fit2)
loo3 = loo(fit3)
loo4 = loo(fit4)
loo5 = loo(fit5)

loo_compare(loo1,loo2,loo3,loo4,loo5)

plot_model(fit4, 'pred', terms = 'work') +
  theme_bw() +
  coord_flip()

# Grab order
works = d |> 
  distinct(work,year) |> 
  rename(variable = work)

# Figure out what the hell the names of the parameters are
variable_names = colnames(as.matrix(fit4))

# Filter the variable names to get only those related to `work`
work_variables = variable_names[str_detect(variable_names, "^work")]

# Extract the draws for the `work` variables using spread_draws
draws_work = fit4 |> 
  gather_draws(!!!rlang::syms(work_variables)) |> 
  mutate(
    value = .value,
    variable = str_replace(.variable, 'work', '')
  ) |> 
  ungroup() |> # nem hiszem el tenyleg
  left_join(works) |> 
  select(variable,year,value) |> 
  mutate(variable = fct_reorder(variable, -year))

# Plot the effect of `work`# Plot the effeyearct of `work`
ggplot(draws_work, aes(x = value, y = variable)) +
  stat_halfeye() +
  theme_bw() +
  geom_vline(xintercept = 0, lty = 3) +
  labs(title = "Bibliai versek zavarodottságfüggvénye\na fordítások között,\n2014-hez képest (normalizált szövegek)",
       y = "",
       x = "Sűrűség")

ggsave('viz/perplexity_model.png', width = 6, height = 6, dpi = 900)

fit6 = stan_glm(perplexity_original ~ 1, data = d, cores = 4)
fit7 = stan_glm(perplexity_original ~ book, data = d, cores = 4)
fit8 = stan_glm(perplexity_original ~ work, data = d, cores = 4)
fit9 = stan_glm(perplexity_original ~ work + book, data = d, cores = 4)
fit10 = stan_glm(perplexity_original ~ work * book, data = d, cores = 4)

loo6 = loo(fit6)
loo7 = loo(fit7)
loo8 = loo(fit8)
loo9 = loo(fit9)
loo10 = loo(fit10)

loo_compare(loo6,loo7,loo8,loo9,loo10)

# Figure out what the hell the names of the parameters are
variable_names = colnames(as.matrix(fit9))

# Filter the variable names to get only those related to `work`
work_variables = variable_names[str_detect(variable_names, "^work")]

# Extract the draws for the `work` variables using spread_draws
draws_work = fit9 |> 
  gather_draws(!!!rlang::syms(work_variables)) |> 
  mutate(
    value = .value,
    variable = str_replace(.variable, 'work', '')
  ) |> 
  ungroup() |> # nem hiszem el tenyleg
  left_join(works) |> 
  select(variable,year,value) |> 
  mutate(variable = fct_reorder(variable, -year))

# Plot the effect of `work`# Plot the effeyearct of `work`
ggplot(draws_work, aes(x = value, y = variable)) +
  stat_halfeye() +
  theme_bw() +
  geom_vline(xintercept = 0, lty = 3) +
  labs(title = "Bibliai versek zavarodottságfüggvénye\na fordítások között,\n2014-hez képest (eredeti szövegek)",
       y = "",
       x = "Sűrűség")

ggsave('viz/perplexity_model_orig.png', width = 6, height = 6, dpi = 900)
