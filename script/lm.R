# -- head -- #

library(mgcv)
library(tidyverse)
library(lme4)
library(performance)
library(sjPlot)


setwd('~/Github/Racz2025Bible/')

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
    work2 = fct_reorder(work2, -year),
    work = fct_reorder(work, -year),
    work3 = ordered(work)
  )

d2 = filter(d, type != 'facsimile')

# -- fit -- #

lm1 = lmer(bigram_perplexity ~ work3 + (1| book/chapter), data = d2)
lm2 = lmer(mdl_over_dl ~ work3 + (1| book/chapter), data = d2)
lm3 = lmer(type_token_ratio ~ work3 + (1| book/chapter), data = d2)
lm4 = lmer(unigram_perplexity ~ work3 + (1| book/chapter), data = d2)
lm5 = lmer(wc ~ work3 + (1| book/chapter), data = d2)

lm1b = lmer(bigram_perplexity ~ (1| book/chapter), data = d2)
lm2b = lmer(mdl_over_dl ~ (1| book/chapter), data = d2)
lm3b = lmer(type_token_ratio ~ (1| book/chapter), data = d2)
lm4b = lmer(unigram_perplexity ~ (1| book/chapter), data = d2)
lm5b = lmer(wc ~ (1| book/chapter), data = d2)

d2 = d2 |> 
  mutate(
    book_chapter = as.factor(glue('{book} {chapter}')),
    work_rank = as.double(work2)
  )

d2 |> 
  distinct(work_rank,work)

ord1 = gam(work_rank ~ 
             s(bigram_perplexity, k = 3) + 
             s(mdl_over_dl, k = 3) + 
             # type_token_ratio + 
             s(unigram_perplexity, k = 3) + 
             # wc + 
             s(book_chapter, bs = 're'), 
             data = d2,
             family = ocat(R=11),
             method = 'ML'
          )

# -- check -- #

plot(compare_performance(lm1,lm1b))
plot(compare_performance(lm2,lm2b))
plot(compare_performance(lm3,lm3b))
plot(compare_performance(lm4,lm4b))
plot(compare_performance(lm5,lm5b))

test_likelihoodratio(lm1b,lm1)
test_likelihoodratio(lm2b,lm2)
test_likelihoodratio(lm3b,lm3)
test_likelihoodratio(lm4b,lm4)
test_likelihoodratio(lm5b,lm5)

plot_model(lm1, 'pred', terms = 'work3')
plot_model(lm2, 'pred', terms = 'work3')
plot_model(lm3, 'pred', terms = 'work3')
plot_model(lm4, 'pred', terms = 'work3')
plot_model(lm5, 'pred', terms = 'work3')

check_collinearity(ord1)
plot_model(ord1, 'pred', terms = 'bigram_perplexity')
plot_model(ord1, 'pred', terms = 'unigram_perplexity')
plot_model(ord1, 'pred', terms = 'mdl_over_dl')

# -- write -- #

saveRDS(lm1, 'models/lm1.rds')
saveRDS(lm2, 'models/lm2.rds')
saveRDS(lm3, 'models/lm3.rds')
saveRDS(lm4, 'models/lm4.rds')
saveRDS(lm5, 'models/lm5.rds')
saveRDS(lm1b, 'models/lm1b.rds')
saveRDS(lm2b, 'models/lm2b.rds')
saveRDS(lm3b, 'models/lm3b.rds')
saveRDS(lm4b, 'models/lm4b.rds')
saveRDS(lm5b, 'models/lm5b.rds')
