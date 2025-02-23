library(tidyverse)

d = read_tsv('~/Github/parallelbible/KaldiNeo_m.tsv', col_names = F)

l = read_lines('~/Github/parallelbible/KaldiNeo_m.tsv')

sl = map(l, ~ str_split(.x, "\t")[[1]])

d2 = tibble(sl) |> 
  mutate(
    X1 = sapply(sl, `[`, 1),
    X2 = sapply(sl, `[`, 2),
    X3 = sapply(sl, `[`, 3),
    X4 = sapply(sl, `[`, 4)
  ) |> 
  select(-sl)

write_tsv(d2, '~/Github/parallelbible/KaldiNeo_m.tsv', col_names = F)

d3 = read_tsv('~/Github/parallelbible/KaldiNeo_m.tsv', col_names = F)
