library(tidyverse)

d = read_tsv('~/Downloads/SzIT_m.tsv', col_names = F)

# p = problems(d)
# 
# dp = d |> 
#   slice(p$row)
# 
# paste(unique(dp$X1), collapse = '|')
# 
# unique(d$X3)

# in a text editor:
# 1.
# sub " (?=(Ter|Bir|1Kir|2Kron|Neh|Eszt|Job|Zsolt|Sir|Iz|Jer|Ez|Mik|Zak)\t)", "\n"
# 2.
# sub "(?<=\d[abcdefgh]) ", "\t"
# 3.
# sub "(?<=\d),(?=\d)", "\t"
# 4.
# sub "(?<=\dab) ", "\t"


book_order = d |> 
  distinct(X1) |> 
  mutate(book_n = 1:n())

d2 = d |> 
  left_join(book_order) |> 
  mutate(
    X2 = as.double(X2),
    X3a = str_extract(X3, '[0-9]+') |> as.double(),
    X3b = str_extract(X3, '([abcdef]|ab)')
  ) |> 
  group_by(
    X1,X2,X3a
  ) |> 
  arrange(book_n,X2,X3a,X3b) |> 
  ungroup() |> 
  select(X1,X2,X3,X4)

write_tsv(d2, '~/Github/parallelbible/SzIT_m.tsv')
