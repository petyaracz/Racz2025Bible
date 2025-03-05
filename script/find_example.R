# -- head -- #

library(tidyverse)
library(glue)

setwd('~/Github/Racz2025Bible/')

# -- fun -- #

# take metric, cross all translations but match book and chapter, so you compare the same book and chapter across translations, calculate absolute difference of metric 1 - metric 2, bin it into ten bins (we want an approximate result), return df
compute_differences = function(d, my_metric = c("perplexity", "complexity", "wc")) {
  # Ensure the metric argument is one of the supported values:
  my_metric = match.arg(my_metric)
  
  # Convert the chosen metric to a symbol for tidy evaluation
  var = sym(my_metric)
  
  # Select relevant columns and keep only the chosen metric
  data_0 = d |>
    select(work, book, chapter, !!var)
  
  # Rename columns for first copy
  data_1 = data_0 |>
    rename(
      work_1    = work,
      book_1    = book,
      chapter_1 = chapter,
      metric_1 := !!var
    )
  
  # Rename columns for second copy
  data_2 = data_0 |>
    rename(
      work_2    = work,
      book_2    = book,
      chapter_2 = chapter,
      metric_2 := !!var
    )
  
  # Cross-join and compute the differences
  results = crossing(data_1, data_2) |>
    filter(
      chapter_1 == chapter_2,
      book_1 == book_2,
      work_1 != work_2
    ) |>
    mutate(
      metric_diff = metric_1 / metric_2
    ) |>
    select(work_1, book_1, chapter_1, work_2, metric_diff) |> 
    rename(
      'book' = book_1,
      'chapter' = chapter_1,
      !!glue('{my_metric}_diff') := metric_diff
    )
  
  return(results)
}


# use the comparison set to extract text from g
extract_comparison = function(g, d, my_comparison_set){
  filter_set = my_comparison_set |> 
    select(work_1,work_2,book,chapter) |> 
    pivot_longer(-c(book,chapter), names_to = 'comparison', values_to = 'work') |> 
    select(-comparison)
  
  g |> 
    select(work,book,chapter,verse,text) |> 
    inner_join(filter_set) |> 
    pivot_wider(names_from = work, values_from = text, values_fill = '')
  
}

# check difference um after the fact
check_set = function(d,my_comparison_set){
  keep_set = my_comparison_set |> 
    distinct(book,chapter)
  
  d |> 
    filter(
      work %in% names(perplexity_comparison_set)[4:5]
    ) |> 
    inner_join(keep_set) |> 
    select(work,book,chapter,perplexity,complexity,wc)
  
}

compute_differences_on_d2 = partial(compute_differences, d = d2)
extract_comparison_on_g = partial(extract_comparison, g = g, d = d2)
check_set_d2 = partial(check_set, d2)

# -- read -- #

d = read_tsv('dat/gospel_entropy.tsv')
g = read_tsv('dat/gospels.gz')

# -- filter -- #

d2 = d |> 
  filter(type == 'normalised')

g = g |> 
  filter(type == 'normalised')

# --  differences -- #

differences = map(
  c('perplexity','complexity','wc'),
  compute_differences_on_d2
)

# -- combine -- #

comparison_set = reduce(
  .x = differences,
  .f = left_join
) |> 
  filter(wc_diff > .95, wc_diff < 1.05)

# -- find nice comparison -- #

perplexity_comparison_set = comparison_set |> 
  arrange(perplexity_diff) |> 
  slice(4) |> 
  extract_comparison_on_g()

check_set_d2(perplexity_comparison_set)

# oh well

perplexity_comparison_set |> 
  googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/1o1xmB6jjdZNdV4ButewH_Xy_SQqgyV_SOqmGeZlaztw/edit?usp=sharing', 'példa')
