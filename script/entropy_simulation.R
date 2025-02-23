library(tidyverse)

tryEntropy = function(n_vocabulary,n){
  vocabulary = letters[1:n_vocabulary]
  generated_string = paste(sample(vocabulary, n, replace = TRUE), collapse = " ")
  generated_vector = strsplit(generated_string, ' ')[[1]]
  generated_table = table(generated_vector)
  generated_p = generated_table / n
  entropy::entropy(generated_p)
}

tryEntropyn13 = partial(tryEntropy, n = 13)
tryEntropynv13 = partial(tryEntropy, n_vocabulary = 13)

sim1 = tibble(
  n_vocabulary = 1:26
) |> 
  rowwise() |> 
  mutate(
    entropy = tryEntropyn13(n_vocabulary)
  )

sim1 |> 
  ggplot(aes(n_vocabulary,entropy)) +
  geom_point()

sim2 = tibble(
  n = 1:26
) |> 
  rowwise() |> 
  mutate(
    entropy = tryEntropynv13(n)
  )

sim2 |> 
  ggplot(aes(n,entropy)) +
  geom_point()

sim3 = crossing(
  n = 1:26,
  n_vocabulary = 1:26
) |>
  rowwise() |> 
  mutate(
    entropy = tryEntropy(n_vocabulary = n_vocabulary, n = n)
  )

sim3 |> 
  ggplot(aes(n_vocabulary,entropy,colour = n)) +
  geom_point() +
  scale_colour_viridis_c()

sim3 |> 
  ggplot(aes(n_vocabulary,entropy,colour = as.factor(n))) +
  geom_smooth() +
  guides(colour = 'none') +
  scale_colour_viridis_d() +
  theme_minimal()

sim3 |> 
  ggplot(aes(n,entropy,colour = as.factor(n_vocabulary))) +
  geom_smooth() +
  guides(colour = 'none') +
  scale_colour_viridis_d() +
  theme_minimal()
