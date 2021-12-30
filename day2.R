#Tidyverse style

library('tidyverse')

input <- read_delim('path/to/input', delim = '/n',
                    col_names = 'position') %>%
  transmute(directions = str_split(position, ' ')) %>%
  mutate(directions = map(directions, ~ set_names(.x, 'direction', 'unit'))) %>%
  unnest_wider(directions) %>%
  mutate(unit = as.integer(unit)) %>%
  pivot_wider(names_from = direction, values_from = unit, values_fn = list) %>%
  mutate(across(everything(), ~ sum(unlist(.x))))

position <- c(input$forward, input$down + input$up)

print(position[[1]] * position[[2]])

                    