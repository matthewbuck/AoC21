#Base R
library('magrittr')

input <- read.delim('c:/users/mbbuc/documents/aoc21/input.txt', header = FALSE) %>%
  unlist()

answer <- 0

for(diff in diff(input)) {
  if(diff > 0) {
    answer <- answer + 1
  }
}

print(answer)

### Part 2

sums <- c()

for(turn in (seq_len(length(input) - 2))) {
  curr_sum <- sum(input[[turn]], input[[turn+1]], input[[turn+2]])
  print(curr_sum)
  sums <- append(sums, curr_sum)
}

print(sums)

answer <- 0

for(diff in diff(sums)) {
  if(diff > 0) {
    answer <- answer + 1
  }
}

print(answer)


## Tidyverse style
library('tidyverse')

input <- read_delim('c:/users/mbbuc/documents/aoc21/input.txt', '/n',
                    col_names = 'depth') %>%
  mutate(diff = depth - lag(depth)) %>%
  mutate(increase = if_else(diff > 0, 1, 0))

answer <- sum(input$increase, na.rm = TRUE)

## Part 2

part2 <- input %>%
  select(depth) %>%
  mutate(window_sum = depth + lead(depth, n = 1) + lead(depth, n = 2)) %>%
  mutate(diff = window_sum - lag(window_sum)) %>%
  mutate(increase = if_else(diff > 0, 1, 0))

answer <- sum(part2$increase, na.rm = TRUE)

## Sonar plot just because

input %>%
  mutate(depth = - depth) %>%
  ggplot(aes(seq_along(depth),depth)) +
  geom_path() +
  theme_minimal()
