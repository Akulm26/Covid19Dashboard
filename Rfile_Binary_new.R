library(tidyverse)
sample_file = read.csv("sample_19.csv",header = T)
sample_file
sample_file_price_mean = mean(sample_file$price)

sample_new = sample_file %>% mutate(price_binary = ifelse(price < sample_file_price_mean, 0, 1))
sample_new
