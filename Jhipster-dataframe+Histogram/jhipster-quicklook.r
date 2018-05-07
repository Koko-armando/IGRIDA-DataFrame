# R script
# 
# author: Xavier Devroey

require('dplyr')



df <- read.csv('jhipster.csv', sep = ';', dec = '.', header = TRUE, quote = '"', stringsAsFactors = FALSE)
	
df %>%
  group_by(Build) %>%
  summarise(count = n())
