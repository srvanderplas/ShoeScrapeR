#!/usr/bin/Rscript

library(ShoeScrapeR)
library(tidyverse)

setwd("/home/srvander/Rprojects/ShoeScrapeR/inst")

# Create a data frame of all combinations of parameters type and population

fcn_opts <- expand.grid(type = c("new", "best", "relevance", "rating"),
                        population = c("all", "women", "men", "boys", "girls"),
                        stringsAsFactors = F) %>%
  as_data_frame() %>%
  mutate(path = "./photos/")

shoe_res <- fcn_opts %>%
  group_by_all() %>%
  pmap_df(scrape_soles) %>%
  unique()

