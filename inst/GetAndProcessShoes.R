#!/usr/bin/Rscript

library(ShoeScrapeR)
library(tidyverse)
library(stringr)

setwd("/home/srvander/Rprojects/CSAFE/ShoeScrapeR/inst/")

# Create a data frame of all combinations of parameters type and population

fcn_opts <- expand.grid(type = c("new", "best", "relevance", "rating"),
                        population = c("all", "women", "men", "boys", "girls"),
                        stringsAsFactors = F) %>%
  as_data_frame() %>%
  mutate(path = "photos/")

shoe_res <- fcn_opts %>%
  group_by_all() %>%
  pmap_df(scrape_soles) %>%
  unique()

system("git add photos/*")
system("git commit -a -m 'Automatic Update'")
system("git push")


# Process image with bash script
system("./ParallelProcess.sh")