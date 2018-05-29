#!/usr/bin/Rscript

library(ShoeScrapeR)
library(tidyverse)
library(stringr)
library(odbc)

if (system2("hostname", stdout=T) == "bigfoot") {
  setwd("/home/srvander/Rprojects/CSAFE/ShoeScrapeR/inst/")
} else {
  setwd("/storage/Rprojects/ShoeScrapeR/inst/")
}

# Create a data frame of all combinations of parameters type and population

fcn_opts <- expand.grid(type = c("new", "best", "relevance", "rating"),
                        population = c("all", "women", "men", "boys", "girls"),
                        query = c("", "boot", "sneakers"),
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


chunkfiles <- list.files("processed/toslice/", full.names = F)
chunkfiles <- sort(chunkfiles)
chunk_df <- data_frame(
  image = str_replace(chunkfiles, ".png", ""),
  crop = str_extract(chunkfiles, "\\d{3}x\\d{3}"),
  flip = str_detect(chunkfiles, "_flip") %>% as.numeric(),
  edge = str_detect(chunkfiles, "_edge") %>% as.numeric()
) %>%
  filter(!is.na(crop)) %>%
  mutate(image = str_replace(image, pattern = '(_flip)?(_edge)?_crop\\d{3}x\\d{3}', replacement = ''))

# Write image list to database
con <- dbConnect(odbc::odbc(), "shoefeatures-connector")
chunkfilesintable <- dbReadTable(con, "files")
towrite <- anti_join(chunk_df, chunkfilesintable)
dbWriteTable(con, "files", towrite, append = T)
dbDisconnect(con)
