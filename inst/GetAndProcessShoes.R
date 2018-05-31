#!/usr/bin/Rscript

library(ShoeScrapeR)
library(tidyverse)
library(stringr)
library(odbc)
library(DBI)

if (system2("hostname", stdout=T) == "bigfoot") {
  setwd("/home/srvander/Rprojects/CSAFE/ShoeScrapeR/inst/")
} else {
  setwd("/storage/Rprojects/ShoeScrapeR/inst/")
}

# Create a data frame of all combinations of parameters type and population

fcn_opts <- expand.grid(type = c("new", "best", "relevance", "rating"),
                        population = c("all", "women", "men"),
                        query = c("", "boot", "sneakers"),
                        stringsAsFactors = F) %>%
  as_data_frame() %>%
  mutate(path = "photos/")

shoe_res <- fcn_opts %>%
  group_by_all() %>%
  pmap_dfr(scrape_soles) %>%
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

slicefiles <- list.files("processed/slices/", full.names = T) %>% sort()
slice_df <- data_frame(
  slice = basename(slicefiles),
  path = slicefiles,
  crop = str_extract(slice, "\\d{3}x\\d{3}"),
  flip = str_detect(slice, "_flip") %>% as.numeric(),
  edge = str_detect(slice, "_edge") %>% as.numeric(),
  image = str_replace(slice, "(_flip)?(_edge)?_crop\\d{3}x\\d{3}_sz\\d{2,}_\\d{3}.png", "")
) %>%
  mutate(slice = str_extract(slice, "\\d{3}.png") %>% str_replace(".png", "") %>% as.numeric) %>%
  select(image, slice, path, crop, flip, edge)


# Write image list to database
con <- dbConnect(odbc::odbc(), "shoefeatures-connector")
chunkfilesintable <- dbReadTable(con, "files")
towrite <- anti_join(chunk_df, chunkfilesintable)
dbWriteTable(con, "files", towrite, append = T)
slicefilesintable <- dbReadTable(con, "slices")
towrite <- anti_join(slice_df, slicefilesintable)
dbWriteTable(con, "slices", towrite, append = T)

# Ensure all files in database actually exist
todrop <- filter(slicefilesintable, !file.exists(path))
if (nrow(todrop) > 0) {
  ids <- paste(todrop$id, collapse = ", ")
  query <- sprintf("DELETE FROM slices WHERE id in (%s);", ids)
  qres <- dbSendStatement(con, query)
  dbClearResult(qres)
}
dbDisconnect(con)