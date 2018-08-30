#!/usr/bin/Rscript

library(ShoeScrapeR)
library(tidyverse)
library(stringr)
library(odbc)
library(DBI)

# Deal with docker
available_containers <- system("docker ps --filter name='splash' -a -q", intern = T)
running_containers <- system("docker ps --filter name='splash' -q", intern = T)
stopped_containers <- available_containers[! available_containers %in% running_containers]
if (length(running_containers) == 0) {
  if (length(available_containers) > 0) {
    system(sprintf("docker start %s", available_containers[1]))
    available_containers <- system("docker ps --filter name='splash' -a -q", intern = T)
    stopped_containers <- available_containers[! available_containers %in% running_containers]
  } else {
    container <- splashr::start_splash()
    available_containers <- system("docker ps --filter name='splash' -a -q", intern = T)
    stopped_containers <- available_containers[! available_containers %in% running_containers]
  }
}

stopifnot(splashr::splash_active())

if (length(stopped_containers) > 1) {
  # Clean up stopped containers
  system(sprintf("docker rm %s", stopped_containers))
}

# Create a data frame of all combinations of parameters type and population

fcn_opts <- expand.grid(type = c("new", "best", "relevance", "rating"),
                        population = c("all", "women", "men"),
                        query = c("", "boot", "sneakers"),
                        stringsAsFactors = F) %>%
  as_data_frame() %>%
  mutate(path = here::here("extra/photos/"))

shoe_res <- fcn_opts %>%
  group_by_all() %>%
  pmap_dfr(scrape_soles) %>%
  unique()

# system("docker stop $(docker ps -a -q)")
# system("docker rm $(docker ps -a -q)")

system("rsync -avzu /home/srvander/Projects/CSAFE/ShoeScrapeR/extra/photos/ /home/srvander/Projects/CSAFE/LabelMe/Images/Shoes/")
#system("git add photos/*")
system("ls extra/photos/* > image_manifest")
system("git add image_manifest inst/cron.log")
system("git commit -a -m 'Automatic Update'")
system("git push")


# Process image with bash script
# system("../inst/ParallelProcess.sh")


# chunkfiles <- list.files("processed/toslice/", full.names = F)
# chunkfiles <- sort(chunkfiles)
# chunk_df <- data_frame(
#   image = str_replace(chunkfiles, ".png", ""),
#   crop = str_extract(chunkfiles, "\\d{3,}x\\d{3,}"),
#   flip = str_detect(chunkfiles, "_flip") %>% as.numeric(),
#   edge = str_detect(chunkfiles, "_edge") %>% as.numeric()
# ) %>%
#   filter(!is.na(crop)) %>%
#   mutate(image = str_replace(image, pattern = '(_flip)?(_edge)?_crop\\d{3,}x\\d{3,}', replacement = ''))
# 
# slicefiles <- list.files("processed/slices/", full.names = T) %>% sort()
# slice_df <- data_frame(
#   slice = basename(slicefiles),
#   path = slicefiles,
#   crop = str_extract(slice, "\\d{3,}x\\d{3,}"),
#   flip = str_detect(slice, "_flip") %>% as.numeric(),
#   edge = str_detect(slice, "_edge") %>% as.numeric(),
#   size = str_extract(slice, "_sz\\d{2,}") %>% str_replace("_sz", "") %>% as.numeric(),
#   image = str_replace(slice, "(_flip)?(_edge)?_crop\\d{3,}x\\d{3,}_sz\\d{2,}_\\d{3}.png", "")
# ) %>%
#   mutate(slice = str_extract(slice, "\\d{3}.png") %>% str_replace(".png", "") %>% as.numeric) %>%
#   select(image, slice, path, crop, flip, edge, size)
# 
# 
# # Write image list to database
# con <- dbConnect(odbc::odbc(), "shoefeatures-connector")
# if (exists("con")) {
#   chunkfilesintable <- dbReadTable(con, "files")
#   towrite <- anti_join(chunk_df, chunkfilesintable)
#   dbWriteTable(con, "files", towrite, append = T)
#   
#   slicefilesintable <- dbReadTable(con, "slices")
#   towrite <- anti_join(slice_df, slicefilesintable)
#   dbWriteTable(con, "slices", towrite, append = T)
#   
#   # Ensure all files in database actually exist
#   todropslices <- filter(slicefilesintable, !file.exists(path))
#   todropfiles <- filter(chunkfilesintable, !file.exists(file.path("processed/toslice/", paste0(image, ".png"))))
#   if (nrow(todropslices) > 0) {
#     # Drop all rows in tmp table and overwrite
#     dbWriteTable(con, "dropslices", todropslices, append = F, overwrite = T)
#     
#     qres <- dbSendStatement(con, "DELETE FROM slices WHERE path NOT IN (SELECT path FROM dropslices)")
#     dbClearResult(qres)
#   }
#   
#   if (nrow(todropfiles) > 0) {
#     # Drop all rows in tmp table and overwrite
#     dbWriteTable(con, "dropfiles", todropfiles, append = F, overwrite = T)
#     
#     qres <- dbSendStatement(con, "DELETE FROM files WHERE (path) NOT IN (SELECT path FROM dropfiles)")
#     dbClearResult(qres)
#   }
#   
#   dbDisconnect(con)
# } else {
#   stop("Couldn't connect to database")
# }
# 
