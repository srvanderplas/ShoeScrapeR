#!/usr/bin/Rscript

library(ShoeScrapeR)
library(tidyverse)
library(stringr)
library(odbc)
library(DBI)

library(RSelenium)
library(docker)

# Deal with docker
# try(system('docker run -d -p 4445:4444 -v /dev/shm:/dev/shm selenium/standalone-firefox'))
try(system('docker run -p 4443:4444 -v /dev/shm:/dev/shm selenium/standalone-chrome:3.12.0'))
# try(system('docker restart -p 4445:4444 -v /dev/shm:/dev/shm selenium/standalone-firefox'))
# remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
# remDr$open()

# Create a data frame of all combinations of parameters type and population

fcn_opts <- expand.grid(type = c("new", "best", "relevance", "rating"),
                        population = c("all", "women", "men"),
                        query = c("", "boot", "sneakers"),
                        stringsAsFactors = F) %>%
  as_tibble() %>%
  mutate(path = "/home/srvander/Projects/CSAFE/ShoeScrapeR/extra/photos/")

try_scrape_soles <- function(...) {
  try(scrape_soles(...))
}

shoe_res <- fcn_opts %>%
  group_by_all() %>%
  pmap_dfr(try_scrape_soles) %>%
  unique()

# system("docker stop $(docker ps -a -q | grep chrome)")
# system("docker rm $(docker ps -a -q)")

system("rsync -avzu /home/srvander/Projects/CSAFE/ShoeScrapeR/extra/photos/ /home/srvander/Projects/CSAFE/LabelMe/Images/Shoes/")

try(system("rsync -avzu --no-perms --no-owner --no-group /home/srvander/Projects/CSAFE/ShoeScrapeR/extra/photos/ /myfiles/las/research/csafe/ShoeNeuralNet/ShoeImages/"))
#system("git add photos/*")
system("find /home/srvander/Projects/CSAFE/ShoeScrapeR/extra/photos/ -type f -name '*.jpg' > image_manifest")
system("git add image_manifest /home/srvander/Projects/CSAFE/ShoeScrapeR/inst/cron.log")
system("git commit -a -m 'Automatic Update'")
system("git pull")
system("git push")

flist <- list.files("/home/srvander/Projects/CSAFE/LabelMe/Images/Shoes", "\\.jpg$")

write.table(data.frame(collection = "Shoes", file = flist), sep = ",",
            "/home/srvander/Projects/CSAFE/LabelMe/DirLists/labelme.txt", 
            row.names = F, col.names = F, quote = F)

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
