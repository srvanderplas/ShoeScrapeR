#!/usr/bin/Rscript

library(ShoeScrapeR)
library(purrr)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(RSelenium)

# Deal with docker
# try(system('docker run -d -p 4445:4444 -v /dev/shm:/dev/shm selenium/standalone-firefox'))
try(system('docker run -p 4443:4444 -v /dev/shm:/dev/shm selenium/standalone-chrome:3.12.0'))
# try(system('docker restart -p 4445:4444 -v /dev/shm:/dev/shm selenium/standalone-firefox'))

# Create a data frame of all combinations of parameters type and population
fcn_opts <- expand.grid(type = c("new", "best", "rating"),
                        population = c("all", "women", "men"),
                        query = c("shoes", "boot", "sneakers"),
                        stringsAsFactors = F) %>%
  as_tibble()

try_scrape_soles <- function(...) {
  try(scrape_soles(...))
}

shoe_res <- fcn_opts %>%
  mutate(newlinks = pmap(., try_scrape_soles, path = "/home/srvander/Projects/CSAFE/ShoeScrapeR/extra/photos/", top_pages = 15)) %>%
  unnest()

# Clean duplicates
full_shoe_res <- shoe_res %>%
  group_by(newlinks) %>% 
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(bottom_img = map(newlinks, get_bottom_image))

full_shoe_res <- full_shoe_res %>%
  unnest() %>%
  filter(!is.na(url))

shoe_specific <- full_shoe_res %>%
  filter(str_detect(img_type, "Shoes")) %>%
  mutate(
    dl = purrr::map2_int(url, filename, ~try(download_image(.x, .y)))
  )


warning("Sync stage reached")

system("rsync -avzu /home/srvander/Projects/CSAFE/ShoeScrapeR/extra/photos/ /home/srvander/Projects/CSAFE/LabelMe/Images/Shoes/")

try(system("rsync -avzu --no-perms --no-owner --no-group /home/srvander/Projects/CSAFE/ShoeScrapeR/extra/photos/ /myfiles/las/research/csafe/ShoeNeuralNet/ShoeImages/"))
system("find /home/srvander/Projects/CSAFE/ShoeScrapeR/extra/photos/ -type f -name '*.jpg' > image_manifest")
system("git add image_manifest /home/srvander/Projects/CSAFE/ShoeScrapeR/inst/cron.log")
system("git commit -a -m 'Automatic Update'")
system("git pull")
system("git push")

flist <- list.files("/home/srvander/Projects/CSAFE/LabelMe/Images/Shoes", "\\.jpg$")

write.table(data.frame(collection = "Shoes", file = flist), sep = ",",
            "/home/srvander/Projects/CSAFE/LabelMe/DirLists/labelme.txt", 
            row.names = F, col.names = F, quote = F)
