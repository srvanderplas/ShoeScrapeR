#!/usr/bin/Rscript

# Packages
# ------------------------------------------------------------------------------
library(furrr)
library(tidyverse)
library(magrittr)
library(rvest)
library(ShoeScrapeR)
library(RSQLite)
# ------------------------------------------------------------------------------

setwd("~/Projects/Work/2020-ShoeScrapeR/")
source(".Rprofile") # Just in case it doesn't source when running via cron

# Parameters
# ------------------------------------------------------------------------------
db_location <- "extra/Scraped_Data.sqlite"
image_save <- "extra/all_photos"
# ------------------------------------------------------------------------------

# Functions
# ------------------------------------------------------------------------------
# Write new rows to pre-existing table (anti-join); if table doesn't exist,
# write all rows in data frame; if purge is true overwrite all rows in data
# frame
dbWriteNewRows <- function(con, tablename, df, quiet = F, purge = F) {
  if (purge) {
    res <- dbWriteTable(con, tablename, df, overwrite = T)
    return(res)
  }

  if (tablename %in% dbListTables(con)) {
    initial_link_db <- dbReadTable(con, tablename)
    new_rows <- suppressMessages(anti_join(df, initial_link_db))
    if (nrow(new_rows) > 0 & !quiet) {
      message(sprintf("%d new rows added", nrow(new_rows)))
    }
    res <- dbWriteTable(con, tablename, new_rows, append = T)
    return(res)
  } else {
    if (!quiet) {
      message(sprintf("Creating %s table and initializing with new data.",
                      tablename))
    }
    res <- dbWriteTable(con, tablename, df)
    return(res)
  }

  warning("Something went wrong in dbWriteNewRows")
  return(NA)
}
# ------------------------------------------------------------------------------

# Set up database if it doesn't exist
# ------------------------------------------------------------------------------
shoe_db_con <- dbConnect(RSQLite::SQLite(), db_location)
# ------------------------------------------------------------------------------

# Get initial zappos search page results - one tile for each shoe model
# ------------------------------------------------------------------------------
plan(multicore)
get_all_shoes_on_page_safely <- safely(get_all_shoes_on_page)

initial_links_to_search <- get_useful_searches() %>%
  mutate(search_page = paste0("http://www.zappos.com", href)) %>%
  mutate(shoe_search = future_map(search_page, get_all_page_links))
initial_links_safe <- initial_links_to_search %>%
  unnest(shoe_search) %>%
  mutate(shoe_page_safe = map(shoe_search, get_all_shoes_on_page_safely)) %>%
  mutate(shoe_page = purrr::map(shoe_page_safe, "result"),
         shoe_page_err = purrr::map(shoe_page_safe, "error"))

initial_link_data <- unnest(initial_links_safe, cols = c(shoe_page)) %>%
  select(-shoe_page_safe, -shoe_page_err)

initial_link_db <- dbReadTable(shoe_db_con, "initial_link")
new_shoes <- suppressMessages(anti_join(initial_link_data, initial_link_db))

# Write initial results to database
dbWriteNewRows(shoe_db_con, "initial_link", new_shoes)

plan(sequential) # Reset multicore
# ------------------------------------------------------------------------------


if (nrow(new_shoes) > 0) {
  # Get shoe-level information from zappos using previously acquired links
  # ------------------------------------------------------------------------------
  safe_get_shoe <- safely(get_shoe_info)

  plan(multicore, workers = 24)
  urls <- new_shoes$url # work with smallest possible data to minimize memory use

  shoe_info <- future_map(paste0("http://www.zappos.com", urls),
                          safe_get_shoe,
                          .progress = TRUE)

  # ------------------------------------------------------------------------------

  # Assemble shoe info and errors
  # ------------------------------------------------------------------------------
  shoe_data <- purrr::map(shoe_info, "result") %>%
    bind_rows(.id = "url") %>%
    mutate(url = urls[as.numeric(url)]) %>%
    rename(ratingCount = `reviewCount ratingCount`)

  shoe_error_list <- purrr::map(shoe_info, "error")
  idxs <- purrr::map_lgl(shoe_error_list, ~!is.null(.)) %>% which()

  shoe_errors <- tibble(url = urls[idxs],
                        message = purrr::map_chr(shoe_error_list[idxs], "message"),
                        call = purrr::map(shoe_error_list[idxs], "call") %>%
                          as.character())
  rm(shoe_error_list, idxs)
  # ------------------------------------------------------------------------------

  # Split shoe info into separate tables
  # ------------------------------------------------------------------------------
  shoe_data_info <- shoe_data %>%
    select(url, color, image, brand, sku, ratingValue, ratingCount) %>%
    unnest(brand) %>%
    select(-logo, -brand_link) %>% # Remove brand logo, brand link because that
    # should be in a separate table
    unique() %>%
    mutate(product_num = str_extract(url, "product/\\d{1,}/") %>%
             str_remove_all("\\D"),
           color_num = str_extract(url, "color/\\d{1,}/") %>%
             str_remove_all("\\D"))

  categories <- shoe_data %>%
    select(url, category) %>%
    unnest(category) %>%
    unique()

  sizes <- shoe_data %>%
    select(url, sizes) %>%
    unnest(sizes) %>%
    unique()

  colors <- shoe_data %>%
    select(url, colors) %>%
    mutate(colors = purrr::map(colors, mutate_all, as.character)) %>%
    unnest(colors) %>%
    unique()

  widths <- shoe_data %>%
    select(url, widths) %>%
    unnest(widths) %>%
    unique()

  brands <- shoe_data %>%
    select(brand) %>%
    unnest(brand) %>%
    unique()

  description <- shoe_data %>%
    select(url, description) %>%
    mutate(
      description = purrr::map(
        description,
        ~tibble(row = 1:length(.), text = as.character(.))
      )
    ) %>%
    unnest(description) %>%
    unique()


  # Record whether sub-tables actually succeed
  shoe_data_tables <- tibble(
    tablename = c("shoes", "categories", "sizes", "colors",
                  "widths", "brand_info", "description"),
    data = list(shoe_data_info, categories, sizes, colors,
                widths, brands, description),
    success = purrr::map2_lgl(tablename, data, dbWriteNewRows,
                              con = shoe_db_con, purge = T))

  rm(categories, sizes, colors, widths, brands, description, shoe_data_info)

  # ------------------------------------------------------------------------------

  # Download images
  # ------------------------------------------------------------------------------

  # Create directory for files (if it doesn't exist)
  if (!dir.exists(image_save)) {
    dir.create(image_save)
  }

  safe_dl_image <- safely(download_image)

  # Set up data frame with proper name
  image_files <- shoe_data %>%
    select(url, images) %>%
    unnest(images) %>%
    rename(view = key, image_link = link) %>%
    mutate(view = str_remove(view, " View") %>% str_trim) %>%
    arrange(url) %>%
    mutate(
      ext = str_extract(image_link, ".[A-z]{1,}$"),
      filename = str_replace_all(url, c("/p/" = "", "/" = "_")) %>%
        paste0(., "_", view, ext)) %>%
    select(url, view, img_url = image_link, filename = filename) %>%
    mutate(filename = file.path(image_save, filename))

  image_res <- image_files %>%
    mutate(dl = !file.exists(filename)) %>%
    filter(dl) %>%
    select(url = img_url, filename)

  if (nrow(image_res) > 0) {
    image_res <- image_res %>%
    mutate(
      success = purrr::pmap(., safe_dl_image),
      result = purrr::map_int(success, ~ifelse(is.null(.$result), NA, .$result)),
      error = purrr::map_chr(success, ~ifelse(is.null(.$error), NA, .$error[[1]])))
  }

  image_files <- image_files %>%
    mutate(success = file.exists(filename))

  sum(!image_files$success)

  dbWriteNewRows(shoe_db_con, "image_location", image_files)
  # ------------------------------------------------------------------------------

  # Sync data to various directories
  # ------------------------------------------------------------------------------
  warning("Sync stage reached")

  updated_imgs <- image_files %>%
    filter(view == "Bottom") %>%
    mutate(filename2 = filename %>%
             str_replace("all_photos", "fixed-photos") %>%
             str_remove("_Bottom")) %>%
    filter(!file.exists(filename2)) %>%
    mutate(copy = purrr::map2_lgl(filename, filename2, file.copy))

  # try(dir.create("extra/fixed-photos"))
  # try(file.remove(list.files("extra/fixed-photos/", "*.*", full.names = T)))
  # try(file.copy(updated_imgs$filename2, "extra/fixed-photos"))

  # Only modify new images
  try(system("~/bin/autocrop-imgs extra/fixed-photos/"))
  try(system("~/bin/imgs-to-jpg extra/fixed-photos/"))


  # Copy files to LabelMe Directory
  # try(system("rsync -rzu --no-perms --no-owner --no-group extra/new-photos/ ~/Projects/CSAFE/LabelMe/Images/Shoes/"))
  # Back up bottom images to LSS
  # try(system("rsync -rzu --no-perms --no-owner --no-group extra/photos/ /lss/research/csafe-shoeprints/ShoeNeuralNet/ShoeSoles/"))
  # Backup all images to LSS
  # try(system("rsync -rzu --no-perms --no-owner --no-group extra/all_photos/ /lss/research/csafe-shoeprints/ShoeNeuralNet/ShoeImages/"))

  # try(system("rsync -rzu --no-perms --no-owner --no-group extra/Scraped_Data.sqlite /lss/research/csafe-shoeprints/ShoeNeuralNet/"))


  # Copy to Textures as well
  # try(system("rsync -rzu --no-perms --no-owner --no-group extra/photos/ ~/Projects/CSAFE/LabelMe/Images/Textures/"))


  # Make image manifest
  system("find extra/fixed-photos/ -type f -name '*.jpg' > image_manifest")
  # flist_shoes <- list.files("~/Projects/CSAFE/LabelMe/Images/Shoes", "\\.jpg$")
  # flist_textures <- list.files("~/Projects/CSAFE/LabelMe/Images/Textures/", "\\.jpg$")
  # im_manifest <- rbind(
    # data.frame(collection = "Shoes", file = flist_shoes),
    # data.frame(collection = "Textures", file = flist_textures)
  # )
  # write.table(im_manifest, sep = ",",
  #             "~/Projects/CSAFE/LabelMe/DirLists/labelme.txt",
  #             row.names = F, col.names = F, quote = F)
  #


  # ------------------------------------------------------------------------------
}

# Close db connection
# ------------------------------------------------------------------------------
dbDisconnect(shoe_db_con)
# ------------------------------------------------------------------------------

# set in Rprofile, ping healthchecks.io to update
if (exists("ping_url")) {
  httr::GET(ping_url)
}


# git housekeeping
# ------------------------------------------------------------------------------
# git2r::add(path = db_location)
# git2r::add(path = "./image_manifest")
git2r::add(path = "inst/cron.log")
git2r::commit(all = T, message = "Automatic Update")
system("git pull")
system("git push")
# ------------------------------------------------------------------------------
