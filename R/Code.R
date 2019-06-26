#' luxury_zappos_imurl <- function(pagesrc) {
#'   n <- 5
#'   loaded <- FALSE
#'   while (n > 0 & !loaded) {
#'     if ("try-error" %in% class(pagesrc)) {
#'       return(FALSE)
#'     } else {
#'       photolinks <- pagesrc %>%
#'         rvest::html_nodes(css = "span.msa-image") %>%
#'         xml2::xml_parent()
#'     }
#'     
#'     loaded <- length(photolinks) > 4
#'     n <- n - 1
#'   }
#'   
#'   # Which one is bottom?
#'   # bottomphoto <- which(rvest::html_attr(photolinks, "data-angle") == "713YXP-FQmL")
#'   
#'   file_url <- photolinks %>%
#'     magrittr::extract(3) %>%
#'     xml2::xml_child() %>%
#'     rvest::html_attr("style") %>%
#'     stringr::str_replace("background-image:url\\((.*)\\)", "\\1") %>%
#'     # Get higher resolution
#'     stringr::str_replace("SR106,78", "SX1920")
#' }
#' 
#' regular_zappos_imurl <- function(pagesrc) {
#'   n <- 5
#'   loaded <- FALSE
#'   while (n > 0 & !loaded) {
#'     if ("try-error" %in% class(pagesrc)) {
#'       return(FALSE)
#'     } else {
#'       photolinks <- pagesrc %>%
#'         rvest::html_nodes(css = "img[alt='BOTT']")
#'     }
#'     
#'     loaded <- length(photolinks) > 4
#'     n <- n - 1
#'   }
#'   
#'   file_url <- photolinks %>%
#'     rvest::html_attr("src") %>%
#'     # Get higher resolution
#'     stringr::str_replace("SR106,78", "SX1920")
#' }
#' 
#' 
#' #' Get image of the sole from a url of a Zappo's shoe
#' #'
#' #' Pass in a url of the style https://www.zappos.com/p/...
#' #' This function requires you have a Selenium chrome installation on port 4443.
#' #' @param i url
#' #' @param path path to save files
#' #' @param sleep seconds to wait between image downloads
#' #' @return image URL
#' #' @export
#' get_bottom_image <- function(i, path = "~/Projects/CSAFE/ShoeScrapeR/extra/photos/", sleep = 1) {
#'   Sys.sleep(sleep)
#'   # i is the shoe page link
#'   remDr <- suppressMessages(RSelenium::remoteDriver(
#'     remoteServerAddr = "localhost", port = 4443L, browserName = "chrome"))
#'   rdo <- remDr$open(silent = T)
#'   on.exit(remDr$close())
#'   if (!dir.exists(path)) {
#'     dir.create(path, recursive =  T)
#'   }
#'   if (i == "") {
#'     return(tibble::tibble(url = NA, filename = NA))
#'   }
#' 
#'   photoname <- stringr::str_remove(i, "https://www.zappos.com/p/") %>%
#'     stringr::str_replace_all("/", "_") %>%
#'     unique()
#' 
#'   dlfile <- paste0(path, photoname, ".jpg")
#'   if (!file.exists(dlfile)) {
#'     # Sys.sleep(1)
#'     remDr$navigate(i) 
#'     
#'     redir_url <- remDr$getCurrentUrl()[[1]]
#'     zappos_division <- stringr::str_extract(redir_url, "\\w*\\.zappos\\.com") %>% 
#'       str_remove("\\.zappos\\.com")
#'     
#'     pagesrc <- try(remDr$getPageSource() %>% 
#'                      magrittr::extract2(1) %>% 
#'                      xml2::read_html())
#'     n <- 5
#'     
#'     # Attempt download again
#'     while (length(pagesrc) == 0 & n > 0) {
#'       remDr$navigate(i) 
#'       pagesrc <- try(remDr$getPageSource() %>% 
#'                        magrittr::extract2(1) %>% 
#'                        xml2::read_html())
#'       n <- n - 1
#'     }
#'     
#'     breadcrumbs <- pagesrc %>%
#'       rvest::xml_nodes(css = "#breadcrumbs a") %>%
#'       xml2::xml_text() %>%
#'       magrittr::extract(-1)
#'     
#'     if (length(breadcrumbs) > 2) {
#'       breadcrumbs <- rev(breadcrumbs) %>%
#'         magrittr::extract(-1)
#'     }
#'     
#'     breadcrumbs <- breadcrumbs %>% rev %>%
#'       paste(collapse = " > ")
#'     
#'     if (!"try-error" %in% class(pagesrc)) {
#'       if (zappos_division == "luxury") {
#'         file_url <- luxury_zappos_imurl(pagesrc)
#'       } else {
#'         file_url <- regular_zappos_imurl(pagesrc)
#'       }
#'     }
#'     
#'     
#'   } else {
#'     # message("File already exists")
#'     file_url <- NA
#'     breadcrumbs <- "AlreadyExists"
#'   }
#'   tibble(img_type = breadcrumbs[1], url = file_url, filename = dlfile)
#' }
#' 
#' 
#' #' Get image of the sole from a url of a Zappo's shoe
#' #'
#' #' Pass in a url of the style https://www.zappos.com/p/...
#' #' @param i url
#' #' @param filename location to save the file
#' #' @param crop should image be cropped using magick?
#' #' @param sleep seconds to wait between image downloads
#' #' @param quiet quiet download?
#' #' @return image URL
#' #' @export
#' download_image <- function(url, filename, crop = TRUE, sleep = 0, quiet = T) {
#'   file_dl <- 1
#'   tries <- 5
#'   while (!file_dl == 0 & tries > 0) {
#'     file_dl <- download.file(url, destfile = filename, quiet = quiet)
#'     if (sleep > 0) Sys.sleep(sleep)
#'     tries <- tries - 1
#'   }
#'   
#'   if (crop & file_dl) {
#'     cmd <- sprintf("convert %s -bordercolor white -border 1x1 -trim +repage -border 5x5 %s", dlfile, dlfile)
#'     system(cmd)
#'   }
#' 
#'   return(file_dl)
#' }
#' 
#' #' Function to scrape shoe sole data from Zappos.com
#' #'
#' #' @param type One of new, best, relevance, or rating. Defaults to relevance.
#' #' @param population One of all, women, men, boys, girls. Defaults to all.
#' #' @param pages Number of pages of links to acquire
#' #' @param top_pages Sample pages from the top_pages pages of results
#' #' @param path Path to save files
#' #' @param query search string
#' #' @param shoelist list of all jpg files in the provided path
#' #' @param max_shoes maximum number of shoes to download images from
#' #' @return list of links and whether or not the image of the sole was downloaded
#' #' @export
#' scrape_soles <- function(type = "rating", population = "all", pages = 3, top_pages = 15,
#'                          path = "~/Projects/CSAFE/ShoeScrapeR/extra/photos/", query = "", 
#'                          shoelist = list.files(path, pattern = "jpg$"),
#'                          max_shoes = 400) {
#'   # 
#'   # Sys.sleep(1)
#' 
#'   if (substr(path, nchar(path), nchar(path)) != "/") {
#'     path <- paste0(path, "/")
#'   }
#' 
#'   if (type == "new") {
#'     ext <- "?s=isNew/desc/goLiveDate/desc/recentSalesStyle/desc/"
#'   } else if (type == "best") {
#'     ext <- "?s=recentSalesStyle/desc/"
#'   } else if (type == "rating") {
#'     ext <- "?s=productRating/desc/"
#'   } else if (type == "relevance") {
#'     ext <- ""
#'   } else {
#'     ext <- ""
#'     warning("type not recognized. Proceeding with relevance as the sort term.\n")
#'   }
#' 
#'   if (population == "women") {
#'     pop <- "women-shoes"
#'   } else if (population == "men") {
#'     pop <- "men-shoes"
#'   } else if (population == "girls") {
#'     pop <- "girls-shoes"
#'   } else if (population == "boys") {
#'     pop <- "boys-shoes"
#'   } else if (population == "all") {
#'     pop <- "shoes"
#'   } else {
#'     warning("population type not recognized. Proceeding as if 'all' was selected.\n")
#'     pop <- "shoes"
#'   }
#' 
#'   stopifnot(is.numeric(pages))
#' 
#' 
#'   if (query == "") {
#'     url <- sprintf("https://www.zappos.com/%s/CK_XAeICAQE.zso%s", pop, ext)
#'   } else {
#'     slash <- paste0(str_replace_all(query, " ", "-"), "-", pop)
#'     qstring <- paste0("&t=", str_replace_all(query, " ", "+"))
#'     url <- sprintf("https://www.zappos.com/%s/.zso%s%s", slash, ext, qstring)
#'   }
#' 
#'   links <- ""
#'   
#'   try({
#'     pgsample <- sample(1:top_pages, pages, replace = (pages > top_pages))
#'     
#'     shoeLinks <- url %>% paste(c("", sprintf("&p=%d", pgsample)), sep = "")
#'     shoeLinkPages <- purrr::map(shoeLinks, xml2::read_html)
#'     
#'     links <- purrr::map(shoeLinkPages, rvest::html_nodes, css = "article a") %>%
#'       unlist(recursive = F) %>%
#'       purrr::map(rvest::html_attr, name = "href", default = NA) %>%
#'       paste0("https://www.zappos.com", .) %>%
#'       unique()
#'     
#'     links <- links[stringr::str_detect(links, "/p/")] # Ensures shoes
#'     
#'     link_file_list <- stringr::str_remove(links,"https://www.zappos.com/p/") %>%
#'       str_replace_all("/", "_") %>%
#'       paste0(., ".jpg")
#'     
#'     # Only get new links
#'     idx <- which(!link_file_list %in% shoelist)
#'     if (length(idx) > 0) {
#'       message(sprintf("Found %d new shoes", length(idx)))
#'       links <- links[idx]
#'     }
#'     
#'     # Only get the specified number of shoes
#'     if (length(links) > max_shoes) {
#'       message(sprintf("Downsampling to %d shoes to download", max_shoes))
#'       links <- sample(links, max_shoes)
#'     }
#'   })
#'   
#'   # dplyr::data_frame(
#'   #   url = links,
#'   #   soleDL = unlist(purrr::map(links, ~try(get_bottom_image(., path = path))))
#'   # )
#'   return(links)
#' }
#' 
#' #' Function to reconstruct paths from MySQL setup
#' #' 
#' #' Returns paths of all matching images
#' #' @param image image name from zappos download. Must be specified.
#' #' @param full TRUE = full image, FALSE = slice (default). Must be specified.
#' #' @param flip 0 or 1
#' #' @param edge 0 (color) or 1 (edge-detected)
#' #' @param crop crop size string
#' #' @param size chunk size string
#' #' @param slice index of slice
#' #' @return image path(s)
#' #' @export
#' reconstruct_path <- function(image, full = F, flip = NULL, edge = NULL, 
#'                              crop = NULL, size = NULL, slice = NULL) {
#'   stopifnot(is.character(image) & !is.null(full))
#'   
#'   # Define default strings
#'   flipstr <- edgestr <- cropstr <- sizestr <- slicestr <- ".*"
#'   
#'   if (!is.null(flip)) {
#'     stopifnot(is.logical(flip) | (is.numeric(flip) && flip %in% c(0, 1)))
#'     
#'     flipstr <- ifelse(flip, "_flip", "")
#'   }
#'   
#'   if (!is.null(edge)) {
#'     stopifnot(is.logical(edge) | (is.numeric(edge) && edge %in% c(0, 1)))
#' 
#'     edgestr <- ifelse(edge, "_edge", "")
#'   }
#'   
#'   if (!is.null(crop)) {
#'     stopifnot(is.character(crop))
#'     
#'     cropstr <- paste0("_crop", crop)
#'   }
#'   
#'   if (full) {
#'     pathbits <- paste(unique(c(flipstr, edgestr, cropstr)), collapse = "", sep = "")
#'     return(file.path("processed", "toslice", sprintf("%s%s.png", image, pathbits)))
#'   } else {
#'     if (!is.null(size)) {
#'       stopifnot(is.numeric(size) | (is.character(size) & !is.na(as.numeric(size))))
#'       
#'       sizestr <- paste0("_sz", size)
#'     }
#'     
#'     if (!is.null(slice)) {
#'       stopifnot(is.numeric(slice) | (is.character(slice) & !is.na(as.numeric(slice))))
#'       
#'       slicestr <- paste0("_", slice)
#'     }
#'     
#'     pathbits <- paste(unique(c(flipstr, edgestr, cropstr, sizestr, slicestr)), collapse = "", sep = "")
#'     
#'     return(file.path("processed", "slices", sprintf("%s%s.png", image, pathbits)))
#'   }
#' }
