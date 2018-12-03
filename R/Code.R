
#' Get image of the sole from a url of a Zappo's shoe
#'
#' Pass in a url of the style https://www.zappos.com/p/...
#' @param i url
#' @param path path to save files
#' @param crop should image be cropped using magick?
#' @return TRUE if image was downloaded (or has been in the past), FALSE otherwise
#' @importFrom magrittr '%>%'
#' @export
get_bottom_image <- function(i, path = "inst/photos/", crop = TRUE) {
  # i is the shoe page link

  if (!dir.exists(path)) {
    dir.create(path, recursive =  T)
  }
  if (i == "") {
    return(NA)
  }

  photoname <- stringr::str_replace(i, "https://www.zappos.com/p/", "") %>%
    stringr::str_replace_all("/", "_") %>%
    unique()

  dlfile <- paste0(path, photoname, ".jpg")
  if (!file.exists(dlfile)) {
    # Sys.sleep(1)
    photolinks <- try(splashr::render_html(url = i))

    if ("try-error" %in% class(photolinks)) {
      return(FALSE)
    } else {
      photolinks <- photolinks %>%
        rvest::html_nodes(css = "button span img")
    }

    # Which one is bottom?
    bottomphoto <- which(rvest::html_attr(photolinks, "alt") == "BOTT")

    if (length(bottomphoto) > 0) {
      tmp <- photolinks %>%
        # Get photo link
        rvest::html_attr("src") %>%
        # Only keep photo of sole
        magrittr::extract(bottomphoto) %>%
        # Get higher resolution
        stringr::str_replace("SR106,78", "SX1920") %>%
        download.file(destfile = dlfile)
      
      if (crop) {
        cmd <- sprintf("convert %s -bordercolor white -border 1x1 -trim +repage -border 5x5 %s", dlfile)
        system(cmd)
      }
      # print(TRUE)
      return(tmp == 0)
      
      sprintf
    } else {
      # print(FALSE)
      return(FALSE)
    }
  }
  # print(TRUE)
  return(TRUE)
}

#' Function to scrape shoe sole data from Zappos.com
#'
#' @param type One of new, best, relevance, or rating. Defaults to relevance.
#' @param population One of all, women, men, boys, girls. Defaults to all.
#' @param path Path to save files
#' @param query search string
#' @return list of links and whether or not the image of the sole was downloaded
#' @importFrom magrittr '%>%'
#' @export
scrape_soles <- function(type = "rating", population = "all", pages = 15, path = "inst/photos/", query = "") {

  sa <- splashr::splash_active()
  if (!sa) {
    # system("docker run -p 5023:5023 -p 8050:8050 -p 8051:8051 scrapinghub/splash:latest &")
    dps <- system("docker ps -a -q", intern = T)
    if (length(dps) > 0) {
      splashr::stop_splash(dps)
      dps <- system("docker ps -a -q", intern = T)
      if (length(dps) > 1) {
        sprintf("docker stop %s", dps) %>% system()
        sprintf("docker rm %s", dps) %>% system()
      }
    }
    # splashr::install_splash(tag = "latest")
    container <- splashr::start_splash()
    on.exit({
      splashr::stop_splash(container)
      # try(system("docker rm /splashr"))
    })
    sa <- splashr::splash_active()
  }
  stopifnot(sa)
  
  Sys.sleep(1)

  if (substr(path, nchar(path), nchar(path)) != "/") {
    path <- paste0(path, "/")
  }

  if (type == "new") {
    ext <- "?s=isNew/desc/goLiveDate/desc/recentSalesStyle/desc/"
  } else if (type == "best") {
    ext <- "?s=recentSalesStyle/desc/"
  } else if (type == "rating") {
    ext <- "?s=productRating/desc/"
  } else if (type == "relevance") {
    ext <- ""
  } else {
    ext <- ""
    warning("type not recognized. Proceeding with relevance as the sort term.\n")
  }

  if (population == "women") {
    pop <- "women-shoes"
  } else if (population == "men") {
    pop <- "men-shoes"
  } else if (population == "girls") {
    pop <- "girls-shoes"
  } else if (population == "boys") {
    pop <- "boys-shoes"
  } else if (population == "all") {
    pop <- "shoes"
  } else {
    warning("population type not recognized. Proceeding as if 'all' was selected.\n")
    pop <- "shoes"
  }

  stopifnot(is.numeric(pages))


  if (query == "") {
    url <- sprintf("https://www.zappos.com/%s/CK_XAeICAQE.zso%s", pop, ext)
  } else {
    slash <- paste0(str_replace_all(query, " ", "-"), "-", pop)
    qstring <- paste0("&t=", str_replace_all(query, " ", "+"))
    url <- sprintf("https://www.zappos.com/%s/.zso%s%s", slash, ext, qstring)
  }

  links <- ""
  
  try({
    shoeLinks <- url %>% paste(c("", sprintf("&p=%d", 1:pages)), sep = "")
    shoeLinkPages <- purrr::map(shoeLinks, xml2::read_html)
    
    links <- purrr::map(shoeLinkPages, rvest::html_nodes, css = "article a") %>%
      unlist(recursive = F) %>%
      purrr::map(rvest::html_attr, name = "href", default = NA) %>%
      paste0("https://www.zappos.com", .) %>%
      unique()
    
    links <- links[stringr::str_detect(links, "/p/")] # Ensures shoes
    
  })
  
  dplyr::data_frame(
    url = links,
    soleDL = unlist(map(links, get_bottom_image, path = path))
  )
}

#' Function to reconstruct paths from MySQL setup
#' 
#' Returns paths of all matching images
#' @param image image name from zappos download. Must be specified.
#' @param full TRUE = full image, FALSE = slice (default). Must be specified.
#' @param flip 0 or 1
#' @param edge 0 (color) or 1 (edge-detected)
#' @param crop crop size string
#' @param size chunk size string
#' @param slice index of slice
#' @return image path(s)
#' @export
#' @importFrom magrittr '%>%'
reconstruct_path <- function(image, full = F, flip = NULL, edge = NULL, 
                             crop = NULL, size = NULL, slice = NULL) {
  stopifnot(is.character(image) & !is.null(full))
  
  # Define default strings
  flipstr <- edgestr <- cropstr <- sizestr <- slicestr <- ".*"
  
  if (!is.null(flip)) {
    stopifnot(is.logical(flip) | (is.numeric(flip) && flip %in% c(0, 1)))
    
    flipstr <- ifelse(flip, "_flip", "")
  }
  
  if (!is.null(edge)) {
    stopifnot(is.logical(edge) | (is.numeric(edge) && edge %in% c(0, 1)))

    edgestr <- ifelse(edge, "_edge", "")
  }
  
  if (!is.null(crop)) {
    stopifnot(is.character(crop))
    
    cropstr <- paste0("_crop", crop)
  }
  
  if (full) {
    pathbits <- paste(unique(c(flipstr, edgestr, cropstr)), collapse = "", sep = "")
    return(file.path("processed", "toslice", sprintf("%s%s.png", image, pathbits)))
  } else {
    if (!is.null(size)) {
      stopifnot(is.numeric(size) | (is.character(size) & !is.na(as.numeric(size))))
      
      sizestr <- paste0("_sz", size)
    }
    
    if (!is.null(slice)) {
      stopifnot(is.numeric(slice) | (is.character(slice) & !is.na(as.numeric(slice))))
      
      slicestr <- paste0("_", slice)
    }
    
    pathbits <- paste(unique(c(flipstr, edgestr, cropstr, sizestr, slicestr)), collapse = "", sep = "")
    
    return(file.path("processed", "slices", sprintf("%s%s.png", image, pathbits)))
  }
}
