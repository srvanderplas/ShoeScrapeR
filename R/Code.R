
#' Get image of the sole from a url of a Zappo's shoe
#'
#' Pass in a url of the style https://www.zappos.com/p/...
#' @param i url
#' @param path path to save files
#' @return TRUE if image was downloaded (or has been in the past), FALSE otherwise
#' @importFrom magrittr '%>%'
get_bottom_image <- function(i, path = "inst/photos/") {
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

      Sys.sleep(3)
      photolinks %>%
        # Get photo link
        rvest::html_attr("src") %>%
        # Only keep photo of sole
        magrittr::extract(bottomphoto) %>%
        # Get higher resolution
        stringr::str_replace("SR106,78", "SX1920") %>%
        download.file(destfile = dlfile)
      # print(TRUE)
      return(TRUE)
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

  if (!splashr::splash_active()) {
    # splashr::install_splash(tag = "latest")
    # system("docker run -p 5023:5023 -p 8050:8050 -p 8051:8051 scrapinghub/splash:latest &")
    dps <- system("docker ps -a -l", intern = T)
    if (length(dps) > 1) {
      container <- str_split(dps[2], "\\s{1,}")[[1]][1]
      splashr::stop_splash(container)
      dps <- system("docker ps -a -l", intern = T)
      if (length(dps) > 1) {
        system("docker rm splashr")
      }
    }
    container <- splashr::start_splash()
    on.exit({
      splashr::stop_splash(container)
      # try(system("docker rm /splashr"))
    })
  }

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
    shoeLinks <- url %>% paste(c("", sprintf("?p%d", 1:pages)), sep = "")
    shoeLinkPages <- purrr::map(shoeLinks, xml2::read_html)
    
    links <- purrr::map(shoeLinkPages, rvest::html_nodes, css = "#searchPage a") %>%
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

