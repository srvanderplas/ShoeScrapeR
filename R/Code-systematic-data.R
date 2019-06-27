#' Get useful search links from Zappos
#' 
#' @param url zappos url
#' @export
#' @return data frame of link, section type, search type, and other information
get_useful_searches <- function(url = "https://www.zappos.com/c/shoes") {
  links <- read_html(url) %>%
    html_nodes("*[data-slot-id='primary-1'] ul a")
  
  
  purrr::map_dfr(
    links, 
    ~tibble(href = html_attr(., "href") %>% if_null_value(NA), 
            text = html_text(., trim = T) %>% str_remove_all("[^A-z &]*") %>% if_null_value(NA),
            parent = html_node(., xpath = "../../../button") %>% 
              html_text() %>% 
              str_remove_all("[^A-z &]*") %>% 
              if_null_value(NA))
  ) %>%
    filter(!str_detect(text, "Home|Crib|(View All)"))
}

#' Get all paginated links
#' 
#' Looks for Previous Page | 1 | 2 | ... in the html and finds links to all pages
#' @param url page url
#' @export
#' @return a character vector of URLs
get_all_page_links <- function(url) {
  pagination <- read_html(url) %>%
    html_nodes(css = "#searchPagination span span a:last-child")
  
  if (length(pagination) == 0) {
    message("Pagination not found. It may be that all items fit on the page.")
    return(url)
  }
  
  get_n_pages <- pagination %>%
    purrr::map(., html_attr, name = "href") %>%
    str_extract("p=\\d{0,}") %>%
    parse_number() %>%
    na.omit() %>% 
    max()
  
  if (is.infinite(get_n_pages)) {
    message("no non-NA pages found.")
    return(x)
  }
  
  return(sprintf("%s?p=%d", url, 0:get_n_pages))
}

#' Get rating from an item
#' 
#' @param node pointer to node of item with rating
#' @export
#' @return rating and number of reviews
get_rating <- function(node) {
  rating <- html_nodes(node, css = "*[itemprop='aggregateRating']") 
  
  if (length(rating) > 0) {
    tibble(
      value = rating %>% 
        html_node("meta[itemprop='ratingValue']") %>%
        html_attr("content"),
      n = rating %>% 
        html_node("meta[itemprop*='ratingCount']") %>%
        html_attr("content")
    )
  } else {
    tibble(value = NA, n = NA)
  }
}

#' Replace NULL values with NA
#' 
#' Used to handle missing nodes in HTML scraping
#' @param x a list or vector
#' @param val a value to use as default (by default, NA)
#' @return NA if x is empty, otherwise, x
if_null_value <- function(x, val = NA) {
  if (length(x) == 0) val else x
}

#' Remove option from vector
#' 
#' @param x character vector of options
#' @param pattern pattern to check for (regular expression). Matching expressions will be removed.
remove_from <- function(x, pattern) {
  x[!str_detect(x, pattern)]
}

#' For a URL with products, get every product on the page
#' 
#' Used to get an entire page of links to actual products
#' @param url url of page to acquire information
#' @export
#' @return data frame containing brand, style, price, url, image, and rating 
#'         information as provided by the HTML structure
get_all_shoes_on_page <- function(url) {
  shoes <- read_html(url) %>%
    html_nodes("#searchPage div *[itemtype='http://schema.org/Product']")
  
  ratings <- purrr::map_dfr(shoes, get_rating) %>%
    set_names(c("review_value", "review_n"))
  
  info <- shoes %>%
    # purrr::map(html_nodes, css = "div") %>%
    purrr::map_df(~{
      
      tibble(
        brand = html_nodes(., css = "*[itemprop='brand'] *[itemprop='name']") %>% 
          html_text() %>% 
          if_null_value(),
        style_name = html_nodes(., css = "p[itemprop='name']") %>% 
          html_text() %>% 
          if_null_value(),
        style_type = html_nodes(., css = "a") %>% 
          html_attr("aria-label") %>% 
          if_null_value() %>% 
          str_extract("Style:.*$") %>%
          str_trim() %>%
          str_remove("Style: ?") %>% 
          str_remove("Rated.*$"),
        price = html_text(.) %>% 
          if_null_value %>%
          str_extract("\\$\\d{0,}.\\d{2}") %>% 
          parse_number(),
        url = html_node(., css = "a") %>% 
          html_attr("href") %>%
          if_null_value,
        image_url = html_node(., css = "*[itemprop='image']") %>% 
          html_attr("content") %>%
          if_null_value
      )
    }) %>%
    bind_cols(ratings)
  
  info
}

#' For a URL of a shoe, get all information about the shoe
#' 
#' @param url url of page with shoe information
#' @export
get_shoe_info <- function(url) {
  Sys.sleep(runif(1, min = 0.5, max = 5.5))
  page <- read_html(url)
  
  categories <- html_nodes(page, "#breadcrumbs div a") %>% html_text() %>%
    (function(.) .[!str_detect(., "Back")])
  
  sizes <- html_nodes(page, "#pdp-size-select option") %>% html_text() %>% if_null_value %>% remove_from("Choose")
  
  widths <- html_nodes(page, "#pdp-width-select option") %>% html_text() %>% if_null_value("M") %>% remove_from("Choose")
  
  colors <- tibble(
    color = html_nodes(page, "#pdp-color-select option") %>% html_text() %>% if_null_value,
    value = html_nodes(page, "#pdp-color-select option") %>% html_attr("value") %>% if_null_value
  )
  
  description <- html_nodes(page, "*[itemprop='description'] li div") %>% html_text() %>%
    str_remove_all("•") %>%
    str_replace_all("\\s{1,}", " ") %>%
    str_trim() %>% if_null_value() 
  
  images <- html_nodes(page, "#thumbnailsList img") %>%
    purrr::map_dfr(~tibble(key = html_attr(., "alt") %>% 
                             if_null_value("PAIR"), 
                           link = html_attr(., "src") %>% 
                             if_null_value())) %>%
    mutate(link = stringr::str_replace(link, "SR106,78", "SX1920"))
  
  brand <- tibble(
    logo = html_node(page, "*[itemprop='logo']") %>% html_attr("src"),
    brand_name = html_node(page, "*[data-track-value='Brand-Logo']") %>% html_attr("title"),
    brand_link = html_node(page, "*[data-track-value='Brand-Logo']") %>% html_attr("href")
  )
  
  iteminfo <- html_nodes(page, "*[itemprop][content]") %>%
    purrr::map_dfr(~tibble(key = html_attr(., "itemprop"), 
                           value = html_attr(., "content"))) %>%
    tidyr::spread(key = key, value = value, fill = NA) %>%
    mutate(brand = list(brand),
           sku = html_nodes(page, "#breadcrumbs div:nth-child(2)") %>% 
             html_text() %>%
             str_remove("SKU ?"),
           category = list(categories),
           sizes = list(sizes),
           colors = list(colors),
           widths = as.list(widths),
           description = list(description),
           images = list(images)) %>%
    mutate(widths = as.list(widths))
  
  iteminfo
}

#' Download one or more images from urls
#' @param url url to download
#' @param filename name to save file to
#' @param path directory to save files in (can be NULL to save in current dir)
#' @export
download_image <- function(url, filename, path = NULL, overwrite = F) {
  assertthat::assert_that(length(url) == length(filename))
  if (!is.null(path)) assertthat::assert_that(dir.exists(path))
  
  destfile <- if (is.null(path)) filename else file.path(path, filename)

  download.file(url, destfile, mode = "wb")
}

clean_field <- function(x) {
  str_replace_all(x, "[^A-z]{1,}", "-") %>% str_remove("-$") %>% str_remove("^-") %>% str_to_lower()
}

# Need to figure out how to set up the file system with picture links to ensure 
# that everything is parse-able and saved correctly
clean_shoe_path <- function(df) {
  df <- df %>%
    mutate(brand_clean = clean_field(brand),
           style_clean = clean_field(style_name),
           type_clean = clean_field(style_type),
           path_name = sprintf("%s_%s_%s_%s", brand_clean, style_clean, sku, type_clean))
}

