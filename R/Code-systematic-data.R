library(tidyverse)
library(rvest)

#' Get useful search links from Zappos
#' 
#' @param url zappos url
#' @return data frame of link, section type, search type, and other information
get_useful_searches <- function(url = "https://www.zappos.com/c/shoes") {
  links <- read_html(url) %>%
    html_nodes("#main > div > div > div.Se > div > ul.Ve a")
  
  search_urls <- links %>%
    html_attrs() %>%
    purrr::map(., rbind) %>%
    purrr::map(., as.data.frame) %>%
    bind_rows() %>%
    mutate(
      href = str_remove(href, "\\?.*$"),
      text = links %>% html_text(trim = T) %>% str_remove_all("[^A-z &]*"),
      parent = links %>% html_node(xpath = "../../../button") %>% html_text() %>% str_remove_all("[^A-z &]*")) %>%
    select(-c(1:2)) %>%
    filter(!str_detect(text, "Crib|(View All)")) %>%
    arrange(href)
  
  search_urls
}

#' Get all paginated links
#' 
#' Looks for Previous Page | 1 | 2 | ... in the html and finds links to all pages
#' @param url page url
#' @return a character vector of URLs
get_all_page_links <- function(url) {
  pagination <- read_html(url) %>%
    html_nodes(css = "#searchPagination span span a:last-child")
  
  if (length(pagination) == 0) {
    message("Pagination not found. It may be that all items fit on the page.")
    return(url)
  }
  
  get_n_pages <- pagination %>%
    purrr::map(., html_attr, name = "href", default = "") %>%
    str_extract("&p=\\d{0,}") %>%
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
#' @return NA if x is empty, otherwise, x
if_null_na <- function(x) {
  if (length(x) == 0) NA else x
}

#' For a URL with products, get every product on the page
#' 
#' Used to get an entire page of links to actual products
#' @param url url of page to acquire information
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
      
      data_frame(
        brand = html_nodes(., css = "*[itemprop='brand'] *[itemprop='name']") %>% 
          html_text() %>% 
          if_null_na(),
        style_name = html_nodes(., css = "p[itemprop='name']") %>% 
          html_text() %>% 
          if_null_na(),
        style_type = html_nodes(., css = "a") %>% 
          html_attr("aria-label") %>% 
          if_null_na() %>% 
          str_extract("Style:.*$") %>%
          str_trim() %>%
          str_remove("Style: ?") %>% 
          str_remove("Rated.*$"),
        price = html_text(.) %>% 
          if_null_na %>%
          str_extract("\\$\\d{0,}.\\d{2}") %>% 
          parse_number(),
        url = html_node(., css = "a") %>% 
          html_attr("href") %>%
          if_null_na,
        image_url = html_node(., css = "*[itemprop='image']") %>% 
          html_attr("content") %>%
          if_null_na
      )
    }) %>%
    bind_cols(ratings)
  
  info
}

#' For a URL of a shoe, get all information about the shoe
#' 
#' @param url url of page with shoe information
get_shoe_info <- function(url) {
  page <- read_html(url)
  
  category <- html_nodes(page, "#breadcrumbs div a") %>% html_text() %>%
    (function(.) .[!str_detect(., "Back")])
  
  color_options <- 
  
  iteminfo <- html_nodes(page, "*[itemprop][content]") %>%
    purrr::map_dfr(~tibble(key = html_attr(., "itemprop"), value = html_attr(., "content"))) %>%
    tidyr::spread(key = key, value = value, fill = NA) %>%
    mutate(logo = html_node(page, "*[itemprop='logo']") %>% html_attr("src"),
           sku = html_nodes(page, "#breadcrumbs div:nth-child(2)") %>% html_text() %>%
             str_remove("SKU ?"),
           category = as.list(category))
  
  
}
