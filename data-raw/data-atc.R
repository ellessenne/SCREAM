### Dataset with ATC Index
library(tidyverse)
library(rvest)

url <- "https://www.atccode.com"

atc.1 <- rvest::read_html(x = url) %>%
  html_nodes(xpath = '//*[@id="article"]') %>%
  html_nodes("li") %>%
  html_text() %>%
  as_tibble() %>%
  separate(col = value, into = c("ATC.Code", "Name"), sep = ":") %>%
  mutate(
    ATC.Code = str_trim(ATC.Code),
    Name = str_trim(Name)
  ) %>%
  mutate(length = str_length(ATC.Code))

atc.2 <- map_dfr(.x = atc.1$ATC.Code[atc.1$length > 1], .f = function(a) {
  rvest::read_html(x = paste0(url, "/", a)) %>%
    html_nodes(xpath = '//*[@id="article"]') %>%
    html_nodes("li") %>%
    html_text() %>%
    as_tibble() %>%
    filter(grepl(pattern = paste0("^", a), x = value)) %>%
    separate(col = value, into = c("ATC.Code", "Name"), sep = ":") %>%
    mutate(
      ATC.Code = str_trim(ATC.Code),
      Name = str_trim(Name)
    ) %>%
    mutate(length = str_length(ATC.Code))
})

atc.3 <- map_dfr(.x = atc.2$ATC.Code, .f = function(a) {
  tryCatch(
    expr = {
      rvest::read_html(x = paste0(url, "/", a)) %>%
        html_nodes(xpath = '//*[@id="article"]') %>%
        html_nodes("li") %>%
        html_text() %>%
        as_tibble() %>%
        filter(grepl(pattern = paste0("^", a), x = value)) %>%
        separate(col = value, into = c("ATC.Code", "Name"), sep = ":") %>%
        mutate(
          ATC.Code = str_trim(ATC.Code),
          Name = str_trim(Name)
        ) %>%
        mutate(length = str_length(ATC.Code))
    }, error = function(e) NULL
  )
})

atc <- bind_rows(atc.1, atc.2, atc.3) %>%
  distinct() %>%
  select(-length) %>%
  arrange(ATC.Code, Name)

usethis::use_data(atc, overwrite = TRUE)
