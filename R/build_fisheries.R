library(tidyverse)
library(readr)
library(purrr)
library(highcharter)
options(scipen=999)

# https://www.ccamlr.org/en/fisheries/toothfish-fisheries

if (!dir.exists("data")) {
dir.create("data")
}

if (!dir.exists("docs")) {
  dir.create("docs")
}



zip_url  <- "https://www.ccamlr.org/en/system/files/CCAMLR_statistical%20bulletin_V37.zip"
zip_file <- file.path("data", "ccamlr_statistical_bulletin_V37.zip")

download.file(zip_url, destfile = zip_file, mode = "wb")

unzip(zip_file, exdir = "data")

################################################################################
    ############################## BUILD ############################## 
################################################################################


ds <- read_csv(
        "data/CCAMLR_statistical bulletin_V37/Combined.csv",
        show_col_types = FALSE,
        col_types = cols(
          .default  = col_guess(),      # keep guessing for everything else
          asd_code  = col_character(),  # force these two
          pot_count = col_double()
        )
      ) %>%
  filter(!if_all(everything(), is.na)) %>%
  mutate(area_code = case_when(
    asd_code %in% c("48", "481", "482", "483", "484", "485", "486") ~ "48",
    asd_code %in% c("58", "584", "5841", "5842", "5843", "5843a", "5843b", "5844", "5844a", "5844b", "585", "5851", "5852", "586", "587") ~ "58",
    asd_code %in% c("88", "881", "882", "883") ~ "88",
    TRUE ~ NA_character_
  ))



write_csv(ds, "data/ccamlr_processed_data.csv")



# Build the output files

rmarkdown::render(
  here::here("R/create_fisheries_html.Rmd"),
  output_file = here::here("docs", "CCAMLR SoE Fisheries.html")
)



rmarkdown::render(
  here::here("R/create_fisheries_word.Rmd"),
  output_file = here::here("docs", "CCAMLR SoE Fisheries.docx")
)
