library(tidyverse)
library(readr)
library(purrr)
library(highcharter)
options(scipen=999)

# https://www.ccamlr.org/en/fisheries/toothfish-fisheries

if (!dir.exists("ccamlr_data")) {
dir.create("ccamlr_data")
}


zip_url  <- "https://www.ccamlr.org/en/system/files/CCAMLR_statistical%20bulletin_V37.zip"
zip_file <- file.path("ccamlr_data", "ccamlr_statistical_bulletin_V37.zip")

download.file(zip_url, destfile = zip_file, mode = "wb")

unzip(zip_file, exdir = "ccamlr_data")

################################################################################
    ############################## CATCH ############################## 
################################################################################




ds <- read_csv(
        "data/ccamlr/ccamlr_statistical_bulletin_volume_37/catch_and_effort/combined.csv",
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

if (!dir.exists("ccamlr_data/processed_data")) {
  dir.create("ccamlr_data/processed_data", recursive = TRUE, showWarnings = FALSE)
}

if (!dir.exists("output")) {
dir.create("output")
}

write_csv(ds, "ccamlr_data/processed_data/ccamlr_processed_data.csv")


rmarkdown::render(
  here::here("R/create_fisheries.Rmd"),
  output_file = here::here("output", "SoE fisheries.html")
)

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
library(dplyr)
library(highcharter)

ds <- read_csv("ccamlr_data/processed_data/ccamlr_processed_data.csv")


ds_catch_summary <- ds %>%
  filter(area_code %in% c("48", "58", "88"),
         taxon_code %in% c("TOA", "TOP", "TOT", "ANI", "CES", "KRI")) %>%
  group_by(season_ccamlr, taxon_vernacular_name, taxon_scientific_name, taxon_code, area_code) %>%
  summarise(catch_tonne = sum(greenweight_caught_tonne, na.rm = TRUE)) %>%
  ungroup()







# ---- choose what to inspect ----
tx  <- "TOA"   # species code
ar  <- "88"    # management area

# ---- Catch data ----
ds_catch_one <- ds_catch_summary %>%
  filter(
    taxon_code == tx,
    area_code  == ar
  ) %>%
  arrange(season_ccamlr)

# ---- Effort data ----
ds_effort_one <- effort %>%
  filter(
    target_species_code == tx,
    area_code == ar
  ) %>%
  arrange(season_ccamlr)

# ---- Join datasets ----
ds_plot <- full_join(
  ds_catch_one,
  ds_effort_one,
  by = c(
    "season_ccamlr",
    "area_code" = "area_code",
    "taxon_code" = "target_species_code"
  )
)

# ---- Build chart ----
highchart() %>%
  
  # Catch = columns
  hc_add_series(
    data = ds_plot,
    type = "column",
    hcaes(x = season_ccamlr, y = catch_tonne),
    name = "Catch (t)",
    yAxis = 0
  ) %>%
  
  # Vessel count = line
  hc_add_series(
    data = ds_plot,
    type = "line",
    hcaes(x = season_ccamlr, y = vessel_count),
    name = "Vessel count",
    yAxis = 1,
    marker = list(enabled = TRUE)
  ) %>%
  
  hc_yAxis_multiples(
    list(
      title = list(text = "Catch (tonnes)")
    ),
    list(
      min = 0,
      max = 100,
      opposite = TRUE,
      title = list(
        text = "Vessel count",
        rotation = 270
      )
    )
  ) %>%
  
  hc_xAxis(title = list(text = "Season CCAMLR")) %>%
  hc_title(text = paste0("Catch vs Vessel Count — ", tx, " — Area ", ar)) %>%
  hc_tooltip(shared = TRUE)








ds_catch_summary %>%
  mutate(catch_tonne = round(catch_tonne, 0)) %>%
  filter(taxon_code == "TOA") %>%
  filter(catch_tonne > 0) %>%
  hchart(
  .,
  type = "column",
  hcaes(x = year, y = catch_tonne, group = area_code)
) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"   # <-- stacked bars
    )
  ) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Catch (tonnes)")) %>%
  hc_title(text = "Dissostichus mawsoni") %>%
  hc_legend(title = list(text = "Management Area"),
            align = "center", verticalAlign = "bottom",
            layout = "horizontal") %>%
  hc_tooltip(shared = TRUE)


################################################################################
############################## SUB AREAS ############################## 
################################################################################

ds_sub_area_catch_summary <- ds %>%
  filter(taxon_code %in% c("TOA", "TOP", "TOT", "ANI", "CES", "KRI"),
         !asd_type %in% "Area") %>%
  group_by(year, taxon_vernacular_name, taxon_scientific_name, taxon_code, asd_subarea_code) %>%
  summarise(
    catch_tonne = sum(greenweight_caught_tonne, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year = as.integer(year),
    catch_tonne = round(catch_tonne, 0)
  ) %>%
  ungroup()









################################################################################
    ############################## EFFORT ############################## 
################################################################################


ds_effort <- ds %>%
  filter(effort_or_catch %in% "effort",
         target_species_code %in% c("TOA", "TOP", "TOT", "ANI", "CES", "KRI"),
         area_code %in% c("48", "58", "88")) %>%
  mutate(taxon_vernacular_name = case_when(target_species_code == "ANI" ~ "Mackerel icefish",
                                           target_species_code == "KRI" ~ "Antarctic krill",
                                           target_species_code == "TOA" ~ "Antarctic toothfish",
                                           target_species_code == "TOP" ~ "Patagonian toothfish",
                                           target_species_code == "CES" ~ "Pike icefish",
                                           TRUE ~ NA_character_),
         taxon_scientific_name = case_when(target_species_code == "ANI" ~ "Champsocephalus gunnari",
                                           target_species_code == "KRI" ~ "Euphausia superba",
                                           target_species_code == "TOA" ~ "Dissostichus mawsoni",
                                           target_species_code == "TOP" ~ "Dissostichus eleginoides",
                                           target_species_code == "CES" ~ "Champsocephalus esox",
                                           TRUE ~ NA_character_),
         ) %>%
  select(effort_id, 
         taxon_vernacular_name, 
         taxon_scientific_name,
         area_code,
         everything())





ds_vessel <- ds_effort %>%
  group_by(year, target_species_code, taxon_vernacular_name, taxon_scientific_name, area_code) %>%
  summarise(vessel_count = sum(vessel_count)) %>%
  ungroup()

ds_fishing_days <- ds_effort %>%
  group_by(year, target_species_code, taxon_vernacular_name, taxon_scientific_name, area_code) %>%
  summarise(fishing_days = sum(fishing_days)) %>%
  ungroup()




hchart(
  ds_vessel,
  type = "column",
  hcaes(x = year, y = vessel_count, group = area_code)
) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"   # <-- stacked bars
    )
  ) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Vessel Count")) %>%
  hc_title(text = "Dissostichus mawsoni") %>%
  hc_legend(title = list(text = "Management Area"),
            align = "center", verticalAlign = "bottom",
            layout = "horizontal") %>%
  hc_tooltip(shared = TRUE)



hchart(
  ds_fishing_days,
  type = "column",
  hcaes(x = year, y = fishing_days, group = area_code)
) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"   # <-- stacked bars
    )
  ) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Fishing Days")) %>%
  hc_title(text = "Dissostichus mawsoni") %>%
  hc_legend(title = list(text = "Management Area"),
            align = "center", verticalAlign = "bottom",
            layout = "horizontal") %>%
  hc_tooltip(shared = TRUE)
