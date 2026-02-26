---
  title: "CCAMLR Catch by Taxon (Stacked by Area)"
output:
  html_document:
  toc: true
toc_depth: 2
theme: readable
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

library(dplyr)
library(readr)
library(highcharter)
```

# Load data

```{r load_data}
ds <- read_csv("ccamlr_data/processed_data/ccamlr_processed_data.csv")
```

# Build summaries

```{r build_data}
ds_catch <- ds %>%
  filter(
    taxon_code %in% c("TOA", "TOP", "TOT", "ANI", "CES", "KRI"),
    area_code %in% c("48", "58", "88")
  )

ds_catch_summary <- ds_catch %>%
  group_by(year, taxon_vernacular_name, taxon_scientific_name, taxon_code, area_code) %>%
  summarise(
    catch_tonne = sum(greenweight_caught_tonne, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year = as.integer(year),
    catch_tonne = round(catch_tonne, 0)
  )
```

# Charts

```{r charts, results='asis'}
make_taxon_chart <- function(taxon) {
  
  ds_one <- ds_catch_summary %>%
    filter(taxon_code == taxon, catch_tonne > 0) %>%
    arrange(year, area_code)
  
  if (nrow(ds_one) == 0) return(NULL)
  
  sci <- ds_one$taxon_scientific_name[1]
  ver <- ds_one$taxon_vernacular_name[1]
  
  title_txt <- paste0(
    sci,
    if (!is.na(ver) && nzchar(ver)) paste0(" — ", ver) else "",
    " (", taxon, ")"
  )
  
  hchart(
    ds_one,
    type = "column",
    hcaes(x = year, y = catch_tonne, group = area_code)
  ) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_xAxis(title = list(text = "Year")) %>%
    hc_yAxis(title = list(text = "Catch (tonnes)")) %>%
    hc_title(text = title_txt) %>%
    hc_legend(
      title = list(text = "Management Area"),
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal"
    ) %>%
    hc_tooltip(shared = TRUE)
}

taxa <- c("TOA", "TOP", "TOT", "ANI", "CES", "KRI")

for (tx in taxa) {
  
  chart <- make_taxon_chart(tx)
  if (is.null(chart)) next
  
  ds_one <- ds_catch_summary %>%
    filter(taxon_code == tx, catch_tonne > 0)
  
  cat("\n\n## ", ds_one$taxon_scientific_name[1], " (", tx, ")\n\n", sep = "")
  
  print(chart)
  
  cat("\n\n")
}
```
