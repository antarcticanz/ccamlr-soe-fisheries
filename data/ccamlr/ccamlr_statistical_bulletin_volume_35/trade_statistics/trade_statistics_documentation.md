# CCAMLR Statistical Bulletin – Trade Statistics Documentation

For Volume 35 (Year 2022)

## Introduction

The *Statistical Bulletin* is published annually and contains aggregated trade statistics for toothfish (*Dissostichus eleginoides* and *Dissostichus mawsoni*). Trade statistics are derived from the landings and exports of toothfish as reported in CCAMLR’s [Catch Documentation Scheme (CDS)](https://www.ccamlr.org/en/compliance/catch-documentation-scheme-cds). The CDS data covers landings and exports of catches taken from inside and outside the CAMLR Convention Area since 2000. The geographic location of exported and imported product is used to assign export, re-export and import countries or entities. Landings and exports reported by some countries or entities may include data in respect of their overseas territories.

The *Statistical Bulletin* is provided as a bundled zip file and contains trade statistics data documentation, data files and selected tables. This documentation is provided in both markdown and pdf format. The data files are listed below and further information on each is contained later in this documentation.

* exports.csv – The data file containing the export and re-export data.
* landings.csv – The data file containing the landing data.

The selected tables are prepared tables generated from the last 10 years of data and presented in pdf format.

* 13_processed_landings_by_year_area.pdf – Table 13 Landings (processed weights, tonnes) of Dissostichus spp. by calendar year and area caught.
* 14_greenweight_landings_by_year_area.pdf – Table 14 Landings (estimated live weight, tonnes) of Dissostichus spp. by calendar year and area caught.
* 15_processed_landings_by_year_flag_area.pdf – Table 15 Landings (processed weights, tonnes) of Dissostichus spp. by calendar year, Flag State and area caught.
* 16_processed_landings_by_year_export_import_country.pdf – Table 16 Exports (processed weights, tonnes) of Dissostichus spp. by calendar year, Flag State, export country and import country.
* 17_reexports_by_year_reexport_import_countrys.pdf – Table 17 Re-exports (processed weights, tonnes) of Dissostichus spp. by calendar year, Flag State, re-export country and import country.

### Revised field names

Field names were revised in the *Statistical Bulletin* Volume 33. The new names used in Volume 33 and previous field names used in Volume 32 and earlier are shown below for reference.

Table | New Field | Previous Field
--- | --- | ---
Exports | -- | ATE_ID
Exports | year | Calendar_Year
Exports | taxon_code | TXN_Code
Exports | flag_country_code | Flag_CTY_Code
Exports | flag_country | --
Exports | exporting_country_code | Exporting_CTY_Code
Exports | exporting_country | --
Exports | unlading_country_code | Unlading_CTY_Code
Exports | unlading_country | --
Exports | reexport_yes_no | Re-Export_YN
Exports | exported_product_weight_tonne | Exported_Product_Weighttonnes
Landings | -- | ATL_ID
Landings | year | Calendar_Year
Landings | asd_code | GAR_Code
Landings | asd_type | --
Landings | asd_area_code | --
Landings | asd_subarea_code | --
Landings | taxon_code | TXN_Code
Landings | flag_country_code | Flag_CTY_Code
Landings | flag_country | --
Landings | landed_product_weight_tonne | Landed_Product_Weight
Landings | estimated_green_weight_tonne | Estimated_Green_Weight

***

## Exports table

Exports and re-exports of toothfish (*Dissostichus eleginoides* and *Dissostichus mawsoni*) aggregated by year, Flag State, export state and unlading state. Note prior to volume 33 the records for years 2000 to 2009 were allocated to year based on the authorised date. From volume 33 all records are allocated to year based on exported dates.

Field | Description
--- | ---
year | Calendar year when the export or re export occurred.
taxon_code | The three letter code for the toothfish species exported (TOA = *Dissostichus mawsoni* or TOP = *Dissostichus eleginoides*).
flag_country_code | The code for the Flag State of the fishing vessels which caught the product using three letter ISO 3166-1 country codes.
flag_country | The Flag State of the fishing vessels.
exporting_country_code | The code for the country exporting the product using three letter ISO 3166-1 country codes.
exporting_country | The country exporting the product.
unlading_country_code | The code for the country where the exported product was removed from an aircraft, truck, train or vessel using three letter ISO 3166-1 country codes.
unlading_country | The country where the exported product was removed from an aircraft, truck, train or vessel.
reexport_yes_no | Identifies if export was a re-export (Y = re-export, N = NOT re-export).
exported_product_weight_tonne | Weight of the exported product in metric tonnes.

***

## Landings table

Landings of toothfish (*Dissostichus eleginoides* and *Dissostichus mawsoni*) aggregated by year, area and Flag State. Landings of toothfish caught inside the Convention Area are summarised by subarea and division. Landings of toothfish caught outside the Convention Area are summarised by statistical area only. Note prior to volume 33 the records for years from 2010 were allocated to year based on the end of fishing or transshipment dates. From volume 33 all records are allocated to year based on landing dates.

Field | Description
--- | ---
year | Calendar year when the landing occurred.
asd_code | The area, subarea or division where the fishing occurred, based on FAO Major Fishing Areas.
asd_type | Refers to the type of area (area, subarea or division) in the asd_code field.
asd_area_code | The FAO Major Fishing Area where the fishing occurred.
asd_subarea_code | The subarea where the fishing occurred. Blank if the asd_type is an area.
taxon_code | The three letter code for the toothfish species exported (TOA = *Dissostichus mawsoni* or TOP = *Dissostichus eleginoides*).
flag_country_code | The code for the Flag State of the fishing vessels which caught the product using three letter ISO 3166-1 country codes.
flag_country | The Flag State of the fishing vessels.
landed_product_weight_tonne | Weight of the landed product in metric tonnes.
estimated_green_weight_tonne | Estimated green weight in metric tonnes derived from the product landed and the conversion factor used in the CDS.
