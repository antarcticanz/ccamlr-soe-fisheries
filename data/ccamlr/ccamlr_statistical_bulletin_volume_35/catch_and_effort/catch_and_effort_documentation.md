# CCAMLR Statistical Bulletin – Catch and Effort Documentation

For Volume 35 (CCAMLR fishing season 2022)

## Introduction

The *Statistical Bulletin* is published annually and contains catch and effort data for fisheries in CCAMLR’s Convention Area from 1970 onward. Data is aggregated by year, month, vessel nationality, fishing purpose, gear type, target species, geographical area, vessel size category and catch species.

The *Statistical Bulletin* is provided as a bundled zip file and contains catch and effort documentation, data files and selected tables. This documentation is provided in both markdown and pdf format. The data files are listed below and further information on each is contained later in this documentation.

* Combined data tables
  * combined.csv.
* Individual data tables
  * effort.csv
  * catch.csv
  * area.csv
  * country.csv
  * fishing_gear.csv
  * fishing_purpose.csv
  * taxon.csv
  * vessel_size.csv.

The selected tables are prepared tables generated from the last 10 years of data and presented in pdf format.

* 4_1_catch_by_area.pdf – Table 4.1 Catch (tonnes) of all species by area/subarea/division.
* 6_catch_by_country_species_area.pdf – Table 6 Catch (tonnes) by country, species and area.
* 7_1_catch_by_species_area.pdf – Table 7.1 Catch (tonnes) by species and area/subarea/division.
* 8_1_catch_by_species_country.pdf – Table 8.1 Catch (tonnes) by species and country.
* 9_1_catch_by_area_species_country.pdf – Table 9.1 Catch (tonnes) by area/subarea/division, species and country.
* 11_5_effort_by_area_target_species_month.pdf – Table 11.5 Effort (fishing days) by area/subdivision/division, target species and month.

Information of individual fisheries can be found in the CCAMLR Fishery Reports [http://fisheryreports.ccamlr.org/](http://fisheryreports.ccamlr.org/).

***

## Additional Notes

### Standardisation

Where possible the *Statistical Bulletin* reporting standards for catch and effort statistics support the international standards set by the FAO Coordinating Working Party on Fisheries Statistics (CWP) [CWP Handbook of Fishery Statistics](http://www.fao.org/cwp-on-fishery-statistics/handbook/en/):

* Country Codes and names are aligned with ISO 3166-1 (see [FAO CWP Country or areas](http://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/country-or-areas/en/)).
* Fishing Gear Types are aligned with the International Standard Statistical Classification of Fishing Gear (see [FAO CWP Fishing Gear Classification](http://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishing-gear-classification/en/)).
* Vessel Size Categories are aligned with the International Standard Statistical Classification of Fishery Vessels by GRT (see [FAO CWP Fishery fleet](http://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishery-fleet/en/)).
* Geographic Areas are aligned with the FAO Major Fishing Areas (see [FAO CWP Handbook – Fishing Areas for Statistical Purposes](http://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/fishing-areas-for-statistical-purposes/en/)).

### CAMLR Convention Area

CAMLR’s Convention Area extends from the coast of Antarctica to the Antarctic Convergence, at latitudes ranging from 45–60° South. The Convention Area is circumpolar and encompasses the southern sectors of the Atlantic Ocean (Statistical Area 48), Indian Ocean (Statistical Area 58) and Pacific Ocean (Statistical Area 88). Each statistical area is divided into subareas (e.g. Subareas 881, 882 and 883) and then further divided into divisions (e.g. Divisions 5851 and 5852). For further information on the CAMLR Convention Area and geographical areas refer to

* [CAMLR Convention Area](https://www.ccamlr.org/en/organisation/convention-area)
* [CCAMLR Online GIS](https://gis.ccamlr.org/).

### Year, month and CCAMLR fishing season allocations

The CCAMLR fishing season was defined in 2001 as 1 December to 30 November (refer [Conservation Measure (CM) 32-01](https://www.ccamlr.org/measure-32-01)). The CCAMLR fishing season is identified by the year at the end of the season. For example, CCAMLR fishing season 2020 is from 1 December 2019 to 30 November 2020. The CCAMLR fishing season has been allocated to historical data based on reported year and month. Note that some of the historical aggregated data was provided only on a calendar year or split year (July to June) basis and without providing a month, hence precluding the assignment of CCAMLR fishing season to these records. The CCAMLR fishing season field for these records remains blank.

For current records, the year, month and CCAMLR fishing season are allocated from the date the fishing activity ended as reported in the haul-by-haul data record. The fishing activity reported in each haul-by-haul data record includes the set start/end dates and haul start/end dates. When the haul date is not recorded due to gear loss, the set date is used as the date the fishing activity ends. All date- and time-related calculations are performed using UTC time.

### Taxonomy code allocations

The CCAMLR taxonomy used to describe species is based on

* FOA CWP Aquatic Sciences and Fisheries Information System (ASFIS) species list, see [http://www.fao.org/cwp-on-fishery-statistics/handbook/](http://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/identifiers-for-aquatic-animals-and-plants/en/)
* World Register of Marine Species (WoRMS), see [https://www.marinespecies.org/](https://www.marinespecies.org/)

From Volume 34 onwards the historical taxonomy codes are updated to the currently used codes as requested in SC-CAMLR-38 paragraph 14.1(iv)(a). The code changes are listed below for reference.

| historical code | historical name | current code | current name |
| --- | --- | --- | --- |
| ANH | Anotopterus pharao | ANP | Anotopterus pharao |
| BTI | Bathydraconidae | BQY | Bathydraconidae |
| DCP | Pandalidae | PDZ | Pandalidae |
| ELZ | Zoarcidae | LVD | Zoarcidae |
| ET2 | Etmopterus viator | EZT | Etmopterus viator |
| FCX | Crustacea | CRU | Crustacea |
| GS2 | Somniosus antarcticus | RZZ | Somniosus antarcticus |
| GYA | Gymnodraco acuticeps | GDR | Gymnodraco acuticeps |
| JEL | Medusae | CNI | Cnidaria |
| LIZ | Liparididae | LPX | Liparidae |
| NOK | Lepidonotothen kempi (squamifrons) | NOS | Lepidonotothen squamifrons |
| PZJ | Pogonophryne phyllopogon | SZT | Pogonophryne scotti |
| SQQ | Teuthoidea | CEP | Cephalopoda |
| SQX | Ommastrephes, Illex | SQ1 | Ommastrephes, Illex |
| SR2 | Raja georgiana var. | SRR | Amblyraja georgiana |
| TMH | Trematomus bernacchii | ERN | Trematomus bernacchii |
| UNK | Unknown | UN1 | Unknown |

### Fishing effort notes

The following notes provide some details on the current calculation methods which should be considered when using fishing effort data.

* Catch records and their associated effort are allocated to a year and month period from the date the fishing activity ended as reported in the haul-by-haul data record. Therefore when the fishing activity is recorded over multiple monthly periods a portion of the effort may not be allocated to the same month that the effort was expended.
* Fishing Days is a count of the unique dates when each fishing activity ends and does not account for dates when a vessel performs setting or fishing activity without completing a haul-by-haul data record for those dates.
* Vessels using continuous trawling report multiple haul-by-haul data records at regular time intervals throughout the day.
* Some vessels use multiple nets at the same time. Trawl duration is the summed trawling duration for each net and not the trawling duration for the vessel.
* Apart from Fishing days and haul count, some fishing effort measures only apply when specific fishing gear is used:
  * Trawl duration hours applies to trawl gear types only
  * Hook count applies to longline gear types only.
  * Pot count applies to pot gear types only.

### Data Collection and history of the Bulletin

The production of a yearly *Statistical Bulletin* was considered and approved by SC-CAMLR-V (paragraph 7.9). The initial volumes 1 and 2 of the *Statistical Bulletin* were first published in 1990 and covered data from 1970 to 1989. The first publications of the *Statistical Bulletin* were only available as printed copies and because of this they included a large number of tabulations of the data. The selected tables that have been retained and are included in this release (4.1, 6, 7.1, 8.1, 9.1, 11.1) are those that were reportedly of most interest to users.

The *Statistical Bulletin* uses STATLANT data as its primary source, however, it should be noted that the methods used to derive and collect this data have changed over the years. STATLANT forms were originated as standard questionnaires designed by the Coordinating Working Party on Fisheries Statistics ([http://www.fao.org/fishery/cwp/en](http://www.fao.org/fishery/cwp/en)) for use in FAO’s acquisition of global fishery statistics. The STATLANT A data provided annual catch data by species and statistical sub-divisions of the relevant FAO major fishing areas. The STATLANT B data (form 08B) provided monthly catch and effort data by species, statistical sub-divisions of the relevantFAO major fishing areas, fishing gear, size class of fishing vessel and target species. In 1983 the Commission endorsed the use of STATLANT forms (CCAMLR-II, paragraph 44; SC-CAMLR-II, paragraph 25) for data reporting from the 1984 split year onwards and the requested data was reported to the CCAMLR Secretariat by national statistics offices. The Secretariat also worked with Members to compile STATLANT-equivalent data for the previous years where possible.

From 1986 Members started reporting detailed fine-scale data for some fisheries, some of which was presented in the *Statistical Bulletin*. Fine-scale data is data aggregated over areas of size 1 degree longitude by 0.5 degree latitude. Over time this scale of reporting was replaced by detailed catch and effort and haul by haul data (C Data submitted in accordance with CM 23-04). To address alignment between the detailed data and reported STATLANT data the Commission endorsed a new process in 2007 in which the Secretariat derived the preliminary STATLANT data from detailed catch and effort data reported to the Secretariat during the course of the year (CCAMLR-XXVI, paragraph 4.89). This preliminary STATLANT data was subsequently reviewed, and in some cases amended, by the national statistics offices of CCAMLR Members. Since 2021 the detailed haul by haul data have been considered as the primary data source, and STATLANT has been only a derived dataset (SC-CAMLR-38, paragraph 3.8).

When using the *Statistical Bulletin* data users should bear in mind this historical context of the history of data collection in CCAMLR.

### Krill Catches by small-scale management units (SSMUs)

Previous *Statistical Bulletins* have included the krill catches by small-scale management units (SSMUs) in Area 48. This data was no longer included in the *Statistical Bulletin* as of Volume 32. This data can now be found in the [CCAMLR Fishery Reports](http://fisheryreports.ccamlr.org/).

### Revised field names

Field names were revised in the *Statistical Bulletin* Volume 33. The new names used in Volume 33 onwards and previous field names used in Volume 32 and earlier are shown below for reference.

Table | New Field | Previous Field
--- | --- | ---
Catch, Effort | effort_id | AFE_ID
Catch, Effort | vessel_nationality_code | Flag_CTY_Code
Catch, Effort | fishing_purpose_code | FAC_Code
Catch, Effort | gear_type_code | GTY_Code
Catch, Effort | target_species_code | Target_TXN_Code
Catch, Effort | asd_code | GAR_Code
Catch, Effort | season_ccamlr | --
Catch, Effort | year | Calendar_Year
Catch, Effort | month | Month
Catch, Effort | vessel_size_category | VSZ_Code
Effort | vessel_count | Vessel_Count
Effort | fishing_days | Fishing_Days
Effort | trawl_duration_hours | Fishing_Hours
Effort | haul_count | Haul_Count
Effort | hook_count | Hook_Count
Effort | pot_count | Pot_Count
Catch | taxon_code | Catch_TXN_Code
Catch | greenweight_caught_tonne | Green_Weight
Area | asd_code | GAR_Code
Area | asd_type | GAR_Type
Area | asd_area_code | --
Area | asd_subarea_code | --
Area | -- | GAR_Name
Area | -- | Parent_GAR_Code
Country | country_code | CTY_Code
Country | country | CTY_Name
Fishing gear | gear_type_code | GTY_Code
Fishing gear | gear_type | GTY_Name
Fishing purpose | fishing_purpose_code | FAC_Code
Fishing purpose | fishing_purpose | FAC_Name
Taxon | taxon_code | TXN_Code
Taxon | taxon_scientific_name | TXN_Name
Taxon | -- | ASFIS_Code
Taxon | taxon_vernacular_name | TXN_English_Name
Taxon | -- | TXN_French_Name
Taxon | -- | TXN_Spanish_Name
Taxon | -- | TXN_Russian_Name
Vessel size | vessel_size_category_code | VSZ_Code
Vessel size | vessel_size_category | VSZ_Category

***

## Combined data table

The combined data table has been introduced in Volume 33 to provide all the catch, effort and reference data in a single file for ease of use. The file is structured with a single effort record followed by multiple associated catch records. Descriptions and other relevant data has also been included for reference. Data is aggregated by year, month, vessel nationality, fishing purpose, gear type, target species, geographical area, vessel size category and catch species.

Field | Description
--- | ---
effort_id | Unique identifier for each effort record. Used to associate the catch records for each taxon with each effort record.
effort_or_catch | Identifies each record as either an effort or catch record.
vessel_nationality_code | The code for the Flag State of the fishing vessels using three letter ISO 3166-1 country codes.
vessel_nationality | The Flag State of the fishing vessel(s).
fishing_purpose_code | Code for the purpose of the fishing activity.
fishing_purpose | The purpose of the fishing activity.
gear_type_code | Code for the gear type used during fishing.
gear_type | The gear type used during fishing.
target_species_code | The three letter code for the species (or taxon) at which the vessel was directing its fishing activity.
asd_code | The area, subarea or division where the fishing occurred, based on FAO Major Fishing Areas.
asd_type | Refers to the type of area (area, subarea or division) in the asd_code field.
asd_area_code | The FAO Major Fishing Area where the fishing occurred.
asd_subarea_code | The subarea where the fishing occurred. Blank if the asd_type is area.
season_ccamlr | The CCAMLR fishing season when the fishing occurred. Currently allocated from the date the fishing activity ends as reported in the haul-by-haul data record.
year | Calendar year when the fishing occurred. Currently allocated from the date the fishing activity ends as reported in the haul-by-haul data record.
month | Month when the fishing occurred. Currently allocated from the date the fishing activity ends as reported in the haul-by-haul data record.
vessel_size_category_code | Code for the size category of the fishing vessels based on gross tonnage.
vessel_size_category | The size category of the fishing vessels based on gross tonnage.
vessel_count | Number of fishing vessels.
fishing_days | Total number of days the vessels were fishing. Currently calculated as the distinct count of dates the fishing activity ends as reported in each of the haul-by-haul data records for each vessel and then summed. Where the haul date is not recorded due to gear loss, the set date is used as the date the fishing activity ends. The summed total is rounded up to the nearest integer to address some historical data which were reported as decimal values.
trawl_duration_hours | Total duration of trawling for each net used by each vessel in hours. Currently calculated as the minutes difference between the end of setting and the start of hauling for each haul-by-haul data record and then summed, converted to hours and rounded to 1 decimal place. Applies to trawl gear types only.
haul_count | Total number of haul-by-haul data records reported. Applies to all gear types.
hook_count | Total number of hooks set. Applies to longline gear types only.
pot_count | Total number of pots set. Applies to pot gear types only.
taxon_code | The three letter code for the specimen that was identified.
taxon_scientific_name | The full scientific name of the lowest level taxonomic rank that can be determined for the specimen that was identified.
taxon_vernacular_name | The English vernacular name for the specimen that was identified.
greenweight_caught_tonne | The green weight of the catch in metric tonnes of the taxon caught and either retained or discarded overboard. Does not include catch released alive or lost. Currently calculated as the sum of the report catch converted to tonnes and rounded to 6 decimal places.

## Individual data tables

The individual data tables provide the same data as the combined data table, but in a more normalised structure with individual files for the catch, effort and reference data. These files can be used individually or joined in a database.

### Catch table

Catch data aggregated by year, month, vessel nationality, fishing purpose, gear type, target species, geographical area, vessel size category and catch species.

Field | Description
--- | ---
effort_id | Unique identifier for each effort record. Used to associate the catch records for each taxon with each effort record.
vessel_nationality_code | The code for the Flag State of the fishing vessels using three letter ISO 3166-1 country codes. Refer to country table for a list of valid codes and country names.
fishing_purpose_code | Code for the purpose of the fishing activity. Refer to fishing purpose table for a list of valid codes and descriptions.
gear_type_code | Code for the gear type used during fishing. Refer to fishing gear table for a list of valid codes and descriptions.
target_species_code | The three letter code for the species (or taxon) at which the vessel was directing its fishing activity. Refer to taxon table for a list of valid codes and names.
asd_code | The area, subarea or division where the fishing occurred, based on FAO Major Fishing Areas. Refer to area table for a list of valid codes and related reference data.
season_ccamlr | The CCAMLR fishing season when the fishing occurred. Currently allocated from the date the fishing activity ends as reported in the haul-by-haul data record.
year | Calendar year when the fishing occurred. Currently allocated from the date the fishing activity ends as reported in the haul-by-haul data record.
month | Month when the fishing occurred. Currently allocated from the date the fishing activity ends as reported in the haul-by-haul data record.
vessel_size_category_code | Code for the size category of the fishing vessels based on gross tonnage. Refer to vessel size table for a list of valid codes and descriptions.
taxon_code | The three letter code for the specimen that was identified. Refer to taxon table for a list of valid codes and names.
greenweight_caught_tonne | The green weight of the catch in metric tonnes of the taxon caught and either retained or discarded overboard. Does not include catch released alive or lost. Currently calculated as the sum of the report catch converted to tonnes and rounded to 6 decimal places.

***

### Effort table

Effort data aggregated by year, month, vessel nationality, fishing purpose, gear type, target species, geographical area and vessel size category.

Field | Description
--- | ---
effort_id | Unique identifier for each effort record. Used to associate the catch records for each taxon with each effort record.
vessel_nationality_code | The code for the Flag State of the fishing vessels using three letter ISO 3166-1 country codes. Refer to country table for a list of valid codes and country names.
fishing_purpose_code | Code for the purpose of the fishing activity. Refer to fishing purpose table for a list of valid codes and descriptions.
gear_type_code | Code for the gear type used during fishing. Refer to fishing gear table for a list of valid codes and descriptions.
target_species_code | The three letter code for the species (or taxon) at which the vessel was directing its fishing activity. Refer to taxon table for a list of valid codes and names.
asd_code | The area, subarea or division where the fishing occurred, based on FAO Major Fishing Areas. Refer to area table for a list of valid codes and related reference data.
season_ccamlr | The CCAMLR fishing season when the fishing occurred. Currently allocated from the date the fishing activity ends as reported in the haul-by-haul data record.
year | Calendar year when the fishing occurred. Currently allocated from the date the fishing activity ends as reported in the haul-by-haul data record.
month | Month when the fishing occurred. Currently allocated from the date the fishing activity ends as reported in the haul-by-haul data record.
vessel_size_category_code | Code for the size category of the fishing vessels based on gross tonnage. Refer to vessel size table for a list of valid codes and descriptions.
vessel_count | Number of fishing vessels.
fishing_days | Total number of days the vessels were fishing. Currently calculated as the distinct count of dates the fishing activity ends as reported in each of the haul-by-haul data records for each vessel and then summed. Where the haul date is not recorded due to gear loss, the set date is used as the date the fishing activity ends. The summed total is rounded up to the nearest integer to address some historical data which were reported as decimal values.
trawl_duration_hours | Total duration of trawling for each net used by each vessel in hours. Currently calculated as the minutes difference between the end of setting and the start of hauling for each haul-by-haul data record and then summed, converted to hours and rounded to 1 decimal place. Applies to trawl gear types only.
haul_count | Total number of haul-by-haul data records reported. Applies to all gear types.
hook_count | Total number of hooks set. Applies to longline gear types only.
pot_count | Total number of pots set. Applies to pot gear types only.

***

### Area table

A geographical area as defined by CCAMLR.

Field | Description
--- | ---
asd_code | A unique area, subarea or division code, based on FAO Major Fishing Areas.
asd_type | Refers to the type of area (area, subarea or division) in the asd_code field.
asd_area_code | The FAO Major Fishing Area.
asd_subarea_code | The subarea. Blank if the asd_type is area.

***

### Country table

A country or related entity.

Field | Description
--- | ---
country_code | A unique code for the country or entity using three letter ISO 3166-1 country codes.
country | Name of the country or entity.

***

### Fishing gear table

Type of gear used in fishing activities.

Field | Description
--- | ---
gear_type_code | A unique code for the gear type used during fishing.
gear_type | The gear type used during fishing.

***

### Fishing purpose table
  
The purpose of the fishing conducted by a vessel.

* C for commercial fishing
* S for survey and research fishing.

Note Volumes 32 and older also included:

* E for exploratory (as of Volume 33 merged into S)
* U for unknown (as of Volume 33 corrected to C).

Field | Description
--- | ---
fishing_purpose_code | A unique code for the purpose of the fishing activity.
fishing_purpose | The purpose of the fishing activity.

***

### Taxon table

A unit of taxonomy used to define the hierarchy of a biological classification system.

Field | Description
--- | ---
taxon_code | The three letter code for the taxon.
taxon_scientific_name | The full scientific name of the lowest level taxonomic rank that can be determined.
taxon_vernacular_name | The English vernacular name for the taxon.

***

### Vessel size table

Size category of fishing vessels based on gross tonnage. Categories relevant to CCAMLR fishing vessels are:

* 7: 250 to <500 tonnes
* 8: 500 to <1000 tonnes
* 9: 1000 to <2000 tonnes
* 10: 2000 to <4000 tonnes
* 11: 4000 to <10000 tonnes
* 12: 10000 to <100000 tonnes.

Field | Description
--- | ---
vessel_size_category_code | A unique code for the vessel size category.
vessel_size_category | The size category of the fishing vessels based on gross tonnage.
