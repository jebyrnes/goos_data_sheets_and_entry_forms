#' -----------------------------------------------------------
#' A general R script to turn data entered using the GOOS intertidal
#' data entry format into clean data for GOOS/OBIS ingestion
#' 
#' This script is built for sample data from the Stone Living 
#' Lab at UMass Boston, but should be easily generalized
#' to any other dataset as long as it is formatted using the 
#' GOOS general templates.
#' 
#' Author: Jarrett Byrnes jarrett.byrnes@umb.edu
#' Last updated: 2023-01-16
#' -----------------------------------------------------------

# load libraries needed
library(readxl)
library(dplyr)
library(readr)

# load data
protocol_info <- read_excel("Stone Livng Lab Intertidal Example/GOOS_EOV_Protocol_Desciptor_Form.xlsx")
admin_info <- read_excel("Stone Livng Lab Intertidal Example/GOOS_EOV_Administrative_Info.xlsx")
observer_info <- read_excel("Stone Livng Lab Intertidal Example/GOOS_EOV_Administrative_Info.xlsx",
                            sheet = 2)
species_list <- read_excel("Stone Livng Lab Intertidal Example/GOOS_EOV_Species_List.xlsx")
quad_data <- read_excel("Stone Livng Lab Intertidal Example/GOOS_Intertidal_Form_v1.xlsx")

# compress quad data to 1 point per quadrat instead of squares
quad_data_summed <- quad_data |>
    group_by(`Collection Method`,
             Year, Month, Day, Site, Block, `Shore height`,
             Quadrat, `Quadrat Latitude`, `Quadrat Longitude`, 
             Slope, Aspect, `Abiotic Substratum`, Relief,
             `Maximum canopy height`, `Species Code`,
             `Measurement Unit`, Observer) |>
    summarize(Measurement = sum(Measurement, na.rm = TRUE), #adds to 100
              .groups = "drop") |>
    mutate(UniqueID = paste(`Collection Method`,
                      Year, Month, Day, Site, Block, `Shore height`,
                      Quadrat, sep = "_"))

# add species and admin info to quad data
quad_data_with_info <- left_join(
    quad_data_summed,
    species_list,
    by = "Species Code"
) |>
    left_join(observer_info,
              by = "Observer") |>
    bind_cols(admin_info)

# begin reformating to GOOS Macroalgal EOV Template
# to get them in the right order, then do a wee bit of renaming
clean_data <- quad_data_with_info |>
    select(UniqueID, `Quadrat Latitude`, `Quadrat Longitude`,
           Site, Day, Month, Year, `Collection Method`,
           Block, Quadrat, `Shore height`, Slope, Aspect,
           `Maximum canopy height`, #subtidal variables
           `Abiotic Substratum`, 
           Vocabulary, `Species Code`, Name,
           `CATAMI Unique Identifier`, `CATAMI Name`, `CATAMI Version`,
           `Measurement Unit`, Measurement, 
           Observer, `Observer ID`, `Observer ID Type`,
           Organisation, `Organisation ID`, `Organisation ID Type`,
           Project, `Data Custodian`, `Data Contact email`, 
           `Data Custodian ID`, `Data Custodian ID Type`
           ) |>
    rename(Latitude = `Quadrat Latitude`,
           Longitude = `Quadrat Longitude`,
           )

write_csv(clean_data, "example clean datasets/sll_intertidal_macroalgal_eov_example.csv")

