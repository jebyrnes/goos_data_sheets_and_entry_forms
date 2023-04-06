#' -----------------------------------------------------------
#' A general R script to turn data entered using the GOOS data
#' entry format for subtidal point count data
#' into clean data for GOOS/OBIS ingestion
#' 
#' This script is built for sample data based on an adaptation of
#' data from the Kelp Ecosystem Ecology Network Of New England
#' as if it were the 30m GOOS format (they are similar save in 
#' transect length). This provides a useful example for any research 
#' program to build off of in order to get their data into the GOOS
#' data format.
#' 
#' Author: Jarrett Byrnes jarrett.byrnes@umb.edu
#' Last updated: 2023-01-16
#' -----------------------------------------------------------

# load libraries needed
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(geosphere) #for calculating bearing


# load data
protocol_info <- read_excel("KEEN 30m Subtidal Example/GOOS_EOV_Protocol_Desciptor_Form.xlsx")
admin_info <- read_excel("KEEN 30m Subtidal Example/GOOS_EOV_Administrative_Info.xlsx")
observer_info <- read_excel("KEEN 30m Subtidal Example/GOOS_EOV_Administrative_Info.xlsx",
                            sheet = 2)
species_list <- read_excel("KEEN 30m Subtidal Example/GOOS_EOV_Species_List.xlsx")
site_info <- read_excel("KEEN 30m Subtidal Example/GOOS_Subtidal_Site_Info_Template_v1.xlsx") |>
    select(Year, Month, Day, Site, Block, `Start Latitude`, `Start Longitude`,
           `End Latitude`, `End Longitude`)
point_count_data <- read_excel("KEEN 30m Subtidal Example/GOOS_Subtidal_Point_Count_template_v1.xlsx")

# compress quad data to 1 point per quadrat instead of squares
point_count_with_species <- point_count_data |>
    pivot_longer(c(`Species Code 0`:`Species Code 5`), 
                 names_to = "Individual",
                 values_to = "Species Code") |>
    filter(!is.na(`Species Code`)) |>
    left_join(species_list) |>
    mutate(Quadrat = NA,
           `Measurement Unit` = "Percent Cover")



point_count_data_summed <- point_count_with_species |>
    group_by(`Collection Method`,
             Year, Month, Day, Site, Block, 
             Quadrat,
             `Name`,
             `Measurement Unit`, 
             Observer,
             `Depth at 0m`,
             `Depth at 30m`,
             Classification,
             Visibility,
             Vocabulary, 
             `CATAMI Name`,
             `CATAMI Version`,
             `CATAMI Unique Identifier`) |>
    summarize(Measurement = n()/60*100, #max of 100% out of 60 points
              .groups = "drop",
              Depth = mean(c(`Depth at 0m`, `Depth at 30m`))) |>
    mutate(`Transect Side` = NA)


# add site info and admin info
point_count_with_info <- point_count_data_summed |>
    left_join(site_info) |>
    left_join(observer_info) |>
    bind_cols(admin_info) |>
    mutate(`Direction of Transect` = bearing(
        cbind(`Start Latitude`, `Start Longitude`),
        cbind(`End Latitude`, `End Longitude`)),
        Project = "KEEN_30_Example",
        UniqueID = paste(Project, `Collection Method`,
                         Year, Month, Day, Site, Block, 
                         sep = "_"),
    )



# begin reformating to GOOS Macroalgal EOV Template
# to get them in the right order, then do a wee bit of renaming
clean_data <- point_count_with_info |>
    select(UniqueID, `Start Latitude`, `Start Longitude`,
           Site, Day, Month, Year, `Collection Method`,
           Block, `Transect Side`, Quadrat, 
           Depth, Visibility, `End Latitude`, `End Longitude`,
           Vocabulary,  Name,
           `CATAMI Unique Identifier`, `CATAMI Name`, `CATAMI Version`,
           `Measurement Unit`, Measurement, 
           Observer, `Observer ID`, `Observer ID Type`,
           Organisation, `Organisation ID`, `Organisation ID Type`,
           Project, `Data Custodian`, `Data Contact email`, 
           `Data Custodian ID`, `Data Custodian ID Type`
    ) |>
    rename(`Transect ID` = Block,
           Latitude = `Start Latitude`,
           Longitude = `Start Longitude`,
    )

write_csv(clean_data, "example clean datasets/keen_30m_macroalgal_cover_eov_example.csv")

