#' -----------------------------------------------------------
#' A general R script to turn data entered using the GOOS data
#' entry format for subtidal quadrat data
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
quad_data <- read_excel("KEEN 30m Subtidal Example/GOOS_Subtidal_Quad_Entry_Template_v1.xlsx")

# combine juvenile and adults in quad data

# compress quad data to 1 point per quadrat instead of squares
quad_data_with_species <- quad_data |>
    pivot_longer(c(`Q0 Off`:`Q30 In`), 
                 names_to = "Quadrat",
                 values_to = "Measurement") |>
    left_join(species_list) |>
    mutate(`Measurement Unit` = "Number per Unit")


quad_data_summed <-  quad_data_with_species |>
    group_by(`Collection Method`,
             Year, Month, Day, Site, Block, 
             Quadrat, 
             `Name`,
             `Measurement Unit`, 
             Observer,
             `Depth at 0m`,
             `Depth at 30m`,
             Visibility,
             Vocabulary, 
             `CATAMI Name`,
             `CATAMI Version`,
             `CATAMI Unique Identifier`) |>
    summarize(Measurement = sum(Measurement, na.rm = TRUE), #adds to 100
              .groups = "drop",
              Depth =mean(c(`Depth at 0m`, `Depth at 30m`))) |>
    mutate(`Transect Side` = gsub("(^.* )", "", Quadrat))
    
    


# add site info and admin info
quad_data_with_info <- quad_data_summed |>
    left_join(site_info) |>
    left_join(observer_info) |>
    bind_cols(admin_info) |>
    mutate(`Direction of Transect` = bearing(
        cbind(`Start Latitude`, `Start Longitude`),
        cbind(`End Latitude`, `End Longitude`)),
        Project = "KEEN_30_Example",
        UniqueID = paste(Project, `Collection Method`,
                         Year, Month, Day, Site, Block, 
                         Quadrat, sep = "_"),
    )


# begin reformating to GOOS Macroalgal EOV Template
# to get them in the right order, then do a wee bit of renaming
clean_data <- quad_data_with_info |>
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

write_csv(clean_data, "example clean datasets/keen_30m_macroalgal_quad_eov_example.csv")
