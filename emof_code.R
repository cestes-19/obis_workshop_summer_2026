# Code used to turn the CMERA data into a emof table to upload to OBIS


# load the packages below


library(dplyr)
library(readxl)
library(writexl)

# Load your data

df <- read_excel(".xlsx")

# input dataset into code below 

emof <- df %>%
  group_by(`Trial  number`) %>%
  mutate(
    eventID      = `Trial  number`,
    occurrenceID = paste0(`Trial  number`, "_", row_number())
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(TL, PcL, Fork, 
             IdL, Trunk, DL, DW),
    names_to  = "measurementType",
    values_to = "measurementValue"
  ) %>%
  filter(!is.na(measurementValue)) %>%
  mutate(
    measurementTypeID = case_when(
      measurementType == "TotalLength"       ~ "http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX",
      measurementType == "PrecaudalLength"   ~ "http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX",
      measurementType == "ForkLength"        ~ "http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX",
      measurementType == "InterdorsalLength" ~ NA_character_,
      measurementType == "Trunk"             ~ NA_character_,
      measurementType == "DiscLength"        ~ NA_character_,
      measurementType == "DiscWidth"         ~ NA_character_
    ),
    measurementUnit   = "cm",
    measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/ULCM"
  ) %>%
  select(eventID, occurrenceID, measurementType, measurementTypeID,
         measurementValue, measurementUnit, measurementUnitID)

# Check formating 

View(emof3)

# the mearument types were based off of our old column names which were 
# abbreviations for Total length, Precadual Length, Fork Length, Inderdorsal 
# Length, Trunch, Disc Length, and Disc Width so preform the steps below to 
# chagne the abbreviations to proper terms.

emof2 <- emof %>%
  mutate(measurementType = case_when(
    measurementType == "TL"   ~ "Total Length",
    measurementType == "PcL"  ~ "Precaudal Length",
    measurementType == "Fork" ~ "Fork Length",
    measurementType == "IdL"  ~ "Interdorsal Length",
    measurementType == "DL"   ~ "Disc Length",
    measurementType == "DW"   ~ "Disc Width",
    TRUE ~ measurementType  # keeps "Trunk" as is
  ))

# Check formatting 

View(emof2)


# ── EXPORT ────────────────────────────────────────────────────────────────────
write_xlsx(emof2,"obis_darwin_core_emof.xlsx")
