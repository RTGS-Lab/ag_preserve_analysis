library(sf)
library(dplyr)
library(readxl)
library(tidyr)
library(beepr)
library(ggplot2)
library(data.table)

# ---- Load and Prepare Geographic Reference Data ----
# 'hennepin_cities' is used later to map CITY_ID to city names (CTU_NAME)
hennepin_cities <- fread("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Hennepin Cities(Report).csv")[, 1:2]

# 'hennepin_city_geometries' provides spatial data for Hennepin County boundaries used in mapping/analysis.
hennepin_city_geometries <- st_read(
  "/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/gpkg_bdry_census2010counties_ctus/bdry_census2010counties_ctus.gpkg",
  layer = 'Census2010CountiesAndCTUs'
)
hennepin_city_geometries <- subset(hennepin_city_geometries, CO_NAME == "Hennepin")
hennepin_city_geometries <- st_as_sf(hennepin_city_geometries)

# ---- Prepare Parcel Data for Analysis ----
# 'combined_data_frame' holds parcel-level data used for ad hoc analysis and later processing.

# Ensure PID is character and pad with a leading zero if needed for consistency.
setDT(combined_data_frame)
combined_data_frame[, PID := as.character(PID)]
combined_data_frame[, PID := if_else(nchar(PID) == 12, paste0("0", PID), PID)]

# Use fast data.table join to map city names from 'hennepin_cities' onto combined_data_frame by CITY_ID.
setDT(hennepin_cities)
setkey(combined_data_frame, CITY_ID)
setkey(hennepin_cities, CITY_ID)
combined_data_frame[hennepin_cities, CTU_NAME := i.NAME]

# Create a 'year' column from TAX_YEAR for later temporal analysis.
combined_data_frame[, year := TAX_YEAR]

# Remove duplicate parcel entries (by PID, SUBRECORD_NO, and year) to reduce processing overhead.
combined_data_frame <- unique(combined_data_frame, by = c("PID", "SUBRECORD_NO", "year"))

# Trim extra whitespace from the PROPERTY_TYPE column.
combined_data_frame[, PROPERTY_TYPE := trimws(PROPERTY_TYPE)]

# ---- Map Specific Property Types to Broader Super Types ----
# 'property_type_map' defines the mapping from specific property types to aggregated "super types"
property_type_map <- data.table(
  property_type = c(
    "R", "RL", "RZ", "Y", "DB", "TP", "A", "AX", "X", "XC", "XM", "RM",
    "HL", "HM", "HR", "HS", "C", "LC", "SL", "I", "LI",
    "F", "FP", "LF", "FF", "FH", "FM", "LA", "LC", "LI", "LR", "LL", "LV",
    "SC", "NC", "NI", "GC", "HF", "ME", "MH"
  ),
  super_type = c(
    rep("residential", 4),
    rep("multi_family_residential", 7),
    rep("low_income_housing", 4),
    rep("commercial", 3),
    rep("industrial", 2),
    rep("agricultural_farm", 6),
    rep("vacant_land", 6),
    rep("special_use", 5),
    rep("manufactured_homes", 2)
  )
)

# For each super type, add a binary indicator column to combined_data_frame.
super_types <- unique(property_type_map$super_type)
for (type in super_types) {
  # Get the specific property types for this super type.
  relevant_property_types <- property_type_map[super_type == type, property_type]
  # Create a binary flag: 1 if PROPERTY_TYPE (for the parcel) is in the relevant list, 0 otherwise.
  combined_data_frame[, (type) := as.integer(sapply(PROPERTY_TYPE, function(x) any(x %in% relevant_property_types)))]
}

# Create a consolidated 'super_property_type' column for later grouping/analysis.
combined_data_frame[, super_property_type := fifelse(residential == 1, "residential",
                                            fifelse(multi_family_residential == 1, "multi_family_residential",
                                            fifelse(low_income_housing == 1, "low_income_housing",
                                            fifelse(commercial == 1, "commercial",
                                            fifelse(industrial == 1, "industrial",
                                            fifelse(agricultural_farm == 1, "agricultural_farm",
                                            fifelse(vacant_land == 1, "vacant_land",
                                            fifelse(special_use == 1, "special_use",
                                            fifelse(manufactured_homes == 1, "manufactured_homes", "unknown")))))))))]

# Convert AG_PRESERVE to integer for subsequent numerical analyses.
combined_data_frame[, AG_PRESERVE := as.integer(AG_PRESERVE)]

# Sound a beep to signal completion of parcel data processing.
beep()

# ---- Load Additional Data for Spatial and Financial Analysis ----
# 'ben_zonation' contains spatial ranking data used later for zoning or conservation prioritization.
#ben_zonation <- st_read("/Users/matthewhockert/Downloads/full_parcels_ranked_final_v1")

# 'cpi' is used for inflation adjustments in financial calculations (e.g., deflating tax or property values).
cpi <- read_excel("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/cpi.xlsx")

# ---- Clean Up Environment ----
# Remove temporary objects to free memory.
rm(property_type_map, hennepin_cities, hennepin_city_geometries)