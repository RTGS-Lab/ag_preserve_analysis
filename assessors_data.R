library(sf)
library(dplyr)
library(readxl)
library(tidyr)
library(beepr)
library(ggplot2)
library(data.table)

# ---- Load Ken Data Files for Conservation Easements ----
# 'combined_data_frame' is Ken's main conservation easement dataset used for further analysis.
combined_data_frame <- fread(
  "/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Conservation Easment Data V4 Main All Years.txt",
  sep = "\t", header = FALSE
)

# 'program_links' provides program linkage data for easements; it will be merged with the main dataset.
program_links <- fread(
  "/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Conservation Easment Data V3 Program Linkage.txt",
  sep = "\t", header = FALSE
)

# 'parcel_sales' contains sales transaction data for parcels (currently optional for merging).
parcel_sales <- fread(
  "/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Conservation Easment Data V3 Sales.txt",
  sep = "\t", header = FALSE
)

# 'city_rate_data' holds city-level tax rate information for later financial analysis.
city_rate_data <- fread(
  "/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Conservation Easment Data V3 City Rate Data.txt",
  sep = "\t", header = FALSE
)

# ---- Assign Column Names for Clarity ----
# Main conservation easement dataset: each column is defined for subsequent processing.
setnames(combined_data_frame, c(
  "TAX_YEAR", "PID", "SUBRECORD_NO", "CITY_ID",
  "SCHOOL_ID", "WATERSHED_ID", "CITY_STORM_SEWER_ID",
  "EMV_LAND", "EMV_BLDG", "ESTIMATED_MARKET_VALUE",
  "LOCAL_NET_TAX_CAPACITY", "LOCAL_NET_TAX_CAPACITY_RATE",
  "LNTC_CITY_SHARE", "AG_PRESERVE", "HOMESTEAD",
  "PROPERTY_TYPE", "ACREAGE"
))

# Program linkage data: links each easement to a specific program.
setnames(program_links, c(
  "TAX_YEAR", "PID", "SUBRECORD_NO", "PROGRAM",
  "PROPERTY_TYPE", "HIGHEMV_LAND", "HIGHEMV_BLDG"
))

# Parcel sales data: includes sales date, price, and a sales code.
# setnames(parcel_sales, c("PID", "SALE_DATE", "SALE_PRICE", "SALE_CODE"))
# Convert SALE_DATE to Date format and extract the sale year.
# parcel_sales[, SALE_DATE := as.Date(paste0(SALE_DATE, "01"), format = "%Y%m%d")]
# parcel_sales[, SALE_YEAR := as.integer(format(SALE_DATE, "%Y"))]

# City tax rate data: assign column names for further tax-related analysis.
setnames(city_rate_data, c("year", "CITY_ID", "CITY_LNTC_TAX_RATE"))

# ---- Merge Program Linkage Data into the Main Dataset ----
# This merge links each conservation easement in 'combined_data_frame' with its program information.
setkey(combined_data_frame, PID, TAX_YEAR, SUBRECORD_NO, PROPERTY_TYPE)
setkey(program_links, PID, TAX_YEAR, SUBRECORD_NO, PROPERTY_TYPE)
combined_data_frame <- merge(
  combined_data_frame, program_links,
  by = c("PID", "TAX_YEAR", "SUBRECORD_NO", "PROPERTY_TYPE"),
  all.x = TRUE
)

# Optional merge for parcel sales data:
# This would join sales transaction information to the main dataset based on PID.
# NOT NECESSARY FOR THIS ANALYSIS
# setkey(combined_data_frame, PID)
# setkey(parcel_sales, PID)
# combined_data_frame <- merge(combined_data_frame, parcel_sales, by = "PID", all.x = TRUE)

# ---- Verify Merged Program Data ----
# Print unique program names from the merged dataset for data validation.
print(unique(combined_data_frame$PROGRAM))
