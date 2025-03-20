# Use Ken data at bottom of page


library(sf)
library(dplyr)
library(readxl)
library(tidyr)
library(beepr)
library(ggplot2)

#### Don't use #####
years <- 2005:2022

# Define the base file path and file extension pattern
base_file_path <- "data/OneDrive_1_8-13-2024/Conservation Easment Data V2 "
file_extension <- ".xlsx"
#readxl::read_excel("/Users/matthewhockert/Desktop/HCRA/OneDrive_1_8-13-2024/Conservation Easment Data V2 2005.xlsx")
# Function to generate the full file path based on year
get_file_path <- function(year) {
  paste0(base_file_path, year, file_extension)
}
col_types <- c(
  "text",  # TAX_YEAR
  "text",  # PID
  "text",  # SUBRECORD_NO
  "text",  # CITY_ID
  "text",  # SCHOOL_ID
  "text",  # WATERSHED_ID
  "text",
  "text",  # PRIMARY_SECONDARY_CD
  "text",  # HMSTD_COOP_REF_ID
  "text",  # HMSTD_CD
  "text",  # PROPERTY_TYPE
  "numeric",  # ESTIMATED_MARKET_VALUE
  "numeric",  # LOCAL_NET_TAX_CAPACITY
  "numeric",  # LOCAL_NET_TAX_CAPACITY_RATE
  "numeric"   # LOCAL_NET_TAX_CAPACITY_TAX
)
# Function to load and process each Excel file
process_file <- function(file_path, year) {
  # Assuming you want to read the first sheet in each Excel file
  df <- readxl::read_excel(file_path,col_types = col_types)
  return(df)
}

# Load, process, and combine all data frames
processed_data_frames <- lapply(years, function(year) {
  file_path <- get_file_path(year)
  process_file(file_path, year)
})

# Identify all unique column names across all data frames
all_columns <- unique(unlist(lapply(processed_data_frames, names)))

# Function to ensure all data frames have the same columns
ensure_columns <- function(df, all_columns) {
  for (col in all_columns) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df <- subset(df, select = all_columns)
  return(df)
}

# Ensure all data frames have the same columns
processed_data_frames <- lapply(processed_data_frames, ensure_columns, all_columns = all_columns)

# Assuming you have a vector of column names you want to keep
# column_names_to_keep <- c("Column1", "Column2", ...) # Specify the columns you want to keep

# Subset the processed data frames to keep only the desired columns
# subset_data_frames <- lapply(processed_data_frames, function(df) {
#  subset(df, select = column_names_to_keep)
# })

# Combine all processed data frames into one
combined_data_frame <- do.call(rbind, processed_data_frames)
# rm(processed_data_frames,combined_data_frame)

#
#### Ken Text File ####
#/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data
# Move to data_clean.R
combined_data_frame <- read.table("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Conservation Easment Data V4 Main All Years.txt", header = F, sep = "\t", stringsAsFactors = FALSE)
program_links <- read.table("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Conservation Easment Data V3 Program Linkage.txt", header = F, sep = "\t", stringsAsFactors = FALSE)
parcel_sales <- read.table("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Conservation Easment Data V3 Sales.txt", header = F, sep = "\t", stringsAsFactors = FALSE)
city_rate_data <- read.table("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Conservation Easment Data V3 City Rate Data.txt", header = F, sep = "\t", stringsAsFactors = FALSE)

#names
names(combined_data_frame) <- c("TAX_YEAR", "PID", "SUBRECORD_NO", "CITY_ID", 
                                "SCHOOL_ID", "WATERSHED_ID", "CITY_STORM_SEWER_ID", 
                                "EMV_LAND", "EMV_BLDG", "ESTIMATED_MARKET_VALUE", 
                                "LOCAL_NET_TAX_CAPACITY", "LOCAL_NET_TAX_CAPACITY_RATE", 
                                "LNTC_CITY_SHARE", "AG_PRESERVE", "HOMESTEAD", 
                                "PROPERTY_TYPE", "ACREAGE")

names(program_links) <- c("TAX_YEAR", "PID", "SUBRECORD_NO", "PROGRAM", 
                                "PROPERTY_TYPE", "HIGHEMV_LAND", "HIGHEMV_BLDG")

names(parcel_sales) <- c("PID", "SALE_DATE", "SALE_PRICE","SALE_CODE")
parcel_sales$SALE_DATE <- as.Date(paste0(parcel_sales$SALE_DATE, "01"), format = "%Y%m%d")
parcel_sales$SALE_YEAR <-   as.integer(format(parcel_sales$SALE_DATE, "%Y"))

combined_data_frame <- merge(combined_data_frame, program_links, by = c("PID","TAX_YEAR","SUBRECORD_NO","PROPERTY_TYPE"), all.x = T)
# combined_data_frame <- merge(combined_data_frame, parcel_sales, by = c("PID"), all.x = T)


# combined_data_frame$SALE_DATE <- as.Date(as.character(combined_data_frame$SALE_DATE),"YYYY-MM-DD")


print(unique(combined_data_frame$PROGRAM))

names(city_rate_data) <- c("year", "CITY_ID", "CITY_LNTC_TAX_RATE")

#### gary easements PIDs ####
# 
# gary_easements <- read_excel("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/Research Oct Nov 2024/gary_easements.xlsx")
# 
# gary_easements <- gary_easements %>%
#   mutate(
#     PID = if_else(
#       nchar(PID) == 12,            
#       paste0("0", PID),            
#       PID                         
#     )
#   )
# gary_easements_sf <- merge(gary_easements,parcel_geometries,by.x = "PID",by.y = "PIN_2")
# gary_easements_sf <- gary_easements_sf%>%
#   select(-year)
# gary_easements_sf <- gary_easements_sf %>%
#   distinct(PID, .keep_all = TRUE)
# gary_easements_sf <- st_as_sf(gary_easements_sf)
# st_write(gary_easements_sf,"gary_easements_sf.shp")
