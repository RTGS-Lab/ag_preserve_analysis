

#unique_ids <- unique(subset_pid$PIN_2)
#unique_subtype <- unique(subset(combined_data_frame,PID %in%unique_ids)$PROPERTY_TYPE)

# Load necessary library
library(dplyr)
library(tidyr)
library(data.table)
# 
# diagnostic_results <- combined_data_frame %>%
#   group_by(PID, TAX_YEAR) %>%
#   summarize(
#     count = n(),
#     mean_value = mean(ESTIMATED_MARKET_VALUE, na.rm = TRUE),
#     sum_value = sum(ESTIMATED_MARKET_VALUE, na.rm = TRUE),
#     sd_value = sd(ESTIMATED_MARKET_VALUE, na.rm = TRUE), # Standard deviation
#     min_value = min(ESTIMATED_MARKET_VALUE, na.rm = TRUE),
#     max_value = max(ESTIMATED_MARKET_VALUE, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(coefficient_of_variation = sd_value / mean_value) # Calculate CV for variability
# 
# write.csv(diagnostic_results, "diagnostic_results.csv", row.names = FALSE)
rm(diagnostic_results)
names(combined_data_frame)
# [1] "PID"                         "TAX_YEAR"                    "SUBRECORD_NO"               
# [4] "CITY_ID"                     "SCHOOL_ID"                   "WATERSHED_ID"               
# [7] "CITY_STORM_SEWER_ID"         "EMV_LAND"                    "EMV_BLDG"                   
# [10] "ESTIMATED_MARKET_VALUE"      "LOCAL_NET_TAX_CAPACITY"      "LOCAL_NET_TAX_CAPACITY_RATE"
# [13] "LNTC_CITY_SHARE"             "AG_PRESERVE"                 "HOMESTEAD"                  
# [16] "PROPERTY_TYPE.x"             "ACREAGE"                     "PROGRAM"                    
# [19] "PROPERTY_TYPE.y"             "HIGHEMV_LAND"                "HIGHEMV_BLDG"               
# [22] "SALE_DATE"                   "SALE_PRICE"                  "SALE_CODE"                  
# [25] "SALE_YEAR"    
# Parameterize the PROPERTY_TYPE column
# aggregate_combined_data_frame <- combined_data_frame %>%
#   group_by(PID, TAX_YEAR) %>%
#   summarize(
#     CITY_ID = first(CITY_ID),
#     SCHOOL_ID = first(SCHOOL_ID),
#     WATERSHED_ID = first(WATERSHED_ID),
#     AG_PRESERVE = coalesce(first(AG_PRESERVE[!is.na(AG_PRESERVE)]), NA),
#     HOMESTEAD = coalesce(first(HOMESTEAD[!is.na(HOMESTEAD)]), NA),
#     PROGRAM = coalesce(first(PROGRAM[!is.na(PROGRAM)]), NA),
#     ESTIMATED_MARKET_VALUE = sum(ESTIMATED_MARKET_VALUE, na.rm = TRUE), 
#     LOCAL_NET_TAX_CAPACITY = sum(LOCAL_NET_TAX_CAPACITY, na.rm = TRUE),
#     LNTC_CITY_SHARE = mean(LNTC_CITY_SHARE, na.rm = TRUE),
#     EMV_LAND = sum(EMV_LAND, na.rm = TRUE),
#     EMV_BLDG = sum(EMV_BLDG, na.rm = TRUE),
#     HIGHEMV_LAND = sum(HIGHEMV_LAND, na.rm = TRUE),
#     HIGHEMV_BLDG = sum(HIGHEMV_BLDG, na.rm = TRUE),
#     LOCAL_NET_TAX_CAPACITY_RATE = mean(LOCAL_NET_TAX_CAPACITY_RATE, na.rm = TRUE), 
#     ACREAGE = sum(ACREAGE,na.rm = T),
#     SALE_DATE = coalesce(first(SALE_DATE[!is.na(SALE_DATE)]), NA),
#     SALE_PRICE = sum(SALE_PRICE,na.rm = T),
#     SALE_CODE = coalesce(first(SALE_CODE[!is.na(SALE_CODE)]), NA),
#     SALE_YEAR = coalesce(first(SALE_YEAR[!is.na(SALE_YEAR)]), NA),
#     PROPERTY_TYPE_LIST = PROPERTY_TYPE, 
#     .groups = 'drop'
#   ) %>%
#   rowwise() %>%
#   mutate(
#     PROPERTY_TYPE_FLAGS = list(setNames(rep(1, length(PROPERTY_TYPE_LIST)), PROPERTY_TYPE_LIST))
#   ) %>%
#   unnest_wider(PROPERTY_TYPE_FLAGS, names_sep = "_") %>%
#   ungroup()
# #rm(combined_data_frame)

beep()
# fix zeroes
# convert_to_zero <- function(x) {
#   ifelse(is.na(x),0,1)
# }
names(combined_data_frame)
combined_data_frame <- combined_data_frame %>%
  mutate(across(starts_with("PROPERTY_TYPE_FLAGS_"), convert_to_zero))
sum(is.na(combined_data_frame$PROPERTY_TYPE))
# colSums(is.na(aggregate_combined_data_frame %>% select(starts_with("PROPERTY_TYPE_FLAGS_"))))
# head(aggregate_combined_data_frame %>% select(starts_with("PROPERTY_TYPE_FLAGS_")))
# colSums((aggregate_combined_data_frame %>% select(starts_with("PROPERTY_TYPE_FLAGS_"))))

# Move to Main.R
#### dt approach ####
options(datatable.showProgress = TRUE)
options(datatable.verbose = T)

dt <- as.data.table(combined_data_frame)

# Perform the aggregation
aggregate_combined_data_frame <- dt[
  , .(
    CITY_ID = first(CITY_ID),
    SCHOOL_ID = first(SCHOOL_ID),
    WATERSHED_ID = first(WATERSHED_ID),
    AG_PRESERVE = first(AG_PRESERVE[!is.na(AG_PRESERVE)]),
    HOMESTEAD = first(HOMESTEAD[!is.na(HOMESTEAD)]),
    PROGRAM = first(PROGRAM[!is.na(PROGRAM)]),
    ESTIMATED_MARKET_VALUE = sum(ESTIMATED_MARKET_VALUE, na.rm = TRUE),
    LOCAL_NET_TAX_CAPACITY = sum(LOCAL_NET_TAX_CAPACITY, na.rm = TRUE),
    LNTC_CITY_SHARE = mean(LNTC_CITY_SHARE, na.rm = TRUE),
    EMV_LAND = sum(EMV_LAND, na.rm = TRUE),
    EMV_BLDG = sum(EMV_BLDG, na.rm = TRUE),
    HIGHEMV_LAND = sum(HIGHEMV_LAND, na.rm = TRUE),
    HIGHEMV_BLDG = sum(HIGHEMV_BLDG, na.rm = TRUE),
    LOCAL_NET_TAX_CAPACITY_RATE = mean(LOCAL_NET_TAX_CAPACITY_RATE, na.rm = TRUE),
    ACREAGE = sum(ACREAGE, na.rm = TRUE),
    SALE_DATE = first(SALE_DATE[!is.na(SALE_DATE)]),
    SALE_PRICE = sum(SALE_PRICE, na.rm = TRUE),
    SALE_CODE = first(SALE_CODE[!is.na(SALE_CODE)]),
    SALE_YEAR = first(SALE_YEAR[!is.na(SALE_YEAR)]),
    PROPERTY_TYPE_LIST = list(unique(PROPERTY_TYPE))
  ),
  by = .(PID, TAX_YEAR)
]
beep()
print(unique(aggregate_combined_data_frame$PROPERTY_TYPE_LIST))

aggregate_combined_data_frame_PID_YEAR_PROPERTY_TYPE <- subset(aggregate_combined_data_frame,select = c("PID","TAX_YEAR","PROPERTY_TYPE_LIST"))
print(unique(aggregate_combined_data_frame_PID_YEAR_PROPERTY_TYPE$PROPERTY_TYPE_LIST))

aggregate_combined_data_frame_PID_YEAR_PROPERTY_TYPE[
  , PROPERTY_TYPE_LIST := lapply(PROPERTY_TYPE_LIST, function(x) trimws(as.character(x)))
]
print(unique(aggregate_combined_data_frame_PID_YEAR_PROPERTY_TYPE$PROPERTY_TYPE_LIST))

# Step 2: Create a long-format data table (one row per property type per PID/TAX_YEAR)
long_format <- aggregate_combined_data_frame_PID_YEAR_PROPERTY_TYPE[
  , .(PROPERTY_TYPE = unlist(PROPERTY_TYPE_LIST)),
  by = .(PID, TAX_YEAR)
]
print(unique(aggregate_combined_data_frame_PID_YEAR_PROPERTY_TYPE$PROPERTY_TYPE_LIST))

# Step 3: Cast the long format back to wide format, creating one column per property type
wide_format <- dcast(
  long_format, 
  PID + TAX_YEAR ~ PROPERTY_TYPE, 
  fun.aggregate = length, 
  value.var = "PROPERTY_TYPE"
)

# Step 4: Replace counts with binary flags (if needed)
wide_format[, (names(wide_format)[3:ncol(wide_format)]) := lapply(.SD, function(x) as.integer(x > 0)), .SDcols = 3:ncol(wide_format)]

aggregate_combined_data_frame <- merge(aggregate_combined_data_frame,wide_format,by=c("PID","TAX_YEAR"))

rm(dt,wide_format,long_format,aggregate_combined_data_frame_PID_YEAR_PROPERTY_TYPE,combined_data_frame,parcel_sales,program_links)
beep()
