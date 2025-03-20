# This file is to manage all the main scripts like geometries and parcels
# The goal is to not build redundant dataframes to limit the effects on memory and speed up necessary processing
hennepin_cities <- read.csv("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/data/ken_data/Hennepin Cities(Report).csv")[, 1:2]
parcel_geometries <- subset(parcels_ag_hmst_acres_met_data,select = c("PIN_2", "year", "geometry"))
st_write(subset(parcel_geometries, year==2022),"parcel_geometries.shp")
parcels_ag_hmst_acres_df_met_data <- st_drop_geometry(parcels_ag_hmst_acres_met_data)
rm(parcels_ag_hmst_acres_met_data)
print(unique(parcels_ag_hmst_acres_df_met_data$year))
#rm(parcels_ag_hmst_acres_df)
#parcels_ag_hmst_acres_sf <- merge()
names(aggregate_combined_data_frame)
names(parcels_ag_hmst_acres_df_met_data)

hennepin_city_geometries <- st_read("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/gpkg_bdry_census2010counties_ctus/bdry_census2010counties_ctus.gpkg",layer = 'Census2010CountiesAndCTUs')
hennepin_city_geometries <- subset(hennepin_city_geometries,CO_NAME == "Hennepin")
hennepin_city_geometries <- st_as_sf(hennepin_city_geometries)
st_write(hennepin_city_geometries,"hennepin_cities.shp")
#create data for ad hoc analysis
combined_data_frame$PID <- as.character(combined_data_frame$PID)
combined_data_frame <- combined_data_frame %>%
  mutate(
    PID = if_else(
      nchar(PID) == 12,            
      paste0("0", PID),            
      PID                         
    )
  )
# parcels_ag_hmst_acres_df_met_data[, PIN_2 := as.character(PIN_2)]

# city_id_name_match <- merge(aggregate_combined_data_frame,parcels_ag_hmst_acres_df_met_data,
#                                                   by.x = c("PID","TAX_YEAR"),by.y = c("PIN_2","year"))
#n_distinct(city_id_name_match)
#n_distinct(aggregate_combined_data_frame)

#dropped <- subset(aggregate_combined_data_frame, !(PID %in% city_id_name_match$PID))

# city_ctu_matches <- city_id_name_match[
#   , .N, by = .(CITY_ID, CTU_NAME)
# ]

# Sort the results for better readability
# city_ctu_matches <- city_ctu_matches[order(-N)]
# city_ctu_matches <- subset(city_ctu_matches,!(is.na(CTU_NAME)), select = c("CITY_ID","CTU_NAME"))
# city_ctu_matches <- subset(city_ctu_matches, !(CITY_ID == 26 & CTU_NAME == "Chanhassen"))

#rm(city_id_name_match)

# unique_pin_count <- parcels_ag_hmst_acres_df_met_data[
#   CTU_NAME %in% c("Shorewood", "Chanhassen"), 
#   .(unique_PIN_2_count = uniqueN(PIN_2)), 
#   by = .(year,CTU_NAME)
# ]
# unique_pin_count

# city_lookup <- hennepin_cities[
#   , .(CTU_NAME = first(NAME)), by = .(CITY_ID)
# ]

combined_data_frame<-as.data.table(combined_data_frame)
combined_data_frame <- copy(combined_data_frame)[
  hennepin_cities, on = .(CITY_ID), CTU_NAME := NAME
]


#rm(parcels_ag_hmst_acres_df_met_data, aggregate_combined_data_frame)
# clean
beep()
combined_data_frame$year <- combined_data_frame$TAX_YEAR

combined_data_frame %>%
  distinct(PID, year, .keep_all = TRUE)

duplicate_count <- combined_data_frame %>%
  group_by(PID,SUBRECORD_NO, year) %>%
  summarize(count = n()) %>%
  filter(count > 1)

combined_data_frame <- combined_data_frame %>%
  distinct(PID,SUBRECORD_NO, year, .keep_all = TRUE)

combined_data_frame$PROPERTY_TYPE <- lapply(
  combined_data_frame$PROPERTY_TYPE, 
  function(row_list) trimws(row_list)
)

# create super groups
#aggregate_combined_data_frame_added_info
# Create property_type_map if not already defined
property_type_map <- tibble::tibble(
  property_type = c(
    "R", "RL", "RZ", "Y", # Residential
    "DB", "TP", "A", "AX", "X", "XC", "XM", "RM", # Multi-Family Residential
    "HL", "HM", "HR", "HS", # Low-Income Housing
    "C", "LC", "SL", # Commercial
    "I", "LI", # Industrial
    "F", "FP", "LF", "FF", "FH", "FM", # Agricultural/Farm
    "LA", "LC", "LI", "LR", "LL", "LV", # Vacant Land
    "SC", "NC", "NI", "GC", "HF", # Special Use
    "ME", "MH" # Manufactured Homes
  ),
  super_type = c(
    "residential", "residential", "residential", "residential",
    "multi_family_residential", "multi_family_residential", "multi_family_residential", 
    "multi_family_residential", "multi_family_residential", "multi_family_residential", 
    "multi_family_residential", "multi_family_residential",
    "low_income_housing", "low_income_housing", "low_income_housing", "low_income_housing",
    "commercial", "commercial", "commercial",
    "industrial", "industrial",
    "agricultural_farm", "agricultural_farm", "agricultural_farm", 
    "agricultural_farm", "agricultural_farm", "agricultural_farm",
    "vacant_land", "vacant_land", "vacant_land", "vacant_land", "vacant_land", "vacant_land",
    "special_use", "special_use", "special_use", "special_use", "special_use",
    "manufactured_homes", "manufactured_homes"
  )
)
unique(property_type_map$super_type)
unique(property_type_map$super_type)
# Add binary columns for each super_type using PROPERTY_TYPE_LIST
super_types <- unique(property_type_map$super_type)

for (type in super_types) {
  relevant_property_types <- property_type_map$property_type[property_type_map$super_type == type]
  print(paste("Super Type:", type))
  print(relevant_property_types)
  # Check if any of the relevant property types are in each PROPERTY_TYPE_LIST
  combined_data_frame[[type]] <- sapply(
    combined_data_frame$PROPERTY_TYPE,
    function(row_list) as.integer(any(relevant_property_types %in% row_list))
  )
}
beep()

#102724110003
combined_data_frame
print(unique(combined_data_frame$PROPERTY_TYPE))
# Backcast CO_NAME and CTU_NAME
# aggregate_combined_data_frame_added_info <- aggregate_combined_data_frame_added_info %>%
#   arrange(PID, year) %>% 
#   group_by(PID) %>% 
#   fill(CO_NAME, CTU_NAME, .direction = "up") %>% 
#   ungroup()

sum(is.na(combined_data_frame$CTU_NAME))
sum(is.na(combined_data_frame$CITY_ID))
sum((combined_data_frame$I))
table(combined_data_frame$super_property_type)

# tax share data
# city_total_revenue <- aggregate_combined_data_frame_added_info %>%
#   group_by(CTU_NAME, year) %>%
#   summarize(
#     total_city_tax = sum(LOCAL_NET_TAX_CAPACITY_TAX, na.rm = TRUE),
#     .groups = "drop"
#   )

# Step 2: Merge total city tax back to the main dataset
# aggregate_combined_data_frame_added_info <- aggregate_combined_data_frame_added_info %>%
#   left_join(city_total_revenue, by = c("CTU_NAME", "year")) %>%
#   mutate(
#     city_tax_share = LOCAL_NET_TAX_CAPACITY_TAX / total_city_tax
#   )

combined_data_frame2 <- combined_data_frame %>%
  mutate(
    super_property_type = case_when(
      residential == 1 ~ "residential",
      multi_family_residential == 1 ~ "multi_family_residential",
      low_income_housing == 1 ~ "low_income_housing",
      commercial == 1 ~ "commercial",
      industrial == 1 ~ "industrial",
      agricultural_farm == 1 ~ "agricultural_farm",
      vacant_land == 1 ~ "vacant_land",
      special_use == 1 ~ "special_use",
      manufactured_homes == 1 ~ "manufactured_homes",
      TRUE ~ "unknown" # Default if no super type is matched
    )
  )

combined_data_frame2[, AG_PRESERVE := as.integer(AG_PRESERVE)]
combined_data_frame2 
beep()

rm(aggregate_combined_data_frame,unique_combinations,unique_pin_count, duplicate_count,hennepin_cities,hennepin_city_geometries,property_type_map)

#### data analysis ####

aggregate_super_groups <- combined_data_frame %>%
  group_by(year)%>%
  summarize(
    LNTC_CITY_SHARE_mean = mean(LNTC_CITY_SHARE,na.rm=T),
    AG_PRESERVE_sum = sum(AG_PRESERVE,na.rm = T)
  )

plot(aggregate_super_groups$AG_PRESERVE_sum,aggregate_super_groups$LNTC_CITY_SHARE_mean)

aggregate_super_groups <- combined_data_frame %>%
  filter(agricultural_farm == 1)%>%
  group_by(CTU_NAME,year, ag_pres_group)%>%
  summarize(
    LNTC_CITY_SHARE_mean = mean(LNTC_CITY_SHARE,na.rm=T),
    AG_PRESERVE_sum = sum(AG_PRESERVE,na.rm = T),
    acres_sum = sum(ACREAGE,na.rm=T)
  )%>%
  filter(CTU_NAME != "Chanhassen")




hist(aggregate_super_groups$AG_PRESERVE_sum)
hist(aggregate_super_groups$LNTC_CITY_SHARE_mean)

ggplot(aggregate_super_groups, aes(x = LNTC_CITY_SHARE_mean)) +
  geom_histogram(bins = 300, fill = "blue", alpha = 0.6)+
  facet_wrap(~ag_pres_group)

plot(log(aggregate_super_groups$AG_PRESERVE_sum+1),aggregate_super_groups$LNTC_CITY_SHARE_mean)
plot(log(aggregate_super_groups$acres_sum+1),aggregate_super_groups$LNTC_CITY_SHARE_mean)


summary(lm(LNTC_CITY_SHARE_mean~log(AG_PRESERVE_sum+1) + acres_sum + as.factor(year),aggregate_super_groups))


#### Zonation data ####

ben_zonation<-st_read("/Users/matthewhockert/Downloads/full_parcels_ranked_final_v1")


#### CPI ####
library(readxl)
cpi <- read_excel("/Users/matthewhockert/Desktop/UMN/hennepin_conservation_easements/hcra_paper/cpi.xlsx")




