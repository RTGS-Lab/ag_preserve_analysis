# WHat is the effect of ag. preserve, green acres, and homestead on tax burden
# pull all ag. preserve, green acres, and homestead parcels
# merge with 
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(beepr)
library(purrr)
#library(patchwork)



#aggregate_combined_data_frame_added_info comes from main.R - this is the script we come back to

print(unique(combined_data_frame2$PROGRAM))
columns_to_drop <- c("A", "AX", "B", "BJ", "C", "CR", "CT", "D", "DB", "DJ", 
                     "F", "FF", "FH", "FM", "FP", "GC", "HF", "HL", "HM", 
                     "HR", "HS", "I", "K", "LA", "LC", "LF", "LI", "LL", 
                     "LM", "LR", "LV", "ME", "MH", "NC", "ND", "NH", "NI", 
                     "NP", "P", "R", "RL", "RM", "RZ", "S", "SC", "SL", 
                     "SM", "SR", "TP", "U", "X", "XC", "XM", "Y", "Z")

names(combined_data_frame2)
# combined_data_frame3 <- combined_data_frame2 %>%
#   select(-all_of(columns_to_drop))

# G	Green Acres
# R	Rural Preserve
# P	Platted Land
# O	Open Space

combined_data_frame3 <- combined_data_frame2 %>%
  mutate(value = ifelse(PROGRAM %in% c("G", "R", "P", "O"), 1, 0)) %>% # Create a value column for valid PROGRAMs
  pivot_wider(
    names_from = PROGRAM,
    values_from = value,
    values_fill = list(value = 0))

colnames(combined_data_frame3) <- colnames(combined_data_frame3) %>%
  sub("^G$", "G_Green_Acres", .) %>%
  sub("^R$", "R_Rural_Preserve", .) %>%
  sub("^P$", "P_Platted_Land", .) %>%
  sub("^O$", "O_Open_Space", .)

names(combined_data_frame3)

# parcels_ag_hmst_acres_df_2 <-merge(parcels_ag_hmst_acres_df,aggregate_combined_data_frame_added_info, by.x = c("PID","year"),by.y=c("PID","year_numeric"))
parcels_ag_hmst_acres_df_2 <- combined_data_frame3 %>%
  mutate(homestead_stan = case_when(
    HOMESTEAD %in% c("TRUE") ~ 1,  
    HOMESTEAD %in% c("FALSE") ~ 0,   
    TRUE ~ NA_integer_                    
  ))
beep()
#rm(parcels_ag_hmst_acres_df_met_data,aggregate_combined_data_frame_added_info_pivot,aggregate_combined_data_frame_added_info_2)

cities_with_programs <- parcels_ag_hmst_acres_df_2 %>%
  filter(G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1) %>%
  distinct(CTU_NAME)

# hennepin_city_geometries$CTU_NAME <- ifelse(hennepin_city_geometries$CTU_NAME == "Hassan Twp.","Hassan Township",hennepin_city_geometries$CTU_NAME)
# 
# hennepin_city_sub <- subset(hennepin_city_geometries,CTU_NAME %in%cities_with_programs$CTU_NAME)%>%
#   filter(!(CTU_NAME %in% c("Minneapolis","Bloomington","Minnetonka","Brooklyn Center",	
#                            "Champlin")))
# hennepin_city_sub <-st_as_sf(hennepin_city_sub)
# plot(hennepin_city_sub$geom)
# print(unique(hennepin_city_sub$CTU_NAME))

# ggplot(data = hennepin_city_geometries) +
#   geom_sf(fill = "lightblue", color = "black") +  # Plot city polygons
#   geom_sf_text(aes(label = CTU_NAME), size = 3, color = "black")

cities_with_programs_sum <- parcels_ag_hmst_acres_df_2 %>%
  filter(G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1) %>%
  group_by(CTU_NAME) %>%
  summarise(
    num_programs = sum(G_Green_Acres == 1, na.rm = TRUE) +
      sum(AG_PRESERVE == 1, na.rm = TRUE) +
      sum(R_Rural_Preserve == 1, na.rm = TRUE)
  )

parcels_ag_hmst_acres_df_2 <- parcels_ag_hmst_acres_df_2 %>%
  filter(CTU_NAME %in% cities_with_programs$CTU_NAME)%>%
  filter(!(CTU_NAME %in% c("Minneapolis","Bloomington","Eden Prairie","Minnetonka","Plymouth","Brooklyn Center",	
                           "Maple Grove","Brooklyn Park","Champlin")))
print(unique(parcels_ag_hmst_acres_df_2$CTU_NAME))

#311923310003
#test <- subset(parcels_ag_hmst_acres_df_2, PID == "311923310003")

fill_non_zero <- function(values) {
  for (i in seq_along(values)) {
    if (values[i] == 0 && i < length(values)) {
      next_non_zero <- values[(i + 1):length(values)][values[(i + 1):length(values)] != 0][1]
      if (!is.na(next_non_zero)) {
        values[i] <- next_non_zero
      }
    }
  }
  return(values)
}

# adjusted_data <- test %>%
#   group_by(PID) %>% # Group by unique parcel ID
#   arrange(year) %>% # Ensure the data is sorted by year
#   mutate(ACREAGE = fill_non_zero(ACREAGE)) %>% # Apply the fill_non_zero function
#   ungroup()
print(unique(parcels_ag_hmst_acres_df_2$SUBRECORD_NO))
table(parcels_ag_hmst_acres_df_2$SUBRECORD_NO)

parcels_ag_hmst_acres_df_2 <- parcels_ag_hmst_acres_df_2 %>%
  group_by(PID) %>% 
  arrange(year) %>% 
  mutate(ACREAGE = fill_non_zero(ACREAGE)) %>% 
  ungroup()


parcels_ag_hmst_acres_df_2 <- parcels_ag_hmst_acres_df_2 %>%
  mutate(
    PID = if_else(
      nchar(PID) == 12,           
      paste0("0", PID),            
      PID                         
    )
  )

parcels_ag_hmst_acres_df_2 <- parcels_ag_hmst_acres_df_2 %>%
  mutate(PID_SUBRECORD = paste0(PID, "_", SUBRECORD_NO))

table(parcels_ag_hmst_acres_df_2$super_property_type,parcels_ag_hmst_acres_df_2$AG_PRESERVE)
table(parcels_ag_hmst_acres_df_2$super_property_type,parcels_ag_hmst_acres_df_2$R_Rural_Preserve)
table(parcels_ag_hmst_acres_df_2$super_property_type,parcels_ag_hmst_acres_df_2$G_Green_Acres)


# Verify the results
#table(parcels_ag_hmst_acres_df_2$PID_SUBRECORD)


#### Calculate dollar taxes to City #####

parcels_ag_hmst_acres_df_2 <- parcels_ag_hmst_acres_df_2 %>%
  mutate(
    CITY_LNTC_TAX_RATE = city_rate_data$CITY_LNTC_TAX_RATE[match(
      paste(year, CITY_ID), 
      paste(city_rate_data$year, city_rate_data$CITY_ID)
    )],
    CITY_LNTC_TAX = LOCAL_NET_TAX_CAPACITY * CITY_LNTC_TAX_RATE
  )

#cpi deflator
base_cpi <- cpi$Annual[cpi$Year == 2005]

parcels_ag_hmst_acres_df_2
parcels_ag_hmst_acres_df_2$CPI <- cpi$Annual[match(parcels_ag_hmst_acres_df_2$TAX_YEAR, cpi$Year)]

# Adjust for inflation
parcels_ag_hmst_acres_df_2$REAL_CITY_TAX_DOLLAR <- parcels_ag_hmst_acres_df_2$CITY_LNTC_TAX * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)
parcels_ag_hmst_acres_df_2$REAL_EMV_LAND <- parcels_ag_hmst_acres_df_2$EMV_LAND * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)
parcels_ag_hmst_acres_df_2$REAL_EMV_BLDG <- parcels_ag_hmst_acres_df_2$EMV_BLDG * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)
parcels_ag_hmst_acres_df_2$REAL_LOCAL_NET_TAX_CAPACITY <- parcels_ag_hmst_acres_df_2$LOCAL_NET_TAX_CAPACITY * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)
parcels_ag_hmst_acres_df_2$REAL_ESTIMATED_MARKET_VALUE <- parcels_ag_hmst_acres_df_2$ESTIMATED_MARKET_VALUE * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)

avg_tax_data <- parcels_ag_hmst_acres_df_2 %>%
  group_by(TAX_YEAR) %>%
  summarise(
    Avg_REAL_CITY_TAX_DOLLAR = mean(REAL_CITY_TAX_DOLLAR, na.rm = TRUE),
    Avg_CITY_LNTC_TAX = mean(CITY_LNTC_TAX, na.rm = TRUE),
    Avg_REAL_EMV_LAND = mean(REAL_EMV_LAND, na.rm = TRUE),
    Avg_EMV_LAND = mean(EMV_LAND, na.rm = TRUE),
    Avg_REAL_ESTIMATED_MARKET_VALUE = mean(REAL_ESTIMATED_MARKET_VALUE, na.rm = TRUE),
    Avg_ESTIMATED_MARKET_VALUE = mean(ESTIMATED_MARKET_VALUE, na.rm = TRUE),
  )

ggplot(avg_tax_data, aes(x = TAX_YEAR)) +
  geom_line(aes(y = Avg_REAL_CITY_TAX_DOLLAR, color = "Avg_REAL_CITY_TAX_DOLLAR"), size = 1) + 
  geom_line(aes(y = Avg_CITY_LNTC_TAX, color = "Avg_CITY_LNTC_TAX"), size = 1) + 
  labs(
    title = "Average Real City Tax Dollar and City LNTC Tax Over Time",
    x = "Year",
    y = "Average Tax Amount",
    color = "Tax Type"
  ) +
  theme_minimal()

ggplot(avg_tax_data, aes(x = TAX_YEAR)) + 
  geom_line(aes(y = Avg_REAL_EMV_LAND, color = "Avg Real EMV Land"), size = 1) + 
  geom_line(aes(y = Avg_EMV_LAND, color = "EMV Land"), size = 1) + 
  labs(
    title = "Average Real EMV Dollar and City EMV Over Time",
    x = "Year",
    y = "Average EMV Amount",
    color = "Tax Type"  # Legend title
  ) + 
  theme_minimal()

# ggplot(avg_tax_data, aes(x = TAX_YEAR)) + 
#   geom_line(aes(y = Avg_REAL_SALE_PRICE, color = "Avg_REAL_SALE_PRICE"), size = 1) + 
#   geom_line(aes(y = Avg_SALE_PRICE, color = "Avg_SALE_PRICE"), size = 1) + 
#   labs(
#     title = "Average Real Sales Dollar and City Sales Over Time",
#     x = "Year",
#     y = "Average Sale Amount",
#     color = "Tax Type"  # Legend title
#   ) + 
#   theme_minimal()


avg_tax_data_treatment <- parcels_ag_hmst_acres_df_2 %>%
  group_by(TAX_YEAR) %>%
  summarise(
    Avg_REAL_CITY_TAX_DOLLAR = mean(REAL_CITY_TAX_DOLLAR, na.rm = TRUE),
    Avg_CITY_LNTC_TAX = mean(CITY_LNTC_TAX, na.rm = TRUE),
    Avg_REAL_EMV_LAND = mean(REAL_EMV_LAND, na.rm = TRUE),
    Avg_REAL_EMV_LAND_treated = mean(REAL_EMV_LAND[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ],na.rm=T),
    Avg_REAL_EMV_LAND_non_treated = mean(REAL_EMV_LAND[
      G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0
    ],na.rm=T),
    Avg_EMV_LAND = mean(EMV_LAND, na.rm = TRUE),
    Avg_REAL_ESTIMATED_MARKET_VALUE = mean(REAL_ESTIMATED_MARKET_VALUE, na.rm = TRUE),
    Avg_ESTIMATED_MARKET_VALUE = mean(ESTIMATED_MARKET_VALUE, na.rm = TRUE),
  )

ggplot(avg_tax_data_treatment, aes(x = TAX_YEAR)) + 
  geom_line(aes(y = Avg_REAL_EMV_LAND_treated, color = "Avg_REAL_EMV_LAND_treated"), size = 1) + 
  geom_line(aes(y = Avg_REAL_EMV_LAND_non_treated, color = "Avg_REAL_EMV_LAND_non_treated"), size = 1) + 
  labs(
    title = "Average Real Sales Dollar and City Sales Over Time",
    x = "Year",
    y = "Average Sale Amount",
    color = "Tax Type"  # Legend title
  ) + 
  theme_minimal()

# property_type_summary <- parcels_ag_hmst_acres_df_2 %>%
#   group_by(PROPERTY_TYPE_LIST) %>%
#   summarize(
#     homestead_count = sum(homestead_stan, na.rm = TRUE),
#     green_acre_count = sum(green_acre_stan, na.rm = TRUE),
#     ag_preserv_count = sum(ag_preserv_stan, na.rm = TRUE),
#     .groups = "drop"
#   )

rm(property_type_summary)
# '/Users/matthewhockert/Desktop/Personal Info/minneapolis_st_paul_housing_reform_paper'
# these are parcels that entered at any point in time but never left
program_status <- parcels_ag_hmst_acres_df_2 %>%
  group_by(PID_SUBRECORD) %>%
  summarize(
    ag_preserv_status = paste(AG_PRESERVE, collapse = ""),
    green_acre_status = paste(G_Green_Acres, collapse = ""),
    rural_preserve_status = paste(R_Rural_Preserve, collapse = "")
  ) %>%
  filter(
    grepl("^0*1+$", ag_preserv_status) |
      grepl("^0*1+$", green_acre_status) |
      grepl("^0*1+$", rural_preserve_status)
  ) %>%
  ungroup()

program_status <- parcels_ag_hmst_acres_df_2 %>%
  group_by(PID_SUBRECORD) %>%
  summarize(
    ag_preserv_status = any(AG_PRESERVE == 1),
    green_acre_status = any(G_Green_Acres == 1),
    rural_preserve_status = any(R_Rural_Preserve == 1)
  ) %>%
  filter(
    ag_preserv_status | green_acre_status | rural_preserve_status
  ) %>%
  ungroup()


#starting with AG_PRESERVE
parcels_in_program <- parcels_ag_hmst_acres_df_2 %>%
  filter(PID_SUBRECORD %in% program_status$PID_SUBRECORD)

####find non program parcels ----

non_program_parcels <- subset(parcels_ag_hmst_acres_df_2,!(PID_SUBRECORD %in% parcels_in_program$PID_SUBRECORD))
table(parcels_in_program$PID_SUBRECORD,parcels_in_program$AG_PRESERVE)

parcel_counts_city <- parcels_in_program %>%
  group_by(CTU_NAME, year) %>% 
  summarise(
    parcel_count_program = n_distinct(PID_SUBRECORD),
    green_acre_stan_count = sum(G_Green_Acres,na.rm=T),
    ag_preserv_stan_count = sum(AG_PRESERVE,na.rm=T),
    rural_preserv_stan_count = sum(R_Rural_Preserve,na.rm=T),
    .groups = "drop" 
  )


ggplot(parcel_counts_city, aes(x = year, y = parcel_count_program, color = CTU_NAME, group = factor(CTU_NAME))) +
  geom_line() 

# new_parcel_addition_years <- parcels_ag_hmst_acres_df_2 %>%
#   group_by(CTU_NAME, year) %>% # Group by city and year
#   summarize(
#     total_ag_preserv_parcels = sum(ag_preserv_participation == 1, na.rm = TRUE),
#     total_homestead_parcels = sum(homestead_participation == 1, na.rm = TRUE),
#     total_green_acre_parcels = sum(green_acre_participation == 1, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   arrange(CTU_NAME, year) %>% # Ensure proper order by city and year
#   group_by(CTU_NAME) %>%
#   mutate(
#     new_ag_preserv_added = total_ag_preserv_parcels - lag(total_ag_preserv_parcels, default = 0),
#     new_homestead_added = total_homestead_parcels - lag(total_homestead_parcels, default = 0),
#     new_green_acre_added = total_green_acre_parcels - lag(total_green_acre_parcels, default = 0)
#   ) %>%
#   filter(
#     new_ag_preserv_added > 0 | new_homestead_added > 0 | new_green_acre_added > 0
#   ) %>%
#   pivot_longer(
#     cols = starts_with("new_"),
#     names_to = "program",
#     values_to = "new_parcels"
#   ) %>%
#   filter(new_parcels > 0) # Only keep rows where new parcels were added

# years_parcels_added <- new_parcel_addition_years %>%
#   select(CTU_NAME, year, program) %>%
#   distinct()


# ggplot(new_parcel_addition_years, aes(x = year, y = new_parcels, fill = program)) +
#   geom_bar(stat = "identity", position = "stack") +
#   facet_wrap(~CTU_NAME) +
#   labs(
#     title = "New Parcels Added to Programs by Year and City",
#     x = "Year",
#     y = "Number of New Parcels",
#     fill = "Program"
#   ) +
#   theme_minimal()

# parcel_counts_city
# parcel_count = n_distinct(PID),
# homestead_stan_count = sum(homestead_stan,na.rm=T),
# green_acre_stan_count = sum(green_acre_stan,na.rm=T),
# ag_preserv_stan_count = sum(ag_preserv_stan,na.rm=T),

# mean_tax_share_city_non_program <- non_program_parcels %>%
#   #filter(super_property_type %in% c("R","C"))%>%
#   group_by(CTU_NAME,year,super_property_type)%>%
#   summarize(mean_tax_share = mean(city_tax_share,a.rm = T),
#             sd_tax_share = sd(city_tax_share,na.rm = T),
#             parcel_count_no_program = n_distinct(PID))
# table(non_program_parcels$PID_SUBRECORD)
mean_tax_share_city_non_program_total <- non_program_parcels %>%
  group_by(CTU_NAME,super_property_type,year)%>%
  summarize(mean_tax_share = mean(LNTC_CITY_SHARE,a.rm = T),
            sd_tax_share = sd(LNTC_CITY_SHARE,na.rm = T),
            parcel_count_no_program = n_distinct(PID_SUBRECORD))

# Dividing by the number of parcels in a program by non-program parcels plus program parcels
mean_tax_share_city_non_program <- merge(mean_tax_share_city_non_program_total,parcel_counts_city,by=c("CTU_NAME","year"))
mean_tax_share_city_non_program$share_in_program <- (mean_tax_share_city_non_program$parcel_count_program/(mean_tax_share_city_non_program$parcel_count_program+mean_tax_share_city_non_program$parcel_count_no_program))
mean_tax_share_city_non_program$share_in_green_acres <- (mean_tax_share_city_non_program$green_acre_stan_count/(mean_tax_share_city_non_program$green_acre_stan_count+mean_tax_share_city_non_program$parcel_count_no_program))
mean_tax_share_city_non_program$share_in_ag_preserv <- (mean_tax_share_city_non_program$ag_preserv_stan_count/(mean_tax_share_city_non_program$ag_preserv_stan_count+mean_tax_share_city_non_program$parcel_count_no_program))
mean_tax_share_city_non_program$share_in_rural_preserv <- (mean_tax_share_city_non_program$rural_preserv_stan_count/(mean_tax_share_city_non_program$rural_preserv_stan_count+mean_tax_share_city_non_program$parcel_count_no_program))

hist(mean_tax_share_city_non_program$share_in_program)
hist(mean_tax_share_city_non_program$mean_tax_share)
hist(mean_tax_share_city_non_program$share_in_green_acres)
hist(mean_tax_share_city_non_program$share_in_ag_preserv)


# log because of skew and not correcting for 0 because they do not have an effect on tax burden
# should you take the log of percentages? bound between 0 and 100. logit transformation?
# 

ggplot(mean_tax_share_city_non_program, aes(x = (share_in_program), y = (mean_tax_share))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~super_property_type)+
  labs(
    title = "City Tax Share of Non-Program Parcels",
    x = "share of parcels within a Program",
    y = "Mean Tax Share of non-program parcels"
  ) +
  theme_minimal()

hist(mean_tax_share_city_non_program$share_in_program)

model <- lm((mean_tax_share) ~ share_in_ag_preserv + CTU_NAME + year, data =subset(mean_tax_share_city_non_program, super_property_type %in% "commercial"))
summary(model)
# Extract R-squared
r_squared <- summary(model)$r.squared
print(paste("R-squared:", r_squared))


# #rm(plot1)
# parcel_level_data <- parcels_ag_hmst_acres_df_2 %>%
#   filter(PROPERTY_TYPE %in% c("R", "C", "F")) %>% # Focus on relevant property types
#   left_join(parcel_counts_city, by = c("CTU_NAME" = "CTU_NAME", "year" = "year")) %>%
#   mutate(share_in_program = (parcel_count_program / (n_distinct(PID))) * 100)

summary_parcels <- parcels_ag_hmst_acres_df_2 %>%
  group_by(year) %>%
  summarize(
    total_parcels = n_distinct(PID_SUBRECORD),  # Total number of unique parcels
    ag_preserv_count = n_distinct(PID_SUBRECORD[AG_PRESERVE == 1]),
    homestead_count = n_distinct(PID_SUBRECORD[homestead_stan == 1]),
    green_acre_count = n_distinct(PID_SUBRECORD[G_Green_Acres == 1]),
    .groups = "drop"
  ) %>%
  mutate(
    ag_preserv_proportion = (ag_preserv_count / total_parcels)*100,
    homestead_proportion = (homestead_count / total_parcels)*100,
    green_acre_proportion = (green_acre_count / total_parcels)*100
  )

#why did ag preserve explode in 2009?
# most parcels have homestead on them




# ggplot(mean_tax_share_city_non_program, aes(x = year, y = mean_tax_share, color = CTU_NAME, group = CTU_NAME)) +
#   geom_line() +
#   labs(
#     title = "City Tax Share Over Time for Sampled Parcels",
#     x = "Tax Year",
#     y = "City Tax Share",
#     color = "Parcel ID (PID)"
#   ) +
#   geom_vline(
#     data = flip_years_long,
#     aes(xintercept = flip_year),
#     color = "red",
#     linetype = "dashed"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "right") +
#   labs(linetype = "Program")


# green acres to ag preserve 
# transitions <- parcels_ag_hmst_acres_df_2 %>%
#   arrange(PID, year) %>% # Ensure data is sorted by parcel and year
#   group_by(PID) %>% # Group by parcel
#   mutate(
#     was_in_green_acres = lag(green_acre_stan == 1, default = FALSE),
#     now_in_ag_preserv = ag_preserv_stan == 1,
#     transitioned = was_in_green_acres & now_in_ag_preserv # Check for transition
#   ) %>%
#   ungroup()
# 
# num_transitions <- transitions %>%
#   group_by(year) %>%
#   filter(transitioned) %>%
#   summarise(num_transitions = n_distinct(PID))


model_baseline <- lm(
  city_tax_share ~ share_in_program + year + super_property_type,
  data = non_program_parcels
)

#### Effects at County, City, and individual level ----

##### County level effects #####
# 
# aggregate_super_groups <- aggregate_combined_data_frame_added_info %>%
#   group_by(year)%>%
#   summarize(
#     LNTC_CITY_SHARE_mean = mean(LNTC_CITY_SHARE,na.rm=T),
#     AG_PRESERVE_sum = sum(AG_PRESERVE,na.rm = T)
#   )
# 
# plot(aggregate_super_groups$AG_PRESERVE_sum,aggregate_super_groups$LNTC_CITY_SHARE_mean)
# 
# county_aggregate <- parcels_ag_hmst_acres_df_2 %>%
#   group_by(year) %>% 
#   summarise(
#     mean_tax_share_in_program = mean(LNTC_CITY_SHARE, na.rm = TRUE),
#     sd_tax_share_in_program = sd(LNTC_CITY_SHARE, na.rm = TRUE),
#     mean_city_tax_in_program = mean(CITY_LNTC_TAX, na.rm = TRUE),
#     total_parcels = n_distinct(PID),
#     green_acre_stan_count = sum(G_Green_Acres[G_Green_Acres == 1],na.rm=T),
#     ag_preserv_stan_count = sum(AG_PRESERVE[AG_PRESERVE == 1],na.rm=T),
#     rural_preserv_stan_count = sum(R_Rural_Preserve[G_Green_Acres == 1],na.rm=T),
#     ag_preserv_acres = sum(ESTIMATED_MARKET_VALUE[AG_PRESERVE == 1], na.rm = TRUE),
#     rural_preserv_acres = sum(ESTIMATED_MARKET_VALUE[R_Rural_Preserve == 1], na.rm = TRUE),
#     green_acres_acres = sum(ESTIMATED_MARKET_VALUE[G_Green_Acres == 1], na.rm = TRUE),
#     total_acres = sum(ESTIMATED_MARKET_VALUE,na.rm = T),
#     mean_real_city_tax_in_program = mean(REAL_CITY_TAX_DOLLAR,na.rm=T),
#     .groups = "drop" 
#   ) %>%
#   mutate(
#     ag_preserv_proportion = (ag_preserv_stan_count / total_parcels)*100,
#     green_acre_proportion = (green_acre_stan_count / total_parcels)*100, 
#     ag_preserv_acres_prop = (ag_preserv_acres / total_acres)*100,
#     rural_preserv_acres_prop = (rural_preserv_acres / total_acres)*100,
#     green_acres_acres_prop = (green_acres_acres / total_acres)*100,
#     share_in_program = ((ag_preserv_acres+rural_preserv_acres+green_acres_acres)/total_acres)*100,
#     property = 1
#   )
# 
# ggplot(parcels_ag_hmst_acres_df_2, aes(x = ACREAGE)) +
#   geom_histogram(bins = 300, fill = "blue", alpha = 0.6)+
#   facet_wrap(~year)
# 
# parcels_ag_hmst_acres_df_2 %>%
#   group_by(year) %>%
#   summarize(
#     total_acres = sum(ACREAGE, na.rm = TRUE),
#     parcel_count = n()
#   )
# 
# new_parcels_2009 <- parcels_ag_hmst_acres_df_2 %>%
#   filter(year == 2009) %>%
#   anti_join(parcels_ag_hmst_acres_df_2 %>% filter(year == 2008), by = "PID")
# sum(new_parcels_2009$ACREAGE)
# 
# new_acres_2009 <- parcels_ag_hmst_acres_df_2 %>%
#   filter(year %in% c(2008, 2009)) %>%
#   group_by(PID) %>%
#   summarize(
#     acreage_2008 = sum(ACREAGE[year == 2008], na.rm = TRUE),
#     acreage_2009 = sum(ACREAGE[year == 2009], na.rm = TRUE),
#     change = acreage_2009 - acreage_2008
#   ) %>%
#   filter(change > 0)
# 
# large_changes <- new_acres_2009 %>%
#   arrange(desc(change))
#roughly 333,000 PIDs needed adjustment



# County aggregate
mean_tax_share_county_non_program_total <- non_program_parcels %>%
  # filter(CTU_NAME != "Chanhassen") %>%
  group_by(year)%>%
  summarize(mean_tax_share_non_program = mean(LNTC_CITY_SHARE,a.rm = T),
            sd_tax_share = sd(LNTC_CITY_SHARE,na.rm = T),
            mean_city_tax_non_program = mean(CITY_LNTC_TAX, na.rm = TRUE),
            total_parcels = n_distinct(PID_SUBRECORD),
            mean_real_city_tax_non_program = mean(REAL_CITY_TAX_DOLLAR,na.rm=T)
            # green_acre_stan_count = sum(G_Green_Acres,na.rm=T),
            # ag_preserv_stan_count = sum(AG_PRESERVE,na.rm=T),
            # rural_preserv_stan_count = sum(R_Rural_Preserve,na.rm=T),
            # ag_preserv_acres = sum(ACREAGE[G_Green_Acres == 0], na.rm = TRUE),
            # rural_preserv_acres = sum(ACREAGE[R_Rural_Preserve == 0], na.rm = TRUE),
            # green_acres_acres = sum(ACREAGE[G_Green_Acres == 0], na.rm = TRUE),
            # total_acres = sum(ACREAGE,na.rm = 0)
  )

mean_tax_share_county_program_total <- parcels_ag_hmst_acres_df_2 %>%
  # filter(CTU_NAME != "Chanhassen") %>%
  group_by(year) %>% 
  summarise(
    mean_tax_share_in_program = mean(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    sd_tax_share_in_program = sd(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    total_parcels = n_distinct(PID_SUBRECORD[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ]),
    mean_city_tax_in_program = mean(CITY_LNTC_TAX[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    mean_real_city_tax_in_program = mean(REAL_CITY_TAX_DOLLAR[
     G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
      ],na.rm=T),
    green_acre_stan_count = sum(G_Green_Acres[G_Green_Acres == 1],na.rm=T),
    ag_preserv_stan_count = sum(AG_PRESERVE[AG_PRESERVE == 1],na.rm=T),
    rural_preserv_stan_count = sum(R_Rural_Preserve[G_Green_Acres == 1],na.rm=T),
    ag_preserv_acres = sum(ESTIMATED_MARKET_VALUE[AG_PRESERVE == 1], na.rm = TRUE),
    rural_preserv_acres = sum(ESTIMATED_MARKET_VALUE[R_Rural_Preserve == 1], na.rm = TRUE),
    green_acres_acres = sum(ESTIMATED_MARKET_VALUE[G_Green_Acres == 1], na.rm = TRUE),
    total_acres = sum(ESTIMATED_MARKET_VALUE,na.rm = T),
    .groups = "drop" 
  ) %>%
  mutate(
    ag_preserv_proportion = (ag_preserv_stan_count / total_parcels)*100,
    green_acre_proportion = (green_acre_stan_count / total_parcels)*100, 
    ag_preserv_acres_prop = (ag_preserv_acres / total_acres)*100,
    rural_preserv_acres_prop = (rural_preserv_acres / total_acres)*100,
    green_acres_acres_prop = (green_acres_acres / total_acres)*100,
    share_in_program = ((ag_preserv_acres+rural_preserv_acres+green_acres_acres)/total_acres)*100,
    property = 1
  )

#summary(lm(mean_tax_share ~ share_in_program + year,data = mean_tax_share_county_program_total))



county_effects <- merge(mean_tax_share_county_non_program_total,mean_tax_share_county_program_total,by=c("year"))

# Tax share between in program and non-program averages
ggplot(county_effects, aes(x = share_in_program)) +
  geom_point(aes(y = mean_tax_share_in_program, color = "In Program"), alpha = 0.7, size = 2) +
  geom_smooth(aes(y = mean_tax_share_in_program, color = "In Program"), method = "lm", se = FALSE) +
  geom_point(aes(y = mean_tax_share_non_program, color = "Non-Program"), alpha = 0.7, size = 2) +
  geom_smooth(aes(y = mean_tax_share_non_program, color = "Non-Program"), method = "lm", se = FALSE) +
  labs(
    title = "Mean Tax Share by Program Participation",
    x = "Share in Program",
    y = "Mean Tax Share",
    color = "Tax Share Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = c("In Program" = "blue", "Non-Program" = "red"))

ggplot(county_effects, aes(x = share_in_program)) +
  geom_point(aes(y = mean_city_tax_in_program, color = "In Program"), alpha = 0.7, size = 2) +
  geom_smooth(aes(y = mean_city_tax_in_program, color = "In Program"), method = "lm", se = FALSE) +
  geom_point(aes(y = mean_city_tax_non_program, color = "Non-Program"), alpha = 0.7, size = 2) +
  geom_smooth(aes(y = mean_city_tax_non_program, color = "Non-Program"), method = "lm", se = FALSE) +
  labs(
    title = "Mean City Tax by Program Participation",
    x = "Share in Program",
    y = "Mean City Tax",
    color = "City Tax Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_color_manual(values = c("In Program" = "blue", "Non-Program" = "red"))

ggplot(county_effects, aes(x = share_in_program)) +
  geom_point(aes(y = mean_real_city_tax_in_program, color = "In Program"), alpha = 0.7, size = 2) +
  geom_smooth(aes(y = mean_real_city_tax_in_program, color = "In Program"), method = "lm", se = FALSE) +
  geom_point(aes(y = mean_real_city_tax_non_program, color = "Non-Program"), alpha = 0.7, size = 2) +
  geom_smooth(aes(y = mean_real_city_tax_non_program, color = "Non-Program"), method = "lm", se = FALSE) +
  labs(
    title = "Mean City Tax by Program Participation",
    x = "Share in Program",
    y = "Mean City Tax",
    color = "City Tax Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_color_manual(values = c("In Program" = "blue", "Non-Program" = "red"))


table(non_program_parcels$super_property_type)
table(parcels_in_program$super_property_type)
table(parcels_in_program$super_property_type, parcels_in_program$super_property_type)

total_non_program <- nrow(non_program_parcels)
total_program <- nrow(parcels_in_program)

# Create dataframes with percentages
non_program_summary <- as.data.frame(table(non_program_parcels$super_property_type)) %>%
  mutate(percentage = Freq / total_non_program * 100)

program_summary <- as.data.frame(table(parcels_in_program$super_property_type)) %>%
  mutate(percentage = Freq / total_program * 100)

# Rename columns for clarity
colnames(non_program_summary) <- c("super_property_type", "non_program_count", "non_program_percentage")
colnames(program_summary) <- c("super_property_type", "program_count", "program_percentage")

# Merge the two dataframes
property_type_comparison <- full_join(non_program_summary, program_summary, by = "super_property_type") %>%
  replace_na(list(non_program_count = 0, non_program_percentage = 0, program_count = 0, program_percentage = 0))

ggplot(property_type_comparison, aes(x = super_property_type)) +
  geom_bar(aes(y = non_program_percentage, fill = "Non-Program"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = program_percentage, fill = "Program"), stat = "identity", position = "dodge") +
  labs(
    x = "Property Type",
    y = "Percentage",
    fill = "Category",
    title = "Percentage Distribution of Property Types in Programs vs. Non-Programs"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(property_type_comparison, aes(x = super_property_type)) +
  geom_bar(aes(y = non_program_percentage, fill = "Non-Program"), stat = "identity", position = position_dodge(width = 0.8)) +
  geom_bar(aes(y = program_percentage, fill = "Program"), stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    x = "Property Type",
    y = "Percentage",
    fill = "Category",
    title = "Percentage Distribution of Property Types in Programs vs. Non-Programs"
  ) +
  scale_fill_manual(values = c("Non-Program" = "red", "Program" = "blue")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


# mean_tax_share_non_program = Average tax shares of parcels not in a program
# mean_tax_share_in_program = Average tax share of parcels in a program

# ggplot(county_effects, aes(x = mean_tax_share)) +
#   geom_histogram(bins = 300, fill = "blue", alpha = 0.6)+
#   facet_wrap(~program)

ggplot(county_effects, aes(x = share_in_program,y = mean_tax_share_non_program, color = as.factor(year))) +
  geom_point()

ggplot(county_effects, aes(x = share_in_program,y = mean_tax_share_non_program, color = as.factor(year))) +
  geom_point()

plot1 <- ggplot(county_effects, aes(x = year, y = mean_tax_share_non_program)) +
  geom_line() +
  labs(
    title = "Mean Tax Share (Non-Program)",
    x = "Year",
    y = "Mean Tax Share"
  ) +
  theme_minimal()

# Create the second plot
plot2 <- ggplot(county_effects, aes(x = year, y = mean_tax_share_in_program)) +
  geom_line() +
  labs(
    title = "Mean Tax Share (In Program)",
    x = "Year",
    y = "Mean Tax Share"
  ) +
  theme_minimal()

# Combine the plots with patchwork
plot1 + plot2

plot1 <- ggplot(county_effects, aes(x = year, y = ag_preserv_acres_prop)) +
  geom_line() +
  labs(title = "Agricultural Preservation 
       Acres Proportion", x = "Year", y = "Proportion")

plot2 <- ggplot(county_effects, aes(x = year, y = green_acres_acres_prop)) +
  geom_line() +
  labs(title = "Green Acres Proportion", x = "Year", y = "Proportion")

plot3 <- ggplot(county_effects, aes(x = year, y = share_in_program)) +
  geom_line() +
  labs(title = "Share in Program", x = "Year", y = "Share")

# Combine the plots
plot1 + plot2 + plot3



# Super property type included
mean_tax_share_county_non_program_total_type <- non_program_parcels %>%
  filter(CTU_NAME != "Chanhassen") %>%
  group_by(super_property_type,year)%>%
  summarize(mean_tax_share_non_program = mean(LNTC_CITY_SHARE,na.rm = T),
            sd_tax_share = sd(LNTC_CITY_SHARE,na.rm = T),
            mean_city_tax_non_program = mean(CITY_LNTC_TAX,na.rm=T),
            total_parcels = n_distinct(PID_SUBRECORD),
            mean_real_city_tax_non_program = mean(REAL_CITY_TAX_DOLLAR,na.rm=T),
            
            # green_acre_stan_count = sum(G_Green_Acres,na.rm=T),
            # ag_preserv_stan_count = sum(AG_PRESERVE,na.rm=T),
            # rural_preserv_stan_count = sum(R_Rural_Preserve,na.rm=T),
            # ag_preserv_acres = sum(ACREAGE[G_Green_Acres == 0], na.rm = TRUE),
            # rural_preserv_acres = sum(ACREAGE[R_Rural_Preserve == 0], na.rm = TRUE),
            # green_acres_acres = sum(ACREAGE[G_Green_Acres == 0], na.rm = TRUE),
            # total_acres = sum(ACREAGE,na.rm = 0)
  )
mean_tax_share_county_program_total_year <- parcels_ag_hmst_acres_df_2 %>%
  # filter(CTU_NAME != "Chanhassen") %>%
  group_by(year) %>%  # Group only by year
  summarise(
    total_parcels_year = n_distinct(PID_SUBRECORD),
    ag_preserv_stan_count_year = sum(AG_PRESERVE[AG_PRESERVE == 1], na.rm = TRUE),
    green_acre_stan_count_year = sum(G_Green_Acres[G_Green_Acres == 1], na.rm = TRUE),
    rural_preserv_stan_count_year = sum(R_Rural_Preserve[R_Rural_Preserve == 1], na.rm = TRUE),
    ag_preserv_acres_year = sum(ESTIMATED_MARKET_VALUE[AG_PRESERVE == 1], na.rm = TRUE),
    rural_preserv_acres_year = sum(ESTIMATED_MARKET_VALUE[R_Rural_Preserve == 1], na.rm = TRUE),
    green_acres_acres_year = sum(ESTIMATED_MARKET_VALUE[G_Green_Acres == 1], na.rm = TRUE),
    total_acres_year = sum(ESTIMATED_MARKET_VALUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ag_preserv_proportion_year = (ag_preserv_stan_count_year / total_parcels_year) * 100,
    green_acre_proportion_year = (green_acre_stan_count_year / total_parcels_year) * 100,
    ag_preserv_acres_prop_year = (ag_preserv_acres_year / total_acres_year) * 100,
    rural_preserv_acres_prop_year = (rural_preserv_acres_year / total_acres_year) * 100,
    green_acres_acres_prop_year = (green_acres_acres_year / total_acres_year) * 100,
    share_in_program_year = ((ag_preserv_acres_year + rural_preserv_acres_year + green_acres_acres_year) / total_acres_year) * 100
  )

# Add year-level program effects to the original grouped data
mean_tax_share_county_program_total_type <- parcels_ag_hmst_acres_df_2 %>%
  # filter(CTU_NAME != "Chanhassen") %>%
  group_by(super_property_type, year) %>%  # Group by super_property_type and year
  summarise(
    mean_tax_share_in_program = mean(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    sd_tax_share_in_program = sd(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    mean_city_tax_in_program = mean(CITY_LNTC_TAX[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    mean_real_city_tax_in_program = mean(REAL_CITY_TAX_DOLLAR[
    G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ],na.rm=T),
    total_parcels = n_distinct(PID_SUBRECORD),
    .groups = "drop"
  ) %>%
  left_join(mean_tax_share_county_program_total_year, by = "year")

county_effects_type <- merge(mean_tax_share_county_non_program_total_type,mean_tax_share_county_program_total_type,by=c("year","super_property_type"))

# ggplot(county_effects, aes(x = mean_tax_share)) +
#   geom_histogram(bins = 300, fill = "blue", alpha = 0.6)+
#   facet_wrap(~program)

ggplot(county_effects_type, aes(x = share_in_program_year)) +
  geom_point(aes(y = mean_tax_share_in_program), color = "blue") +
  geom_smooth(aes(y = mean_tax_share_in_program), method = "lm", se = FALSE, color = "blue") +
  geom_point(aes(y = mean_tax_share_non_program), color = "red") +
  geom_smooth(aes(y = mean_tax_share_non_program), method = "lm", se = FALSE, color = "red") +
  theme_minimal()+
  facet_wrap(~super_property_type, scales="free")

ggplot(county_effects_type, aes(x = share_in_program_year)) +
  geom_point(aes(y = mean_city_tax_in_program), color = "blue") +
  geom_smooth(aes(y = mean_city_tax_in_program), method = "lm", se = FALSE, color = "blue") +
  geom_point(aes(y = mean_city_tax_non_program), color = "red") +
  geom_smooth(aes(y = mean_city_tax_non_program), method = "lm", se = FALSE, color = "red") +
  theme_minimal()+
  facet_wrap(~super_property_type, scales="free")

ggplot(county_effects_type, aes(x = share_in_program_year)) +
  geom_point(aes(y = mean_real_city_tax_in_program), color = "blue") +
  geom_smooth(aes(y = mean_real_city_tax_in_program), method = "lm", se = FALSE, color = "blue") +
  geom_point(aes(y = mean_real_city_tax_non_program), color = "red") +
  geom_smooth(aes(y = mean_real_city_tax_non_program), method = "lm", se = FALSE, color = "red") +
  theme_minimal()+
  facet_wrap(~super_property_type, scales="free")

ggplot(county_effects_type, aes(x = share_in_program_year)) +
  geom_point(aes(y = mean_tax_share_non_program), color = "blue") +
  geom_point(aes(y = mean_tax_share_in_program * 100), color = "red") +
  scale_y_continuous(
    name = "Mean Tax Share (Non-Program)",
    sec.axis = sec_axis(~ . / 100, name = "Mean Tax Share (In Program)")
  ) 

ggplot(county_effects_type, aes(x = share_in_program_year)) +
  geom_point(aes(y = mean_tax_share_non_program), color = "blue") +
  geom_point(aes(y = mean_tax_share_in_program * 1), color = "red") +
  scale_y_continuous(
    name = "Mean Tax Share (Non-Program)",
    sec.axis = sec_axis(~ . / 1, name = "Mean Tax Share (In Program)")
  ) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal()

ggplot(county_effects_type, aes(x = year)) +
  geom_line(aes(y = mean_tax_share_non_program, color = "Non-Program")) +
  geom_line(aes(y = mean_tax_share_in_program * 1, color = "In Program")) +
  scale_y_continuous(
    name = "Mean Tax Share (Non-Program)",
    sec.axis = sec_axis(~ . / 1, name = "Mean Tax Share (In Program)")
  ) +
  scale_color_manual(
    values = c("Non-Program" = "blue", "In Program" = "red"),
    name = "Tax Share Type"
  ) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Mean Tax Share by Program and Property Type",
    x = "Year"
  )
ggplot(county_effects_type, aes(x = year,y = share_in_program_year, group = as.factor(super_property_type),color = as.factor(super_property_type))) +
  geom_line()+
  facet_wrap(~super_property_type,scales = "free")

ggplot(county_effects_type, aes(x = year,y = mean_city_tax_in_program, group = as.factor(super_property_type),color = as.factor(super_property_type))) +
  geom_line()+
  facet_wrap(~super_property_type,scales = "free")

ggplot(county_effects_type, aes(x = year,y = mean_city_tax_non_program, group = as.factor(super_property_type),color = as.factor(super_property_type))) +
  geom_line()+
  facet_wrap(~super_property_type,scales = "free")

summary(lm(mean_tax_share_non_program ~ ag_preserv_acres_prop_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))
summary(lm(mean_tax_share_in_program ~ ag_preserv_acres_prop_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))

summary(lm(mean_tax_share_non_program ~ green_acres_acres_prop_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))
summary(lm(mean_tax_share_in_program ~ green_acres_acres_prop_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))

summary(lm(mean_city_tax_non_program ~ green_acres_acres_prop_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))
summary(lm(mean_city_tax_in_program ~ green_acres_acres_prop_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))

summary(lm(mean_city_tax_non_program ~ ag_preserv_acres_prop_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))
summary(lm(mean_city_tax_in_program ~ ag_preserv_acres_prop_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))

summary(lm(mean_tax_share_non_program ~ share_in_program_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))
summary(lm(mean_tax_share_in_program ~ share_in_program_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))

summary(lm(mean_city_tax_non_program ~ share_in_program_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))
summary(lm(mean_city_tax_in_program ~ share_in_program_year +as.factor(super_property_type) +as.factor(year),data = county_effects_type))




## general county level trends ##

general_trends <- parcels_ag_hmst_acres_df_2 %>%
  # filter(CTU_NAME != "Chanhassen") %>%
  group_by(super_property_type,year) %>% 
  summarise(
    mean_tax_share = mean(LNTC_CITY_SHARE, na.rm = TRUE),
    sd_tax_share = sd(LNTC_CITY_SHARE, na.rm = TRUE),
    mean_city_tax = mean(CITY_LNTC_TAX,na.rm=T),
    sum_city_tax = sum(CITY_LNTC_TAX,na.rm=T),
    total_parcels = n_distinct(PID_SUBRECORD),
    green_acre_stan_count = sum(G_Green_Acres[G_Green_Acres == 1],na.rm=T),
    ag_preserv_stan_count = sum(AG_PRESERVE[AG_PRESERVE == 1],na.rm=T),
    rural_preserv_stan_count = sum(R_Rural_Preserve[G_Green_Acres == 1],na.rm=T),
    ag_preserv_acres = sum(ACREAGE[AG_PRESERVE == 1], na.rm = TRUE),
    rural_preserv_acres = sum(ACREAGE[R_Rural_Preserve == 1], na.rm = TRUE),
    green_acres_acres = sum(ACREAGE[G_Green_Acres == 1], na.rm = TRUE),
    total_acres = sum(ACREAGE,na.rm = T),
    .groups = "drop" 
  ) %>%
  group_by(year) %>% # Group by year to calculate yearly total city tax
  mutate(
    total_city_tax_year = sum(sum_city_tax, na.rm = TRUE),
    total_city_parcels_year = sum(total_parcels, na.rm = TRUE),
    total_city_acres_year = sum(total_acres, na.rm = TRUE),
    aggregate_tax_share = (sum_city_tax / total_city_tax_year)*100,
    aggregate_parcel_share = (total_parcels / total_city_parcels_year)*100,
    aggregate_acreage_share = (total_acres / total_city_acres_year)*100
  ) %>%
  ungroup()

ggplot(general_trends, aes(x = year,y = aggregate_tax_share, group = as.factor(super_property_type),color = as.factor(super_property_type))) +
  geom_line()+
  facet_wrap(~super_property_type,scales = "free")

ggplot(general_trends, aes(x = year,y = aggregate_parcel_share, group = as.factor(super_property_type),color = as.factor(super_property_type))) +
  geom_line()+
  facet_wrap(~super_property_type,scales = "free")

ggplot(general_trends, aes(x = year,y = aggregate_acreage_share, group = as.factor(super_property_type),color = as.factor(super_property_type))) +
  geom_line()+
  facet_wrap(~super_property_type,scales = "free")


##### City level effects #####

# aggregate_super_groups_city <- aggregate_combined_data_frame_added_info %>%
#   filter(agricultural_farm == 1)%>%
#   group_by(CTU_NAME,year, AG_PRESERVE)%>%
#   summarize(
#     LNTC_CITY_SHARE_mean = mean(LNTC_CITY_SHARE,na.rm=T),
#     AG_PRESERVE_sum = sum(AG_PRESERVE,na.rm = T),
#     acres_sum = sum(ACREAGE,na.rm=T)
#   )%>%
#   filter(CTU_NAME != "Chanhassen")
# 
# hist(aggregate_super_groups_city$AG_PRESERVE_sum)
# hist(aggregate_super_groups_city$LNTC_CITY_SHARE_mean)
# 
# ggplot(aggregate_super_groups_city, aes(x = LNTC_CITY_SHARE_mean)) +
#   geom_histogram(bins = 300, fill = "blue", alpha = 0.6)+
#   facet_wrap(~ag_pres_group)

plot(log(aggregate_super_groups_city$AG_PRESERVE_sum+1),aggregate_super_groups_city$LNTC_CITY_SHARE_mean)
plot(log(aggregate_super_groups_city$acres_sum+1),aggregate_super_groups_city$LNTC_CITY_SHARE_mean)


summary(lm(LNTC_CITY_SHARE_mean~log(AG_PRESERVE_sum+1) + acres_sum + as.factor(year),aggregate_super_groups_city))

# Acreage in program

mean_tax_share_city_non_program_total <- non_program_parcels %>%
  filter(CTU_NAME != "Chanhassen") %>%
  group_by(CTU_NAME,year)%>%
  summarize(mean_tax_share_non_program = mean(LNTC_CITY_SHARE,a.rm = T),
            mean_city_tax_non_program = mean(CITY_LNTC_TAX,na.rm=T),
            sd_tax_share = sd(LNTC_CITY_SHARE,na.rm = T),
            total_parcels = n_distinct(PID_SUBRECORD),
            total_tax_revenue_non_program = sum(CITY_LNTC_TAX, na.rm = TRUE),
            mean_real_city_tax_non_program = mean(REAL_CITY_TAX_DOLLAR,na.rm=T),
            # green_acre_stan_count = sum(G_Green_Acres,na.rm=T),
            # ag_preserv_stan_count = sum(AG_PRESERVE,na.rm=T),
            # rural_preserv_stan_count = sum(R_Rural_Preserve,na.rm=T),
            # ag_preserv_acres = sum(ACREAGE[G_Green_Acres == 0], na.rm = TRUE),
            # rural_preserv_acres = sum(ACREAGE[R_Rural_Preserve == 0], na.rm = TRUE),
            # green_acres_acres = sum(ACREAGE[G_Green_Acres == 0], na.rm = TRUE),
            # total_acres = sum(ACREAGE,na.rm = 0)
  )

mean_tax_share_city_program_total <- parcels_ag_hmst_acres_df_2 %>%
  # filter(CTU_NAME != "Chanhassen") %>%
  group_by(CTU_NAME,year) %>% 
  summarise(
    mean_tax_share_in_program = mean(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    sd_tax_share_in_program = sd(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    mean_city_tax_in_program = mean(CITY_LNTC_TAX[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ],na.rm=T),
    mean_real_city_tax_in_program = mean(REAL_CITY_TAX_DOLLAR[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ],na.rm=T),
    total_parcels = n_distinct(PID_SUBRECORD),
    total_tax_revenue = sum(CITY_LNTC_TAX, na.rm = TRUE),
    mean_tax_dollars = mean(CITY_LNTC_TAX, na.rm = TRUE),
    green_acre_stan_count = sum(G_Green_Acres[G_Green_Acres == 1],na.rm=T),
    ag_preserv_stan_count = sum(AG_PRESERVE[AG_PRESERVE == 1],na.rm=T),
    rural_preserv_stan_count = sum(R_Rural_Preserve[G_Green_Acres == 1],na.rm=T),
    ag_preserv_acres = sum(ESTIMATED_MARKET_VALUE[AG_PRESERVE == 1], na.rm = TRUE),
    rural_preserv_acres = sum(ESTIMATED_MARKET_VALUE[R_Rural_Preserve == 1], na.rm = TRUE),
    green_acres_acres = sum(ESTIMATED_MARKET_VALUE[G_Green_Acres == 1], na.rm = TRUE),
    total_acres = sum(ESTIMATED_MARKET_VALUE,na.rm = T),
    .groups = "drop" 
  ) %>%
  mutate(
    ag_preserv_proportion = (ag_preserv_stan_count / total_parcels)*100,
    green_acre_proportion = (green_acre_stan_count / total_parcels)*100, 
    ag_preserv_acres_prop = (ag_preserv_acres / total_acres)*100,
    rural_preserv_acres_prop = (rural_preserv_acres / total_acres)*100,
    green_acres_acres_prop = (green_acres_acres / total_acres)*100,
    share_in_program = ((ag_preserv_acres+rural_preserv_acres+green_acres_acres)/total_acres)*100,
    property = 1
  )

parcels_ag_hmst_acres_df_2 %>%
  filter(CTU_NAME == "Brooklyn Center") %>%
  group_by(CTU_NAME, year) %>%
  summarise(
    valid_rows = sum(G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1),
    valid_non_na = sum(!is.na(LNTC_CITY_SHARE) & 
                         (G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1)),
    .groups = "drop"
  )


#summary(lm(mean_tax_share ~ share_in_program + year,data = mean_tax_share_county_program_total))
city_effects <- merge(mean_tax_share_city_non_program_total,mean_tax_share_city_program_total,by=c("CTU_NAME","year"))

ggplot() +
  geom_point(data = city_effects, aes(x = share_in_program, y = mean_tax_share_in_program), color = "blue") +
  geom_point(data = city_effects, aes(x = share_in_program, y = mean_tax_share_non_program), color = "red") +
  theme_minimal()

ggplot() +
  geom_point(data = city_effects, aes(x = share_in_program, y = mean_tax_dollars), color = "red") +
  theme_minimal()

ggplot() +
  geom_point(data = city_effects, aes(x = share_in_program, y = log(total_tax_revenue+1)), color = "red") +
  geom_point(data = city_effects, aes(x = share_in_program, y = log(total_tax_revenue_no_program+1)), color = "blue") +
  theme_minimal()
# table(non_program_parcels$super_property_type)
# table(parcels_in_program$super_property_type)
# 
# total_non_program <- nrow(non_program_parcels)
# total_program <- nrow(parcels_in_program)
# 
# # Create dataframes with percentages
# non_program_summary <- as.data.frame(table(non_program_parcels$super_property_type)) %>%
#   mutate(percentage = Freq / total_non_program * 100)
# 
# program_summary <- as.data.frame(table(parcels_in_program$super_property_type)) %>%
#   mutate(percentage = Freq / total_program * 100)
# 
# # Rename columns for clarity
# colnames(non_program_summary) <- c("super_property_type", "non_program_count", "non_program_percentage")
# colnames(program_summary) <- c("super_property_type", "program_count", "program_percentage")
# 
# # Merge the two dataframes
# property_type_comparison <- full_join(non_program_summary, program_summary, by = "super_property_type") %>%
#   replace_na(list(non_program_count = 0, non_program_percentage = 0, program_count = 0, program_percentage = 0))
# 
# ggplot(property_type_comparison, aes(x = super_property_type)) +
#   geom_bar(aes(y = non_program_percentage, fill = "Non-Program"), stat = "identity", position = "dodge") +
#   geom_bar(aes(y = program_percentage, fill = "Program"), stat = "identity", position = "dodge") +
#   labs(
#     x = "Property Type",
#     y = "Percentage",
#     fill = "Category",
#     title = "Percentage Distribution of Property Types in Programs vs. Non-Programs"
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# mean_tax_share_non_program = Average tax shares of parcels not in a program
# mean_tax_share_in_program = Average tax share of parcels in a program

# ggplot(county_effects, aes(x = mean_tax_share)) +
#   geom_histogram(bins = 300, fill = "blue", alpha = 0.6)+
#   facet_wrap(~program)


filtered_city_effects <- city_effects %>%
  group_by(CTU_NAME) %>%
  filter(any(!is.na(mean_tax_share_in_program) & mean_tax_share_in_program != 0)) %>%
  ungroup()

# Plot the filtered data
ggplot(filtered_city_effects, aes(x = share_in_program)) +
  geom_point(aes(y = mean_tax_share_non_program), color = "blue") +
  geom_smooth(aes(y = mean_tax_share_non_program), method = "lm", se = FALSE, color = "blue") +
  geom_point(aes(y = mean_tax_share_in_program * 1), color = "red") +
  geom_smooth(aes(y = mean_tax_share_in_program * 1), method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(
    name = "Mean Tax Share (Non-Program, blue)",
    sec.axis = sec_axis(~ . / 1, name = "Mean Tax Share (In Program, red)")
  ) +
  facet_wrap(~CTU_NAME, scales = "free") +
  theme_minimal()

ggplot(filtered_city_effects, aes(x = share_in_program)) +
  geom_point(aes(y = mean_real_city_tax_non_program), color = "blue") +
  geom_smooth(aes(y = mean_real_city_tax_non_program), method = "lm", se = FALSE, color = "blue") +
  geom_point(aes(y = mean_real_city_tax_in_program * 1), color = "red") +
  geom_smooth(aes(y = mean_real_city_tax_in_program * 1), method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(
    name = "Mean Tax Share (Non-Program, blue)",
    sec.axis = sec_axis(~ . / 1, name = "Mean Tax Share (In Program, red)")
  ) +
  facet_wrap(~CTU_NAME, scales = "free") +
  theme_minimal()


percent_change_data <- filtered_city_effects %>%
  filter(year %in% c(2005, 2024)) %>%
  group_by(CTU_NAME) %>%
  summarize(
    pct_change_mean_tax_share_non_program = 100 * (mean(mean_tax_share_non_program[year == 2024], na.rm = TRUE) -
                                                     mean(mean_tax_share_non_program[year == 2005], na.rm = TRUE)) /
      mean(mean_tax_share_non_program[year == 2005], na.rm = TRUE),
    pct_change_mean_tax_share_in_program = 100 * (mean(mean_tax_share_in_program[year == 2024], na.rm = TRUE) -
                                                    mean(mean_tax_share_in_program[year == 2005], na.rm = TRUE)) /
      mean(mean_tax_share_in_program[year == 2005], na.rm = TRUE),
    pct_change_share_in_program = 100 * (mean(share_in_program[year == 2024], na.rm = TRUE) -
                                           mean(share_in_program[year == 2005], na.rm = TRUE)) /
      mean(share_in_program[year == 2005], na.rm = TRUE)
  ) %>%
  ungroup()


ggplot(percent_change_data, aes(x = pct_change_share_in_program)) +
  geom_point(aes(y = pct_change_mean_tax_share_non_program, color = "Non-Program Tax Share"), alpha = 0.7) +
  geom_smooth(aes(y = pct_change_mean_tax_share_non_program, color = "Non-Program Tax Share"), method = "lm", se = FALSE) +
  geom_point(aes(y = pct_change_mean_tax_share_in_program, color = "In Program Tax Share"), alpha = 0.7) +
  geom_smooth(aes(y = pct_change_mean_tax_share_in_program, color = "In Program Tax Share"), method = "lm", se = FALSE) +
  theme_minimal() +
  scale_color_manual(
    values = c("Non-Program Tax Share" = "blue", "In Program Tax Share" = "red"),
    name = "Tax Share Type"
  ) +
  labs(
    x = "Percent Change in Share in Program",
    y = "Percent Change in Tax Share",
    title = "Percent Changes in Tax Share vs Share in Program (2005 to 2024)"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )


# summary(lm(mean_tax_share_non_program ~ share_in_program,data = subset(filtered_city_effects,CTU_NAME == "Corcoran")))

ggplot(city_effects, aes(x = share_in_program,y = mean_tax_share_non_program, color = as.factor(year))) +
  geom_point()

ggplot(county_effects, aes(x = year,y = mean_tax_share_non_program)) +
  geom_line()
ggplot(county_effects, aes(x = year,y = mean_tax_share_in_program)) +
  geom_line()

ggplot(county_effects, aes(x = year,y = ag_preserv_acres_prop)) +
  geom_line()

ggplot(county_effects, aes(x = year,y = green_acres_acres_prop)) +
  geom_line()

ggplot(county_effects, aes(x = year,y = share_in_program)) +
  geom_line()


summary(lm(mean_tax_share_non_program ~ green_acres_acres_prop +as.factor(CTU_NAME) +as.factor(year),data = city_effects))
summary(lm(mean_tax_share_in_program ~ green_acres_acres_prop +as.factor(CTU_NAME) +as.factor(year),data = city_effects))

summary(lm(mean_city_tax_non_program ~ green_acres_acres_prop +as.factor(CTU_NAME) +as.factor(year),data = city_effects))
summary(lm(mean_city_tax_in_program ~ green_acres_acres_prop +as.factor(CTU_NAME) +as.factor(year),data = city_effects))

summary(lm(mean_city_tax_non_program ~ ag_preserv_acres_prop +as.factor(CTU_NAME) +as.factor(year),data = city_effects))
summary(lm(mean_city_tax_in_program ~ ag_preserv_acres_prop +as.factor(CTU_NAME) +as.factor(year),data = city_effects))

summary(lm(mean_tax_share_non_program ~ ag_preserv_acres_prop +as.factor(CTU_NAME) +as.factor(year),data = city_effects))
summary(lm(mean_tax_share_in_program ~ ag_preserv_acres_prop +as.factor(CTU_NAME) +as.factor(year),data = city_effects))


summary(lm(mean_city_tax_non_program ~ share_in_program +as.factor(CTU_NAME) +as.factor(year),data = city_effects))
summary(lm(mean_city_tax_in_program ~ share_in_program +as.factor(CTU_NAME) +as.factor(year),data = city_effects))

summary(lm(mean_tax_share_non_program ~ share_in_program +as.factor(CTU_NAME) +as.factor(year),data = city_effects))
summary(lm(mean_tax_share_in_program ~ share_in_program +as.factor(CTU_NAME) +as.factor(year),data = city_effects))

# Include Super Clusters
mean_tax_share_city_non_program_total_type <- non_program_parcels %>%
  # filter(CTU_NAME != "Chanhassen") %>%
  group_by(CTU_NAME,year,super_property_type)%>%
  summarize(mean_tax_share_non_program = mean(LNTC_CITY_SHARE,a.rm = T),
            mean_city_tax_non_program = mean(CITY_LNTC_TAX,na.rm=T),
            sd_tax_share = sd(LNTC_CITY_SHARE,na.rm = T),
            total_parcels = n_distinct(PID_SUBRECORD),
            mean_real_city_tax_non_program = mean(REAL_CITY_TAX_DOLLAR,na.rm=T)
            # green_acre_stan_count = sum(G_Green_Acres,na.rm=T),
            # ag_preserv_stan_count = sum(AG_PRESERVE,na.rm=T),
            # rural_preserv_stan_count = sum(R_Rural_Preserve,na.rm=T),
            # ag_preserv_acres = sum(ACREAGE[G_Green_Acres == 0], na.rm = TRUE),
            # rural_preserv_acres = sum(ACREAGE[R_Rural_Preserve == 0], na.rm = TRUE),
            # green_acres_acres = sum(ACREAGE[G_Green_Acres == 0], na.rm = TRUE),
            # total_acres = sum(ACREAGE,na.rm = 0)
  )

mean_tax_share_city_program_total_city <- parcels_ag_hmst_acres_df_2 %>%
  # filter(CTU_NAME != "Chanhassen") %>%
  group_by(CTU_NAME, year) %>%  
  summarise(
    total_parcels_city = n_distinct(PID_SUBRECORD),
    ag_preserv_stan_count_city = sum(AG_PRESERVE[AG_PRESERVE == 1], na.rm = TRUE),
    green_acre_stan_count_city = sum(G_Green_Acres[G_Green_Acres == 1], na.rm = TRUE),
    rural_preserv_stan_count_city = sum(R_Rural_Preserve[R_Rural_Preserve == 1], na.rm = TRUE),
    ag_preserv_acres_city = sum(ESTIMATED_MARKET_VALUE[AG_PRESERVE == 1], na.rm = TRUE),
    rural_preserv_acres_city = sum(ESTIMATED_MARKET_VALUE[R_Rural_Preserve == 1], na.rm = TRUE),
    green_acres_acres_city = sum(ESTIMATED_MARKET_VALUE[G_Green_Acres == 1], na.rm = TRUE),
    total_acres_city = sum(ESTIMATED_MARKET_VALUE),
    .groups = "drop"
  ) %>%
  mutate(
    ag_preserv_proportion_city = (ag_preserv_stan_count_city / total_parcels_city) * 100,
    green_acre_proportion_city = (green_acre_stan_count_city / total_parcels_city) * 100,
    ag_preserv_acres_prop_city = (ag_preserv_acres_city / total_acres_city) * 100,
    rural_preserv_acres_prop_city = (rural_preserv_acres_city / total_acres_city) * 100,
    green_acres_acres_prop_city = (green_acres_acres_city / total_acres_city) * 100,
    share_in_program_city = ((ag_preserv_acres_city + rural_preserv_acres_city + green_acres_acres_city) / total_acres_city) * 100
  )

# Add city-level program effects to the original grouped data
mean_tax_share_city_program_total_type <- parcels_ag_hmst_acres_df_2 %>%
  filter(CTU_NAME != "Chanhassen") %>%
  group_by(CTU_NAME, year, super_property_type) %>%
  summarise(
    mean_tax_share_in_program = mean(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    sd_tax_share_in_program = sd(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    mean_city_tax_in_program = mean(CITY_LNTC_TAX[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    mean_real_city_tax_in_program = mean(REAL_CITY_TAX_DOLLAR[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ],na.rm=T),
    total_parcels = n_distinct(PID_SUBRECORD),
    .groups = "drop"
  ) %>%
  left_join(mean_tax_share_city_program_total_city, by = c("CTU_NAME", "year"))

city_effects_type <- merge(mean_tax_share_city_non_program_total_type,mean_tax_share_city_program_total_type,by=c("year","CTU_NAME","super_property_type"))
print(unique(parcels_ag_hmst_acres_df_2$CTU_NAME))
# ggplot(county_effects, aes(x = mean_tax_share)) +
#   geom_histogram(bins = 300, fill = "blue", alpha = 0.6)+
#   facet_wrap(~program)
filtered_city_effects_type <- city_effects_type %>%
  group_by(CTU_NAME,super_property_type) %>%
  filter(any(!is.na(mean_tax_share_in_program) & mean_tax_share_in_program != 0)) %>%
  ungroup()

ggplot(filtered_city_effects_type, aes(x = total_acres_city)) +
  geom_point(aes(y = mean_tax_share_non_program), color = "blue") +
  geom_point(aes(y = mean_tax_share_in_program * 1), color = "red") +
  scale_y_continuous(
    name = "Mean Tax Share (Non-Program)",
    sec.axis = sec_axis(~ . / 1, name = "Mean Tax Share (In Program)")
  ) 

# THIS IS THE GRAPH!
ggplot(filtered_city_effects_type, aes(x = ag_preserv_acres_prop_city)) +
  geom_point(aes(y = mean_tax_share_non_program), color = "red") +
  geom_smooth(aes(y = mean_tax_share_non_program), method = "lm", se = FALSE, color = "red", linetype = "solid") +
  geom_point(aes(y = mean_tax_share_in_program), color = "blue") +
  geom_smooth(aes(y = mean_tax_share_in_program), method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  scale_y_continuous(
    name = "Mean Tax Share (Non-Program)",
    sec.axis = sec_axis(~ ., name = "Mean Tax Share (In Program)")
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("red" = "Non-Program", "blue" = "In Program")
  ) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal()

ggplot(filtered_city_effects_type, aes(x = ag_preserv_acres_prop_city)) +
  geom_point(aes(y = log(mean_city_tax_non_program)), color = "red") +
  geom_smooth(aes(y = log(mean_city_tax_non_program)), method = "lm", se = FALSE, color = "red", linetype = "solid") +
  geom_point(aes(y = log(mean_city_tax_in_program)), color = "blue") +
  geom_smooth(aes(y = log(mean_city_tax_in_program)), method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  scale_y_continuous(
    name = "mean_city_tax_non_program (Non-Program)",
    sec.axis = sec_axis(~ ., name = "Mean Tax Share (In Program)")
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("red" = "Non-Program", "blue" = "In Program")
  ) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal()

ggplot(filtered_city_effects_type, aes(x = ag_preserv_acres_prop_city)) +
  geom_point(aes(y = log(mean_real_city_tax_non_program+1)), color = "red") +
  geom_smooth(aes(y = log(mean_real_city_tax_non_program+1)), method = "lm", se = FALSE, color = "red", linetype = "solid") +
  geom_point(aes(y = log(mean_real_city_tax_in_program+1)), color = "blue") +
  geom_smooth(aes(y = log(mean_real_city_tax_in_program+1)), method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  scale_y_continuous(
    name = "mean_city_tax_non_program (Non-Program)",
    sec.axis = sec_axis(~ ., name = "Mean Tax Share (In Program)")
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("red" = "Non-Program", "blue" = "In Program")
  ) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal()



percent_change_data_type <- filtered_city_effects_type %>%
  filter(year %in% c(2005, 2024)) %>%
  group_by(CTU_NAME) %>%
  summarize(
    pct_change_mean_tax_share_non_program = 100 * (mean(mean_tax_share_non_program[year == 2024], na.rm = TRUE) -
                                                     mean(mean_tax_share_non_program[year == 2005], na.rm = TRUE)) /
      mean(mean_tax_share_non_program[year == 2005], na.rm = TRUE),
    pct_change_mean_tax_share_in_program = 100 * (mean(mean_tax_share_in_program[year == 2024], na.rm = TRUE) -
                                                    mean(mean_tax_share_in_program[year == 2005], na.rm = TRUE)) /
      mean(mean_tax_share_in_program[year == 2005], na.rm = TRUE),
    pct_change_share_in_program = 100 * (mean(share_in_program_city[year == 2024], na.rm = TRUE) -
                                           mean(share_in_program_city[year == 2005], na.rm = TRUE)) /
      mean(share_in_program_city[year == 2005], na.rm = TRUE)
  ) %>%
  ungroup()


ggplot(subset(percent_change_data_type,!(CTU_NAME %in% c("Rogers","Shorewood"))), aes(x = pct_change_share_in_program)) +
  geom_point(aes(y = pct_change_mean_tax_share_non_program, color = "Non-Program Tax Share"), alpha = 0.7) +
  geom_smooth(aes(y = pct_change_mean_tax_share_non_program, color = "Non-Program Tax Share"), method = "lm", se = FALSE) +
  geom_point(aes(y = pct_change_mean_tax_share_in_program, color = "In Program Tax Share"), alpha = 0.7) +
  geom_smooth(aes(y = pct_change_mean_tax_share_in_program, color = "In Program Tax Share"), method = "lm", se = FALSE) +
  theme_minimal() +
  scale_color_manual(
    values = c("Non-Program Tax Share" = "blue", "In Program Tax Share" = "red"),
    name = "Tax Share Type"
  ) +
  labs(
    x = "Percent Change in Share in Program",
    y = "Percent Change in Tax Share",
    title = "Percent Changes in Tax Share vs Share in Program (2005 to 2024)"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# PERCENT CHANGES
mean_tax_share_city_non_program_total_type <- mean_tax_share_city_non_program_total_type %>%
  group_by(CTU_NAME, super_property_type) %>%
  mutate(
    YoY_change_mean_tax_share_non_program = (mean_tax_share_non_program - lag(mean_tax_share_non_program)) / lag(mean_tax_share_non_program) * 100,
    YoY_change_mean_city_tax_non_program = (mean_city_tax_non_program - lag(mean_city_tax_non_program)) / lag(mean_city_tax_non_program) * 100
  ) %>%
  ungroup()

mean_tax_share_city_program_total_city <- mean_tax_share_city_program_total_city %>%
  group_by(CTU_NAME) %>%
  mutate(
    YoY_change_share_in_program_city = (share_in_program_city - lag(share_in_program_city)) / lag(share_in_program_city) * 100
  ) %>%
  ungroup()

mean_tax_share_city_program_total_type <- mean_tax_share_city_program_total_type %>%
  group_by(CTU_NAME, super_property_type) %>%
  mutate(
    YoY_change_mean_tax_share_in_program = (mean_tax_share_in_program - lag(mean_tax_share_in_program)) / lag(mean_tax_share_in_program) * 100,
    YoY_change_mean_city_tax_in_program = (mean_city_tax_in_program - lag(mean_city_tax_in_program)) / lag(mean_city_tax_in_program) * 100
  ) %>%
  ungroup()

city_effects_type <- merge(
  mean_tax_share_city_non_program_total_type,
  mean_tax_share_city_program_total_type,
  by = c("year", "CTU_NAME", "super_property_type"),
  all = TRUE
)

city_effects_type <- merge(
  city_effects_type,
  mean_tax_share_city_program_total_city %>% select(CTU_NAME, year, YoY_change_share_in_program_city),
  by = c("year", "CTU_NAME"),
  all.x = TRUE
)

ggplot(
  subset(city_effects_type,!(CTU_NAME %in% c("Rogers","Shorewood"))), 
  aes(x = YoY_change_share_in_program_city.x)
) +
  geom_point(
    aes(y = YoY_change_mean_tax_share_non_program, color = "Non-Program Tax Share"), 
    alpha = 0.7
  ) +
  geom_smooth(
    aes(y = YoY_change_mean_tax_share_non_program, color = "Non-Program Tax Share"), 
    method = "lm", 
    se = FALSE
  ) +
  geom_point(
    aes(y = YoY_change_mean_tax_share_in_program, color = "In Program Tax Share"), 
    alpha = 0.7
  ) +
  geom_smooth(
    aes(y = YoY_change_mean_tax_share_in_program, color = "In Program Tax Share"), 
    method = "lm", 
    se = FALSE
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("Non-Program Tax Share" = "blue", "In Program Tax Share" = "red"),
    name = "Tax Share Type"
  ) +
  labs(
    x = "Percent Change in Share in Program",
    y = "Percent Change in Tax Share",
    title = "Year-over-Year Percent Changes in Tax Share vs Share in Program"
  ) +
  facet_wrap(~super_property_type,scales = "free")+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )



ggplot(filtered_city_effects_type, aes(x = year)) +
  geom_line(aes(y = mean_tax_share_non_program), color = "blue") +
  geom_line(aes(y = mean_tax_share_in_program * 1), color = "red") +
  scale_y_continuous(
    name = "Mean Tax Share (Non-Program)",
    sec.axis = sec_axis(~ . / 1, name = "Mean Tax Share (In Program)")
  ) +
  facet_grid(CTU_NAME~super_property_type, scales = "free") +
  theme_minimal()

ggplot(filtered_city_effects_type, aes(x = year,y = share_in_program_city, group = as.factor(super_property_type),color = as.factor(super_property_type))) +
  geom_line()+
  facet_wrap(~super_property_type,scales = "free")


summary(lm(mean_tax_share_non_program ~ ag_preserv_acres_prop_city +as.factor(super_property_type) +as.factor(CTU_NAME) +as.factor(year),data = city_effects_type))
summary(lm(mean_tax_share_in_program ~ ag_preserv_acres_prop_city +as.factor(super_property_type)+as.factor(CTU_NAME) +as.factor(year),data = city_effects_type))

summary(lm(mean_city_tax_non_program ~ green_acres_acres_prop +as.factor(super_property_type)+as.factor(CTU_NAME) +as.factor(year),data = city_effects_type))
summary(lm(mean_city_tax_in_program ~ green_acres_acres_prop +as.factor(super_property_type)+as.factor(CTU_NAME) +as.factor(year),data = city_effects_type))

summary(lm(mean_city_tax_non_program ~ ag_preserv_acres_prop +as.factor(super_property_type)+as.factor(CTU_NAME) +as.factor(year),data = city_effects_type))
summary(lm(mean_city_tax_in_program ~ ag_preserv_acres_prop +as.factor(super_property_type)+as.factor(CTU_NAME) +as.factor(year),data = city_effects_type))

summary(lm(mean_tax_share_non_program ~ share_in_program_city +as.factor(super_property_type)+as.factor(CTU_NAME) +as.factor(year)+as.factor(CTU_NAME)*as.factor(year),data = city_effects_type))
summary(lm(mean_tax_share_in_program ~ share_in_program_city+as.factor(super_property_type) +as.factor(CTU_NAME) +as.factor(year)+as.factor(CTU_NAME)*as.factor(year),data = city_effects_type))

summary(lm(mean_city_tax_non_program ~ share_in_program_city +as.factor(super_property_type)+as.factor(CTU_NAME) +as.factor(year)+as.factor(CTU_NAME)*as.factor(year),data = city_effects_type))
summary(lm(mean_city_tax_in_program ~ share_in_program_city+as.factor(super_property_type) +as.factor(CTU_NAME) +as.factor(year)+as.factor(CTU_NAME)*as.factor(year),data = city_effects_type))


#agricultural_farm, commercial, multi_family_residential, residential

##### Individual level effects ####
table(parcels_ag_hmst_acres_df_2$super_property_type,parcels_ag_hmst_acres_df_2$G_Green_Acres)
table(parcels_ag_hmst_acres_df_2$super_property_type,parcels_ag_hmst_acres_df_2$R_Rural_Preserve)
table(parcels_ag_hmst_acres_df_2$super_property_type,parcels_ag_hmst_acres_df_2$AG_PRESERVE)

mean_tax_share_city_program_total <- parcels_ag_hmst_acres_df_2 %>%
  # filter(CTU_NAME != "Chanhassen") %>%
  group_by(CTU_NAME,year) %>% 
  summarise(
    mean_tax_share_in_program = mean(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    sd_tax_share_in_program = sd(LNTC_CITY_SHARE[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ], na.rm = TRUE),
    mean_city_tax_in_program = mean(CITY_LNTC_TAX[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ],na.rm=T),
    mean_real_city_tax_in_program = mean(REAL_CITY_TAX_DOLLAR[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ],na.rm=T),
    mean_real_emv_land_in_program = mean(REAL_EMV_LAND[
      G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1
    ],na.rm=T),
    total_parcels = n_distinct(PID_SUBRECORD),
    green_acre_stan_count = sum(G_Green_Acres[G_Green_Acres == 1],na.rm=T),
    ag_preserv_stan_count = sum(AG_PRESERVE[AG_PRESERVE == 1],na.rm=T),
    rural_preserv_stan_count = sum(R_Rural_Preserve[G_Green_Acres == 1],na.rm=T),
    ag_preserv_acres = sum(ESTIMATED_MARKET_VALUE[AG_PRESERVE == 1], na.rm = TRUE),
    rural_preserv_acres = sum(ESTIMATED_MARKET_VALUE[R_Rural_Preserve == 1], na.rm = TRUE),
    green_acres_acres = sum(ESTIMATED_MARKET_VALUE[G_Green_Acres == 1], na.rm = TRUE),
    total_acres = sum(ESTIMATED_MARKET_VALUE,na.rm = T),
    .groups = "drop" 
  ) %>%
  mutate(
    ag_preserv_proportion = (ag_preserv_stan_count / total_parcels)*100,
    green_acre_proportion = (green_acre_stan_count / total_parcels)*100, 
    ag_preserv_acres_prop = (ag_preserv_acres / total_acres)*100,
    rural_preserv_acres_prop = (rural_preserv_acres / total_acres)*100,
    green_acres_acres_prop = (green_acres_acres / total_acres)*100,
    share_in_program = ((ag_preserv_acres+rural_preserv_acres+green_acres_acres)/total_acres)*100,
    share_in_program_count = ((ag_preserv_proportion+green_acre_proportion)/total_parcels)*100,
    property = 1
  )


parcels_with_city_share <- non_program_parcels %>%
  left_join(mean_tax_share_city_program_total, by = c("CTU_NAME","year"))

# parcels_with_city_share <- parcels_with_city_share %>%
#   mutate(share_in_program_centered = share_in_program - mean(share_in_program, na.rm = TRUE))
non_program_parcels_simplified<- (filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0))
table(non_program_parcels_simplified$LNTC_CITY_SHARE,non_program_parcels_simplified$super_property_type)
# hist(filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)$LNTC_CITY_SHARE)
# hist(log(filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)$LNTC_CITY_SHARE+1))
# range(filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)$LNTC_CITY_SHARE)

names(non_program_parcels_simplified)
nrow(non_program_parcels_simplified)
nrow(parcels_with_city_share)


non_program_parcels_simplified <- non_program_parcels_simplified %>%
  group_by(TAX_YEAR, CTU_NAME) %>%
  mutate(
    LNTC_CITY_SHARE_demeaned = LNTC_CITY_SHARE - mean(LNTC_CITY_SHARE, na.rm = TRUE),
    REAL_CITY_TAX_DOLLAR_demeaned = REAL_CITY_TAX_DOLLAR - mean(REAL_CITY_TAX_DOLLAR, na.rm = TRUE),
    CITY_LNTC_TAX_demeaned = CITY_LNTC_TAX - mean(CITY_LNTC_TAX, na.rm = TRUE),
    REAL_ESTIMATED_MARKET_VALUE_demeaned = REAL_ESTIMATED_MARKET_VALUE - mean(REAL_ESTIMATED_MARKET_VALUE, na.rm = TRUE)) %>%
  ungroup()

# tax share
ggplot(data = non_program_parcels_simplified, aes(x = LNTC_CITY_SHARE_demeaned)) +
  geom_histogram() +
  facet_wrap(~super_property_type,scales = "free")+
  theme_minimal()

ggplot(non_program_parcels_simplified, aes(x = share_in_program, y = LNTC_CITY_SHARE_demeaned, color = factor(CTU_NAME))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", inherit.aes = FALSE, aes(x = share_in_program, y = LNTC_CITY_SHARE_demeaned)) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal()

# real tax dollars
ggplot(data = non_program_parcels_simplified, aes(x = REAL_CITY_TAX_DOLLAR_demeaned)) +
  geom_histogram() +
  facet_wrap(~super_property_type,scales = "free")+
  theme_minimal()

ggplot(non_program_parcels_simplified, aes(x = share_in_program, y = REAL_CITY_TAX_DOLLAR_demeaned, color = factor(CTU_NAME))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", inherit.aes = FALSE, aes(x = share_in_program, y = (REAL_CITY_TAX_DOLLAR_demeaned))) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal()

# Nominal taxes
ggplot(data = non_program_parcels_simplified, aes(x = CITY_LNTC_TAX_demeaned)) +
  geom_histogram() +
  facet_wrap(~super_property_type,scales = "free")+
  theme_minimal()

ggplot(non_program_parcels_simplified, aes(x = share_in_program, y = (CITY_LNTC_TAX_demeaned), color = factor(CTU_NAME))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", inherit.aes = FALSE, aes(x = share_in_program, y = (REAL_CITY_TAX_DOLLAR_demeaned))) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal()

# real market value

ggplot(data = non_program_parcels_simplified, aes(x = REAL_ESTIMATED_MARKET_VALUE_demeaned)) +
  geom_histogram() +
  facet_wrap(~super_property_type,scales = "free")+
  theme_minimal()

ggplot(non_program_parcels_simplified, aes(x = share_in_program, y = (REAL_ESTIMATED_MARKET_VALUE_demeaned), color = factor(CTU_NAME))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", inherit.aes = FALSE, aes(x = share_in_program, y = (REAL_CITY_TAX_DOLLAR_demeaned))) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal()

# nominal market value
ggplot(data = non_program_parcels_simplified, aes(x = ESTIMATED_MARKET_VALUE)) +
  geom_histogram() +
  facet_wrap(~super_property_type,scales = "free")+
  theme_minimal()

ggplot(non_program_parcels_simplified, aes(x = share_in_program, y = log(ESTIMATED_MARKET_VALUE+1), color = factor(CTU_NAME))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", inherit.aes = FALSE, aes(x = share_in_program, y = log(REAL_CITY_TAX_DOLLAR_demeaned+1))) +
  facet_wrap(~super_property_type, scales = "free") +
  theme_minimal()

# Every type has green acres
table(parcels_with_city_share$super_property_type,parcels_with_city_share$G_Green_Acres)

summary(lm(LNTC_CITY_SHARE ~ share_in_program + super_property_type + as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))
summary(lm(LNTC_CITY_SHARE ~ ag_preserv_acres_prop + super_property_type + as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))
summary(lm(LNTC_CITY_SHARE ~ green_acres_acres_prop + super_property_type + as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))
summary(lm(LNTC_CITY_SHARE ~ rural_preserv_acres_prop + super_property_type + as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))

summary(lm(log(CITY_LNTC_TAX+1)~  share_in_program + super_property_type + as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))
summary(lm(log(CITY_LNTC_TAX+1) ~ ag_preserv_acres_prop+ super_property_type + as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))
summary(lm(log(CITY_LNTC_TAX+1) ~ green_acres_acres_prop + super_property_type+ as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))
summary(lm(log(CITY_LNTC_TAX+1) ~ rural_preserv_acres_prop + super_property_type+ as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))

summary(lm(log(REAL_CITY_TAX_DOLLAR+1)~  share_in_program + super_property_type + as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))
summary(lm(log(REAL_CITY_TAX_DOLLAR+1) ~ ag_preserv_acres_prop+ super_property_type + as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))
summary(lm(log(REAL_CITY_TAX_DOLLAR+1) ~ green_acres_acres_prop + super_property_type+ as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))
summary(lm(log(REAL_CITY_TAX_DOLLAR+1) ~ rural_preserv_acres_prop + super_property_type+ as.factor(CTU_NAME)+ as.factor(year), data = filter(parcels_with_city_share,G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0)))


summary(lm(log(REAL_CITY_TAX_DOLLAR+1)~ share_in_program+ LNTC_CITY_SHARE + I(LNTC_CITY_SHARE^2)+log(ESTIMATED_MARKET_VALUE+1) + log(ACREAGE+1) +as.factor(HOMESTEAD)+ log(SALE_PRICE+1) + as.factor(CTU_NAME) + as.factor(year)*share_in_program+as.factor(SCHOOL_ID), data = subset(parcels_with_city_share,super_property_type == "agricultural_farm")))

hist(parcels_with_city_share$REAL_CITY_TAX_DOLLAR)
hist(log(parcels_with_city_share$REAL_CITY_TAX_DOLLAR+1))
names(parcels_with_city_share)
summary(lm(log(REAL_EMV_LAND+1)~ share_in_program+ LNTC_CITY_SHARE + I(LNTC_CITY_SHARE^2)+log(ESTIMATED_MARKET_VALUE+1) + log(ACREAGE+1) +as.factor(HOMESTEAD)+ log(SALE_PRICE+1) + as.factor(super_property_type)*share_in_program+ as.factor(CTU_NAME) + as.factor(year)+as.factor(SCHOOL_ID), data = parcels_with_city_share))
summary(lm(log(REAL_CITY_TAX_DOLLAR+1)~ share_in_program+ LNTC_CITY_SHARE + I(LNTC_CITY_SHARE^2)+log(ESTIMATED_MARKET_VALUE+1) + log(ACREAGE+1) +as.factor(HOMESTEAD)+ log(SALE_PRICE+1) + as.factor(super_property_type)*share_in_program+ as.factor(CTU_NAME) + as.factor(year)+as.factor(SCHOOL_ID), data = parcels_with_city_share))


# ggplot(city_year_share, aes(x = share_in_green_acre)) +
#   geom_histogram(bins = 300, fill = "blue", alpha = 0.6) +
#   labs(
#     title = "Distribution of Share in Program",
#     x = "Share in Program",
#     y = "Frequency"
#   ) +
#   theme_minimal()

hist((parcels_with_city_share$CITY_LNTC_TAX))
names(parcels_with_city_share)

ggplot(parcels_with_city_share, aes(x = share_in_program, y = LNTC_CITY_SHARE)) +
  geom_point() +
  # geom_smooth(
  #   aes(color = "red"), 
  #   method = "lm", 
  #   se = FALSE
  # )+
  facet_wrap(~super_property_type)+
  theme_minimal()



parcels_with_city_share %>%
  filter(G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0) %>%
  ggplot(aes(x = share_in_program, y = LNTC_CITY_SHARE)) +
  geom_point() +
  geom_smooth(
    aes(color = "red"),
    method = "lm",
    se = FALSE
  )+
  facet_wrap(~super_property_type) +
  theme_minimal()

parcels_with_city_share %>%
  filter(G_Green_Acres == 0 | AG_PRESERVE == 0 | R_Rural_Preserve == 0) %>%
  ggplot(aes(x = share_in_program, y = CITY_LNTC_TAX)) +
  geom_point() +
  geom_smooth(
    aes(color = "red"),
    method = "lm",
    se = FALSE
  )+
  facet_wrap(~super_property_type) +
  theme_minimal()



ggplot(parcels_with_city_share, aes(x = share_in_program, y = log(CITY_LNTC_TAX))) +
  geom_point() +
  geom_smooth(
    aes(color = "red"), 
    method = "lm", 
    se = FALSE
  )+
  facet_wrap(~super_property_type)+
  theme_minimal()

subset(parcels_with_city_share,CTU_NAME == "Minnetrista")
ggplot(parcels_with_city_share, aes(x = LNTC_CITY_SHARE, y = (CITY_LNTC_TAX))) +
  geom_point() +
  # geom_smooth(
  #   aes(color = "red"), 
  #   method = "lm", 
  #   se = FALSE
  # )+
  facet_wrap(~super_property_type)+
  theme_minimal()

hist(parcels_with_city_share$ESTIMATED_MARKET_VALUE)
ggplot(parcels_with_city_share, aes(x =log(ESTIMATED_MARKET_VALUE+1), y = LNTC_CITY_SHARE)) +
  geom_point() +
  geom_smooth(
    aes(color = "red"), 
    method = "lm", 
    se = FALSE
  )+
  facet_wrap(~super_property_type)+
  theme_minimal()

ggplot(parcels_with_city_share, aes(x =log(ESTIMATED_MARKET_VALUE+1), y = log(CITY_LNTC_TAX+1))) +
  geom_point() +
  geom_smooth(
    aes(color = "red"), 
    method = "lm", 
    se = FALSE
  )+
  facet_wrap(~super_property_type)+
  theme_minimal()

hist(parcels_with_city_share$ACREAGE)
ggplot(parcels_with_city_share, aes(x = LNTC_CITY_SHARE, y = log(ACREAGE+1))) +
  geom_point() +
  geom_smooth(
    aes(color = "red"), 
    method = "lm", 
    se = FALSE
  )+
  facet_wrap(~super_property_type)+
  theme_minimal()

# Identify treated parcels
treated_parcels_by_program <- parcels_ag_hmst_acres_df_2 %>%
  group_by(PID_SUBRECORD,super_property_type) %>%
  summarize(
    G_Green_Acres_treatment_year = ifelse(any(G_Green_Acres == 1), min(year[G_Green_Acres == 1]), NA),
    AG_PRESERVE_treatment_year = ifelse(any(AG_PRESERVE == 1), min(year[AG_PRESERVE == 1]), NA),
    R_Rural_Preserve_treatment_year = ifelse(any(R_Rural_Preserve == 1), min(year[R_Rural_Preserve == 1]), NA),
    ever_treated = any(G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1)
  ) %>%
  mutate(
    treatment_year = pmin(G_Green_Acres_treatment_year, AG_PRESERVE_treatment_year, R_Rural_Preserve_treatment_year, na.rm = TRUE),
    program_treated = case_when(
      treatment_year == G_Green_Acres_treatment_year ~ "G_Green_Acres",
      treatment_year == AG_PRESERVE_treatment_year ~ "AG_PRESERVE",
      treatment_year == R_Rural_Preserve_treatment_year ~ "R_Rural_Preserve",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(treatment_year)) # Keep only treated parcels

parcels_with_treatment <- parcels_ag_hmst_acres_df_2 %>%
  left_join(treated_parcels_by_program, by = c("PID_SUBRECORD","super_property_type")) %>%
  filter(!is.na(treatment_year)) %>% # Only include treated parcels
  mutate(
    before_treatment = ifelse(year < treatment_year, 1, 0),
    after_treatment = ifelse(year >= treatment_year, 1, 0)
  )

before_after_summary <- parcels_with_treatment %>%
  group_by(program_treated, before_treatment,super_property_type) %>%
  summarize(
    mean_LNTC_CITY_SHARE = mean(LNTC_CITY_SHARE, na.rm = TRUE),
    mean_CITY_LNTC_TAX = mean(CITY_LNTC_TAX,na.rm = T),
    sd_LNTC_CITY_SHARE = sd(LNTC_CITY_SHARE, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(period = ifelse(before_treatment == 1, "Before", "After"))

before_after_summary

beep()


#
####Testing the effects of programs on the local parcel ----



#####General program entry simple paired t-test ---- 
parcels_ag_hmst_acres_df_2$log_CITY_LNTC_TAX <- log(parcels_ag_hmst_acres_df_2$CITY_LNTC_TAX+1)
parcels_ag_hmst_acres_df_2$log_REAL_CITY_TAX_DOLLAR <- log(parcels_ag_hmst_acres_df_2$REAL_CITY_TAX_DOLLAR+1)
parcels_ag_hmst_acres_df_2$log_EMV_LAND <- log(parcels_ag_hmst_acres_df_2$EMV_LAND+1)
parcels_ag_hmst_acres_df_2$log_REAL_EMV_LAND <- log(parcels_ag_hmst_acres_df_2$REAL_EMV_LAND+1)
parcels_ag_hmst_acres_df_2$log_ESTIMATED_MARKET_VALUE <- log(parcels_ag_hmst_acres_df_2$ESTIMATED_MARKET_VALUE+1)
parcels_ag_hmst_acres_df_2$log_REAL_ESTIMATED_MARKET_VALUE <- log(parcels_ag_hmst_acres_df_2$REAL_ESTIMATED_MARKET_VALUE+1)

hist(parcels_ag_hmst_acres_df_2$REAL_ESTIMATED_MARKET_VALUE)

# Did a PID_SUBRECORD enter any program?
data_general <- parcels_ag_hmst_acres_df_2 %>%
  arrange(PID_SUBRECORD, TAX_YEAR) %>%
  group_by(PID_SUBRECORD) %>%
  filter(any(AG_PRESERVE == 1 | G_Green_Acres == 1 | R_Rural_Preserve == 1)) %>%
  mutate(
    switch_year = min(TAX_YEAR[AG_PRESERVE == 1 | G_Green_Acres == 1 | R_Rural_Preserve == 1], na.rm = TRUE),
    pre_post = ifelse(TAX_YEAR < switch_year, "Pre", "Post")
  ) %>%
  ungroup()

#make sure they are paired
paired_ids <- data_general %>%
  group_by(PID_SUBRECORD) %>%
  filter(all(c("Pre", "Post") %in% pre_post)) %>%
  pull(PID_SUBRECORD) %>%
  unique()

data_general <- data_general %>%
  filter(PID_SUBRECORD %in% paired_ids)

table(data_general$pre_post)

###### t-test----


# 
# vars <- c(
#   "LNTC_CITY_SHARE",
#   "CITY_LNTC_TAX",
#   "REAL_CITY_TAX_DOLLAR",
#   "EMV_LAND",
#   "REAL_EMV_LAND",
#   "ESTIMATED_MARKET_VALUE",
#   "REAL_ESTIMATED_MARKET_VALUE"
# )

vars <- c(
  "LNTC_CITY_SHARE",
  "log_CITY_LNTC_TAX",
  "log_REAL_CITY_TAX_DOLLAR",
  "log_EMV_LAND",
  "log_REAL_EMV_LAND",
  "log_ESTIMATED_MARKET_VALUE",
  "log_REAL_ESTIMATED_MARKET_VALUE"
)


general_ttest_data <- data_general %>%
  group_by(PID_SUBRECORD, pre_post) %>%
  summarise(across(all_of(vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(
    names_from = pre_post,
    values_from = all_of(vars),
    names_sep = "_"
  ) %>%
  drop_na()

# Loop over variables
ttest_results <- map(vars, ~ {
  pre_col <- paste0(.x, "_Pre")
  post_col <- paste0(.x, "_Post")
  
  ttest <- t.test(general_ttest_data[[pre_col]], general_ttest_data[[post_col]], paired = TRUE)
  list(variable = .x, ttest = ttest)
})

# Display nicely
ttest_results

###### lm ----
names(data_general)
#test lm
# general_lm_data <- data_general %>%
#   mutate(pre_post = ifelse(pre_post == "Pre", 0, 1))
# summary(lm(general_lm_data$LNTC_CITY_SHARE ~ pre_post, data = general_lm_data))
general_lm_data <- data_general %>%
  mutate(pre_post = ifelse(pre_post == "Pre", 0, 1))

lm_results <- map(vars, ~ {
  formula <- as.formula(paste0(.x, " ~ pre_post + factor(PID_SUBRECORD)+factor(CTU_NAME) + factor(SCHOOL_ID) + factor(super_property_type) + factor(HOMESTEAD) + ACREAGE + factor(year)"))
  model <- lm(formula, data = general_lm_data)
  list(variable = .x, model = summary(model))
})

# View results
lm_results[1:3]
lm_results

results_df <- map_dfr(lm_results, function(res) {
  tidy_model <- broom::tidy(res$model) %>%
    filter(term == "pre_post")
  tidy_model %>%
    mutate(
      program = res$program,
      variable = res$variable,
      r_squared = res$model$r.squared,
      adj_r_squared = res$model$adj.r.squared
    )
})

write_xlsx(results_df, "lm_results.xlsx")

##### Program-specific entry ----

data_program <- parcels_ag_hmst_acres_df_2 %>%
  group_by(PID_SUBRECORD) %>%
  mutate(
    entry_year_ag_preserv = year[AG_PRESERVE - lag(AG_PRESERVE, default = 0) == 1][1],
    entry_year_rural_pres = year[R_Rural_Preserve - lag(R_Rural_Preserve, default = 0) == 1][1],
    entry_year_green_acre = year[G_Green_Acres - lag(G_Green_Acres, default = 0) == 1][1]
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("entry_year_"),
    names_to = "program",
    values_to = "entry_year"
  ) %>%
  filter(!is.na(entry_year)) %>%
  mutate(period = ifelse(year < entry_year, "Pre", "Post"))

###### t-test ----
# program_ttest_data <- data_program %>%
#   group_by(PID_SUBRECORD, program, period) %>%
#   summarise(avg_city_tax_share = mean(LNTC_CITY_SHARE, na.rm = TRUE), .groups = "drop") %>%
#   group_by(PID_SUBRECORD, program) %>%
#   filter(all(c("Pre", "Post") %in% period)) %>%
#   pivot_wider(names_from = period, values_from = avg_city_tax_share) %>%
#   drop_na()
# 
# # Green Acres only
# t.test(program_ttest_data %>% filter(program == "entry_year_green_acre") %>% pull(Pre),
#        program_ttest_data %>% filter(program == "entry_year_green_acre") %>% pull(Post),
#        paired = TRUE)

program_ttest_data <- data_program %>%
  group_by(PID_SUBRECORD, program, period) %>%
  summarise(across(all_of(vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  group_by(PID_SUBRECORD, program) %>%
  filter(all(c("Pre", "Post") %in% period)) %>%
  pivot_wider(
    names_from = period,
    values_from = all_of(vars),
    names_sep = "_"
  ) %>%
  drop_na()

programs <- unique(program_ttest_data$program)

program_ttest_results <- expand.grid(program = programs, variable = vars) %>%
  split(., seq(nrow(.))) %>%
  map(~{
    p <- .x$program
    v <- .x$variable
    pre_col <- paste0(v, "_Pre")
    post_col <- paste0(v, "_Post")
    
    df_sub <- program_ttest_data %>% filter(program == p)
    
    ttest <- t.test(df_sub[[pre_col]], df_sub[[post_col]], paired = TRUE)
    
    list(program = p, variable = v, ttest = ttest)
  })

program_ttest_results

###### lm ----
# program_lm_data <- data_program %>%
#   filter(PID_SUBRECORD %in% unique(program_ttest_data$PID_SUBRECORD)) %>%
#   mutate(period = ifelse(period == "Pre", 0, 1))
# names(program_lm_data)
# 
# summary(lm(LNTC_CITY_SHARE ~ period, data = filter(program_lm_data, program == "entry_year_green_acre")))
# lm(LNTC_CITY_SHARE ~ period * program, data = program_lm_data)
program_lm_data <- data_program %>%
  filter(PID_SUBRECORD %in% unique(program_ttest_data$PID_SUBRECORD)) %>%
  mutate(period = ifelse(period == "Pre", 0, 1))

programs <- unique(program_lm_data$program)

program_lm_results <- expand.grid(program = programs, variable = vars) %>%
  split(., seq(nrow(.))) %>%
  map(~ {
    p <- .x$program
    v <- .x$variable
    
    df_sub <- program_lm_data %>%
      filter(program == p)
    
    formula <- as.formula(paste0(v, " ~ period+ factor(PID_SUBRECORD)+factor(CTU_NAME) + factor(SCHOOL_ID) + factor(super_property_type) + factor(HOMESTEAD) + ACREAGE + factor(year)"))
    
    model <- lm(formula, data = df_sub)
    
    list(program = p, variable = v, model = summary(model))
  })
program_lm_results[3:7]
program_lm_results

results_df <- map_dfr(program_lm_results, function(res) {
  tidy_model <- broom::tidy(res$model) %>%
    filter(term == "period")
  tidy_model %>%
    mutate(
      program = res$program,
      variable = res$variable,
      r_squared = res$model$r.squared,
      adj_r_squared = res$model$adj.r.squared
    )
})

write_xlsx(results_df, "program_lm_results.xlsx")


#
#### Compare Green Acres and Ag. Preserve parcels that left programs Vs those that retain enrollment. #####

library(dplyr)

# Step 1: Aggregate program status for each parcel
program_status_on <- parcels_ag_hmst_acres_df_2 %>%
  group_by(PID) %>%
  summarize(
    ag_preserv_status = paste(AG_PRESERVE, collapse = ""),
    green_acre_status = paste(G_Green_Acres, collapse = ""),
    rural_preserve_status = paste(R_Rural_Preserve, collapse = "")
  ) %>%
  filter(
    grepl("^0*1+$", ag_preserv_status) |  
      grepl("^0*1+$", green_acre_status) |  
      grepl("^0*1+$", rural_preserve_status) |  
      (!grepl("1", ag_preserv_status) & !grepl("1", green_acre_status) & !grepl("1", rural_preserve_status))
  ) %>%
  ungroup()
#
# Step 2: Merge program status with parcel data
parcels_ag_hmst_acres_df_3 <- parcels_ag_hmst_acres_df_2 %>%
  semi_join(program_status_on, by = "PID") %>%
  left_join(program_status_on, by = "PID")

# Step 3: Identify and assign the first treatment year per parcel
parcels_ag_hmst_acres_df_3 <- parcels_ag_hmst_acres_df_3 %>%
  group_by(PID) %>%
  mutate(
    treatment_year = ifelse(
      any(AG_PRESERVE == 1 | G_Green_Acres == 1 | R_Rural_Preserve == 1),
      min(year[AG_PRESERVE == 1 | G_Green_Acres == 1 | R_Rural_Preserve == 1], na.rm = TRUE),
      NA
    )
  ) %>%
  ungroup()

# Step 4: Ensure all years after treatment are marked as treated
parcels_ag_hmst_acres_df_3 <- parcels_ag_hmst_acres_df_3 %>%
  mutate(
    treated = ifelse(!is.na(treatment_year) & year >= treatment_year, 1, 0)
  )

# Step 5: Validate balance
table(parcels_ag_hmst_acres_df_3$treated, parcels_ag_hmst_acres_df_3$year)
table(parcels_ag_hmst_acres_df_3$treatment_year, parcels_ag_hmst_acres_df_3$year)

library(fixest)
library(did)
library(beepr)
hist(parcels_ag_hmst_acres_df_3$LNTC_CITY_SHARE)
model_tax_burden <- feols(
  log(LNTC_CITY_SHARE+1) ~ treated:year | PID + year+as.factor(super_property_type) + CTU_NAME+ HOMESTEAD+log(ACREAGE),  # Year fixed effects absorb the "post" period
  cluster = ~ PID,
  data = parcels_ag_hmst_acres_df_3
)
summary(model_tax_burden)



hist(parcels_ag_hmst_acres_df_3$CITY_LNTC_TAX)
# parcels_ag_hmst_acres_df_3 <- parcels_ag_hmst_acres_df_3 %>%
#   mutate(treated = as.factor(treated))
# model_city_tax <- feols(log(CITY_LNTC_TAX+1)~ i(year,treated,ref=2005) | PID + year + as.factor(super_property_type) + CTU_NAME + HOMESTEAD+log(ACREAGE), 
#                         data = parcels_ag_hmst_acres_df_3)
# summary(model_city_tax)

model_city_tax <- feols(log(CITY_LNTC_TAX+1)~ treated:year | PID + year + as.factor(super_property_type) + CTU_NAME + HOMESTEAD+log(ACREAGE), 
                        data = parcels_ag_hmst_acres_df_3)
summary(model_city_tax)

model_city_tax_real <- feols(log(REAL_CITY_TAX_DOLLAR+1)~ treated:year | PID + year + as.factor(super_property_type) + CTU_NAME + HOMESTEAD+log(ACREAGE), 
                        data = parcels_ag_hmst_acres_df_3)
summary(model_city_tax_real)

hist(parcels_ag_hmst_acres_df_3$EMV_LAND)
model_market_value <- feols(log(EMV_LAND+1) ~treated:year | PID + year + as.factor(super_property_type) + CTU_NAME+ HOMESTEAD+log(ACREAGE), 
                            data = parcels_ag_hmst_acres_df_3)
summary(model_market_value)

model_market_value_real <- feols(log(REAL_EMV_LAND+1) ~treated:year | PID + year + as.factor(super_property_type) + CTU_NAME+ HOMESTEAD+log(ACREAGE), 
                            data = parcels_ag_hmst_acres_df_3)
summary(model_market_value_real)
# Display results
etable(model_tax_burden, model_city_tax, model_market_value,model_market_value_real)

