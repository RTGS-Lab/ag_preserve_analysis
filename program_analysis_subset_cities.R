# ---- Load Required Libraries ----
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(beepr)
library(purrr)
library(readxl)
library(data.table)

# ---- Inspect Program Codes in the Main Data (from main.R) ----
combined_data_frame2 <- combined_data_frame
print(unique(combined_data_frame2$PROGRAM))
# Define columns that could be dropped (not used in this snippet)
columns_to_drop <- c("A", "AX", "B", "BJ", "C", "CR", "CT", "D", "DB", "DJ",
                     "F", "FF", "FH", "FM", "FP", "GC", "HF", "HL", "HM",
                     "HR", "HS", "I", "K", "LA", "LC", "LF", "LI", "LL",
                     "LM", "LR", "LV", "ME", "MH", "NC", "ND", "NH", "NI",
                     "NP", "P", "R", "RL", "RM", "RZ", "S", "SC", "SL",
                     "SM", "SR", "TP", "U", "X", "XC", "XM", "Y", "Z")
names(combined_data_frame2)

# ---- Create Program Participation Indicators ----
# Valid programs: G (Green Acres), R (Rural Preserve), P (Platted Land), O (Open Space)
combined_data_frame3 <- combined_data_frame2 %>%
  filter(PROGRAM != "") %>%  # Remove rows with empty PROGRAM values
  mutate(value = ifelse(PROGRAM %in% c("G", "R", "P", "O"), 1, 0)) %>% 
  pivot_wider(
    names_from = PROGRAM,
    values_from = value,
    values_fill = list(value = 0)
  )
# Rename columns to more descriptive names
colnames(combined_data_frame3) <- colnames(combined_data_frame3) %>%
  sub("^G$", "G_Green_Acres", .) %>%
  sub("^R$", "R_Rural_Preserve", .) %>%
  sub("^P$", "P_Platted_Land", .) %>%
  sub("^O$", "O_Open_Space", .)
names(combined_data_frame3)

# ---- Create a Binary Homestead Indicator and Build the Working Dataframe ----
# 'homestead_stan' is 1 if HOMESTEAD is "TRUE" and 0 if "FALSE"
parcels_ag_hmst_acres_df_2 <- combined_data_frame3 %>%
  mutate(homestead_stan = case_when(
    HOMESTEAD %in% c("TRUE") ~ 1,
    HOMESTEAD %in% c("FALSE") ~ 0,
    TRUE ~ NA_integer_
  ))
beep()

# ---- Identify Cities with Program Participation ----
# Keep only cities where at least one program (Green Acres, Ag Preserve, or Rural Preserve) is active
cities_with_programs <- parcels_ag_hmst_acres_df_2 %>%
  filter(G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1) %>%
  distinct(CTU_NAME)

# ---- Summarize the Number of Program Parcels per City ----
cities_with_programs_sum <- parcels_ag_hmst_acres_df_2 %>%
  filter(G_Green_Acres == 1 | AG_PRESERVE == 1 | R_Rural_Preserve == 1) %>%
  group_by(CTU_NAME) %>%
  summarise(
    num_programs = sum(G_Green_Acres == 1, na.rm = TRUE) +
      sum(AG_PRESERVE == 1, na.rm = TRUE) +
      sum(R_Rural_Preserve == 1, na.rm = TRUE)
  )

# ---- Filter Data to Only Include Relevant Cities ----
parcels_ag_hmst_acres_df_2 <- parcels_ag_hmst_acres_df_2 %>%
  filter(CTU_NAME %in% cities_with_programs$CTU_NAME) %>%
  filter(!(CTU_NAME %in% c("Minneapolis", "Bloomington", "Eden Prairie", "Minnetonka",
                           "Plymouth", "Brooklyn Center", "Maple Grove", "Brooklyn Park", "Champlin")))
print(unique(parcels_ag_hmst_acres_df_2$CTU_NAME))

# ---- Define Function to Fill Zero Acreage Values ----
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

# Check unique SUBRECORD_NO values and frequency (for diagnostics)
print(unique(parcels_ag_hmst_acres_df_2$SUBRECORD_NO))
table(parcels_ag_hmst_acres_df_2$SUBRECORD_NO)

# ---- Apply Zero-Filling to Acreage and Standardize Parcel IDs ----
parcels_ag_hmst_acres_df_2 <- parcels_ag_hmst_acres_df_2 %>%
  group_by(PID) %>%
  arrange(year) %>%
  mutate(ACREAGE = fill_non_zero(ACREAGE)) %>%
  ungroup() %>%
  mutate(
    PID = if_else(nchar(PID) == 12, paste0("0", PID), PID),
    PID_SUBRECORD = paste0(PID, "_", SUBRECORD_NO)
  )

# ---- Cross-tabulate Program Variables by Property Type ----
table(parcels_ag_hmst_acres_df_2$super_property_type, parcels_ag_hmst_acres_df_2$AG_PRESERVE)
table(parcels_ag_hmst_acres_df_2$super_property_type, parcels_ag_hmst_acres_df_2$R_Rural_Preserve)
table(parcels_ag_hmst_acres_df_2$super_property_type, parcels_ag_hmst_acres_df_2$G_Green_Acres)

# ---- Calculate City Tax Variables and Adjust for Inflation ----
parcels_ag_hmst_acres_df_2 <- parcels_ag_hmst_acres_df_2 %>%
  mutate(
    CITY_LNTC_TAX_RATE = city_rate_data$CITY_LNTC_TAX_RATE[match(
      paste(year, CITY_ID),
      paste(city_rate_data$year, city_rate_data$CITY_ID)
    )],
    CITY_LNTC_TAX = LOCAL_NET_TAX_CAPACITY * CITY_LNTC_TAX_RATE
  )
# Set CPI base (year 2005) and match CPI to TAX_YEAR
base_cpi <- cpi$Annual[cpi$Year == 2005]
parcels_ag_hmst_acres_df_2$CPI <- cpi$Annual[match(parcels_ag_hmst_acres_df_2$TAX_YEAR, cpi$Year)]
# Adjust tax and property values for inflation
parcels_ag_hmst_acres_df_2$REAL_CITY_TAX_DOLLAR <- parcels_ag_hmst_acres_df_2$CITY_LNTC_TAX * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)
parcels_ag_hmst_acres_df_2$REAL_EMV_LAND <- parcels_ag_hmst_acres_df_2$EMV_LAND * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)
parcels_ag_hmst_acres_df_2$REAL_EMV_BLDG <- parcels_ag_hmst_acres_df_2$EMV_BLDG * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)
parcels_ag_hmst_acres_df_2$REAL_LOCAL_NET_TAX_CAPACITY <- parcels_ag_hmst_acres_df_2$LOCAL_NET_TAX_CAPACITY * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)
parcels_ag_hmst_acres_df_2$REAL_ESTIMATED_MARKET_VALUE <- parcels_ag_hmst_acres_df_2$ESTIMATED_MARKET_VALUE * (base_cpi / parcels_ag_hmst_acres_df_2$CPI)

# ---- Compute Average Tax Data by TAX_YEAR for Plotting ----
avg_tax_data <- parcels_ag_hmst_acres_df_2 %>%
  group_by(TAX_YEAR) %>%
  summarise(
    Avg_REAL_CITY_TAX_DOLLAR = mean(REAL_CITY_TAX_DOLLAR, na.rm = TRUE),
    Avg_CITY_LNTC_TAX = mean(CITY_LNTC_TAX, na.rm = TRUE),
    Avg_REAL_EMV_LAND = mean(REAL_EMV_LAND, na.rm = TRUE),
    Avg_EMV_LAND = mean(EMV_LAND, na.rm = TRUE),
    Avg_REAL_ESTIMATED_MARKET_VALUE = mean(REAL_ESTIMATED_MARKET_VALUE, na.rm = TRUE),
    Avg_ESTIMATED_MARKET_VALUE = mean(ESTIMATED_MARKET_VALUE, na.rm = TRUE)
  )

# ---- Plot Average Tax Trends Over Time ----
ggplot(avg_tax_data, aes(x = TAX_YEAR)) +
  geom_line(aes(y = Avg_REAL_CITY_TAX_DOLLAR, color = "Avg_REAL_CITY_TAX_DOLLAR"), size = 1) +
  geom_line(aes(y = Avg_CITY_LNTC_TAX, color = "Avg_CITY_LNTC_TAX"), size = 1) +
  labs(title = "Average Real City Tax Dollar and City LNTC Tax Over Time",
       x = "Year", y = "Average Tax Amount", color = "Tax Type") +
  theme_minimal()

ggplot(avg_tax_data, aes(x = TAX_YEAR)) +
  geom_line(aes(y = Avg_REAL_EMV_LAND, color = "Avg Real EMV Land"), size = 1) +
  geom_line(aes(y = Avg_EMV_LAND, color = "EMV Land"), size = 1) +
  labs(title = "Average Real EMV Dollar and City EMV Over Time",
       x = "Year", y = "Average EMV Amount", color = "Tax Type") +
  theme_minimal()

# ---- Fit Baseline Regression Model for Non-Program Parcels ----

####find non program parcels ----

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

parcels_in_program <- parcels_ag_hmst_acres_df_2 %>%
  filter(PID_SUBRECORD %in% program_status$PID_SUBRECORD)

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

