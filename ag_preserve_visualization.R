#### Effects at County, City, and individual level ----

##### County level effects #####


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
ggplot(county_effects, aes(x = year, y = mean_tax_share_in_program)) +
  geom_line() +
  labs(
    title = "Mean Tax Share (In Program)",
    x = "Year",
    y = "Mean Tax Share"
  ) +
  theme_minimal()

ggplot(county_effects, aes(x = year, y = ag_preserv_acres_prop)) +
  geom_line() +
  labs(title = "Agricultural Preservation 
       Acres Proportion", x = "Year", y = "Proportion")

ggplot(county_effects, aes(x = year, y = green_acres_acres_prop)) +
  geom_line() +
  labs(title = "Green Acres Proportion", x = "Year", y = "Proportion")

ggplot(county_effects, aes(x = year, y = share_in_program)) +
  geom_line() +
  labs(title = "Share in Program", x = "Year", y = "Share")


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
  geom_point(data = city_effects, aes(x = share_in_program, y = log(total_tax_revenue_non_program+1)), color = "blue") +
  theme_minimal()

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
  aes(x = YoY_change_share_in_program_city)
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
