# ============================================
# County Health Rankings 2025 — Data Load
# ASA Poster Competition 2025
# ============================================

# Load libraries (already installed, just loading)
library(tidyverse)
library(readxl)

# Load main dataset
chr <- read_csv("data/analytic_data2025_v3.csv")

# First look
glimpse(chr)
dim(chr)

# See all column names
names(chr)

# First 6 rows
head(chr)

library(tidyverse)

# Load raw data
chr <- read_csv("data/analytic_data2025_v3.csv")

# Remove header row (row 1) and national summary (row 2)
# Keep only actual county rows
chr_clean <- chr |>
  slice(-1, -2) |>  # remove first two rows
  filter(`County FIPS Code` != "000")  # remove state-level summary rows

# Select only the raw value columns we care about
# These are our outcome + predictor variables
chr_select <- chr_clean |>
  select(
    state = `State Abbreviation`,
    county = Name,
    fips = `5-digit FIPS Code`,
    life_expectancy = `Life Expectancy raw value`,
    premature_death = `Premature Death raw value`,
    poor_health = `Poor or Fair Health raw value`,
    adult_smoking = `Adult Smoking raw value`,
    adult_obesity = `Adult Obesity raw value`,
    physical_inactivity = `Physical Inactivity raw value`,
    excessive_drinking = `Excessive Drinking raw value`,
    drug_overdose = `Drug Overdose Deaths raw value`,
    uninsured = `Uninsured raw value`,
    primary_care = `Primary Care Physicians raw value`,
    mental_health_providers = `Mental Health Providers raw value`,
    preventable_hospital = `Preventable Hospital Stays raw value`,
    some_college = `Some College raw value`,
    unemployment = `Unemployment raw value`,
    children_poverty = `Children in Poverty raw value`,
    income_inequality = `Income Inequality raw value`,
    median_income = `Median Household Income raw value`,
    food_insecurity = `Food Insecurity raw value`,
    insufficient_sleep = `Insufficient Sleep raw value`,
    social_associations = `Social Associations raw value`,
    air_pollution = `Air Pollution: Particulate Matter raw value`,
    broadband = `Broadband Access raw value`,
    homeownership = `Homeownership raw value`,
    pct_rural = `% Rural raw value`,
    teen_births = `Teen Births raw value`,
    firearm_fatalities = `Firearm Fatalities raw value`,
    homicides = `Homicides raw value`,
    voter_turnout = `Voter Turnout raw value`,
    feelings_loneliness = `Feelings of Loneliness raw value`,
    school_funding = `School Funding Adequacy raw value`,
    diabetes = `Diabetes Prevalence raw value`,
    population = `Population raw value`
  )

# Convert all numeric columns from character to numeric
chr_select <- chr_select |>
  mutate(across(-c(state, county, fips), as.numeric))

# Check result
glimpse(chr_select)
dim(chr_select)

# Check how many NAs in life expectancy
sum(is.na(chr_select$life_expectancy))

# Basic summary of life expectancy
summary(chr_select$life_expectancy)

# Save cleaned data
saveRDS(chr_select, "data/chr_clean.rds")

library(tidyverse)

chr <- readRDS("data/chr_clean.rds") |> 
  filter(!is.na(life_expectancy))

# How many usable counties
nrow(chr)

# Bottom 10 counties by life expectancy
chr |>
  arrange(life_expectancy) |>
  select(county, state, life_expectancy) |>
  head(10)

# Top 10 counties by life expectancy
chr |>
  arrange(desc(life_expectancy)) |>
  select(county, state, life_expectancy) |>
  head(10)

# The actual gap
max(chr$life_expectancy) - min(chr$life_expectancy)

# Distribution shape
hist(chr$life_expectancy, breaks = 50)

# Full correlation with life expectancy — ranked by strength
cor_with_le <- chr |>
  select(-state, -county, -fips, -population) |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  select(life_expectancy) |>
  rownames_to_column("variable") |>
  filter(variable != "life_expectancy") |>
  arrange(desc(abs(life_expectancy))) |>
  rename(correlation = life_expectancy)

print(cor_with_le, n = 30)

# Also check within-state disparity
state_disparity <- chr |>
  group_by(state) |>
  summarise(
    max_le = max(life_expectancy, na.rm = TRUE),
    min_le = min(life_expectancy, na.rm = TRUE),
    gap = max_le - min_le,
    n_counties = n()
  ) |>
  filter(n_counties >= 5) |>  # only states with enough counties
  arrange(desc(gap))

print(state_disparity, n = 20)

cor_with_le

# See full disparity table
print(state_disparity, n = 50)

# Which states have the SMALLEST internal gap
state_disparity |> arrange(gap) |> head(10)

# For the Native American angle — flag reservation counties
# These FIPS codes are known reservation counties
chr <- chr |>
  mutate(
    is_reservation = state %in% c("SD", "ND", "MT", "NM", "AZ") &
    life_expectancy < 65
  )

# What is the gap WITHOUT reservation counties
chr |>
  filter(!is_reservation) |>
  summarise(
    max_le = max(life_expectancy),
    min_le = min(life_expectancy),
    gap = max_le - min_le
  )

# What variables look different for reservation vs non-reservation
chr |>
  group_by(is_reservation) |>
  summarise(
    avg_life_expectancy = mean(life_expectancy, na.rm = TRUE),
    avg_income = mean(median_income, na.rm = TRUE),
    avg_some_college = mean(some_college, na.rm = TRUE),
    avg_physical_inactivity = mean(physical_inactivity, na.rm = TRUE),
    avg_uninsured = mean(uninsured, na.rm = TRUE),
    n = n()
  )




# 1. Investigate the excessive drinking paradox more
# Is it confounded by something else?
chr |>
  select(excessive_drinking, median_income, some_college, 
         life_expectancy, pct_rural) |>
  cor(use = "pairwise.complete.obs")

# 2. Investigate voter turnout correlation
chr |>
  select(voter_turnout, median_income, some_college,
         life_expectancy) |>
  cor(use = "pairwise.complete.obs")

# 3. Virginia disparity — what makes VA so unequal internally?
chr |>
  filter(state == "VA") |>
  arrange(life_expectancy) |>
  select(county, life_expectancy, median_income, 
         some_college, pct_rural) |>
  print(n = 20)

# 4. Add US region classification
chr <- chr |>
  mutate(region = case_when(
    state %in% c("ME","NH","VT","MA","RI","CT",
                 "NY","NJ","PA") ~ "Northeast",
    state %in% c("OH","IN","IL","MI","WI","MN",
                 "IA","MO","ND","SD","NE","KS") ~ "Midwest",
    state %in% c("DE","MD","DC","VA","WV","NC","SC",
                 "GA","FL","KY","TN","AL","MS",
                 "AR","LA","OK","TX") ~ "South",
    state %in% c("MT","ID","WY","CO","NM","AZ",
                 "UT","NV","WA","OR","CA","AK","HI") ~ "West",
    TRUE ~ "Other"
  ))

# 5. Regional summary
chr |>
  group_by(region) |>
  summarise(
    avg_life_expectancy = mean(life_expectancy, na.rm = TRUE),
    median_life_expectancy = median(life_expectancy, na.rm = TRUE),
    min_le = min(life_expectancy, na.rm = TRUE),
    max_le = max(life_expectancy, na.rm = TRUE),
    internal_gap = max_le - min_le,
    n = n()
  ) |>
  arrange(desc(avg_life_expectancy))

# 6. Does the excessive drinking paradox disappear by region?
chr |>
  group_by(region) |>
  summarise(
    cor_drinking_le = cor(excessive_drinking, life_expectancy, 
                          use = "pairwise.complete.obs"),
    cor_income_le = cor(median_income, life_expectancy,
                        use = "pairwise.complete.obs")
  )




# Virginia deeper dive - what separates top from bottom?
chr |>
  filter(state == "VA") |>
  mutate(va_group = ifelse(life_expectancy >= 80, "High (80+)", 
                    ifelse(life_expectancy < 70, "Low (<70)", "Middle"))) |>
  group_by(va_group) |>
  summarise(
    avg_le = mean(life_expectancy),
    avg_income = mean(median_income, na.rm = TRUE),
    avg_college = mean(some_college, na.rm = TRUE),
    avg_rural = mean(pct_rural, na.rm = TRUE),
    avg_teen_births = mean(teen_births, na.rm = TRUE),
    n = n()
  )

# Multivariate regression with focused variable set
model <- lm(life_expectancy ~ 
              median_income +
              some_college +
              adult_smoking +
              physical_inactivity +
              excessive_drinking +
              teen_births +
              voter_turnout +
              food_insecurity,
            data = chr)

summary(model)

# What does model predict vs actual - find outliers
chr <- chr |>
  mutate(
    predicted_le = predict(model, newdata = chr),
    residual = life_expectancy - predicted_le
  )

# Counties beating predictions most
chr |>
  arrange(desc(residual)) |>
  select(county, state, life_expectancy, predicted_le, residual) |>
  head(15)

# Counties failing predictions most  
chr |>
  arrange(residual) |>
  select(county, state, life_expectancy, predicted_le, residual) |>
  head(15)

# Regional correlation breakdown for excessive drinking
# Is the paradox consistent or region-specific?
chr |>
  group_by(region) |>
  summarise(
    avg_excessive_drinking = mean(excessive_drinking, na.rm = TRUE),
    avg_life_expectancy = mean(life_expectancy, na.rm = TRUE),
    avg_income = mean(median_income, na.rm = TRUE)
  ) |>
  arrange(desc(avg_life_expectancy))






# 1. Investigate the physical inactivity paradox
# Is it confounded by rurality or region?
chr |>
  group_by(region) |>
  summarise(
    cor_inactivity_le = cor(physical_inactivity, life_expectancy,
                            use = "pairwise.complete.obs"),
    cor_inactivity_rural = cor(physical_inactivity, pct_rural,
                               use = "pairwise.complete.obs"),
    avg_inactivity = mean(physical_inactivity, na.rm = TRUE)
  )

# 2. Investigate Kingman County KS - biggest underperformer
chr |>
  filter(county == "Kingman County" & state == "KS") |>
  select(county, state, life_expectancy, predicted_le, residual,
         median_income, some_college, adult_smoking,
         physical_inactivity, excessive_drinking, teen_births,
         drug_overdose, homicides, firearm_fatalities)

# 3. Investigate Presidio County TX - biggest mystery overperformer
chr |>
  filter(county == "Presidio County" & state == "TX") |>
  select(county, state, life_expectancy, predicted_le, residual,
         median_income, some_college, adult_smoking,
         physical_inactivity, excessive_drinking, teen_births,
         drug_overdose, homicides, firearm_fatalities,
         pct_rural, uninsured)

# 4. Run cleaner model without confounded variables
# Remove voter_turnout (not significant)
# Keep core variables
model2 <- lm(life_expectancy ~
               median_income +
               adult_smoking +
               excessive_drinking +
               teen_births +
               food_insecurity,
             data = chr)

summary(model2)

# 5. Does the excessive drinking paradox hold within income groups?
chr <- chr |>
  mutate(income_quartile = ntile(median_income, 4))

chr |>
  group_by(income_quartile) |>
  summarise(
    cor_drinking_le = cor(excessive_drinking, life_expectancy,
                          use = "pairwise.complete.obs"),
    avg_drinking = mean(excessive_drinking, na.rm = TRUE),
    avg_le = mean(life_expectancy, na.rm = TRUE),
    avg_income = mean(median_income, na.rm = TRUE)
  )

# 6. State disparity - is the gap driven by reservation counties?
# Compare states with and without identified low-LE reservation counties
reservation_states <- c("SD", "ND", "MT", "NM", "AZ")

state_disparity |>
  mutate(has_reservation = state %in% reservation_states) |>
  group_by(has_reservation) |>
  summarise(
    avg_gap = mean(gap),
    median_gap = median(gap)
  )








# 1. Does broadband access predict longevity?
# Modern variable - internet access as health predictor
cor(chr$broadband, chr$life_expectancy, use = "pairwise.complete.obs")

# 2. Does school funding predict longevity?
cor(chr$school_funding, chr$life_expectancy, use = "pairwise.complete.obs")

# 3. Drug overdose by region - is the opioid crisis geographic?
chr |>
  group_by(region) |>
  summarise(
    avg_drug_overdose = mean(drug_overdose, na.rm = TRUE),
    cor_overdose_le = cor(drug_overdose, life_expectancy,
                         use = "pairwise.complete.obs")
  )

# 4. Is the excessive drinking paradox stronger in certain regions?
chr |>
  group_by(income_quartile) |>
  summarise(
    avg_drinking = mean(excessive_drinking, na.rm = TRUE),
    avg_smoking = mean(adult_smoking, na.rm = TRUE),
    avg_le = mean(life_expectancy, na.rm = TRUE)
  )

# 5. Smoking vs drinking tradeoff
# Do counties drink MORE but smoke LESS - and which matters more?
chr |>
  mutate(
    high_drinking = excessive_drinking > median(excessive_drinking, na.rm = TRUE),
    high_smoking = adult_smoking > median(adult_smoking, na.rm = TRUE)
  ) |>
  group_by(high_drinking, high_smoking) |>
  summarise(
    avg_le = mean(life_expectancy, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(avg_le))

# 6. Kingman County KS - show all its values to understand why it fails
chr |>
  filter(county == "Kingman County", state == "KS") |>
  select(life_expectancy, predicted_le, residual,
         median_income, adult_smoking, excessive_drinking,
         teen_births, drug_overdose, firearm_fatalities,
         food_insecurity, pct_rural, broadband)




# Check what's available related to your ideas
names(chr) |> 
  grep(pattern = "transport|commut|firearm|food|tax|job|care|park|access", 
       value = TRUE, ignore.case = TRUE)

# Firearm fatalities correlation and regional pattern
chr |>
  group_by(region) |>
  summarise(
    avg_firearm = mean(firearm_fatalities, na.rm = TRUE),
    cor_firearm_le = cor(firearm_fatalities, life_expectancy,
                        use = "pairwise.complete.obs")
  )

# Food insecurity vs food environment index
cor(chr$food_insecurity, chr$life_expectancy, use = "pairwise.complete.obs")

# Access to parks correlation
cor(chr$`access_to_parks`, chr$life_expectancy, 
    use = "pairwise.complete.obs")

# Long commute correlation
chr |>
  select(life_expectancy) |>
  bind_cols(chr |> select(contains("commut"))) |>
  cor(use = "pairwise.complete.obs")

# Homeownership vs life expectancy by region
chr |>
  group_by(region) |>
  summarise(
    cor_homeown_le = cor(homeownership, life_expectancy,
                        use = "pairwise.complete.obs"),
    avg_homeown = mean(homeownership, na.rm = TRUE)
  )

# Social associations - community involvement
# Already in dataset - run by region
chr |>
  group_by(region) |>
  summarise(
    cor_social_le = cor(social_associations, life_expectancy,
                       use = "pairwise.complete.obs"),
    avg_social = mean(social_associations, na.rm = TRUE)
  )





# Fix parks - find exact column name
names(chr)[grep("park|Park", names(chr))]

# Check exact column names for access variables
names(chr)[grep("Access|access", names(chr))]

# 1. Regional patterns - smoking vs drinking tradeoff BY REGION
# Does the pattern hold everywhere or only certain regions?
chr |>
  group_by(region) |>
  summarise(
    cor_smoking_le = cor(adult_smoking, life_expectancy,
                        use = "pairwise.complete.obs"),
    cor_drinking_le = cor(excessive_drinking, life_expectancy,
                         use = "pairwise.complete.obs"),
    avg_smoking = mean(adult_smoking, na.rm = TRUE),
    avg_drinking = mean(excessive_drinking, na.rm = TRUE),
    avg_le = mean(life_expectancy, na.rm = TRUE)
  )

# 2. Midwest overdose + healthcare hypothesis
# Does more healthcare access reduce the overdose death impact?
chr |>
  filter(region == "Midwest") |>
  summarise(
    cor_overdose_primarycare = cor(drug_overdose, primary_care,
                                  use = "pairwise.complete.obs"),
    cor_overdose_mentalhealth = cor(drug_overdose, mental_health_providers,
                                   use = "pairwise.complete.obs"),
    cor_primarycare_le = cor(primary_care, life_expectancy,
                            use = "pairwise.complete.obs"),
    cor_mentalhealth_le = cor(mental_health_providers, life_expectancy,
                             use = "pairwise.complete.obs")
  )

# Compare overdose deaths by healthcare access quartile IN MIDWEST ONLY
chr |>
  filter(region == "Midwest") |>
  mutate(healthcare_quartile = ntile(primary_care, 4)) |>
  group_by(healthcare_quartile) |>
  summarise(
    avg_drug_overdose = mean(drug_overdose, na.rm = TRUE),
    avg_le = mean(life_expectancy, na.rm = TRUE),
    avg_primary_care = mean(primary_care, na.rm = TRUE),
    n = n()
  )

# 3. Firearm fatalities regional story
# South AND West both average ~20 deaths per 100,000 but different correlations
# What else is different between them?
chr |>
  filter(region %in% c("South", "West", "Northeast", "Midwest")) |>
  group_by(region) |>
  summarise(
    avg_firearm_per100k = mean(firearm_fatalities, na.rm = TRUE),
    avg_homicide_per100k = mean(homicides, na.rm = TRUE),
    avg_le = mean(life_expectancy, na.rm = TRUE),
    # What proportion of firearm deaths are homicides vs suicide?
    avg_ratio = mean(homicides / firearm_fatalities, na.rm = TRUE)
  )

# 4. Smoking vs drinking 2x2 table by REGION
# Does the pattern hold in all 4 regions?
chr |>
  group_by(region) |>
  mutate(
    high_drinking = excessive_drinking > median(excessive_drinking, na.rm = TRUE),
    high_smoking = adult_smoking > median(adult_smoking, na.rm = TRUE)
  ) |>
  group_by(region, high_drinking, high_smoking) |>
  summarise(
    avg_le = mean(life_expectancy, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(region, desc(avg_le))

# 5. Urban vs rural life expectancy
# Simple but important - is rural actually worse?
chr |>
  mutate(rural_category = case_when(
    pct_rural < 0.20 ~ "Urban (<20% rural)",
    pct_rural < 0.50 ~ "Suburban (20-50% rural)",
    pct_rural < 0.80 ~ "Mixed (50-80% rural)",
    TRUE ~ "Rural (80%+ rural)"
  )) |>
  group_by(rural_category) |>
  summarise(
    avg_le = mean(life_expectancy, na.rm = TRUE),
    avg_income = mean(median_income, na.rm = TRUE),
    avg_smoking = mean(adult_smoking, na.rm = TRUE),
    avg_drinking = mean(excessive_drinking, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(avg_le))

# 6. Broadband deeper dive - is it just a proxy for income?
chr |>
  group_by(income_quartile) |>
  summarise(
    avg_broadband = mean(broadband, na.rm = TRUE),
    cor_broadband_le = cor(broadband, life_expectancy,
                          use = "pairwise.complete.obs"),
    avg_le = mean(life_expectancy, na.rm = TRUE)
  )



# Check ALL remaining variables we haven't analyzed yet
chr |>
  select(life_expectancy, 
         insufficient_sleep,
         feelings_loneliness,
         social_associations,
         income_inequality,
         preventable_hospital,
         homeownership,
         teen_births,
         diabetes,
         air_pollution,
         drug_overdose,
         homicides,
         pct_rural,
         school_funding,
         voter_turnout,
         broadband) |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  select(life_expectancy) |>
  rownames_to_column("variable") |>
  filter(variable != "life_expectancy") |>
  arrange(desc(abs(life_expectancy)))

# Variables we haven't properly explored
chr |>
  select(life_expectancy,
         insufficient_sleep,
         feelings_loneliness, 
         social_associations,
         income_inequality,
         preventable_hospital,
         teen_births,
         diabetes,
         air_pollution,
         homicides,
         school_funding,
         child_care = `child_care_cost_burden`,
         disconnected_youth = `disconnected_youth`) |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  select(life_expectancy) |>
  rownames_to_column("variable") |>
  filter(variable != "life_expectancy") |>
  arrange(desc(abs(life_expectancy)))

# Loneliness specifically - new variable, potentially very interesting
chr |>
  group_by(region) |>
  summarise(
    avg_loneliness = mean(feelings_loneliness, na.rm = TRUE),
    cor_loneliness_le = cor(feelings_loneliness, life_expectancy,
                           use = "pairwise.complete.obs"),
    cor_loneliness_smoking = cor(feelings_loneliness, adult_smoking,
                                use = "pairwise.complete.obs")
  )

# Income inequality - Gini coefficient essentially
# Does HOW income is distributed matter beyond average income?
chr |>
  group_by(income_quartile) |>
  summarise(
    avg_inequality = mean(income_inequality, na.rm = TRUE),
    cor_inequality_le = cor(income_inequality, life_expectancy,
                           use = "pairwise.complete.obs"),
    avg_le = mean(life_expectancy, na.rm = TRUE)
  )

# Diabetes as mediator
# Is diabetes the mechanism through which obesity affects life expectancy?
chr |>
  summarise(
    cor_obesity_diabetes = cor(adult_obesity, diabetes, 
                              use = "pairwise.complete.obs"),
    cor_diabetes_le = cor(diabetes, life_expectancy,
                         use = "pairwise.complete.obs"),
    cor_obesity_le = cor(adult_obesity, life_expectancy,
                        use = "pairwise.complete.obs")
  )

# Teen births - is it just poverty or independent effect?
chr |>
  group_by(income_quartile) |>
  summarise(
    avg_teen_births = mean(teen_births, na.rm = TRUE),
    cor_teen_le = cor(teen_births, life_expectancy,
                     use = "pairwise.complete.obs"),
    avg_le = mean(life_expectancy, na.rm = TRUE)
  )

# Insufficient sleep by region
chr |>
  group_by(region) |>
  summarise(
    avg_sleep = mean(insufficient_sleep, na.rm = TRUE),
    cor_sleep_le = cor(insufficient_sleep, life_expectancy,
                      use = "pairwise.complete.obs")
  )
  