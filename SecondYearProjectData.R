library(readr)
library(tidyverse)
library(tidycensus)
library(survey)
library(modelsummary)
library(broom)


readRenviron("~/.Renviron")
acs_controls <- get_acs(
  geography = "county",
  variables = c(
    population = "B01003_001",
    median_income = "B19013_001",
    median_age = "B01002_001",
    
    educ_total = "B15003_001",
    ba = "B15003_022",
    ma = "B15003_023",
    prof = "B15003_024",
    phd = "B15003_025",
    
    white = "B02001_002",
    black = "B02001_003",
    asian = "B02001_005",
    hispanic = "B03003_003",
    
    m18_19 = "B01001_007",
    m20 = "B01001_008",
    m21 = "B01001_009",
    m22_24 = "B01001_010",
    m25_29 = "B01001_011",
    
    f18_19 = "B01001_031",
    f20 = "B01001_032",
    f21 = "B01001_033",
    f22_24 = "B01001_034",
    f25_29 = "B01001_035"
  ),
  year = 2024,
  survey = "acs5"
)






readRenviron("~/.Renviron")
acs_CT <- get_acs(
  geography = "county",
  state= "CT",
  variables = c(
    population = "B01003_001",
    median_income = "B19013_001",
    median_age = "B01002_001",
    
    educ_total = "B15003_001",
    ba = "B15003_022",
    ma = "B15003_023",
    prof = "B15003_024",
    phd = "B15003_025",
    
    white = "B02001_002",
    black = "B02001_003",
    asian = "B02001_005",
    hispanic = "B03003_003",
    
    
    m18_19 = "B01001_007",
    m20 = "B01001_008",
    m21 = "B01001_009",
    m22_24 = "B01001_010",
    m25_29 = "B01001_011",
    
    f18_19 = "B01001_031",
    f20 = "B01001_032",
    f21 = "B01001_033",
    f22_24 = "B01001_034",
    f25_29 = "B01001_035"
  ),
  year = 2021,
  survey = "acs5"
)



acs_ct_wide <- acs_CT |>
  select(GEOID, NAME, variable, estimate) |>
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) |>
  mutate(
    ba_plus = ba + ma + prof + phd,
    ba_plus_share = ba_plus / educ_total,
    white_share = white / population,
    black_share = black / population,
    asian_share = asian / population,
    hispanic_share = hispanic / population,
    age18_29 = m18_19 + m20 + m21 + m22_24 + m25_29 +
      f18_19 + f20 + f21 + f22_24 + f25_29,
    age18_29_share = age18_29 / population
  ) |>
  separate(NAME, into = c("county_name", "state"), sep = ", ") |>
  mutate(
    state       = toupper(state),
    county_name = toupper(county_name),
    county_name = gsub("\\.", "", county_name),
    county_name = gsub(" COUNTY$", "", county_name),
    county_name = gsub(" PARISH$", "", county_name)
  ) |>
  mutate(
    county_name = if_else(state == "MISSOURI" & county_name == "ST LOUIS",
                          "ST LOUIS COUNTY", county_name)
  )




ct_acs_merge <- acs_ct_wide |>
  select(state, county_name, population, median_income, median_age,
         ba_plus_share, white_share, black_share, asian_share, 
         hispanic_share, age18_29_share)


analysis_data1 <- analysis_data |>
  left_join(ct_acs_merge, 
            by = c("state", "county_name"),
            suffix = c("", "_ct")) |>
  mutate(
    population     = coalesce(population,     population_ct),
    median_income  = coalesce(median_income,  median_income_ct),
    median_age     = coalesce(median_age,     median_age_ct),
    ba_plus_share  = coalesce(ba_plus_share,  ba_plus_share_ct),
    white_share    = coalesce(white_share,    white_share_ct),
    black_share    = coalesce(black_share,    black_share_ct),
    asian_share    = coalesce(asian_share,    asian_share_ct),
    hispanic_share = coalesce(hispanic_share, hispanic_share_ct),
    age18_29_share = coalesce(age18_29_share, age18_29_share_ct)
  ) |>
  select(-ends_with("_ct"))

va_independent_cities <- c(
  "ALEXANDRIA", "BRISTOL", "BUENA VISTA", "CHARLOTTESVILLE",
  "CHESAPEAKE", "COLONIAL HEIGHTS", "COVINGTON", "DANVILLE",
  "EMPORIA", "FALLS CHURCH", "FREDERICKSBURG",
  "GALAX", "HAMPTON", "HARRISONBURG", "HOPEWELL", "LEXINGTON",
  "LYNCHBURG", "MANASSAS", "MANASSAS PARK", "MARTINSVILLE",
  "NEWPORT NEWS", "NORFOLK", "NORTON", "PETERSBURG",
  "POQUOSON", "PORTSMOUTH", "RADFORD",
  "SALEM", "STAUNTON", "SUFFOLK",
  "VIRGINIA BEACH", "WAYNESBORO", "WILLIAMSBURG", "WINCHESTER"
)





## Downloading Data
all_protest_data <- read_csv("~/Downloads/ccc_compiled_20212024.csv")
swing_states <- c("PA","MI","WI","NV","GA","AZ","NC")
county_elections <- read_csv("~/Downloads/countypres_2000-2024.csv")
anes_raw <- read_csv("~/Downloads/anes_timeseries_2024_csv_20250808/anes_timeseries_2024_csv_20250808.csv")
panel <- read_sav("anes_mergedfile_2016-2020-2024panel_20251030.sav") |>
  as_factor()

## Filtering protest data
gaza_protests <- all_protest_data |>
  mutate(date = as.Date(date)) |>
  filter(date >= as.Date("2023-10-08") & date <= as.Date("2024-11-05")) |>
  filter(grepl("Palestinian liberation", claims_summary, ignore.case = TRUE)) 

## Grouping by county
gaza_protests <- gaza_protests |>
  group_by(state, resolved_county) |>
  summarise(protest_count = n(), .groups = "drop") |> 
  filter(!is.na(resolved_county)) |>
  rename(county = resolved_county)

## Grouping election data
county_pres_wide <- county_elections |>
  filter(year > 2008) |>
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) |>
  # For Mahaska 2024 specifically, filter to TOTAL mode only
  filter(!(year == 2024 & state == "IOWA" & 
             county_name == "MAHASKA" & mode != "TOTAL")) |>
  group_by(year, state, county_name, party) |>
  summarise(
    party_votes = if_else(
      # Use sum for 2020 AND for NC 2024, max for everyone else in 2024
      year[1] == 2020 | (year[1] == 2024 & state[1] == "NORTH CAROLINA"),
      sum(candidatevotes, na.rm = TRUE),
      max(candidatevotes, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  left_join(
    county_elections |>
      group_by(year, state, county_name) |>
      summarise(totalvotes = max(totalvotes, na.rm = TRUE), .groups = "drop"),
    by = c("year", "state", "county_name")
  ) |>
  mutate(vote_pct = party_votes / totalvotes) |>
  select(year, state, county_name, party, vote_pct) |>
  pivot_wider(
    names_from = party,
    values_from = vote_pct
  ) |>
  rename(
    pct_dem = DEMOCRAT,
    pct_rep = REPUBLICAN
  )

county_pres_wide <- county_pres_wide |>
  mutate(
    county_name = gsub("\\.", "", county_name)
  )

county_pres_wide <- county_pres_wide |>
  mutate(
    county_name = gsub("\\.", "", county_name),
    county_name = case_when(
      county_name == "SAINT LOUIS" ~ "ST LOUIS",
      county_name == "DONA ANA"    ~ "DOÑA ANA",
      state == "ILLINOIS" & county_name == "DEWITT"                  ~ "DE WITT",
      state == "ILLINOIS" & county_name == "JODAVIESS"               ~ "JO DAVIESS",
      # Missouri fix
      state == "MISSOURI" & county_name == "DE KALB"                 ~ "DEKALB",
      state == "LOUISIANA" & county_name == "LA SALLE"                ~ "LASALLE",
      state == "VIRGINIA" & county_name %in% va_independent_cities   ~ paste0(county_name, " CITY"),
      TRUE ~ county_name
    )
  )
county_pres_wide <- county_pres_wide |>
  distinct(state, county_name, year, .keep_all = TRUE)

county_pres_countywide <- county_pres_wide |>
  pivot_wider(
    id_cols = c(state, county_name),
    names_from = year,
    values_from = c(pct_dem, pct_rep),
    names_glue = "{.value}_{year}"
  )

## Making pre 2024 election averages
county_pres_countywide <- county_pres_countywide |>
  mutate(
    typical_dem = rowMeans(
      cbind(pct_dem_2012, pct_dem_2016, pct_dem_2020),
      na.rm = TRUE
    )
  )

## Merging election data and Gaza data
gaza_protests_clean <- gaza_protests |>
  mutate(
    state  = iconv(state,  from = "", to = "UTF-8", sub = ""),
    county = iconv(county, from = "", to = "UTF-8", sub = "")
  ) |>
  mutate(
    state = recode(state,
                   "AL" = "ALABAMA",
                   "AK" = "ALASKA",
                   "AZ" = "ARIZONA",
                   "AR" = "ARKANSAS",
                   "CA" = "CALIFORNIA",
                   "CO" = "COLORADO",
                   "CT" = "CONNECTICUT",
                   "DE" = "DELAWARE",
                   "FL" = "FLORIDA",
                   "GA" = "GEORGIA",
                   "HI" = "HAWAII",
                   "ID" = "IDAHO",
                   "IL" = "ILLINOIS",
                   "IN" = "INDIANA",
                   "IA" = "IOWA",
                   "KS" = "KANSAS",
                   "KY" = "KENTUCKY",
                   "LA" = "LOUISIANA",
                   "ME" = "MAINE",
                   "MD" = "MARYLAND",
                   "MA" = "MASSACHUSETTS",
                   "MI" = "MICHIGAN",
                   "MN" = "MINNESOTA",
                   "MS" = "MISSISSIPPI",
                   "MO" = "MISSOURI",
                   "MT" = "MONTANA",
                   "NE" = "NEBRASKA",
                   "NV" = "NEVADA",
                   "NH" = "NEW HAMPSHIRE",
                   "NJ" = "NEW JERSEY",
                   "NM" = "NEW MEXICO",
                   "NY" = "NEW YORK",
                   "NC" = "NORTH CAROLINA",
                   "ND" = "NORTH DAKOTA",
                   "OH" = "OHIO",
                   "OK" = "OKLAHOMA",
                   "OR" = "OREGON",
                   "PA" = "PENNSYLVANIA",
                   "RI" = "RHODE ISLAND",
                   "SC" = "SOUTH CAROLINA",
                   "SD" = "SOUTH DAKOTA",
                   "TN" = "TENNESSEE",
                   "TX" = "TEXAS",
                   "UT" = "UTAH",
                   "VT" = "VERMONT",
                   "VA" = "VIRGINIA",
                   "WA" = "WASHINGTON",
                   "WV" = "WEST VIRGINIA",
                   "WI" = "WISCONSIN",
                   "WY" = "WYOMING",
                   "DC" = "DISTRICT OF COLUMBIA"
    ),
    county = toupper(county),
    county = str_replace_all(county, "\\.", ""),
    county = str_replace(county, " COUNTY$", ""),
    county = str_replace(county, " PARISH$", ""),
    county = if_else(county == "ST LOUIS", "ST LOUIS COUNTY", county),
    county = if_else(county == "DO\u00ffA ANA" | 
                       county == "DO?A ANA"  | 
                       county == "DOA ANA",    "DO\u00d1A ANA", county)
  )


merged_data <- county_pres_countywide |>
  left_join(
    gaza_protests_clean,
    by = c("state", "county_name" = "county")
  ) |>
  mutate(
    protest_count = coalesce(protest_count, 0)
  ) |>
  filter(!state=="ALASKA") |>
  filter(!county_name=="STATEWIDE WRITEIN")




acs_controls_wide <- acs_controls |>
  select(GEOID, NAME, variable, estimate) |>
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) |>
  mutate(
    ba_plus = ba + ma + prof + phd,
    ba_plus_share = ba_plus / educ_total,
    white_share = white / population,
    black_share = black / population,
    asian_share = asian / population,
    hispanic_share = hispanic / population,
    age18_29 = m18_19 + m20 + m21 + m22_24 + m25_29 +
      f18_19 + f20 + f21 + f22_24 + f25_29,
    age18_29_share = age18_29 / population
  ) |>
  separate(NAME, into = c("county_name", "state"), sep = ", ") %>%
  mutate(
    state       = toupper(state),
    county_name = toupper(county_name),
    county_name = gsub("\\.", "", county_name),
    county_name = gsub(" COUNTY$", "", county_name),
    county_name = gsub(" PARISH$", "", county_name)
  ) %>%
  # Second mutate AFTER stripping - add COUNTY back for ST LOUIS MO only
  mutate(
    county_name = if_else(state == "MISSOURI" & county_name == "ST LOUIS",
                          "ST LOUIS COUNTY", county_name)
  )

## Merging all Data
analysis_data <- merged_data |>
  left_join(
    acs_controls_wide |>
      select(
        state, county_name, population, median_income, median_age,
        ba_plus_share, white_share, black_share, asian_share, hispanic_share, age18_29_share
      ),
    by = c("state", "county_name")
  )

ct_acs_merge <- acs_ct_wide %>%
  select(state, county_name, population, median_income, median_age,
         ba_plus_share, white_share, black_share, asian_share, 
         hispanic_share, age18_29_share)

# Update the NA ACS values for CT rows in analysis_data
analysis_data <- analysis_data %>%
  left_join(ct_acs_merge, 
            by = c("state", "county_name"),
            suffix = c("", "_ct")) %>%
  mutate(
    population     = coalesce(population,     population_ct),
    median_income  = coalesce(median_income,  median_income_ct),
    median_age     = coalesce(median_age,     median_age_ct),
    ba_plus_share  = coalesce(ba_plus_share,  ba_plus_share_ct),
    white_share    = coalesce(white_share,    white_share_ct),
    black_share    = coalesce(black_share,    black_share_ct),
    asian_share    = coalesce(asian_share,    asian_share_ct),
    hispanic_share = coalesce(hispanic_share, hispanic_share_ct),
    age18_29_share = coalesce(age18_29_share, age18_29_share_ct)
  ) %>%
  # Drop the CT duplicate columns
  select(-ends_with("_ct"))

analysis_data <- analysis_data |>
  mutate(
    protest_count = coalesce(protest_count, 0),
    protests_per_100k = protest_count / population * 100000
  )

## Making 2 DV variables
analysis_data <- analysis_data |>
  mutate(
    dem_shift_24_20 = pct_dem_2024 - pct_dem_2020
  ) |>
  mutate(
    dem_vs_typical = pct_dem_2024 - typical_dem
  ) 

analysis_data <- analysis_data |>
  mutate(any_protests = ifelse(protests_per_100k > 0, 1, 0))
analysis_protest_only <- analysis_data |>
  filter(protest_count > 0)
analysis_data <- analysis_data |>
  mutate(log_protests = log1p(protests_per_100k))
counties_sf <- counties(cb = TRUE, resolution = "20m") %>%
  mutate(fips = paste0(STATEFP, COUNTYFP))

counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) %>%
  mutate(

    state = str_to_upper(STATE_NAME),

    county_match = str_to_upper(NAMELSAD),
  )

analysis_data <- analysis_data |>
  mutate(
    county_name = if_else(state == "MISSOURI" & county_name == "ST LOUIS COUNTY",
                          "ST LOUIS", county_name)
  )

analysis_data_clean <- analysis_data |>
  mutate(
    county_match = case_when(
      # Convert "(NAME) CITY" format to "NAME CITY" to match NAMELSAD format
      str_detect(county_name, "^\\(.*\\) CITY$") ~ 
        str_replace(county_name, "^\\((.*)\\) CITY$", "\\1 CITY"),
      TRUE ~ county_name
    )
  )



analysis_data_clean |>
  anti_join(counties_sf |>
              st_drop_geometry() |>
              mutate(state = str_to_upper(STATE_NAME),
                     county_match = str_to_upper(NAMELSAD)),
            by = c("state", "county_match")) |>
  select(state, county_name, county_match) |>
  head(20)




counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) |>
  mutate(
    state = str_to_upper(STATE_NAME),
    county_match = str_to_upper(NAMELSAD),
    county_match = str_remove(county_match, " COUNTY$"),
    county_match = str_remove(county_match, " PARISH$"),
    county_match = str_remove(county_match, " BOROUGH$"),
    county_match = str_remove(county_match, " CENSUS AREA$"),
    county_match = str_remove(county_match, " MUNICIPALITY$"),
    county_match = str_replace_all(county_match, "\\bST\\.", "ST"),
    county_match = str_replace_all(county_match, "\\bSTE\\.", "STE"),
    county_match = str_replace_all(county_match, "DOÑA", "DOÑA"),
    county_match = str_remove_all(county_match, "'"),
    county_match = if_else(state == "LOUISIANA" & county_match == "LASALLE",
                           "LASALLE", county_match)
  )

analysis_data_clean <- analysis_data |>
  mutate(
    county_match = str_replace(county_name,
                               "^\\((.*)\\) CITY$", "\\1 CITY"),
    county_match = str_remove_all(county_match, "'")
  )


## Final join
map_data <- counties_sf |>
  left_join(
    analysis_data_clean %>% select(state, county_match, dem_shift_24_20, log_protests),
    by = c("state", "county_match")
  )






## Recoding Negatives to NA Across All Key Variables
anes_weighted <- anes_raw |>
  mutate(across(c(V242096x, V241106x, V241409x, V241412x,
                  V241403x, V241406x, V241227x, V241146x,
                  V241294x, V241248, V241388, V241156,
                  V242065, V242071, V242072),
                ~ ifelse(. < 0, NA, .))) |>
  ## Fixing Non-Standard Missing Codes
  mutate(
    V241409x = ifelse(V241409x == 8,  NA, V241409x),  # removing "neither" category
    V241248  = ifelse(V241248  == 99, NA, V241248)     # removing "haven't thought about it"
  ) |>
  ## Creating Vote Recall and Outcome Variables
  mutate(
    vote_2020 = case_when(
      V241106x == 2 ~ "Biden",
      V241106x == 3 ~ "Trump",
      V241106x == 1 ~ "NonVoter",
      TRUE          ~ NA_character_
    ),
    ## Creating Primary Outcome: Switched from Biden to Non-Harris
    switched_from_biden = case_when(
      vote_2020 == "Biden" & V242096x == 1 ~ 0,
      vote_2020 == "Biden" & V242096x != 1 ~ 1,
      TRUE ~ NA_real_
    ),
    ## Creating Full Outcome Variable Using Turnout Follow-Up
    outcome_full = case_when(
      V242096x == 1                                          ~ "Voted Harris",
      V242096x == 2                                          ~ "Voted Trump",
      V242096x %in% c(3, 4, 5, 6)                           ~ "Voted Third Party",
      V242065 %in% c(1,2,3) & V242072 == 1                  ~ "Abstained — Preferred Harris",
      V242065 %in% c(1,2,3) & V242072 == 2                  ~ "Abstained — Preferred Trump",
      V242065 %in% c(1,2,3) & V242072 %in% c(4,5,6)        ~ "Abstained — Preferred Third Party",
      V242065 %in% c(1,2,3) & V242071 == 2                  ~ "Abstained — No Preference",
      TRUE                                                    ~ "Missing/Unknown"
    ),
    ## Creating Broad Defection Variable Including Abstainers
    defected_broad = case_when(
      outcome_full == "Voted Harris"                          ~ 0,
      outcome_full %in% c("Voted Trump", "Voted Third Party",
                          "Abstained — Preferred Harris",
                          "Abstained — No Preference",
                          "Abstained — Preferred Trump",
                          "Abstained — Preferred Third Party") ~ 1,
      TRUE ~ NA_real_
    ),
    ## Creating Abstention-Only Outcome Variable
    abstained = case_when(
      outcome_full == "Voted Harris"                          ~ 0,
      outcome_full %in% c("Abstained — Preferred Harris",
                          "Abstained — No Preference",
                          "Abstained — Preferred Trump",
                          "Abstained — Preferred Third Party") ~ 1,
      TRUE ~ NA_real_
    )
  ) |>
  ## Filtering to Valid Weights Only
  filter(!is.na(V240107b) & V240107b > 0)

## Setting Up Full Survey Design
anes_design <- svydesign(
  ids     = ~V240107c,
  strata  = ~V240107d,
  weights = ~V240107b,
  data    = anes_weighted,
  nest    = TRUE
)

## Building Common Sample with Valid Responses on All Variables
anes_sample <- anes_weighted |>
  filter(vote_2020 == "Biden") |>
  filter(!is.na(switched_from_biden)) |>
  filter(!is.na(defected_broad)) |>
  filter(!is.na(V241409x)) |>
  filter(!is.na(V241412x)) |>
  filter(!is.na(V241403x)) |>
  filter(!is.na(V241406x)) |>
  filter(!is.na(V241227x)) |>
  filter(!is.na(V241146x)) |>
  filter(!is.na(V241294x)) |>
  filter(!is.na(V241248))  |>
  filter(!is.na(V241388))

cat("Common sample N:", nrow(anes_sample), "\n")

## Setting Up Restricted Survey Design on Common Sample
options(survey.lonely.psu = "adjust")
anes_design_restricted <- svydesign(
  ids     = ~V240107c,
  strata  = ~V240107d,
  weights = ~V240107b,
  data    = anes_sample,
  nest    = TRUE
)



## Defining Helper Function to Extract Numeric from Factor Labels
extract_num <- function(x) {
  as.numeric(sub("^(-?[0-9]+)\\..*", "\\1", as.character(x)))
}

## Creating Defector Variable and Computing Change Scores
panel_clean <- panel |>
  ## Identifying 2020 Biden Voters Who Did Not Vote Harris in 2024
  mutate(
    defected = case_when(
      V202073 == "1. Joe Biden" & V242096x != "1. Kamala Harris" ~ 1,
      V202073 == "1. Joe Biden" & V242096x == "1. Kamala Harris" ~ 0,
      TRUE ~ NA_real_
    )
  ) |>
  ## Extracting Numeric Values from Factor Labels
  mutate(across(c(V201327x, V241294x,
                  V201502,  V241451,
                  V201231x,
                  V201200,  V241177,
                  V201336,  V241248,
                  V202232,  V242227,
                  V201129x, V241140x),
                extract_num)) |>
  ## Extracting Numeric from 2024 Party ID Summary Variable
  mutate(V241227x_num = extract_num(V241227x)) |>
  ## Recoding Negatives and Non-Standard Missing Codes to NA
  mutate(across(c(V201327x, V241294x, V201502, V241451,
                  V201231x, V241227x_num, V201200, V241177,
                  V201336, V241248, V202232, V242227,
                  V201129x, V241140x),
                ~ ifelse(. < 0 | . == 99, NA, .))) |>
  ## Rescaling Abortion to Common 0-1 Scale Across Waves
  mutate(
    V201336_clean     = ifelse(V201336 == 5, NA, V201336),
    abort_2020_scaled = (V201336_clean - 1) / (4 - 1),
    abort_2024_scaled = (V241248 - 1)       / (7 - 1)
  ) |>
  ## Computing Attitude Change Scores Between 2020 and 2024 Waves
  mutate(
    econ_retro_change      = V241294x     - V201327x,
    personal_fin_change    = V241451      - V201502,
    partyid_change_fixed   = V241227x_num - V201231x,
    ideology_change        = V241177      - V201200,
    abortion_change_scaled = abort_2024_scaled - abort_2020_scaled,
    immig_change           = V242227      - V202232,
    approval_change        = V241140x     - V201129x
  )


model1 <- lm(
  pct_dem_2024 ~ pct_dem_2020 + log_protests + age18_29_share +
    ba_plus_share + median_income + hispanic_share +
    black_share,
  data = analysis_data,
  weights = population
)

model2 <- lm(
  pct_dem_2024 ~ pct_dem_2020 + log_protests + age18_29_share +
    ba_plus_share + median_income + hispanic_share + black_share,
  data = analysis_data,
  weights = log(population)
)

model3 <- lm(
  dem_shift_24_20 ~ pct_dem_2020 + log_protests  + age18_29_share + ba_plus_share +
    median_income + hispanic_share + black_share,
  data = analysis_data,
  weights = log(population)
)


model4 <- lm(
  pct_dem_2024 ~ pct_dem_2020 + log_protests + age18_29_share +
    ba_plus_share + median_income +  hispanic_share + black_share +
    factor(state),
  data = analysis_data,
  weights = log(population)
)







model_cat <- lm(
  dem_shift_24_20 ~ protest_category + age18_29_share + ba_plus_share +
    median_income + hispanic_share + black_share,
  data    = analysis_data,
  weights = log(population)
)

biden_counties <- analysis_data |>
  filter(pct_dem_2020 > pct_rep_2020)


model_cat_biden <- lm(
  dem_shift_24_20 ~ protest_category + age18_29_share + ba_plus_share +
    median_income + hispanic_share + black_share,
  data    = biden_counties,
  weights = log(population)
)
modelsummary(
  list(
    "M3-Cat: All Counties"    = model_cat,
    "M3-Cat: Biden Counties"  = model_cat_biden
  ),
  coef_map = c(
    "protest_categoryLow"    = "Low Protest Activity (vs. None)",
    "protest_categoryMedium" = "Medium Protest Activity (vs. None)",
    "protest_categoryHigh"   = "High Protest Activity (vs. None)",
    "age18_29_share"         = "Age 18-29 Share",
    "ba_plus_share"          = "BA+ Education Share",
    "median_income"          = "Median Income",
    "hispanic_share"         = "Hispanic Share",
    "black_share"            = "Black Share",
    "(Intercept)"            = "Intercept"
  ),
  stars   = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "Robustness Check: Categorical Protest Intensity Measure",
  notes   = "Dependent variable is dem_shift_24_20 (swing from 2020 to 2024).
             Reference category is counties with no recorded protests.
             Low = 0-5 protests per 100k, Medium = 5-20, High = 20+.
             Right column restricted to counties where Biden won in 2020 
             (pct_dem_2020 > pct_rep_2020) — the universe where protest-driven 
             defection would most plausibly operate."
)


modelsummary(
  list(
    "M1: Levels (Pop. Weights)"     = model1,
    "M2: Levels (Log Pop. Weights)" = model2,
    "M3: Swing (Log Pop. Weights)"  = model3,
    "M4: Levels + State FE"         = model4
  ),
  coef_map = c(
    "pct_dem_2020"               = "Dem Vote Share 2020",
    "log_protests"               = "Log Protests per 100k",
    "ba_plus_share"              = "BA+ Education Share",
    "log_protests:ba_plus_share" = "Log Protests × BA+ Share",
    "age18_29_share"             = "Age 18-29 Share",
    "median_income"              = "Median Income",
    "hispanic_share"             = "Hispanic Share",
    "black_share"                = "Black Share",
    "(Intercept)"                = "Intercept"
  ),
  stars   = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "aic", "bic"),
  title   = "County-Level Models: Democratic Vote Share and Protest Intensity",
  notes   = "State fixed effects included in M4 but not shown.
             Dependent variable is pct_dem_2024 in M1, M2, M4 and
             dem_shift_24_20 in M3 and M5. M3 includes interaction between
             protest intensity and college education share. M5 restricted to
             seven swing states: MI, WI, PA, AZ, NV, GA, NC."
)





model_switcher_gaza <- svyglm(
  switched_from_biden ~ V241409x + V241227x + V241146x +
    V241294x + V241248 + V241388,
  design = anes_design_restricted,
  family = quasibinomial()
)


model_switcher_protest <- svyglm(
  switched_from_biden ~ V241412x + V241227x + V241146x +
    V241294x + V241248 + V241388,
  design = anes_design_restricted,
  family = quasibinomial()
)


model_switcher_military <- svyglm(
  switched_from_biden ~ V241403x + V241227x + V241146x +
    V241294x + V241248 + V241388,
  design = anes_design_restricted,
  family = quasibinomial()
)


model_switcher_humanitarian <- svyglm(
  switched_from_biden ~ V241406x + V241227x + V241146x +
    V241294x + V241248 + V241388,
  design = anes_design_restricted,
  family = quasibinomial()
)
modelsummary(
  list(
    "M1: Israeli-Palestinian\nSympathy"     = model_switcher_gaza,
    "M2: Campus Protest\nApproval"          = model_switcher_protest,
    "M3: Military Aid\nto Israel"           = model_switcher_military,
    "M4: Humanitarian Aid\nto Palestinians" = model_switcher_humanitarian
  ),
  coef_map = c(
    "V241409x" = "Israeli-Palestinian Sympathy",
    "V241412x" = "Campus Protest Approval (1=Approve, 7=Disapprove)",
    "V241403x" = "Military Aid to Israel (1=Favor, 7=Oppose)",
    "V241406x" = "Humanitarian Aid to Palestinians (1=Favor, 7=Oppose)",
    "V241227x" = "Party ID (1=Strong Dem, 7=Strong Rep)",
    "V241146x" = "Biden Foreign Policy Disapproval (1=Approve, 4=Disapprove)",
    "V241294x" = "Economic Retrospective (1=Much Better, 5=Much Worse)",
    "V241248"  = "Abortion Scale (1=Always Legal, 7=Always Illegal)",
    "V241388"  = "Immigration Attitudes",
    "(Intercept)" = "Intercept"
  ),
  stars   = TRUE,
  gof_map = c("nobs"),
  title   = "ANES 2024: Gaza Attitudes Do Not Predict Defection or Abstention",
  notes   = "Outcome in M1-M4 is switching from Biden (2020) to non-Harris (2024).
             M5 includes abstainers as defectors. Models estimated via svyglm() with
             ANES complex survey weights. Sample restricted to 2020 Biden voters."
)

model_panel <- glm(
  defected ~ econ_retro_change + personal_fin_change +
    partyid_change_fixed + ideology_change +
    abortion_change_scaled + immig_change +
    approval_change,
  data   = panel_clean |> filter(!is.na(defected)),
  family = binomial()
)


model_panel_no_approval <- glm(
  defected ~ econ_retro_change + personal_fin_change +
    partyid_change_fixed + ideology_change +
    abortion_change_scaled + immig_change,
  data   = panel_clean |> filter(!is.na(defected)),
  family = binomial()
)


modelsummary(
  list(
    "M1: With Approval" = model_panel,
    "M2: No Approval"   = model_panel_no_approval
  ),
  coef_map = c(
    "econ_retro_change"      = "Economic Retrospective Change (1-5 scale)",
    "personal_fin_change"    = "Personal Financial Situation Change (1-5 scale)",
    "partyid_change_fixed"   = "Party ID Change (→ Republican, 1-7 scale)",
    "ideology_change"        = "Ideology Change (→ Conservative, 1-7 scale)",
    "abortion_change_scaled" = "Abortion Views Change (→ Restrictive, 0-1 scale)",
    "immig_change"           = "Immigration Views Change (→ Decrease, 1-5 scale)",
    "approval_change"        = "Presidential Approval Change (→ Disapprove, 1-4 scale)",
    "(Intercept)"            = "Intercept"
  ),
  stars   = TRUE,
  gof_map = c("nobs", "AIC"),
  title   = "ANES Panel: Attitude Changes Predicting Defection from Harris",
  notes   = "Outcome is switching from Biden (2020) to non-Harris (2024).
             Predictors are change scores (2024 minus 2020 wave values).
             Abortion rescaled to 0-1 to account for different scale lengths
             across waves (4-point in 2020, 7-point in 2024).
             Party ID uses V241227x (7-point summary) for 2024 wave.
             Presidential approval removed in M2: (1) mediation concern —
             approval likely reflects same economic dissatisfaction driving
             defection; (2) compares approval of different presidents
             (Trump 2020 vs Biden 2024) — not the same construct.
             Models estimated via glm() with binomial family.
             Sample restricted to 2020 Biden voters in panel."
)
