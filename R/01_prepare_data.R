# ==============================================================================
# 01_prepare_data.R -- raw GSS extract -> analysis-ready panel
#
# Input : data/raw/GSS_with_MPROFF.csv        (see notebooks/00_build_mproff.ipynb for variable defintion)
#         data/raw/industry_naics_mapping.xlsx
# Output: data/processed/filtered_GSS.csv
# ==============================================================================

source(here::here("R", "00_setup.R"))

df <- read_csv(file.path(path_raw, "GSS_with_MPROFF.csv"), show_col_types = FALSE)

industry_lookup_df <- read_excel(file.path(path_raw, "industry_naics_mapping.xlsx")) %>%
  distinct(Industry, Broad_Industry, .keep_all = TRUE)

filtered_df <- df %>% dplyr::select(
  year, id_, ballot,                                            # identifiers
  happy, satjob, life, health, satfin,                          # wellbeing
  age, sex, race, marital, educ, region,                        # demographics
  wrktype, wrksched, hrs2, weekswrk, indus10, occ10, occ10_code,
  MPROFF, MPROFF_Label, conrinc, wrkslf,                        # work
  jobpromo, learnnew, workfast, opdevel, fairearn)              # job quality

# --- Blank non-responses, require a work arrangement -------------------------
filtered_df <- filtered_df %>%
  mutate(across(everything(), ~ ifelse(. %in% NON_RESPONSE_VALUES, NA, .))) %>%
  filter(!is.na(wrktype))

# Restrict to the waves in which the work-arrangement question was asked.
# The 2002 wave is excluded due to small sample size.
filtered_df <- filtered_df %>%
  filter(year %in% SURVEY_WAVES)

na_summary <- data.frame(
  Missing_Count   = colSums(is.na(filtered_df)),
  Missing_Percent = round(colSums(is.na(filtered_df)) / nrow(filtered_df) * 100, 3))
print(na_summary)

# Drop columns with excessive missingness, then require complete cases.
filtered_df <- filtered_df %>%
  select(-life, -health, -hrs2, -jobpromo) %>%
  drop_na()

# --- Industry mapping (2022 NAICS broad categories) --------------------------
filtered_df <- filtered_df %>%
  select(-starts_with("Broad_Industry")) %>%
  left_join(industry_lookup_df, by = c("indus10" = "Industry"))

# --- Geography ---------------------------------------------------------------
filtered_df <- filtered_df %>%
  mutate(region_group = case_when(
    region %in% c("New england", "Middle atlantic")            ~ "Northeast",
    region %in% c("East north central", "West north central")  ~ "Midwest",
    region %in% c("South atlantic", "East south atlantic")     ~ "Southeast",
    region == "West south central"                             ~ "Southwest",
    region %in% c("Mountain", "Pacific")                       ~ "West",
    TRUE                                                       ~ "Other"))

# --- Analytical variables ----------------------------------------------------
filtered_df <- filtered_df %>%
  mutate(
    Alternative_Worker = as.integer(wrktype %in% names(ALT_TYPE_LABELS)),
    Alternative_Worker_Status = factor(
      ifelse(Alternative_Worker == 1, "Alternative Worker", "Regular Worker"),
      levels = c("Regular Worker", "Alternative Worker")),
    married_dummy = as.integer(marital == "Married"),
    race   = factor(race, levels = c("White", "Black", "Other")),
    sex    = factor(sex, levels = c("MALE", "FEMALE")),
    happy  = factor(happy,
                    levels = c("Not too happy", "Pretty happy", "Very happy"),
                    ordered = TRUE),
    satjob = factor(satjob,
                    levels = c("Very dissatisfied", "A little dissatisfied",
                               "Moderately satisfied", "Very satisfied"),
                    ordered = TRUE),
    educ = case_when(
      educ == "No formal schooling" ~ 0,
      str_detect(educ, "grade") ~ as.numeric(str_extract(educ, "\\d+")),
      str_detect(educ, "year[s]? of college") ~ as.numeric(str_extract(educ, "\\d+")) + 12,
      educ == "8 or more years of college" ~ 20,
      TRUE ~ NA_real_))

filtered_df <- filtered_df %>%
  mutate(
    age     = as.numeric(ifelse(age == "89 or older", "89", age)),
    conrinc = as.numeric(conrinc),
    weekswrk = as.numeric(case_when(
      str_trim(as.character(weekswrk)) == "None or zero" ~ "0",
      TRUE ~ str_trim(as.character(weekswrk)))))

write.csv(filtered_df, file.path(path_processed, "filtered_GSS.csv"),
          row.names = FALSE)

cat("Wrote", nrow(filtered_df), "rows to data/processed/filtered_GSS.csv\n")
cat("Waves:", paste(sort(unique(filtered_df$year)), collapse = ", "), "\n")
