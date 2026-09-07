# ==============================================================================
# 03_tables.R -- Table 1, Table 2, Appendix Table A3, Appendix Table A4
#
# Input : data/processed/filtered_GSS.csv
# Output: output/tables/*.csv  (various analytical table)
#         output/tables/*.html (formatted gtsummary / gt tables)
#
# ==============================================================================

source(here::here("R", "00_setup.R"))

filtered_df <- read.csv(file.path(path_processed, "filtered_GSS.csv"),
                        stringsAsFactors = FALSE) %>%
  mutate(
    Alternative_Worker_Status = factor(
      Alternative_Worker_Status,
      levels = c("Regular Worker", "Alternative Worker")),
    sex    = factor(sex, levels = c("MALE", "FEMALE")),
    happy  = factor(happy,
                    levels = c("Not too happy", "Pretty happy", "Very happy"),
                    ordered = TRUE),
    satjob = factor(satjob,
                    levels = c("Very dissatisfied", "A little dissatisfied",
                               "Moderately satisfied", "Very satisfied"),
                    ordered = TRUE))


# ==============================================================================
# Table 1 -- descriptive statistics by work arrangement
# Temporary agency INCLUDED.
# ==============================================================================
table1 <- filtered_df %>%
  select(Alternative_Worker_Status, happy, satjob, MPROFF, educ, conrinc,
         age, sex, weekswrk, married_dummy) %>%
  tbl_summary(
    by = Alternative_Worker_Status,
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no",
    label = list(
      happy ~ "General Happiness", satjob ~ "Job Satisfaction",
      MPROFF ~ "Professional Occupation", educ ~ "Education (years)",
      conrinc ~ "Income (constant $)", age ~ "Age", sex ~ "Gender",
      weekswrk ~ "Weeks Worked Last Year", married_dummy ~ "Married")) %>%
  add_overall() %>%
  add_p(test = list(all_continuous()  ~ "t.test",
                    all_categorical() ~ "chisq.test"),
        pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>%
  modify_header(label ~ "**Variable**") %>%
  bold_labels() %>%
  modify_footnote(p.value ~ paste("t-test for continuous variables;",
                                  "chi-square test for categorical variables"))

# Fixed `id` keeps the rendered HTML byte-stable across runs; gt otherwise
# stamps a random div id, which shows up as a spurious diff on every rerun.
gt::gtsave(as_gt(table1, id = "table1_descriptives"),
           file.path(path_tables, "table1_descriptives.html"))


# ==============================================================================
# Table 2 -- work type distribution by year
# Temporary agency INCLUDED: the row showing it is under 2% in every wave is
# what justifies dropping the category downstream.
# ==============================================================================
wrktype_percent <- filtered_df %>%
  count(year, wrktype) %>%
  group_by(year) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  ungroup()

wrktype_table <- wrktype_percent %>%
  mutate(percentage_label = paste0(percentage, "%")) %>%
  select(year, wrktype, percentage_label) %>%
  pivot_wider(names_from = year, values_from = percentage_label,
              values_fill = "0%")

total_n <- filtered_df %>% count(year) %>% deframe()

colnames(wrktype_table) <- c(
  "Work Type", paste0(names(total_n), " (n = ", total_n, ")"))

print(wrktype_table)
write.csv(wrktype_percent, file.path(path_tables, "table2_worktype_by_year.csv"),
          row.names = FALSE)


# ==============================================================================
# Analysis sample -- temporary agency dropped from here on
# ==============================================================================
analysis_df <- filtered_df %>%
  mutate(worker_type = case_when(
    wrktype == "Regular, permanent employee"                         ~ "Regular Employee",
    wrktype == "Independent contractor/consultant/freelance worker"  ~ "Independent Contractor",
    wrktype == "Work for contractor who provides workers/services"   ~ "Contract Worker",
    wrktype == "On-call, work only when called to work"              ~ "On-Call Worker",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(worker_type)) %>%
  mutate(worker_type = factor(worker_type,
    levels = c("Regular Employee", "Independent Contractor",
               "Contract Worker", "On-Call Worker")))


# ==============================================================================
# Appendix Table A3 -- summary statistics by worker type
# ==============================================================================
sample_sizes <- analysis_df %>% count(worker_type) %>%
  arrange(worker_type) %>% pull(n)

table_a3 <- analysis_df %>%
  select(worker_type, happy, satjob, MPROFF, educ, conrinc, age, sex,
         weekswrk, married_dummy) %>%
  tbl_summary(
    by = worker_type,
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no",
    label = list(
      happy ~ "General Happiness", satjob ~ "Job Satisfaction",
      educ ~ "Education (years)", conrinc ~ "Income (constant $)",
      age ~ "Age", sex ~ "Gender",
      weekswrk ~ "Weeks Worked Last Year", married_dummy ~ "Married")) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_header(
    stat_0 = glue::glue("**Overall**\n(N = {nrow(analysis_df)})"),
    stat_1 = glue::glue("**Regular Employee**\n(N = {sample_sizes[1]})"),
    stat_2 = glue::glue("**Independent Contractor**\n(N = {sample_sizes[2]})"),
    stat_3 = glue::glue("**Contract Worker**\n(N = {sample_sizes[3]})"),
    stat_4 = glue::glue("**On-Call Worker**\n(N = {sample_sizes[4]})"))

gt::gtsave(
  as_gt(table_a3, id = "appendix_table_A3") %>%
    tab_header(
      title = "Summary Statistics by Worker Type",
      subtitle = paste("Ordered by: Overall, Regular Employee,",
                       "Independent Contractor, Contract Worker,",
                       "On-Call Worker")) %>%
    tab_options(column_labels.font.weight = "bold",
                column_labels.padding = "8px"),
  file.path(path_tables, "appendix_table_A3_by_worker_type.html"))


# ==============================================================================
# Appendix Table A4 -- industry characteristics by work arrangement
# 2022 NAICS broad categories.
# ==============================================================================
industry_comparison <- analysis_df %>%
  filter(!is.na(Broad_Industry)) %>%
  group_by(Broad_Industry) %>%
  summarise(
    Total_Employed   = n(),
    RegWorker_Number = sum(Alternative_Worker == 0, na.rm = TRUE),
    RegWorker_Pct    = RegWorker_Number / Total_Employed * 100,
    RegWorker_Sat    = mean(as.numeric(satjob[Alternative_Worker == 0]), na.rm = TRUE),
    AltWorker_Number = sum(Alternative_Worker == 1, na.rm = TRUE),
    AltWorker_Pct    = AltWorker_Number / Total_Employed * 100,
    AltWorker_Sat    = mean(as.numeric(satjob[Alternative_Worker == 1]), na.rm = TRUE),
    .groups = "drop") %>%
  mutate(across(c(RegWorker_Pct, AltWorker_Pct, RegWorker_Sat, AltWorker_Sat),
                ~ round(., 1)))

write.csv(industry_comparison,
          file.path(path_tables, "appendix_table_A4_industry.csv"),
          row.names = FALSE)

industry_comparison %>%
  rename(Industry = Broad_Industry) %>%
  kbl(format = "markdown", align = "lrrrrrr",
      col.names = c("Industry", "Total Employed",
                    "Number", "% of total employed", "Avg Sat Level",
                    "Number", "% of total employed", "Avg Sat Level"),
      caption = "Industry Characteristics by Alternative Worker Status",
      digits = c(0, 0, 0, 1, 1, 0, 1, 1)) %>%
  add_header_above(c(" " = 2, "Regular Workers" = 3,
                     "Alternative Workers" = 3), bold = FALSE) %>%
  add_header_above(c(" " = 1, " " = 1, "Status on Employment" = 6), bold = FALSE)

cat("Tables written to output/tables/\n")
