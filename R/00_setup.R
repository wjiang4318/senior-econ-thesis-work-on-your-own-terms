# ==============================================================================
# 00_setup.R -- packages, paths, and shared constants
# ==============================================================================

libs <- c(
  "here", "readxl", "readr", "dplyr", "ggplot2", "patchwork",
  "tidyr", "stringr", "scales", "gtsummary", "gt", "kableExtra",
  "extrafont", "tibble", "glue")

missing <- libs[!vapply(libs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall with: install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))")
}
invisible(lapply(libs, library, character.only = TRUE))

# --- Paths (all relative to the project root, resolved by `here`) ------------
path_raw       <- here::here("data", "raw")
path_processed <- here::here("data", "processed")
path_figures   <- here::here("output", "figures")
path_tables    <- here::here("output", "tables")

for (p in c(path_processed, path_figures, path_tables)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}

# --- Analysis constants ------------------------------------------------------

# GSS waves in which the work-arrangement question was asked.
SURVEY_WAVES <- c(2006, 2010, 2014, 2018, 2022)

# The four nonstandard arrangements, and the short labels used in figures.
ALT_TYPE_LABELS <- c(
  "Independent contractor/consultant/freelance worker" = "Independent Contractor",
  "Work for contractor who provides workers/services"  = "Contract Firm Worker",
  "On-call, work only when called to work"             = "On-Call",
  "Paid by a temporary agency"                         = "Temporary Agency")

TEMP_AGENCY <- "Paid by a temporary agency"

# GSS non-response codes, blanked to NA during cleaning.
NON_RESPONSE_VALUES <- c(
  ".i:  Inapplicable", ".n:  No answer",
  ".d:  Do not Know/Cannot Choose", ".s:  Skipped on Web")

# --- Shared plot styling -----------------------------------------------------
base_font <- if ("Times New Roman" %in% extrafont::fonts()) "Times New Roman" else "serif"

ALT_COLORS <- c(
  "Independent Contractor" = "#E41A1C",
  "Contract Firm Worker"   = "#377EB8",
  "On-Call"                = "#4DAF4A",
  "Temporary Agency"       = "#984EA3")

JOB_COLORS <- c("Very satisfied"        = "#FFD700",
                "Moderately satisfied"  = "#66C2A5",
                "A little dissatisfied" = "#3288BD",
                "Very dissatisfied"     = "#542788")

HAPPY_COLORS <- c("Very happy"    = "#FFD700",
                  "Pretty happy"  = "#66C2A5",
                  "Not too happy" = "#542788")

base_theme <- theme_minimal(base_size = 14, base_family = base_font) +
  theme(
    plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title    = element_text(face = "bold"),
    legend.position = "top",
    legend.title  = element_text(face = "bold", size = 11, family = base_font),
    legend.text   = element_text(size = 10, family = base_font),
    legend.key.size = unit(0.6, "lines"),
    panel.grid.minor = element_blank(),
    axis.text     = element_text(size = 12),
    axis.ticks    = element_blank(),
    plot.caption  = element_text(size = 10, hjust = 0, margin = margin(t = 10)))
