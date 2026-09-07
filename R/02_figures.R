# ==============================================================================
# 02_figures.R -- Appendix Figures A1, A2, A3 and the income distribution plot
#
# Input : data/processed/filtered_GSS.csv
# Output: output/figures/*.png
#
# SAMPLE NOTE -- two samples are used deliberately: 
#   filtered_df  all four alternative types, temporary agency INCLUDED.
#   analysis_df  temporary agency EXCLUDED (under 2% of workers in every wave).
# ==============================================================================

source(here::here("R", "00_setup.R"))

filtered_df <- read.csv(file.path(path_processed, "filtered_GSS.csv"),
                        stringsAsFactors = FALSE)

filtered_df <- filtered_df %>%
  mutate(
    happy  = factor(happy,
                    levels = c("Not too happy", "Pretty happy", "Very happy"),
                    ordered = TRUE),
    satjob = factor(satjob,
                    levels = c("Very dissatisfied", "A little dissatisfied",
                               "Moderately satisfied", "Very satisfied"),
                    ordered = TRUE))

analysis_df <- filtered_df %>% filter(wrktype != TEMP_AGENCY)


# ==============================================================================
# Appendix Figure A3 -- share of the labor force by detailed alternative type
#
# The denominator is ALL workers surveyed in a wave (regular employees
# included), so each series is a share of the labor force rather than a share
# of alternative workers. Uses filtered_df: the Temporary Agency series is one
# of the four lines.
# ==============================================================================
alt_work_proportions <- filtered_df %>%
  group_by(year) %>%
  mutate(n_year = n()) %>%
  ungroup() %>%
  filter(wrktype %in% names(ALT_TYPE_LABELS)) %>%
  count(year, wrktype, n_year, name = "n") %>%
  mutate(
    wrktype_clean = ALT_TYPE_LABELS[wrktype],
    proportion    = n / n_year) %>%
  dplyr::select(year, wrktype_clean, n, n_year, proportion) %>%
  arrange(wrktype_clean, year)

# Order the legend by average share, largest first.
type_order <- alt_work_proportions %>%
  group_by(wrktype_clean) %>%
  summarise(avg_prop = mean(proportion, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_prop)) %>%
  pull(wrktype_clean)

alt_work_proportions <- alt_work_proportions %>%
  mutate(wrktype_clean = factor(wrktype_clean, levels = type_order))

fig_a3 <- ggplot(alt_work_proportions,
                 aes(x = year, y = proportion, color = wrktype_clean)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5, shape = 21, fill = "white", stroke = 0.4) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.15)) +
  scale_x_continuous(breaks = seq(2006, 2022, 2), limits = c(2006, 2022)) +
  scale_color_manual(values = ALT_COLORS, name = NULL) +
  labs(
    title    = "Growth of Alternative Work Arrangements, 2006-2022",
    subtitle = "Share of U.S. Labor Force by Work Type",
    x = "Survey Year", y = "Percentage of Workers",
    caption  = "Source: General Social Survey (GSS), 2006-2022") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    plot.title     = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle  = element_text(size = 12, hjust = 0.5),
    plot.caption   = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
    axis.title     = element_text(face = "bold"),
    axis.text      = element_text(size = 11),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title    = element_blank(),
    legend.text     = element_text(size = 11),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA))

ggsave(file.path(path_figures, "appendix_figure_A3_alt_work_types.png"),
       fig_a3, width = 8, height = 5.2, dpi = 300, bg = "white")

# Values behind the figure, for cross-checking against the paper.
write.csv(alt_work_proportions,
          file.path(path_tables, "appendix_figure_A3_values.csv"),
          row.names = FALSE)


# ==============================================================================
# Appendix Figure A2 -- regular vs alternative share of the labor force
# Uses analysis_df (temporary agency excluded).
# ==============================================================================
fig_a2 <- analysis_df %>%
  group_by(year, Alternative_Worker) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percentage = count / sum(count),
         WorkLabel = ifelse(Alternative_Worker == 1,
                            "Alternative Worker", "Regular Worker")) %>%
  ggplot(aes(x = as.factor(year), y = percentage, fill = WorkLabel)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = percent(percentage, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5, fontface = "bold", family = base_font) +
  scale_fill_manual(values = c("Regular Worker"     = "#1f77b4",
                               "Alternative Worker" = "#ff7f0e")) +
  scale_y_continuous(labels = percent_format(accuracy = 10)) +
  labs(title = "Alternative Work Participation Over Time",
       x = "Year", y = "Percentage of Participants", fill = "Employment Type",
       caption = "Source: General Social Survey (GSS), 2006-2022") +
  base_theme +
  theme(panel.grid.major.x = element_blank())

ggsave(file.path(path_figures, "appendix_figure_A2_participation.png"),
       fig_a2, width = 6, height = 5, dpi = 300, bg = "white")


# ==============================================================================
# Appendix Figure A1 -- job satisfaction and happiness trends by employment type
# Uses analysis_df.
# ==============================================================================
trend_panel <- function(dat, var, colors, title, legend_title) {
  dat %>%
    mutate(.v = factor(.data[[var]], levels = names(colors))) %>%
    group_by(year, .v, Alternative_Worker) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(year, Alternative_Worker) %>%
    mutate(percentage = count / sum(count),
           WorkLabel = ifelse(Alternative_Worker == 1,
                              "Alternative Worker", "Regular Worker")) %>%
    ggplot(aes(x = year, y = percentage, color = .v)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    facet_wrap(~WorkLabel) +
    scale_color_manual(values = colors) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = title, x = "Year", y = "Percentage", color = legend_title,
         caption = "Source: General Social Survey (GSS), 2006-2022") +
    base_theme +
    theme(strip.text = element_text(face = "bold", size = 14))
}

fig_a1_sat   <- trend_panel(analysis_df, "satjob", JOB_COLORS,
                            "Job Satisfaction Trends by Employment Type",
                            "Satisfaction Level")
fig_a1_happy <- trend_panel(analysis_df, "happy", HAPPY_COLORS,
                            "Happiness Trends by Employment Type",
                            "Happiness Level")

ggsave(file.path(path_figures, "appendix_figure_A1_satisfaction_happiness.png"),
       fig_a1_sat + fig_a1_happy, width = 12, height = 5, dpi = 300, bg = "white")


# ==============================================================================
# Income distribution over time
# ==============================================================================
fig_income <- ggplot(analysis_df, aes(x = as.factor(year), y = conrinc)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2) +
  labs(title = "Income Distribution Over Years",
       x = "Year", y = "Income (Constant Dollars)",
       caption = "Source: General Social Survey (GSS), 2006-2022") +
  base_theme +
  theme(panel.grid.major.x = element_blank())

ggsave(file.path(path_figures, "figure_income_distribution.png"),
       fig_income, width = 6, height = 5, dpi = 300, bg = "white")

cat("Figures written to output/figures/\n")
