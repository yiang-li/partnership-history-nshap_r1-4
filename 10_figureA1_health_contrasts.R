# Author: Yiang Li
# Plot Round 3 health contrasts for the broad partnership history.
library(dplyr)
library(ggplot2)
library(ggh4x)
library(broom)
library(survey)

options(survey.lonely.psu = "adjust")

#### Round 3 sample ####
round3_design <- svydesign(
  ids = ~r3_cluster,
  strata = ~r3_stratum,
  weights = ~r3_weight,
  data = readRDS("../tmp/nshap_core.rds") |>
    mutate(
      r3_ever_widowed = as.integer(r3_ever_widowed),
      partnership_history = factor(
        r3_part_history_7,
        levels = c(
          "Continuously married",
          "Remarried",
          "Divorced/separated",
          "Widowed",
          "Currently cohabiting",
          "Formerly cohabited",
          "Never partnered"
        )
      )
    ) |>
    filter(!is.na(r3_weight), !is.na(r3_stratum), !is.na(r3_cluster)),
  nest = TRUE
)
round3_analysis_design <- subset(round3_design, r3_health_analysis_sample)

#### Panel A: partnership biography ####
biography_self_rated_health_model <- svyglm(
  r3_self_rated_health_z ~ r3_n_marriages + r3_n_cohabitations +
    r3_ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)

biography_depressive_symptoms_model <- svyglm(
  r3_depressive_symptoms_z ~ r3_n_marriages + r3_n_cohabitations +
    r3_ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)

biography_functional_limitations_model <- svyglm(
  r3_functional_limitations ~ r3_n_marriages + r3_n_cohabitations +
    r3_ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = quasipoisson()
)

#### Panel B: partnership history ####
history_self_rated_health_model <- svyglm(
  r3_self_rated_health_z ~ partnership_history + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)

history_depressive_symptoms_model <- svyglm(
  r3_depressive_symptoms_z ~ partnership_history + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)

history_functional_limitations_model <- svyglm(
  r3_functional_limitations ~ partnership_history + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = quasipoisson()
)

#### Contrasts ####
contrast_data <- bind_rows(
  tidy(biography_self_rated_health_model) |>
    mutate(panel = "A. Partnership\nbiography",
           outcome = "Self-rated health", on_ratio_scale = FALSE,
           residual_df = biography_self_rated_health_model$df.residual),
  tidy(biography_depressive_symptoms_model) |>
    mutate(panel = "A. Partnership\nbiography",
           outcome = "Depressive Symptoms", on_ratio_scale = FALSE,
           residual_df = biography_depressive_symptoms_model$df.residual),
  tidy(biography_functional_limitations_model) |>
    mutate(panel = "A. Partnership\nbiography",
           outcome = "Functional Limitations", on_ratio_scale = TRUE,
           residual_df = biography_functional_limitations_model$df.residual),
  tidy(history_self_rated_health_model) |>
    mutate(panel = "B. Partnership\nhistory",
           outcome = "Self-rated health", on_ratio_scale = FALSE,
           residual_df = history_self_rated_health_model$df.residual),
  tidy(history_depressive_symptoms_model) |>
    mutate(panel = "B. Partnership\nhistory",
           outcome = "Depressive Symptoms", on_ratio_scale = FALSE,
           residual_df = history_depressive_symptoms_model$df.residual),
  tidy(history_functional_limitations_model) |>
    mutate(panel = "B. Partnership\nhistory",
           outcome = "Functional Limitations", on_ratio_scale = TRUE,
           residual_df = history_functional_limitations_model$df.residual)
) |>
  filter(term %in% c("r3_n_marriages", "r3_n_cohabitations",
                     "r3_ever_widowed") |
           startsWith(term, "partnership_history")) |>
  transmute(
    panel,
    outcome = factor(
      outcome,
      levels = c("Self-rated health", "Depressive Symptoms",
                 "Functional Limitations")
    ),
    predictor = factor(
      recode(
        sub("^partnership_history", "", term),
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        r3_ever_widowed = "Ever widowed"
      ),
      levels = rev(c(
        "Number of marriages", "Number of cohabitations", "Ever widowed",
        "Remarried",
        "Divorced/separated",
        "Widowed",
        "Currently cohabiting",
        "Formerly cohabited",
        "Never partnered"
      ))
    ),
    reported_estimate = if_else(on_ratio_scale, exp(estimate), estimate),
    lower_ci = estimate - qt(.975, residual_df) * std.error,
    upper_ci = estimate + qt(.975, residual_df) * std.error,
    lower_ci = if_else(on_ratio_scale, exp(lower_ci), lower_ci),
    upper_ci = if_else(on_ratio_scale, exp(upper_ci), upper_ci),
    reference_value = if_else(on_ratio_scale, 1, 0),
    ci_excludes_reference = lower_ci > reference_value |
      upper_ci < reference_value
  )

figure_a1 <- ggplot(contrast_data, aes(reported_estimate, predictor)) +
  geom_vline(
    data = distinct(contrast_data, panel, outcome, reference_value),
    aes(xintercept = reference_value),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_pointrange(
    aes(xmin = lower_ci, xmax = upper_ci, color = ci_excludes_reference)
  ) +
  scale_color_manual(
    values = c(`FALSE` = "grey40", `TRUE` = "firebrick"),
    guide = "none"
  ) +
  facet_grid2(panel ~ outcome, scales = "free", independent = "x",
              space = "free_y", switch = "y") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, size = 11, face = "bold"))

ggsave(
  "tables_figures/FigureA1.pdf",
  figure_a1, width = 12, height = 6
)
