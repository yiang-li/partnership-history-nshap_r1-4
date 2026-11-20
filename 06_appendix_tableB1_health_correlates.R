# Author: Yiang Li
# Build Appendix B Table 1 from the cleaned respondent data.
library(dplyr)
library(openxlsx)
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
        r3_part_history_11,
        levels = c(
          "Continuously married",
          "Remarried, after divorce/separation",
          "Remarried, after widowhood",
          "Remarried, multiple or unknown dissolutions",
          "Divorced/separated",
          "Widowed",
          "Formerly married, multiple or unknown dissolutions",
          "Cohabiting, formerly married",
          "Cohabiting, never married",
          "Formerly cohabited, never married",
          "Never partnered"
        )
      )
    ) |>
    filter(!is.na(r3_weight), !is.na(r3_stratum), !is.na(r3_cluster)),
  nest = TRUE
)
round3_analysis_design <- subset(round3_design, r3_health_analysis_sample)

#### Partnership biography ####
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

#### Partnership history ####
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

#### Appendix B Table 1 ####
biography_self_rated_health <- tidy(biography_self_rated_health_model) |>
  filter(term %in% c(
    "r3_n_marriages", "r3_n_cohabitations", "r3_ever_widowed"
  )) |>
  mutate(
    Predictor = case_when(
      term == "r3_n_marriages" ~ "Number of marriages",
      term == "r3_n_cohabitations" ~ "Number of cohabitations",
      term == "r3_ever_widowed" ~ "Ever Widowed"
    ),
    stars = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ ""
    )
  ) |>
  transmute(
    Predictor,
    `Self-rated health` = sprintf(
      "%.3f%s (%.3f)", estimate, stars, std.error
    )
  )

biography_depressive_symptoms <- tidy(
  biography_depressive_symptoms_model
) |>
  filter(term %in% c(
    "r3_n_marriages", "r3_n_cohabitations", "r3_ever_widowed"
  )) |>
  mutate(
    Predictor = case_when(
      term == "r3_n_marriages" ~ "Number of marriages",
      term == "r3_n_cohabitations" ~ "Number of cohabitations",
      term == "r3_ever_widowed" ~ "Ever Widowed"
    ),
    stars = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ ""
    )
  ) |>
  transmute(
    Predictor,
    `Depressive Symptoms` = sprintf(
      "%.3f%s (%.3f)", estimate, stars, std.error
    )
  )

biography_functional_limitations <- tidy(
  biography_functional_limitations_model
) |>
  filter(term %in% c(
    "r3_n_marriages", "r3_n_cohabitations", "r3_ever_widowed"
  )) |>
  mutate(
    Predictor = case_when(
      term == "r3_n_marriages" ~ "Number of marriages",
      term == "r3_n_cohabitations" ~ "Number of cohabitations",
      term == "r3_ever_widowed" ~ "Ever Widowed"
    ),
    stars = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ ""
    )
  ) |>
  transmute(
    Predictor,
    `Functional Limitations` = sprintf(
      "%.3f%s (%.3f)", exp(estimate), stars,
      exp(estimate) * std.error
    )
  )

biography_rows <- biography_self_rated_health |>
  left_join(biography_depressive_symptoms, by = "Predictor") |>
  left_join(biography_functional_limitations, by = "Predictor")

history_self_rated_health <- tidy(history_self_rated_health_model) |>
  filter(startsWith(term, "partnership_history")) |>
  mutate(
    Predictor = sub("^partnership_history", "", term),
    stars = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ ""
    )
  ) |>
  transmute(
    Predictor,
    `Self-rated health` = sprintf(
      "%.3f%s (%.3f)", estimate, stars, std.error
    )
  )

history_depressive_symptoms <- tidy(history_depressive_symptoms_model) |>
  filter(startsWith(term, "partnership_history")) |>
  mutate(
    Predictor = sub("^partnership_history", "", term),
    stars = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ ""
    )
  ) |>
  transmute(
    Predictor,
    `Depressive Symptoms` = sprintf(
      "%.3f%s (%.3f)", estimate, stars, std.error
    )
  )

history_functional_limitations <- tidy(
  history_functional_limitations_model
) |>
  filter(startsWith(term, "partnership_history")) |>
  mutate(
    Predictor = sub("^partnership_history", "", term),
    stars = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ ""
    )
  ) |>
  transmute(
    Predictor,
    `Functional Limitations` = sprintf(
      "%.3f%s (%.3f)", exp(estimate), stars,
      exp(estimate) * std.error
    )
  )

history_rows <- history_self_rated_health |>
  left_join(history_depressive_symptoms, by = "Predictor") |>
  left_join(history_functional_limitations, by = "Predictor")

appendix_table_b1 <- bind_rows(
  tibble(
    Predictor = "A. Partnership biography",
    `Self-rated health` = "",
    `Depressive Symptoms` = "",
    `Functional Limitations` = ""
  ),
  biography_rows,
  tibble(
    Predictor = "  n",
    `Self-rated health` = as.character(
      nrow(biography_self_rated_health_model$model)
    ),
    `Depressive Symptoms` = as.character(
      nrow(biography_depressive_symptoms_model$model)
    ),
    `Functional Limitations` = as.character(
      nrow(biography_functional_limitations_model$model)
    )
  ),
  tibble(
    Predictor = "  Deviance explained",
    `Self-rated health` = sprintf(
      "%.3f", 1 - biography_self_rated_health_model$deviance /
        biography_self_rated_health_model$null.deviance
    ),
    `Depressive Symptoms` = sprintf(
      "%.3f", 1 - biography_depressive_symptoms_model$deviance /
        biography_depressive_symptoms_model$null.deviance
    ),
    `Functional Limitations` = sprintf(
      "%.3f", 1 - biography_functional_limitations_model$deviance /
        biography_functional_limitations_model$null.deviance
    )
  ),
  tibble(
    Predictor = "B. Partnership history (vs Continuously married)",
    `Self-rated health` = "",
    `Depressive Symptoms` = "",
    `Functional Limitations` = ""
  ),
  history_rows,
  tibble(
    Predictor = "  n",
    `Self-rated health` = as.character(
      nrow(history_self_rated_health_model$model)
    ),
    `Depressive Symptoms` = as.character(
      nrow(history_depressive_symptoms_model$model)
    ),
    `Functional Limitations` = as.character(
      nrow(history_functional_limitations_model$model)
    )
  ),
  tibble(
    Predictor = "  Deviance explained",
    `Self-rated health` = sprintf(
      "%.3f", 1 - history_self_rated_health_model$deviance /
        history_self_rated_health_model$null.deviance
    ),
    `Depressive Symptoms` = sprintf(
      "%.3f", 1 - history_depressive_symptoms_model$deviance /
        history_depressive_symptoms_model$null.deviance
    ),
    `Functional Limitations` = sprintf(
      "%.3f", 1 - history_functional_limitations_model$deviance /
        history_functional_limitations_model$null.deviance
    )
  ),
  tibble(
    Predictor = "Significance: * p<.05  ** p<.01  *** p<.001",
    `Self-rated health` = "",
    `Depressive Symptoms` = "",
    `Functional Limitations` = ""
  )
)

#### Save Appendix B Table 1 ####
workbook <- createWorkbook()
modifyBaseFont(workbook, fontSize = 10.5, fontName = "Times New Roman")
addWorksheet(workbook, "Appendix B Table 1")
header_style <- createStyle(textDecoration = "bold", border = "bottom",
                            borderStyle = "thin")
writeData(workbook, "Appendix B Table 1", appendix_table_b1,
          headerStyle = header_style)
setColWidths(workbook, "Appendix B Table 1", cols = 1, widths = 64)
setColWidths(workbook, "Appendix B Table 1",
             cols = 2:ncol(appendix_table_b1), widths = 22)
setRowHeights(workbook, "Appendix B Table 1",
              rows = seq_len(nrow(appendix_table_b1)) + 1, heights = 30)
addStyle(
  workbook, "Appendix B Table 1",
  createStyle(wrapText = TRUE, valign = "center"),
  rows = seq_len(nrow(appendix_table_b1)) + 1,
  cols = seq_len(ncol(appendix_table_b1)), gridExpand = TRUE, stack = TRUE
)
mergeCells(workbook, "Appendix B Table 1", rows = 2,
           cols = seq_len(ncol(appendix_table_b1)))
addStyle(
  workbook, "Appendix B Table 1",
  createStyle(halign = "left", textDecoration = "bold",
              border = "top", borderStyle = "thin"),
  rows = 2, cols = 1, stack = TRUE
)
mergeCells(workbook, "Appendix B Table 1", rows = 8,
           cols = seq_len(ncol(appendix_table_b1)))
addStyle(
  workbook, "Appendix B Table 1",
  createStyle(halign = "left", textDecoration = "bold",
              border = "top", borderStyle = "thin"),
  rows = 8, cols = 1, stack = TRUE
)
addStyle(
  workbook, "Appendix B Table 1",
  createStyle(border = "bottom", borderStyle = "thin"),
  rows = nrow(appendix_table_b1) + 1,
  cols = seq_len(ncol(appendix_table_b1)), stack = TRUE
)
saveWorkbook(workbook, "tables_figures/appendix_tableB1_health_correlates.xlsx",
             overwrite = TRUE)
