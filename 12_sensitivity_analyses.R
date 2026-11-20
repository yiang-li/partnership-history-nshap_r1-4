# Author: Yiang Li
# Check whether the Round 3 health results depend on analytic choices.
library(dplyr)
library(broom)
library(haven)
library(openxlsx)
library(survey)

options(survey.lonely.psu = "adjust")

core_data <- readRDS("../tmp/nshap_core.rds") |>
  left_join(
    read_dta("../data/nshap_w3_core.dta") |>
      transmute(su_id, r3_birth_year = as.numeric(ageyear)),
    by = "su_id"
  ) |>
  mutate(
    ever_widowed = as.integer(r3_ever_widowed),
    partnership_history_11 = factor(
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
    ),
    partnership_history_7 = factor(
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
    ),
    partnership_history_14 = factor(
      r3_part_history_14,
      levels = c(
        "Continuously married",
        "Remarried, after divorce/separation",
        "Remarried, after widowhood",
        "Remarried, after multiple dissolutions",
        "Remarried, prior dissolution(s) unknown",
        "Divorced/separated",
        "Widowed",
        "Formerly married, multiple dissolutions",
        "Formerly married, dissolution(s) unknown",
        "Cohabiting, formerly married",
        "Continuously cohabiting",
        "Cohabiting, after prior cohabitation(s)",
        "Formerly cohabited, never married",
        "Never partnered"
      )
    )
  )

sample_counts <- tibble(
  Check = c(
    "All respondents in the cleaned core",
    "Round 3 common health analysis sample",
    "Born 1920-1965 in the Round 3 analysis sample",
    "Unknown dissolution in the Round 3 analysis sample",
    "Never partnered in the Round 3 analysis sample"
  ),
  Count = c(
    nrow(core_data),
    sum(core_data$r3_health_analysis_sample, na.rm = TRUE),
    sum(core_data$r3_health_analysis_sample &
          core_data$r3_birth_year >= 1920 &
          core_data$r3_birth_year <= 1965, na.rm = TRUE),
    sum(core_data$r3_health_analysis_sample &
          core_data$r3_unknown_dissolution, na.rm = TRUE),
    sum(core_data$r3_health_analysis_sample &
          core_data$r3_part_history_11 == "Never partnered", na.rm = TRUE)
  ),
  Interpretation = c(
    "Respondents represented in the cleaned partnership file",
    "Complete Round 3 sample used for all three health outcomes",
    "Official birth-cohort target used in the age-eligible sensitivity",
    "Removed in the unknown-dissolution sensitivity analysis",
    "Removed in the never-partnered sensitivity analysis"
  )
)

#### Round 3 survey domains ####
round3_design <- svydesign(
  ids = ~r3_cluster,
  strata = ~r3_stratum,
  weights = ~r3_weight,
  data = filter(core_data, !is.na(r3_weight), !is.na(r3_stratum),
                !is.na(r3_cluster)),
  nest = TRUE
)
round3_analysis_design <- subset(round3_design, r3_health_analysis_sample)
round3_age_eligible_design <- subset(
  round3_analysis_design,
  r3_birth_year >= 1920 & r3_birth_year <= 1965
)
round3_known_dissolution_design <- subset(
  round3_analysis_design, !r3_unknown_dissolution
)
round3_partnered_design <- subset(
  round3_analysis_design, r3_part_history_11 != "Never partnered"
)

#### Primary models ####
primary_self_rated_health_biography <- svyglm(
  r3_self_rated_health_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)
primary_depressive_symptoms_biography <- svyglm(
  r3_depressive_symptoms_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)
primary_functional_limitations_biography <- svyglm(
  r3_functional_limitations ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = quasipoisson()
)

primary_self_rated_health_history_11 <- svyglm(
  r3_self_rated_health_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)
primary_depressive_symptoms_history_11 <- svyglm(
  r3_depressive_symptoms_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)
primary_functional_limitations_history_11 <- svyglm(
  r3_functional_limitations ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = quasipoisson()
)

#### Born 1920-1965 ####
age_eligible_self_rated_health_biography <- svyglm(
  r3_self_rated_health_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_age_eligible_design,
  family = gaussian()
)
age_eligible_depressive_symptoms_biography <- svyglm(
  r3_depressive_symptoms_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_age_eligible_design,
  family = gaussian()
)
age_eligible_functional_limitations_biography <- svyglm(
  r3_functional_limitations ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_age_eligible_design,
  family = quasipoisson()
)

age_eligible_self_rated_health_history_11 <- svyglm(
  r3_self_rated_health_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_age_eligible_design,
  family = gaussian()
)
age_eligible_depressive_symptoms_history_11 <- svyglm(
  r3_depressive_symptoms_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_age_eligible_design,
  family = gaussian()
)
age_eligible_functional_limitations_history_11 <- svyglm(
  r3_functional_limitations ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_age_eligible_design,
  family = quasipoisson()
)

#### Exclude unknown dissolution ####
known_dissolution_self_rated_health_biography <- svyglm(
  r3_self_rated_health_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_known_dissolution_design,
  family = gaussian()
)
known_dissolution_depressive_symptoms_biography <- svyglm(
  r3_depressive_symptoms_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_known_dissolution_design,
  family = gaussian()
)
known_dissolution_functional_limitations_biography <- svyglm(
  r3_functional_limitations ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_known_dissolution_design,
  family = quasipoisson()
)

known_dissolution_self_rated_health_history_11 <- svyglm(
  r3_self_rated_health_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_known_dissolution_design,
  family = gaussian()
)
known_dissolution_depressive_symptoms_history_11 <- svyglm(
  r3_depressive_symptoms_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_known_dissolution_design,
  family = gaussian()
)
known_dissolution_functional_limitations_history_11 <- svyglm(
  r3_functional_limitations ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_known_dissolution_design,
  family = quasipoisson()
)

#### Exclude never partnered ####
partnered_self_rated_health_biography <- svyglm(
  r3_self_rated_health_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_partnered_design,
  family = gaussian()
)
partnered_depressive_symptoms_biography <- svyglm(
  r3_depressive_symptoms_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_partnered_design,
  family = gaussian()
)
partnered_functional_limitations_biography <- svyglm(
  r3_functional_limitations ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  design = round3_partnered_design,
  family = quasipoisson()
)

partnered_self_rated_health_history_11 <- svyglm(
  r3_self_rated_health_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_partnered_design,
  family = gaussian()
)
partnered_depressive_symptoms_history_11 <- svyglm(
  r3_depressive_symptoms_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_partnered_design,
  family = gaussian()
)
partnered_functional_limitations_history_11 <- svyglm(
  r3_functional_limitations ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_partnered_design,
  family = quasipoisson()
)

#### Unweighted Round 3 ####
unweighted_self_rated_health_biography <- glm(
  r3_self_rated_health_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  data = filter(core_data, r3_health_analysis_sample),
  family = gaussian()
)
unweighted_depressive_symptoms_biography <- glm(
  r3_depressive_symptoms_z ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  data = filter(core_data, r3_health_analysis_sample),
  family = gaussian()
)
unweighted_functional_limitations_biography <- glm(
  r3_functional_limitations ~ r3_n_marriages + r3_n_cohabitations +
    ever_widowed + r3_age + r3_female + r3_education + r3_ethnicity,
  data = filter(core_data, r3_health_analysis_sample),
  family = quasipoisson()
)

unweighted_self_rated_health_history_11 <- glm(
  r3_self_rated_health_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  data = filter(core_data, r3_health_analysis_sample),
  family = gaussian()
)
unweighted_depressive_symptoms_history_11 <- glm(
  r3_depressive_symptoms_z ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  data = filter(core_data, r3_health_analysis_sample),
  family = gaussian()
)
unweighted_functional_limitations_history_11 <- glm(
  r3_functional_limitations ~ partnership_history_11 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  data = filter(core_data, r3_health_analysis_sample),
  family = quasipoisson()
)

#### Partnership biography results ####
biography_results <- bind_rows(
  tidy(primary_self_rated_health_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Self-rated health",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(primary_self_rated_health_biography)
    ),
  tidy(primary_depressive_symptoms_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Depressive Symptoms",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(primary_depressive_symptoms_biography)
    ),
  tidy(primary_functional_limitations_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Functional Limitations",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(primary_functional_limitations_biography)
    ),
  tidy(known_dissolution_self_rated_health_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Exclude unknown dissolution",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Self-rated health",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(known_dissolution_self_rated_health_biography)
    ),
  tidy(known_dissolution_depressive_symptoms_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Exclude unknown dissolution",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Depressive Symptoms",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(known_dissolution_depressive_symptoms_biography)
    ),
  tidy(known_dissolution_functional_limitations_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Exclude unknown dissolution",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Functional Limitations",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(known_dissolution_functional_limitations_biography)
    ),
  tidy(partnered_self_rated_health_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Exclude never partnered",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Self-rated health",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(partnered_self_rated_health_biography)
    ),
  tidy(partnered_depressive_symptoms_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Exclude never partnered",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Depressive Symptoms",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(partnered_depressive_symptoms_biography)
    ),
  tidy(partnered_functional_limitations_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Exclude never partnered",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Functional Limitations",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(partnered_functional_limitations_biography)
    ),
  tidy(unweighted_self_rated_health_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Unweighted Round 3",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Self-rated health",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(unweighted_self_rated_health_biography)
    ),
  tidy(unweighted_depressive_symptoms_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Unweighted Round 3",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Depressive Symptoms",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(unweighted_depressive_symptoms_biography)
    ),
  tidy(unweighted_functional_limitations_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Unweighted Round 3",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Functional Limitations",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(unweighted_functional_limitations_biography)
    )
)

#### Eleven-category history results ####
history_results_11 <- bind_rows(
  tidy(primary_self_rated_health_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Self-rated health",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(primary_self_rated_health_history_11)
    ),
  tidy(primary_depressive_symptoms_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Depressive Symptoms",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(primary_depressive_symptoms_history_11)
    ),
  tidy(primary_functional_limitations_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Functional Limitations",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(primary_functional_limitations_history_11)
    ),
  tidy(known_dissolution_self_rated_health_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Exclude unknown dissolution",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Self-rated health",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(known_dissolution_self_rated_health_history_11)
    ),
  tidy(known_dissolution_depressive_symptoms_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Exclude unknown dissolution",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Depressive Symptoms",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(known_dissolution_depressive_symptoms_history_11)
    ),
  tidy(known_dissolution_functional_limitations_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Exclude unknown dissolution",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Functional Limitations",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(known_dissolution_functional_limitations_history_11)
    ),
  tidy(partnered_self_rated_health_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Exclude never partnered",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Self-rated health",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(partnered_self_rated_health_history_11)
    ),
  tidy(partnered_depressive_symptoms_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Exclude never partnered",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Depressive Symptoms",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(partnered_depressive_symptoms_history_11)
    ),
  tidy(partnered_functional_limitations_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Exclude never partnered",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Functional Limitations",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(partnered_functional_limitations_history_11)
    ),
  tidy(unweighted_self_rated_health_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Unweighted Round 3",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Self-rated health",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(unweighted_self_rated_health_history_11)
    ),
  tidy(unweighted_depressive_symptoms_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Unweighted Round 3",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Depressive Symptoms",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(unweighted_depressive_symptoms_history_11)
    ),
  tidy(unweighted_functional_limitations_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Unweighted Round 3",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Functional Limitations",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(unweighted_functional_limitations_history_11)
    )
)

#### Birth-cohort target results ####
age_eligible_results <- bind_rows(
  tidy(age_eligible_self_rated_health_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Born 1920-1965",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Self-rated health",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(age_eligible_self_rated_health_biography)
    ),
  tidy(age_eligible_depressive_symptoms_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Born 1920-1965",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Depressive Symptoms",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(age_eligible_depressive_symptoms_biography)
    ),
  tidy(age_eligible_functional_limitations_biography) |>
    filter(term %in% c(
      "r3_n_marriages", "r3_n_cohabitations", "ever_widowed"
    )) |>
    transmute(
      Sensitivity = "Born 1920-1965",
      Model = "Partnership biography",
      Typology = "",
      Outcome = "Functional Limitations",
      Predictor = recode(
        term,
        r3_n_marriages = "Number of marriages",
        r3_n_cohabitations = "Number of cohabitations",
        ever_widowed = "Ever widowed"
      ),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(age_eligible_functional_limitations_biography)
    ),
  tidy(age_eligible_self_rated_health_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Born 1920-1965",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Self-rated health",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(age_eligible_self_rated_health_history_11)
    ),
  tidy(age_eligible_depressive_symptoms_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Born 1920-1965",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Depressive Symptoms",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(age_eligible_depressive_symptoms_history_11)
    ),
  tidy(age_eligible_functional_limitations_history_11) |>
    filter(startsWith(term, "partnership_history_11")) |>
    transmute(
      Sensitivity = "Born 1920-1965",
      Model = "Partnership-history typology",
      Typology = "11 categories",
      Outcome = "Functional Limitations",
      Predictor = sub("^partnership_history_11", "", term),
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = exp(estimate),
      Reported_standard_error = exp(estimate) * std.error,
      Scale = "IRR",
      N = nobs(age_eligible_functional_limitations_history_11)
    )
)

#### Seven-category history ####
self_rated_health_history_7 <- svyglm(
  r3_self_rated_health_z ~ partnership_history_7 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)
depressive_symptoms_history_7 <- svyglm(
  r3_depressive_symptoms_z ~ partnership_history_7 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)
functional_limitations_history_7 <- svyglm(
  r3_functional_limitations ~ partnership_history_7 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = quasipoisson()
)

history_results_7 <- bind_rows(
  tidy(self_rated_health_history_7) |>
    filter(startsWith(term, "partnership_history_7")) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership-history typology",
      Typology = "7 categories",
      Outcome = "Self-rated health",
      Predictor = sub("^partnership_history_7", "", term),
      Supported = TRUE,
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(self_rated_health_history_7)
    ),
  tidy(depressive_symptoms_history_7) |>
    filter(startsWith(term, "partnership_history_7")) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership-history typology",
      Typology = "7 categories",
      Outcome = "Depressive Symptoms",
      Predictor = sub("^partnership_history_7", "", term),
      Supported = TRUE,
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(depressive_symptoms_history_7)
    ),
  tidy(functional_limitations_history_7) |>
    filter(startsWith(term, "partnership_history_7")) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership-history typology",
      Typology = "7 categories",
      Outcome = "Functional Limitations",
      Predictor = sub("^partnership_history_7", "", term),
      Supported = Predictor %in%
        filter(round3_analysis_design$variables,
               r3_functional_limitations > 0)$partnership_history_7,
      Estimate = if_else(Supported, estimate, NA),
      Standard_error = if_else(Supported, std.error, NA),
      P_value = if_else(Supported, p.value, NA),
      Reported_estimate = if_else(Supported, exp(estimate), NA),
      Reported_standard_error = if_else(
        Supported, exp(estimate) * std.error, NA
      ),
      Scale = "IRR",
      N = nobs(functional_limitations_history_7)
    )
)

#### Fourteen-category history ####
self_rated_health_history_14 <- svyglm(
  r3_self_rated_health_z ~ partnership_history_14 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)
depressive_symptoms_history_14 <- svyglm(
  r3_depressive_symptoms_z ~ partnership_history_14 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = gaussian()
)
functional_limitations_history_14 <- svyglm(
  r3_functional_limitations ~ partnership_history_14 + r3_age + r3_female +
    r3_education + r3_ethnicity,
  design = round3_analysis_design,
  family = quasipoisson()
)

history_results_14 <- bind_rows(
  tidy(self_rated_health_history_14) |>
    filter(startsWith(term, "partnership_history_14")) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership-history typology",
      Typology = "14 categories",
      Outcome = "Self-rated health",
      Predictor = sub("^partnership_history_14", "", term),
      Supported = TRUE,
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(self_rated_health_history_14)
    ),
  tidy(depressive_symptoms_history_14) |>
    filter(startsWith(term, "partnership_history_14")) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership-history typology",
      Typology = "14 categories",
      Outcome = "Depressive Symptoms",
      Predictor = sub("^partnership_history_14", "", term),
      Supported = TRUE,
      Estimate = estimate,
      Standard_error = std.error,
      P_value = p.value,
      Reported_estimate = estimate,
      Reported_standard_error = std.error,
      Scale = "Outcome SD units",
      N = nobs(depressive_symptoms_history_14)
    ),
  tidy(functional_limitations_history_14) |>
    filter(startsWith(term, "partnership_history_14")) |>
    transmute(
      Sensitivity = "Primary Round 3 survey design",
      Model = "Partnership-history typology",
      Typology = "14 categories",
      Outcome = "Functional Limitations",
      Predictor = sub("^partnership_history_14", "", term),
      Supported = Predictor %in%
        filter(round3_analysis_design$variables,
               r3_functional_limitations > 0)$partnership_history_14,
      Estimate = if_else(Supported, estimate, NA),
      Standard_error = if_else(Supported, std.error, NA),
      P_value = if_else(Supported, p.value, NA),
      Reported_estimate = if_else(Supported, exp(estimate), NA),
      Reported_standard_error = if_else(
        Supported, exp(estimate) * std.error, NA
      ),
      Scale = "IRR",
      N = nobs(functional_limitations_history_14)
    )
)

#### Combine results ####
model_results <- bind_rows(
  biography_results,
  history_results_11,
  age_eligible_results,
  history_results_7,
  history_results_14
)

sensitivity_results <- model_results |>
  left_join(
    model_results |>
      filter(Sensitivity == "Primary Round 3 survey design") |>
      select(
        Model, Typology, Outcome, Predictor,
        Primary_reported_estimate = Reported_estimate
      ),
    by = c("Model", "Typology", "Outcome", "Predictor")
  ) |>
  mutate(
    Change_from_primary = Reported_estimate - Primary_reported_estimate
  ) |>
  arrange(Model, Typology, Outcome, Predictor, Sensitivity)

#### Save sensitivity analyses ####
workbook <- createWorkbook()
modifyBaseFont(workbook, fontSize = 10.5, fontName = "Times New Roman")
addWorksheet(workbook, "Sensitivity analyses")
header_style <- createStyle(
  textDecoration = "bold", border = "bottom", borderStyle = "thin"
)
writeData(
  workbook, "Sensitivity analyses", sensitivity_results,
  headerStyle = header_style
)
writeData(
  workbook, "Sensitivity analyses", "Sample counts",
  startRow = nrow(sensitivity_results) + 4
)
writeData(
  workbook, "Sensitivity analyses", sample_counts,
  startRow = nrow(sensitivity_results) + 5,
  headerStyle = header_style
)
setColWidths(workbook, "Sensitivity analyses", cols = 1:3, widths = 24)
setColWidths(
  workbook, "Sensitivity analyses",
  cols = 4:ncol(sensitivity_results), widths = 16
)
addStyle(
  workbook, "Sensitivity analyses",
  createStyle(wrapText = TRUE, valign = "center"),
  rows = seq_len(nrow(sensitivity_results) + nrow(sample_counts) + 6),
  cols = seq_len(ncol(sensitivity_results)),
  gridExpand = TRUE,
  stack = TRUE
)
saveWorkbook(
  workbook, "tables_figures/sensitivity_analyses.xlsx", overwrite = TRUE
)
