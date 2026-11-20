# Author: Yiang Li
# Harmonize core measures and survey variables across the four rounds.
library(dplyr)
library(haven)

#### Core interviews ####
core_data <- bind_rows(
  read_dta("../data/nshap_w1_core.dta") |>
    zap_labels() |>
    mutate(
      interview_round = 1,
      interview_month = as.integer(int_start) %% 12 + 1,
      interview_year = 1960 + as.integer(int_start) %/% 12
    ) |>
    select(
      su_id, interview_round, interview_month, interview_year, gender,
      ethnicity_code = ethgrp, education_code = educ, birth_year = ageyear
    ),
  read_dta("../data/nshap_w2_core.dta") |>
    zap_labels() |>
    mutate(
      interview_round = 2,
      interview_month = as.integer(int_start) %% 12 + 1,
      interview_year = 1960 + as.integer(int_start) %/% 12
    ) |>
    select(
      su_id, interview_round, interview_month, interview_year, gender,
      ethnicity_code = ethgrp, education_code = educ, birth_year = ageyear
    ),
  read_dta("../data/nshap_w3_core.dta") |>
    zap_labels() |>
    mutate(
      interview_round = 3,
      interview_month = as.integer(int_start) %% 12 + 1,
      interview_year = 1960 + as.integer(int_start) %/% 12
    ) |>
    mutate(
      across(
        all_of(c(
          "notgetgo", "noteat", "fltdep", "flteff", "nosleep", "washapy",
          "waslonly", "unfriend", "enjlife", "fltsad", "dislikd"
        )),
        ~ case_when(
          .x == 1 ~ 0,
          .x == 2 ~ 1,
          .x %in% c(3, 4) ~ 2
        )
      ),
      across(
        all_of(c("washapy", "enjlife")),
        ~ 2 - .x
      ),
      across(
        all_of(c(
          "walkroom", "dressing", "bathing", "eating", "inoutbed", "toilet"
        )),
        ~ case_when(
          .x == 0 ~ 0,
          .x %in% 1:4 ~ 1
        )
      ),
      r3_interviewed = TRUE,
      r3_self_rated_health = physhlth,
      r3_depressive_symptoms = if_else(
        if_all(all_of(c(
          "notgetgo", "noteat", "fltdep", "flteff", "nosleep", "washapy",
          "waslonly", "unfriend", "enjlife", "fltsad", "dislikd"
        )), ~ !is.na(.x)),
        rowSums(pick(all_of(c(
          "notgetgo", "noteat", "fltdep", "flteff", "nosleep", "washapy",
          "waslonly", "unfriend", "enjlife", "fltsad", "dislikd"
        )))),
        NA
      ),
      r3_functional_limitations = if_else(
        if_all(all_of(c(
          "walkroom", "dressing", "bathing", "eating", "inoutbed", "toilet"
        )), ~ !is.na(.x)),
        rowSums(pick(all_of(c(
          "walkroom", "dressing", "bathing", "eating", "inoutbed", "toilet"
        )))),
        NA
      ),
      r3_age = age,
      r3_gender = gender,
      r3_ethnicity_code = ethgrp,
      r3_education_code = educ,
      r3_weight = as.numeric(weight_adj),
      r3_stratum = as.numeric(stratum),
      r3_cluster = as.numeric(cluster)
    ) |>
    select(
      su_id, interview_round, interview_month, interview_year, gender,
      ethnicity_code = ethgrp, education_code = educ, birth_year = ageyear,
      starts_with("r3_")
    ),
  read_dta("../data/nshap_r4_core.dta") |>
    left_join(
      read_dta(
        "../data/2024_03_20_nshap_r4_raw_plus_lbq.dta",
        col_select = c(su_id, ageyear, gender, educ, raceh, intmo, intyr)
      ) |>
        transmute(
          su_id, birth_year = ageyear,
          raw_gender = gender,
          raw_education = educ,
          raw_ethnicity = raceh,
          intmo, intyr
        ),
      by = "su_id"
    ) |>
    left_join(
      read_dta("../data/r4_newrespondents_gender_ethgrp_educ.dta") |>
        transmute(
          su_id,
          supplement_gender = gender,
          supplement_education = educ,
          supplement_ethnicity = ethgrp
        ),
      by = "su_id"
    ) |>
    mutate(
      gender = coalesce(raw_gender, supplement_gender),
      # Education for new Round 4 respondents comes from the supplement.
      education_code = coalesce(supplement_education, raw_education),
      ethnicity_code = coalesce(
        as.numeric(raw_ethnicity),
        as.numeric(supplement_ethnicity)
      )
    ) |>
    zap_labels() |>
    mutate(
      interview_round = 4,
      interview_month = case_when(
        !is.na(int_start) ~ as.integer(int_start) %% 12 + 1,
        intmo %in% 1:12 ~ as.numeric(intmo)
      ),
      interview_year = case_when(
        !is.na(int_start) ~ 1960 + as.integer(int_start) %/% 12,
        intyr > 0 ~ as.numeric(intyr)
      )
    ) |>
    mutate(across(where(is.numeric), ~ if_else(.x < 0, NA, .x))) |>
    select(
      su_id, interview_round, interview_month, interview_year, gender,
      ethnicity_code, education_code, birth_year
    )
)

# Keep the latest available core response for each respondent.
core_data <- core_data |>
  arrange(su_id, interview_round) |>
  group_by(su_id) |>
  mutate(
    interview_month = last(interview_month),
    interview_year = last(interview_year),
    birth_year = last(birth_year[!is.na(birth_year)], default = NA),
    gender = last(gender[!is.na(gender)], default = NA),
    ethnicity_code = last(ethnicity_code[!is.na(ethnicity_code)],
                          default = NA),
    education_code = last(education_code[!is.na(education_code)],
                          default = NA),
    r3_interviewed = any(r3_interviewed %in% TRUE),
    across(
      starts_with("r3_") & !matches("r3_interviewed"),
      ~ first(.x[!is.na(.x)], default = NA)
    )
  ) |>
  slice_tail(n = 1) |>
  ungroup() |>
  mutate(
    birth_cohort = factor(
      case_when(
        birth_year >= 1920 & birth_year <= 1947 ~ "Silent Generation",
        birth_year >= 1948 & birth_year <= 1965 ~ "Baby Boom"
      ),
      levels = c("Silent Generation", "Baby Boom")
    ),
    ethnicity = factor(
      ethnicity_code,
      levels = 1:4,
      labels = c("NH White", "NH Black", "Hispanic", "Other")
    ),
    education = factor(
      education_code,
      levels = 1:4,
      labels = c(
        "< High School", "High School", "Some College",
        "Bachelor +"
      )
    ),
    r3_female = as.integer(r3_gender == 2),
    r3_ethnicity = factor(
      r3_ethnicity_code,
      levels = 1:4,
      labels = c("NH White", "NH Black", "Hispanic", "Other")
    ),
    r3_education = factor(
      r3_education_code,
      levels = 1:4,
      labels = c(
        "< High School", "High School", "Some College",
        "Bachelor +"
      )
    )
  )

#### Respondent partnership histories ####
core_data <- core_data |>
  left_join(
    readRDS("output/nshap_par_hisr1234.rds") |>
      left_join(
        core_data |>
          filter(!is.na(interview_year)) |>
          transmute(
            su_id,
            latest_interview_year = interview_year,
            latest_interview_date = interview_year +
              (coalesce(interview_month, 6) - 1) / 12,
            birth_year
          ),
        by = "su_id"
      ) |>
      mutate(
        # Use midyear or a nearby month when only a year was reported.
        marriage_start = marbeginy + (case_when(
          !is.na(marbeginm) ~ marbeginm,
          has_mar & has_coh & is.na(cobeginm) &
            !is.na(marbeginy) & marbeginy == cobeginy ~ 6,
          has_mar & has_coh & !is.na(cobeginm) &
            !is.na(marbeginy) & marbeginy == cobeginy ~
            pmin(12, cobeginm + 1),
          !is.na(marbeginy) & !is.na(marendy) &
            marbeginy == marendy ~ pmax(1, coalesce(marendm, 6) - 1),
          TRUE ~ 6
        ) - 1) / 12,
        marriage_end = marendy + (case_when(
          !is.na(marendm) ~ marendm,
          !is.na(marendy) & !is.na(marbeginy) & marendy == marbeginy ~
            pmin(12, coalesce(marbeginm, 6) + 1),
          TRUE ~ 6
        ) - 1) / 12,
        cohabitation_start = cobeginy + (case_when(
          !is.na(cobeginm) ~ cobeginm,
          has_mar & has_coh & is.na(marbeginm) &
            !is.na(cobeginy) & cobeginy == marbeginy ~ 5,
          has_mar & has_coh & !is.na(marbeginm) &
            !is.na(cobeginy) & cobeginy == marbeginy ~
            pmax(1, marbeginm - 1),
          !is.na(cobeginy) & !is.na(coendy) & cobeginy == coendy ~
            pmax(1, coalesce(coendm, 6) - 1),
          TRUE ~ 6
        ) - 1) / 12,
        cohabitation_end = coendy + (case_when(
          !is.na(coendm) ~ coendm,
          !is.na(coendy) & !is.na(cobeginy) & coendy == cobeginy ~
            pmin(12, coalesce(cobeginm, 6) + 1),
          TRUE ~ 6
        ) - 1) / 12,
        bad_premarital_dates = has_mar & has_coh &
          !is.na(marbeginy) & !is.na(cobeginy) &
          (marbeginy < cobeginy |
             (marbeginy == cobeginy & !is.na(marbeginm) &
                !is.na(cobeginm) & marbeginm < cobeginm)),
        # Intact unions are censored at the latest core interview.
        marriage_years = case_when(
          !has_mar | is.na(marriage_start) ~ NA,
          mar_ended & is.na(marriage_end) ~ NA,
          mar_ended & marriage_end < marriage_start ~ NA,
          mar_ended ~ marriage_end - marriage_start,
          !mar_ended & latest_interview_date < marriage_start ~ NA,
          !mar_ended ~ latest_interview_date - marriage_start
        ),
        # Premarital cohabitation ends at marriage.
        cohabitation_years = case_when(
          !has_coh | is.na(cohabitation_start) ~ NA,
          bad_premarital_dates ~ NA,
          has_mar & !is.na(marriage_start) ~
            marriage_start - cohabitation_start,
          has_mar ~ NA,
          !is.na(cohabitation_end) &
            cohabitation_end < cohabitation_start ~ NA,
          !is.na(cohabitation_end) ~ cohabitation_end - cohabitation_start,
          coh_ended ~ NA,
          !coh_ended & latest_interview_date < cohabitation_start ~ NA,
          !coh_ended ~ latest_interview_date - cohabitation_start
        ),
        bad_marriage_dates = has_mar & !is.na(marriage_start) &
          !is.na(marriage_end) & marriage_end < marriage_start,
        bad_cohabitation_dates = has_coh & !is.na(cohabitation_start) &
          !is.na(cohabitation_end) & cohabitation_end < cohabitation_start,
        bad_marriage_year = (!is.na(marbeginy) & marbeginy < 1900) |
          (!is.na(marbeginy) & marbeginy > latest_interview_year) |
          (!is.na(marendy) & marendy < 1900) |
          (!is.na(marendy) & marendy > latest_interview_year),
        bad_cohabitation_year = (!is.na(cobeginy) & cobeginy < 1900) |
          (!is.na(cobeginy) & cobeginy > latest_interview_year) |
          (!is.na(coendy) & coendy < 1900) |
          (!is.na(coendy) & coendy > latest_interview_year),
        marriage_after_interview = has_mar & !is.na(marriage_start) &
          marriage_start > latest_interview_date,
        cohabitation_after_interview = has_coh & !is.na(cohabitation_start) &
          cohabitation_start > latest_interview_date,
        marriage_end_after_update = has_mar & !is.na(marendy) &
          marendy > int_year,
        cohabitation_end_after_update = has_coh & !is.na(coendy) &
          coendy > int_year,
        bad_marriage_age = has_mar & !is.na(marriage_start) &
          !is.na(birth_year) & marriage_start - birth_year < 12,
        bad_cohabitation_age = has_coh & !is.na(cohabitation_start) &
          !is.na(birth_year) & cohabitation_start - birth_year < 12
      ) |>
      mutate(
        marriage_years = if_else(
          coalesce(bad_marriage_dates, FALSE) |
            coalesce(bad_marriage_year, FALSE) |
            coalesce(marriage_after_interview, FALSE) |
            coalesce(marriage_end_after_update, FALSE) |
            coalesce(bad_marriage_age, FALSE),
          NA, marriage_years
        ),
        cohabitation_years = if_else(
          coalesce(bad_cohabitation_dates, FALSE) |
            coalesce(bad_premarital_dates, FALSE) |
            coalesce(bad_cohabitation_year, FALSE) |
            coalesce(cohabitation_after_interview, FALSE) |
            coalesce(cohabitation_end_after_update, FALSE) |
            coalesce(bad_cohabitation_age, FALSE),
          NA, cohabitation_years
        ),
        marriage_start = if_else(
          coalesce(marriage_after_interview, FALSE) |
            coalesce(bad_marriage_age, FALSE),
          NA, marriage_start
        ),
        cohabitation_start = if_else(
          coalesce(bad_premarital_dates, FALSE) |
            coalesce(cohabitation_after_interview, FALSE) |
            coalesce(bad_cohabitation_age, FALSE),
          NA, cohabitation_start
        )
      ) |>
      arrange(su_id, marriage_start) |>
      group_by(su_id) |>
      # Count overlapping years only once in the totals.
      mutate(
        marriage_years_added = pmax(
          0, marriage_start + marriage_years -
            pmax(marriage_start, cummax(coalesce(
              lag(marriage_start + marriage_years), -Inf
            )))
        )
      ) |>
      arrange(su_id, cohabitation_start) |>
      mutate(
        cohabitation_years_added = pmax(
          0, cohabitation_start + cohabitation_years -
            pmax(cohabitation_start, cummax(coalesce(
              lag(if_else(has_coh & !has_mar,
                          cohabitation_start + cohabitation_years, NA)), -Inf
            )))
        )
      ) |>
      ungroup() |>
      arrange(su_id, scid) |>
      group_by(su_id) |>
      summarise(
        part_history_14 = first(part_history_14),
        part_history_11 = first(part_history_11),
        part_history_7 = first(part_history_7),
        n_partnership_records = sum(has_mar | has_coh),
        n_marriages = sum(has_mar),
        n_cohabitations = sum(has_coh),
        n_nonmarital_cohabitations = sum(has_coh & !has_mar),
        n_marriages_divorced_separated = sum(
          has_mar & end_reason == "divorced_separated",
          na.rm = TRUE
        ),
        n_marriages_widowed = sum(
          has_mar & end_reason == "widowed",
          na.rm = TRUE
        ),
        ever_widowed = any(
          (has_mar & end_reason == "widowed") | maritlst_latest == 5,
          na.rm = TRUE
        ),
        ever_divorced = any(
          (has_mar & end_reason == "divorced_separated") |
            maritlst_latest %in% c(3, 4),
          na.rm = TRUE
        ),
        ever_premarital_cohabitation = case_when(
          any(has_mar & livebfmr == 1, na.rm = TRUE) ~ TRUE,
          any(has_mar & is.na(livebfmr)) ~ NA,
          any(has_mar) ~ FALSE
        ),
        first_marriage_start = if_else(
          any(has_mar & is.na(marriage_start)), NA,
          min(marriage_start, Inf, na.rm = TRUE)
        ),
        first_cohabitation_start = if_else(
          any(has_coh & is.na(cohabitation_start)), NA,
          min(cohabitation_start, Inf, na.rm = TRUE)
        ),
        any_marriage = any(has_mar),
        any_cohabitation = any(has_coh),
        first_nonmarital_cohabitation_start = if_else(
          any(has_coh & !has_mar & is.na(cohabitation_start)), NA,
          min(cohabitation_start[has_coh & !has_mar], Inf, na.rm = TRUE)
        ),
        first_premarital_cohabitation_start = if_else(
          any(has_mar & livebfmr == 1 & is.na(cohabitation_start),
              na.rm = TRUE), NA,
          min(cohabitation_start[has_mar & livebfmr == 1], Inf, na.rm = TRUE)
        ),
        # Years from the first premarital cohabitation to that marriage.
        years_cohabiting_before_marriage = if_else(
          any(has_mar & livebfmr == 1 &
                (is.na(cohabitation_start) | is.na(marriage_start)),
              na.rm = TRUE), NA,
          first((marriage_start - cohabitation_start)[which.min(if_else(
            has_mar & livebfmr %in% 1 & !is.na(marriage_start),
            cohabitation_start, NA
          ))])
        ),
        mean_marriage_years = if_else(
          any(has_mar & is.na(marriage_years)), NA,
          mean(marriage_years[has_mar])
        ),
        mean_nonmarital_cohabitation_years = if_else(
          any(has_coh & !has_mar & is.na(cohabitation_years)), NA,
          mean(cohabitation_years[has_coh & !has_mar])
        ),
        total_marriage_years = if_else(
          any(has_mar & is.na(marriage_years)), NA,
          sum(marriage_years_added[has_mar])
        ),
        total_nonmarital_cohabitation_years = if_else(
          any(has_coh & !has_mar & is.na(cohabitation_years)), NA,
          sum(cohabitation_years_added[has_coh & !has_mar])
        ),
        .groups = "drop"
      ) |>
      mutate(
        across(
          c(
            first_marriage_start, first_cohabitation_start,
            first_nonmarital_cohabitation_start,
            first_premarital_cohabitation_start, mean_marriage_years,
            mean_nonmarital_cohabitation_years
          ),
          ~ if_else(is.infinite(.x) | is.nan(.x), NA, .x)
        ),
        first_union_type = case_when(
          !any_marriage & !any_cohabitation ~ NA,
          any_marriage & !any_cohabitation ~ "marriage",
          !any_marriage & any_cohabitation ~ "cohabitation",
          is.na(first_marriage_start) | is.na(first_cohabitation_start) ~ NA,
          first_marriage_start < first_cohabitation_start ~ "marriage",
          first_cohabitation_start < first_marriage_start ~ "cohabitation",
          ever_premarital_cohabitation %in% TRUE ~ "cohabitation"
        )
      ),
    by = "su_id"
  ) |>
  mutate(
    first_union_marriage = first_union_type == "marriage",
    first_union_cohabitation = first_union_type == "cohabitation",
    age_first_marriage = first_marriage_start - birth_year,
    age_first_nonmarital_cohabitation =
      first_nonmarital_cohabitation_start - birth_year,
    age_first_premarital_cohabitation =
      first_premarital_cohabitation_start - birth_year,
    age_first_marriage = if_else(
      age_first_marriage >= 12, age_first_marriage, NA
    ),
    age_first_nonmarital_cohabitation = if_else(
      age_first_nonmarital_cohabitation >= 12,
      age_first_nonmarital_cohabitation, NA
    ),
    age_first_premarital_cohabitation = if_else(
      age_first_premarital_cohabitation >= 12,
      age_first_premarital_cohabitation, NA
    ),
    years_cohabiting_before_marriage = if_else(
      years_cohabiting_before_marriage >= 0,
      years_cohabiting_before_marriage, NA
    )
  ) |>
  left_join(
    readRDS("../tmp/nshap_partnership_history_round3.rds"),
    by = "su_id"
  ) |>
  mutate(
    r3_health_analysis_sample = r3_interviewed & if_all(
      c(
        r3_self_rated_health, r3_depressive_symptoms,
        r3_functional_limitations, r3_age, r3_female, r3_education,
        r3_ethnicity, r3_weight, r3_stratum, r3_cluster,
        r3_part_history_7, r3_part_history_11, r3_part_history_14,
        r3_n_marriages, r3_n_cohabitations, r3_ever_widowed
      ),
      ~ !is.na(.x)
    ),
    r3_self_rated_health_z = if_else(
      r3_health_analysis_sample,
      (r3_self_rated_health - mean(
        r3_self_rated_health[r3_health_analysis_sample]
      )) / sd(r3_self_rated_health[r3_health_analysis_sample]),
      NA
    ),
    r3_depressive_symptoms_z = if_else(
      r3_health_analysis_sample,
      (r3_depressive_symptoms - mean(
        r3_depressive_symptoms[r3_health_analysis_sample]
      )) / sd(r3_depressive_symptoms[r3_health_analysis_sample]),
      NA
    )
  )

saveRDS(core_data, "../tmp/nshap_core.rds")
