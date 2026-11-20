# Author: Yiang Li
# Build Table 1 from the cleaned respondent data.
library(dplyr)
library(tidyr)
library(openxlsx)

table1_data <- readRDS("../tmp/nshap_core.rds") |>
  transmute(
    Overall = TRUE,
    Male = gender == 1,
    Female = gender == 2,
    `NH White` = ethnicity == "NH White",
    `NH Black` = ethnicity == "NH Black",
    Hispanic = ethnicity == "Hispanic",
    Other = ethnicity == "Other",
    `< High School` = education == "< High School",
    `High School` = education == "High School",
    `Some College` = education == "Some College",
    `Bachelor +` = education == "Bachelor +",
    `Silent Generation` = birth_cohort == "Silent Generation",
    `Baby Boom` = birth_cohort == "Baby Boom",
    n_partnership_records,
    n_marriages,
    n_cohabitations,
    n_nonmarital_cohabitations,
    n_marriages_divorced_separated,
    n_marriages_widowed,
    mean_marriage_years,
    total_marriage_years,
    mean_nonmarital_cohabitation_years,
    total_nonmarital_cohabitation_years,
    first_union_marriage,
    first_union_cohabitation,
    ever_premarital_cohabitation,
    ever_divorced,
    ever_widowed,
    age_first_marriage,
    age_first_nonmarital_cohabitation,
    age_first_premarital_cohabitation,
    years_cohabiting_before_marriage
  )

#### Table 1 ####
table1_long <- tribble(
  ~Measure, ~variable, ~display,
  "A. Partnership counts per respondent, mean (SD)", NA, "header",
  " Number of partnership records", "n_partnership_records", "numeric",
  " Number of marriages", "n_marriages", "numeric",
  " Number of cohabitations (premarital + non-marital)",
  "n_cohabitations", "numeric",
  " Number of non-marital cohabitations", "n_nonmarital_cohabitations",
  "numeric",
  " Number of marriages ending in divorce or separation",
  "n_marriages_divorced_separated", "numeric",
  " Number of marriages ending in widowhood", "n_marriages_widowed", "numeric",
  "B. Durations, mean (SD), years", NA, "header",
  " Mean marriage duration", "mean_marriage_years", "numeric",
  " Total years in marriage", "total_marriage_years", "numeric",
  " Mean non-marital cohabitation duration",
  "mean_nonmarital_cohabitation_years", "numeric",
  " Total years in non-marital cohabitation",
  "total_nonmarital_cohabitation_years", "numeric",
  "C. Dissolution and sequence, %", NA, "header",
  " First union was a marriage", "first_union_marriage", "binary",
  " First union was a cohabitation", "first_union_cohabitation", "binary",
  " Any premarital cohabitation", "ever_premarital_cohabitation",
  "binary",
  " Ever divorced or separated", "ever_divorced", "binary",
  " Ever widowed", "ever_widowed", "binary",
  "D. Ages, mean (SD), years", NA, "header",
  " Age at first marriage", "age_first_marriage", "numeric",
  " Age at first non-marital cohabitation",
  "age_first_nonmarital_cohabitation", "numeric",
  " Age at first premarital cohabitation",
  "age_first_premarital_cohabitation", "numeric",
  " Gap (yrs) from premarital cohab to that marriage",
  "years_cohabiting_before_marriage", "numeric"
) |>
  mutate(row = row_number() + 3) |>
  left_join(
    table1_data |>
      pivot_longer(n_partnership_records:years_cohabiting_before_marriage,
                   names_to = "variable", values_drop_na = TRUE),
    by = "variable"
  )

table1 <- table1_long |>
  summarise(
    N = if_else(first(display) == "header", "", format(n(), big.mark = ",")),
    across(Overall:`Baby Boom`, ~ case_when(
      first(display) == "header" ~ "",
      !any(.x %in% TRUE) ~ NA,
      first(display) == "binary" ~
        sprintf("%.2f%%", 100 * mean(value[.x %in% TRUE] == 1)),
      TRUE ~ sprintf("%.2f (%.2f)", mean(value[.x %in% TRUE]),
                     sd(value[.x %in% TRUE]))
    )),
    .by = c(row, Measure)
  ) |>
  select(-row) |>
  # Add subgroup sizes to the column names.
  rename_with(
    ~ sprintf("%s\n(n = %s)", .x,
              format(colSums(table1_data[.x], na.rm = TRUE),
                     big.mark = ",", trim = TRUE)),
    Overall:`Baby Boom`
  )

# Welch t-test of each subgroup against everyone else.
table1_fills <- table1_long |>
  filter(display != "header") |>
  pivot_longer(Male:`Baby Boom`, names_to = "group", values_to = "in_group",
               values_drop_na = TRUE) |>
  filter(sum(in_group) > 1, sum(!in_group) > 1,
         sd(value[in_group]) > 0 | sd(value[!in_group]) > 0,
         .by = c(row, group)) |>
  summarise(
    p_value = t.test(value[in_group], value[!in_group])$p.value,
    higher_group = mean(value[in_group]) > mean(value[!in_group]),
    .by = c(row, group)
  ) |>
  transmute(
    row,
    col = match(group, names(table1_data)) + 2,
    fill = case_when(
      p_value < .001 & higher_group ~ "#FDB863",
      p_value < .001 ~ "#B2ABD2",
      p_value < .01 & higher_group ~ "#FEE0B6",
      p_value < .01 ~ "#D8DAEB",
      p_value < .05 & higher_group ~ "#FFF4E0",
      p_value < .05 ~ "#EFEFF6"
    )
  ) |>
  filter(!is.na(fill))

#### Save Table 1 ####
workbook <- createWorkbook()
modifyBaseFont(workbook, fontSize = 10.5, fontName = "Times New Roman")
addWorksheet(workbook, "Table 1")
header_style <- createStyle(textDecoration = "bold", border = "bottom",
                            borderStyle = "thin")
writeData(
  workbook, "Table 1",
  sprintf("Table 1. Unweighted Partnership History, Overall and by Demographic Subgroup, NSHAP Rounds 1-4 (N = %s)", format(nrow(table1_data), big.mark = ",")),
  startRow = 1, colNames = FALSE
)
mergeCells(workbook, "Table 1", rows = 1, cols = seq_len(ncol(table1)))
addStyle(
  workbook, "Table 1",
  createStyle(halign = "left", textDecoration = "bold"),
  rows = 1, cols = 1
)
writeData(workbook, "Table 1", table1, startRow = 3,
          headerStyle = header_style)
setColWidths(workbook, "Table 1", cols = 1, widths = 46)
setColWidths(workbook, "Table 1", cols = 2, widths = 9)
setColWidths(workbook, "Table 1", cols = 3:ncol(table1), widths = 14)
setRowHeights(workbook, "Table 1", rows = 3, heights = 32)
setRowHeights(workbook, "Table 1", rows = seq_len(nrow(table1)) + 3,
              heights = 18)
addStyle(
  workbook, "Table 1",
  createStyle(wrapText = TRUE, valign = "center"),
  rows = seq_len(nrow(table1) + 1) + 2,
  cols = seq_len(ncol(table1)), gridExpand = TRUE, stack = TRUE
)

for (section_row in which(grepl("^[A-D]\\. ", table1$Measure))) {
  mergeCells(workbook, "Table 1", rows = section_row + 3,
             cols = seq_len(ncol(table1)))
  addStyle(
    workbook, "Table 1",
    createStyle(halign = "left", textDecoration = "bold",
                border = "top", borderStyle = "thin"),
    rows = section_row + 3, cols = 1, stack = TRUE
  )
}

# Shade cells with significant differences.
for (fill_color in unique(table1_fills$fill)) {
  addStyle(
    workbook, "Table 1", createStyle(fgFill = fill_color),
    rows = table1_fills$row[table1_fills$fill == fill_color],
    cols = table1_fills$col[table1_fills$fill == fill_color],
    stack = TRUE
  )
}
addStyle(
  workbook, "Table 1",
  createStyle(border = "bottom", borderStyle = "thin"),
  rows = nrow(table1) + 3, cols = seq_len(ncol(table1)), stack = TRUE
)

writeData(
  workbook, "Table 1",
  "Note: Cell entries are means with standard deviations for continuous measures (Panels A, B, and D) and percentages for binary indicators (Panel C). Column headers give each subgroup's number of respondents, and the N column gives the number of respondents contributing to that measure overall. Mean and total durations require every relevant episode to have a computable duration, use midyear when a month is missing, and count overlapping episodes once. Total non-marital cohabitation includes zero years for respondents with no non-marital cohabitation. Dissolution counts refer to marriages. Entries and tests are unweighted and describe the analytic sample, not the U.S. population. Shading uses two-sided Welch tests of each subgroup against the rest of the sample, without adjustment for multiple comparisons. Orange means the subgroup estimate is higher than the rest of the sample and purple means it is lower. The darkest shade is p < .001, the medium shade p < .01, and the lightest p < .05. Source: NSHAP Rounds 1-4 partnership history file.",
  startRow = nrow(table1) + 5, colNames = FALSE
)
mergeCells(workbook, "Table 1", rows = nrow(table1) + 5,
           cols = seq_len(ncol(table1)))
addStyle(
  workbook, "Table 1", createStyle(wrapText = TRUE, valign = "top"),
  rows = nrow(table1) + 5, cols = 1
)
setRowHeights(workbook, "Table 1", rows = nrow(table1) + 5, heights = 90)

saveWorkbook(workbook, "tables_figures/table1_descriptives.xlsx",
             overwrite = TRUE)
