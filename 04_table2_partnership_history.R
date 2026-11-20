# Author: Yiang Li
# Build Table 2 from the cleaned respondent data.
library(dplyr)
library(tidyr)
library(openxlsx)

table2_data <- readRDS("../tmp/nshap_core.rds") |>
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
    part_history_11
  )

#### Table 2 ####
table2_long <- tibble(
  category = c(
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
) |>
  mutate(row = row_number() + 2) |>
  cross_join(table2_data) |>
  mutate(value = as.integer(part_history_11 == category)) |>
  filter(!is.na(value))

table2 <- bind_rows(
  tibble(Measure = "Table 2. Unweighted Partnership History (11 categories)"),
  table2_long |>
    summarise(
      across(Overall:`Baby Boom`, ~ if_else(
        any(.x %in% TRUE),
        sprintf("%.2f%%", 100 * mean(value[.x %in% TRUE] == 1)),
        NA
      )),
      .by = c(row, category)
    ) |>
    transmute(Measure = paste0(" ", category), across(Overall:`Baby Boom`))
) |>
  # Add subgroup sizes to the column names.
  rename_with(
    ~ sprintf("%s\n(n = %s)", .x,
              format(colSums(table2_data[.x], na.rm = TRUE),
                     big.mark = ",", trim = TRUE)),
    Overall:`Baby Boom`
  )

# Welch t-test of each subgroup against everyone else.
table2_fills <- table2_long |>
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
    col = match(group, names(table2_data)) + 1,
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

#### Save Table 2 ####
workbook <- createWorkbook()
modifyBaseFont(workbook, fontSize = 10.5, fontName = "Times New Roman")
addWorksheet(workbook, "Table 2")
header_style <- createStyle(textDecoration = "bold", border = "bottom",
                            borderStyle = "thin")
writeData(workbook, "Table 2", table2, headerStyle = header_style)
setColWidths(workbook, "Table 2", cols = 1, widths = 52)
setColWidths(workbook, "Table 2", cols = 2:ncol(table2), widths = 14)
setRowHeights(workbook, "Table 2", rows = 1, heights = 32)
setRowHeights(workbook, "Table 2", rows = seq_len(nrow(table2)) + 1,
              heights = 18)
addStyle(
  workbook, "Table 2",
  createStyle(wrapText = TRUE, valign = "center"),
  rows = seq_len(nrow(table2)) + 1,
  cols = seq_len(ncol(table2)), gridExpand = TRUE, stack = TRUE
)
mergeCells(workbook, "Table 2", rows = 2, cols = seq_len(ncol(table2)))
addStyle(
  workbook, "Table 2",
  createStyle(halign = "left", textDecoration = "bold",
              border = "top", borderStyle = "thin"),
  rows = 2, cols = 1, stack = TRUE
)

# Shade cells with significant differences.
for (fill_color in unique(table2_fills$fill)) {
  addStyle(
    workbook, "Table 2", createStyle(fgFill = fill_color),
    rows = table2_fills$row[table2_fills$fill == fill_color],
    cols = table2_fills$col[table2_fills$fill == fill_color],
    stack = TRUE
  )
}
addStyle(
  workbook, "Table 2",
  createStyle(border = "bottom", borderStyle = "thin"),
  rows = nrow(table2) + 1, cols = seq_len(ncol(table2)), stack = TRUE
)

writeData(
  workbook, "Table 2",
  "Note: Entries are percentages of respondents in each subgroup, whose sizes label the columns. Columns sum to approximately 100%. Percentages and tests are unweighted and describe the analytic sample, not the U.S. population. Shading uses two-sided Welch tests of each subgroup against the rest of the sample, without adjustment for multiple comparisons. Orange means the subgroup estimate is higher than the rest of the sample and purple means it is lower. The darkest shade is p < .001, the medium shade p < .01, and the lightest p < .05. Source: NSHAP Rounds 1-4 partnership history file.",
  startRow = nrow(table2) + 3, colNames = FALSE
)
mergeCells(workbook, "Table 2", rows = nrow(table2) + 3,
           cols = seq_len(ncol(table2)))
addStyle(
  workbook, "Table 2", createStyle(wrapText = TRUE, valign = "top"),
  rows = nrow(table2) + 3, cols = 1
)
setRowHeights(workbook, "Table 2", rows = nrow(table2) + 3, heights = 75)

saveWorkbook(workbook, "tables_figures/table2_partnership_history.xlsx",
             overwrite = TRUE)
