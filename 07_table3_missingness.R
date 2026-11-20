# Author: Yiang Li
# Build Table 3 for every column in the cleaned partnership history.
library(dplyr)
library(tidyr)
library(openxlsx)

partnership_history <- readRDS("output/nshap_par_hisr1234.rds")

#### Missingness by variable ####
table3 <- partnership_history |>
  transmute(
    has_mar, has_coh, mar_ended, coh_ended,
    across(everything(), is.na, .names = "missing_{.col}")
  ) |>
  pivot_longer(starts_with("missing_"), names_to = "variable",
               names_prefix = "missing_", values_to = "missing") |>
  mutate(
    logically_not_applicable = case_when(
      variable %in% c("mstatus", "beginy_est", "fstsexy", "pgender", "page",
                      "pagerel", "end_reason") ~ !has_mar & !has_coh,
      variable %in% c("marbeginm", "marbeginy", "marendm", "marendy",
                      "livebfmr") ~ !has_mar,
      variable %in% c("cobeginm", "cobeginy") ~ !has_coh,
      variable %in% c("coendm", "coendy") ~ !has_coh | (has_mar & coh_ended),
      TRUE ~ FALSE
    ),
    ongoing_relationship = case_when(
      variable %in% c("marendm", "marendy") ~ has_mar & !mar_ended,
      variable %in% c("coendm", "coendy") ~ has_coh & !coh_ended,
      variable == "end_reason" ~ (has_mar & !mar_ended) |
        (has_coh & !has_mar & !coh_ended),
      TRUE ~ FALSE
    )
  ) |>
  summarise(
    `% missing total` = round(100 * mean(missing), 2),
    `% logically not applicable` =
      round(100 * mean(missing & logically_not_applicable), 2),
    `% relationship ongoing` =
      round(100 * mean(missing & ongoing_relationship), 2),
    `% other missing or unavailable` = round(
      100 * mean(missing & !logically_not_applicable & !ongoing_relationship),
      2
    ),
    .by = variable
  ) |>
  mutate(
    Variable = sprintf(
      "%s (%s)",
      recode(
        variable,
        su_id = "Respondent ID",
        scid = "Partnership Sequence Number",
        round = "Last Partnership Update Round",
        mstatus = "Relationship Marital Status",
        marbeginm = "Marriage Begin Month",
        marbeginy = "Marriage Begin Year",
        marendm = "Marriage End Month",
        marendy = "Marriage End Year",
        livebfmr = "Lived Together Before Marriage",
        cobeginm = "Began Living Together Month",
        cobeginy = "Began Living Together Year",
        coendm = "Stopped Living Together Month",
        coendy = "Stopped Living Together Year",
        fstsexy = "Year of First Sex With This Partner",
        beginy_est = "Estimated Partnership Begin Year",
        pgender = "Partner Gender",
        page = "Partner Age",
        pagerel = "Partner Age Relative to Respondent",
        has_mar = "Marriage Record Indicator",
        has_coh = "Cohabitation Record Indicator",
        mar_ended = "Marriage Ended Indicator",
        coh_ended = "Cohabitation Ended Indicator",
        end_reason = "Partnership End Reason",
        int_month = "Interview Month",
        int_year = "Interview Year",
        maritlst_latest = "Latest Reported Marital Status",
        part_history_14 = "Partnership History, 14 Categories",
        part_history_11 = "Partnership History, 11 Categories",
        part_history_7 = "Partnership History, 7 Categories",
        n_othersex_rounds = "Other Sex Partners Reported Across Rounds",
        status_implied = "Row Implied by Latest Marital Status"
      ),
      variable
    ),
    .before = 1
  ) |>
  select(-variable)

#### Save Table 3 ####
workbook <- createWorkbook()
modifyBaseFont(workbook, fontSize = 10.5, fontName = "Times New Roman")
addWorksheet(workbook, "Table 3")
pageSetup(
  workbook, "Table 3", orientation = "landscape", paperSize = 9,
  fitToWidth = 1, fitToHeight = 1, printTitleRows = 1,
  left = 0.35, right = 0.35, top = 0.45, bottom = 0.45,
  header = 0.2, footer = 0.2
)
writeData(
  workbook, "Table 3", table3,
  headerStyle = createStyle(
    textDecoration = "bold", border = "bottom", borderStyle = "thin"
  )
)
setColWidths(workbook, "Table 3", cols = 1, widths = 54)
setColWidths(workbook, "Table 3", cols = 2:ncol(table3), widths = 24)
setRowHeights(workbook, "Table 3", rows = 1, heights = 45)
addStyle(
  workbook, "Table 3",
  createStyle(wrapText = TRUE, valign = "center"),
  rows = seq_len(nrow(table3)) + 1,
  cols = seq_len(ncol(table3)), gridExpand = TRUE, stack = TRUE
)
addStyle(
  workbook, "Table 3", createStyle(numFmt = "0.00"),
  rows = seq_len(nrow(table3)) + 1, cols = 2:ncol(table3),
  gridExpand = TRUE, stack = TRUE
)
addStyle(
  workbook, "Table 3",
  createStyle(border = "bottom", borderStyle = "thin"),
  rows = nrow(table3) + 1, cols = seq_len(ncol(table3)), stack = TRUE
)
writeData(
  workbook, "Table 3",
  paste0(
    "Note: All percentages equal the number of records in the named category divided by ",
    format(nrow(partnership_history), big.mark = ","),
    " records, multiplied by 100. ",
    "Logically not applicable means that the field does not apply to that record type, such as a marriage date on a cohabitation-only row. Relationship ongoing means that an end field is missing because the relevant marriage or cohabitation was still intact at the latest update. ",
    "Other missing or unavailable means that the field applies, but its missing value is not explained by an ongoing relationship and no usable value is available. The three categories are mutually exclusive and sum to total missingness, apart from rounding. ",
    "Source: NSHAP Rounds 1-4 partnership history file."
  ),
  startRow = nrow(table3) + 3, colNames = FALSE
)
mergeCells(workbook, "Table 3", rows = nrow(table3) + 3,
           cols = seq_len(ncol(table3)))
addStyle(
  workbook, "Table 3", createStyle(wrapText = TRUE, valign = "top"),
  rows = nrow(table3) + 3, cols = 1
)
setRowHeights(workbook, "Table 3", rows = nrow(table3) + 3, heights = 90)
saveWorkbook(
  workbook, "tables_figures/table3_missingness.xlsx", overwrite = TRUE
)
