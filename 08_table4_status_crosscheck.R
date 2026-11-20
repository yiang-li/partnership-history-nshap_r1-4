# Author: Yiang Li
# Cross-tabulate partnership history and latest marital status.
library(dplyr)
library(tidyr)
library(openxlsx)

table4 <- readRDS("output/nshap_par_hisr1234.rds") |>
  distinct(su_id, part_history_11, maritlst_latest) |>
  transmute(
    `Partnership history` = factor(
      part_history_11,
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
    `Latest marital status` = factor(
      maritlst_latest,
      levels = 1:6,
      labels = c(
        "Married", "Cohabiting", "Separated", "Divorced", "Widowed",
        "Never married"
      )
    )
  ) |>
  count(`Partnership history`, `Latest marital status`) |>
  complete(
    `Partnership history`, `Latest marital status`,
    fill = list(n = 0)
  ) |>
  pivot_wider(names_from = `Latest marital status`, values_from = n) |>
  arrange(`Partnership history`) |>
  mutate(`Partnership history` = as.character(`Partnership history`))

workbook <- createWorkbook()
modifyBaseFont(workbook, fontSize = 10.5, fontName = "Times New Roman")
addWorksheet(workbook, "Table 4")
writeData(
  workbook, "Table 4", table4,
  headerStyle = createStyle(
    textDecoration = "bold", border = "bottom", borderStyle = "thin"
  )
)
setColWidths(workbook, "Table 4", cols = 1, widths = 54)
setColWidths(workbook, "Table 4", cols = 2:ncol(table4), widths = 16)
addStyle(
  workbook, "Table 4",
  createStyle(wrapText = TRUE, valign = "center"),
  rows = seq_len(nrow(table4)) + 1,
  cols = seq_len(ncol(table4)), gridExpand = TRUE, stack = TRUE
)
addStyle(
  workbook, "Table 4",
  createStyle(border = "bottom", borderStyle = "thin"),
  rows = nrow(table4) + 1, cols = seq_len(ncol(table4)), stack = TRUE
)
writeData(
  workbook, "Table 4",
  "Note: Rows are the 11-category partnership history and columns are the respondent's latest reported marital status. Latest marital status is used to reconcile current unions during cleaning, so this table is an internal consistency check, not an independent validation. Source: NSHAP Rounds 1-4 partnership history file.",
  startRow = nrow(table4) + 3, colNames = FALSE
)
mergeCells(workbook, "Table 4", rows = nrow(table4) + 3,
           cols = seq_len(ncol(table4)))
addStyle(
  workbook, "Table 4", createStyle(wrapText = TRUE, valign = "top"),
  rows = nrow(table4) + 3, cols = 1
)
setRowHeights(workbook, "Table 4", rows = nrow(table4) + 3, heights = 60)
saveWorkbook(
  workbook, "tables_figures/table4_status_crosscheck.xlsx",
  overwrite = TRUE
)
