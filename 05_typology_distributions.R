# Author: Yiang Li
# Build the 7- and 14-category typology distributions behind Figures A1 and A2.
library(dplyr)
library(tidyr)
library(openxlsx)

typology_data <- readRDS("../tmp/nshap_core.rds") |>
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
    part_history_7, part_history_14
  )

#### Typology distributions ####
typology_rows <- tibble(
  typology = rep(c("part_history_7", "part_history_14"), c(7, 14)),
  category = c(
    "Continuously married", "Remarried", "Divorced/separated", "Widowed",
    "Currently cohabiting", "Formerly cohabited", "Never partnered",
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
) |>
  inner_join(
    typology_data |>
      pivot_longer(c(part_history_7, part_history_14),
                   names_to = "typology", values_to = "history"),
    by = "typology", relationship = "many-to-many"
  ) |>
  mutate(value = as.integer(history == category)) |>
  filter(!is.na(value)) |>
  summarise(
    across(Overall:`Baby Boom`, ~ if_else(
      any(.x %in% TRUE),
      sprintf("%.2f%%", 100 * mean(value[.x %in% TRUE] == 1)),
      NA
    )),
    .by = c(typology, category)
  ) |>
  mutate(Measure = paste0(" ", category), .before = 1)

typology_distributions <- bind_rows(
  tibble(Measure = c("Partnership History Typologies",
                     "Panel A. 7 categories")),
  filter(typology_rows, typology == "part_history_7"),
  tibble(Measure = "Panel B. 14 categories"),
  filter(typology_rows, typology == "part_history_14")
) |>
  select(-typology, -category) |>
  # Add subgroup sizes to the column names.
  rename_with(
    ~ sprintf("%s\n(n = %s)", .x,
              format(colSums(typology_data[.x], na.rm = TRUE),
                     big.mark = ",", trim = TRUE)),
    Overall:`Baby Boom`
  )

#### Save the typology distributions ####
workbook <- createWorkbook()
modifyBaseFont(workbook, fontSize = 10.5, fontName = "Times New Roman")
addWorksheet(workbook, "Typology distributions")
header_style <- createStyle(textDecoration = "bold", border = "bottom",
                            borderStyle = "thin")
writeData(workbook, "Typology distributions", typology_distributions,
          headerStyle = header_style)
setColWidths(workbook, "Typology distributions", cols = 1, widths = 55)
setColWidths(workbook, "Typology distributions",
             cols = 2:ncol(typology_distributions), widths = 14)
setRowHeights(workbook, "Typology distributions", rows = 1, heights = 32)
setRowHeights(workbook, "Typology distributions",
              rows = seq_len(nrow(typology_distributions)) + 1, heights = 18)
addStyle(
  workbook, "Typology distributions",
  createStyle(wrapText = TRUE, valign = "center"),
  rows = seq_len(nrow(typology_distributions)) + 1,
  cols = seq_len(ncol(typology_distributions)), gridExpand = TRUE, stack = TRUE
)

section_rows <- which(grepl("^(Partnership History Typologies|Panel [AB]\\.)",
                            typology_distributions$Measure))
for (section_row in section_rows) {
  mergeCells(workbook, "Typology distributions", rows = section_row + 1,
             cols = seq_len(ncol(typology_distributions)))
  addStyle(
    workbook, "Typology distributions",
    createStyle(halign = "left", textDecoration = "bold",
                border = "top", borderStyle = "thin"),
    rows = section_row + 1, cols = 1, stack = TRUE
  )
}

addStyle(
  workbook, "Typology distributions",
  createStyle(border = "bottom", borderStyle = "thin"),
  rows = nrow(typology_distributions) + 1,
  cols = seq_len(ncol(typology_distributions)), stack = TRUE
)

saveWorkbook(workbook,
             "tables_figures/typology_distributions.xlsx",
             overwrite = TRUE)
