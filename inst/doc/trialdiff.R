## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(trialdiff)
str(adsl_cut1, max.level = 1)

## -----------------------------------------------------------------------------
diff <- compare_cut(
  old = adsl_cut1,
  new = adsl_cut2,
  by = "USUBJID",
  dataset = "ADSL"
)
diff

## -----------------------------------------------------------------------------
diff$modified[, c("USUBJID", "variable", "old_value", "new_value", "change")]

## -----------------------------------------------------------------------------
classified <- classify_changes(diff)
table(classified$register$category_label)

## -----------------------------------------------------------------------------
classified$register$reason[classified$register$category == "treatment_assignment_change"]

## -----------------------------------------------------------------------------
lineage <- define_lineage(
  lineage_edge("ADSL.TRT01P", "ADLB.TRT01P", relationship = "groups_by"),
  lineage_edge("ADLB.AVAL", "MMRM", relationship = "models"),
  lineage_edge("MMRM", "Table_14_2_1", relationship = "reports")
)
trace_dependencies(lineage, from = "ADSL.TRT01P")

## -----------------------------------------------------------------------------
impact <- assess_impact(classified, adsl_adlb_lineage)
impact$impacts[, c("node", "node_type", "level", "requires_rerun")]

## -----------------------------------------------------------------------------
report <- report_diff(diff, impact = impact, output = "list")
report$data$review_items

