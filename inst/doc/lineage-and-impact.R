## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(trialdiff)
lineage <- define_lineage(
  lineage_edge("ADSL.TRT01P", "ADLB.TRT01P", relationship = "groups_by"),
  lineage_edge("ADLB.AVAL", "ADLB.BASE", relationship = "derives"),
  lineage_edge("ADLB.AVAL", "ADLB.CHG", relationship = "derives"),
  lineage_edge("ADLB.AVAL", "MMRM", relationship = "models"),
  lineage_edge("MMRM", "Table_14_2_1", relationship = "reports")
)
lineage

## -----------------------------------------------------------------------------
trace_dependencies(lineage, from = "ADLB.AVAL")

## -----------------------------------------------------------------------------
trace_dependencies(lineage, from = "Table_14_2_1", direction = "upstream")

## -----------------------------------------------------------------------------
classified <- classify_changes(
  compare_cut(adsl_cut1, adsl_cut2, by = "USUBJID", dataset = "ADSL")
)
impact <- assess_impact(classified, adsl_adlb_lineage)
impact$impacts[, c("node", "node_type", "level", "depth", "requires_rerun")]

## -----------------------------------------------------------------------------
impact_policy()

## -----------------------------------------------------------------------------
impact$unlinked

## ----eval = FALSE-------------------------------------------------------------
# mc <- metacore::define_to_metacore("define.xml")
# lin <- lineage_from_metadata(mc)
# trace_dependencies(lin, from = "ADSL.TRT01P")

## ----eval = FALSE-------------------------------------------------------------
# lineage_provenance(lin)
# lineage_review(lin)

