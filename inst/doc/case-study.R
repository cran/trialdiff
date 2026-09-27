## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
has_pva <- requireNamespace("pharmaverseadam", quietly = TRUE)

## ----eval = has_pva-----------------------------------------------------------
library(trialdiff)

## ----eval = has_pva-----------------------------------------------------------
params <- c("ALT", "AST", "CREAT")
adsl_full <- pharmaverseadam::adsl
adlb_full <- subset(pharmaverseadam::adlb, PARAMCD %in% params)

subjects <- unique(as.character(adsl_full$USUBJID))
new_subjects <- tail(subjects, 2)

adsl_cut1 <- adsl_full[!adsl_full$USUBJID %in% new_subjects, ]
adlb_cut1 <- adlb_full[!adlb_full$USUBJID %in% new_subjects, ]
adsl_cut2 <- adsl_full
adlb_cut2 <- adlb_full

trt_subject <- adsl_cut2$USUBJID[which(adsl_cut2$TRT01P == "Placebo")[1]]
for (v in c("TRT01P", "TRT01A")) {
  adsl_cut2[[v]][adsl_cut2$USUBJID == trt_subject] <- "Xanomeline Low Dose"
}
for (v in c("TRT01P", "TRTP")) {
  adlb_cut2[[v]][adlb_cut2$USUBJID == trt_subject] <- "Xanomeline Low Dose"
}

i <- which(adlb_cut2$USUBJID == trt_subject & adlb_cut2$PARAMCD == "ALT" &
             adlb_cut2$AVISIT == "Week 4")[1]
adlb_cut2$AVAL[i] <- adlb_cut2$AVAL[i] + 7
adlb_cut2$CHG[i] <- adlb_cut2$AVAL[i] - adlb_cut2$BASE[i]

j <- which(adlb_cut2$PARAMCD == "AST" & adlb_cut2$AVISIT == "Week 2")[1]
adlb_cut2$AVAL[j] <- NA_real_
adlb_cut2$CHG[j] <- NA_real_

c(adsl = nrow(adsl_cut2) - nrow(adsl_cut1), adlb = nrow(adlb_cut2) - nrow(adlb_cut1))

## ----eval = has_pva-----------------------------------------------------------
metadata <- list(
  ds_spec = data.frame(
    dataset = c("ADSL", "ADLB"),
    label = c("Subject-Level Analysis", "Laboratory Analysis")
  ),
  ds_vars = data.frame(
    dataset = c("ADSL", "ADSL", "ADSL", "ADLB", "ADLB", "ADLB", "ADLB", "ADLB"),
    variable = c("USUBJID", "TRT01P", "SAFFL", "USUBJID", "PARAMCD",
                 "TRT01P", "AVAL", "CHG")
  ),
  value_spec = data.frame(
    dataset = c("ADSL", "ADLB", "ADLB", "ADLB", "ADLB"),
    variable = c("TRT01P", "TRT01P", "AVAL", "BASE", "CHG"),
    derivation_id = c("MT.ADSL.TRT01P", "MT.ADLB.TRT01P", "MT.ADLB.AVAL",
                      "MT.ADLB.BASE", "MT.ADLB.CHG"),
    where = c(NA, NA, "PARAMCD == 'ALT'", NA, NA)
  ),
  derivations = data.frame(
    derivation_id = c("MT.ADSL.TRT01P", "MT.ADLB.TRT01P", "MT.ADLB.AVAL",
                      "MT.ADLB.BASE", "MT.ADLB.CHG"),
    derivation = c("DM.ARM", "ADSL.TRT01P", "LB.LBSTRESN", "ADLB.AVAL",
                   "AVAL - BASE")
  )
)

registry <- output_registry(
  td_output("Lab_Summary_By_Treatment",
            depends_on = c("ADLB.TRT01P", "ADLB.AVAL"),
            type = "analysis", relationship = "summarises"),
  td_output("MMRM", depends_on = c("ADLB.AVAL", "ADLB.CHG")),
  td_output("Table_14_2_1", depends_on = "Lab_Summary_By_Treatment",
            type = "output"),
  td_output("Table_14_2_2", depends_on = "MMRM", type = "output")
)

lineage <- lineage_from_metadata(metadata, overrides = registry)
lineage

## ----eval = has_pva-----------------------------------------------------------
lineage_provenance(lineage)[, c("from", "to", "relationship", "source")]

## ----eval = has_pva-----------------------------------------------------------
adsl_diff <- compare_cut(adsl_cut1, adsl_cut2, by = "USUBJID",
                         dataset = "ADSL") |>
  classify_changes()
adlb_diff <- compare_cut(adlb_cut1, adlb_cut2,
                         by = c("USUBJID", "PARAMCD", "AVISIT"),
                         dataset = "ADLB") |>
  classify_changes()

knitr::kable(table(adsl_diff$register$category_label),
             col.names = c("Category", "ADSL"))
knitr::kable(table(adlb_diff$register$category_label),
             col.names = c("Category", "ADLB"))

## ----eval = has_pva-----------------------------------------------------------
adlb_impact <- assess_impact(adlb_diff, lineage)
adlb_impact$impacts[, c("node", "node_type", "level", "depth", "requires_rerun")]

## ----eval = has_pva-----------------------------------------------------------
report <- report_diff(adsl_diff,
                      impact = assess_impact(adsl_diff, lineage),
                      output = "list")
report$data$review_items

