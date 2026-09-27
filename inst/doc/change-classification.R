## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(trialdiff)
td_categories()

## -----------------------------------------------------------------------------
diff <- compare_cut(adsl_cut1, adsl_cut2, by = "USUBJID", dataset = "ADSL")
reg <- as_register(diff)
reg[, c(".change_id", "record_type", "USUBJID", "variable", "old_value",
        "new_value", "change")]

## -----------------------------------------------------------------------------
classified <- classify_changes(diff)
classified$register[, c(".change_id", "category", "category_label", "reason")]

## -----------------------------------------------------------------------------
age_rule <- td_rule(
  name = "age_change",
  label = "Age change",
  priority = 1L,
  test = function(register, context) {
    register$record_type == "modified" & register$variable == "AGE"
  },
  reason = function(register, context) {
    sprintf("Age changed for %s.", register$.subject)
  }
)

custom <- classify_changes(
  diff,
  rules = c(list(age_rule), td_default_rules())
)
custom$register$category[custom$register$variable == "AGE"]

## -----------------------------------------------------------------------------
old <- data.frame(USUBJID = c("S1", "S2"), AVAL = c(NA, 5))
new <- data.frame(USUBJID = c("S1", "S2"), AVAL = c(3, NA))
classified_missing <- classify_changes(compare_cut(old, new, by = "USUBJID",
                                                    dataset = "ADLB"))
classified_missing$modified[, c("USUBJID", "variable", "change", "category")]

